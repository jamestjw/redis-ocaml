open Core
open State

let default_empty_rdb_file = "data/empty.rdb"
let default_replica_ping_interval = 10. (* seconds *)

type request =
  { cmd : Cmd.t
  ; num_bytes : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

type t = { mailbox : (request * Response.t Lwt_mvar.t) Lwt_mvar.t }

let mk () = { mailbox = Lwt_mvar.create_empty () }

let filter_expired (v, expiry) =
  match expiry with
  | Some timeout
    when Int63.compare (Time_now.nanoseconds_since_unix_epoch ()) timeout >= 0 -> None
  | _ -> Some v
;;

let get { store; _ } key = StringMap.find_opt key store |> Option.bind ~f:filter_expired

let set ({ store; _ } as state) key value expiry =
  { state with store = StringMap.add key (value, expiry) store }
;;

let get_keys { store; _ } =
  StringMap.to_list store
  |> List.filter_map ~f:(fun (k, (_, expiry)) -> filter_expired (k, expiry))
;;

let get_config { configs; _ } key = StringMap.find_opt key configs

let fetch_replication_info replication _ =
  let kv_pairs =
    match replication with
    | MASTER { replication_id; offset; _ } ->
      [ "role", "master"
      ; "master_replid", replication_id
      ; "master_repl_offset", string_of_int offset
      ]
    | REPLICA _ -> [ "role", "slave" ]
  in
  kv_pairs
  |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s:%s" k v)
  |> String.concat ~sep:"\r\n"
;;

let timeout_to_int = function
  | None -> None
  | Some (Cmd.PX ms) ->
    Some Int63.(Time_now.nanoseconds_since_unix_epoch () + ms_to_ns ms)
  | Some (Cmd.EX secs) ->
    Some Int63.(Time_now.nanoseconds_since_unix_epoch () + s_to_ns secs)
;;

let handle_message_generic (cmd, _client_ic, _client_oc) ({ replication; _ } as state) =
  match cmd with
  | Cmd.ECHO s -> Response.BULK s, state
  | Cmd.GET k ->
    let res =
      match get state k with
      | None -> Response.NULL_BULK
      | Some v -> Response.BULK v
    in
    res, state
  | Cmd.GET_CONFIG keys ->
    let res =
      List.map ~f:(fun k -> get_config state k |> Option.map ~f:(fun v -> [ k; v ])) keys
      |> List.filter_map ~f:(fun x -> x)
      |> Stdlib.List.flatten
      |> List.map ~f:(fun e -> Response.BULK e)
    in
    Response.ARRAY res, state
  | Cmd.KEYS query ->
    let matcher = Glob.glob_to_matcher query in
    let keys =
      get_keys state
      |> List.filter_map ~f:(fun e -> if matcher e then Some (Response.BULK e) else None)
    in
    Response.ARRAY keys, state
  | Cmd.INFO args -> Response.BULK (fetch_replication_info replication args), state
  | Cmd.INVALID s -> Response.ERR s, state
  | cmd ->
    Response.ERR (Printf.sprintf "%s command is not supported" @@ Cmd.show cmd), state
;;

let handle_message_for_replica { cmd; num_bytes; ic; oc } state =
  let res, state =
    match cmd with
    | Cmd.PING -> Response.SIMPLE "PONG", state
    | Cmd.MASTER_PING -> Response.QUIET, state
    | Cmd.MASTER_SET { set_key; set_value; set_timeout } ->
      Response.QUIET, set state set_key set_value (timeout_to_int set_timeout)
    | Cmd.REPL_CONF_GET_ACK _ ->
      ( Response.strs_to_bulk_array
          [ "REPLCONF"; "ACK"; string_of_int @@ State.get_replication_offset state ]
      , state )
    | Cmd.INVALID s -> Response.ERR s, state
    | other -> handle_message_generic (other, ic, oc) state
  in
  res, incr_replication_offset state ~delta:num_bytes
;;

let handle_message_for_master
  { cmd; ic = client_ic; oc = client_oc; num_bytes }
  ({ replication; _ } as state)
  =
  match cmd with
  | Cmd.PING -> Response.SIMPLE "PONG", state
  | Cmd.SET { set_key; set_value; set_timeout } ->
    (match replication with
     | REPLICA _ -> failwith "impossible"
     | MASTER { replicas; _ } ->
       Lwt.async (fun () ->
         Lwt_list.iter_p (fun (ic, oc) -> Client.propagate_set (ic, oc) cmd) replicas);
       ( Response.SIMPLE "OK"
       , set state set_key set_value (timeout_to_int set_timeout)
         |> incr_replication_offset ~delta:num_bytes ))
  | Cmd.GET_CONFIG keys ->
    let res =
      List.map ~f:(fun k -> get_config state k |> Option.map ~f:(fun v -> [ k; v ])) keys
      |> List.filter_map ~f:(fun x -> x)
      |> Stdlib.List.flatten
      |> List.map ~f:(fun e -> Response.BULK e)
    in
    Response.ARRAY res, state
  (* TODO: actually do something with these two *)
  | Cmd.REPL_CONF_PORT _ -> Response.SIMPLE "OK", state
  | Cmd.REPL_CONF_CAPA _ -> Response.SIMPLE "OK", state
  | Cmd.PSYNC _ ->
    (match replication with
     | MASTER ({ replication_id; replicas; _ } as master) ->
       let rdb_contents = In_channel.read_all default_empty_rdb_file in
       ( Response.FULL_RESYNC (replication_id, rdb_contents)
       , { state with
           replication =
             MASTER { master with replicas = (client_ic, client_oc) :: replicas }
         } )
     | REPLICA _ -> failwith "impossible")
  | Cmd.WAIT (_num_replicas, _timeout) ->
    (* Response.INTEGER (State.get_number_replicas state), state *)
    Response.QUIET, state
  | Cmd.INVALID s -> Response.ERR s, state
  | other -> handle_message_generic (other, client_ic, client_oc) state
;;

type task =
  | PING_REPLICA of State.t
  | REQUEST of (request * Response.t Lwt_mvar.t)

let maybe_ping_replicas ({ replication; _ } as st) =
  let open Lwt in
  match replication with
  | MASTER { replicas; _ } when List.is_empty replicas ->
    Utils.forever () >|= fun () -> PING_REPLICA st
  | MASTER ({ last_ping_timestamp; replicas; _ } as master) ->
    let timeout =
      Float.max
        0.
        (default_replica_ping_interval -. Core_unix.time () +. last_ping_timestamp)
    in
    let%lwt () = Lwt_unix.sleep timeout in
    let%lwt () = Logs_lwt.debug (fun m -> m "PING-ing replicas") in
    Lwt.async (fun () ->
      (* TODO: if ping fails, we should assume the replica is dead and pop it from
         the list *)
      Lwt_list.iter_p (fun (ic, oc) -> Client.send_ping_no_resp (ic, oc)) replicas);
    let st =
      { st with
        replication = MASTER { master with last_ping_timestamp = Core_unix.time () }
      }
      |> State.incr_replication_offset ~delta:14
      (* 14 bytes in a PING command *)
    in
    Lwt.return @@ PING_REPLICA st
  | REPLICA _ -> Utils.forever () >|= fun () -> PING_REPLICA st
;;

let run { mailbox } ~rdb_source ~replication =
  let take mailbox =
    let open Lwt in
    Lwt_mvar.take mailbox >|= fun e -> REQUEST e
  in
  let handle_message =
    match replication with
    | State.MASTER _ -> handle_message_for_master
    | State.REPLICA _ -> handle_message_for_replica
  in
  let do_task state task =
    match task with
    | PING_REPLICA state -> state
    | REQUEST (req, response_mailbox) ->
      let resp, state = handle_message req state in
      Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
      state
  in
  let rec inner state =
    let%lwt tasks = Lwt.npick [ maybe_ping_replicas state; take mailbox ] in
    let state = List.fold ~init:state ~f:do_task tasks in
    (* let%lwt req, response_mailbox = Lwt_mvar.take mailbox in *)
    (* let resp, state = handle_message req state in *)
    (* Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp); *)
    inner state
  in
  let state = mk_state ~rdb_source ~replication in
  inner state
;;

let execute_cmd req { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox (req, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
