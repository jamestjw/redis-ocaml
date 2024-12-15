open Core
open State

let default_empty_rdb_file = "data/empty.rdb"

type t =
  { mailbox :
      ((Cmd.t * Lwt_io.input_channel * Lwt_io.output_channel) * Response.t Lwt_mvar.t)
        Lwt_mvar.t
  }

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

let handle_message (cmd, client_ic, client_oc) ({ replication; _ } as state) =
  match cmd with
  | Cmd.PING -> Response.SIMPLE "PONG", state
  | Cmd.ECHO s -> Response.BULK s, state
  | Cmd.GET k ->
    let res =
      match get state k with
      | None -> Response.NULL_BULK
      | Some v -> Response.BULK v
    in
    res, state
  | Cmd.SET { set_key; set_value; set_timeout } ->
    (match replication with
     | REPLICA _ -> Response.ERR "cannot set on replica", state
     | MASTER { replicas; _ } ->
       Lwt.async (fun () ->
         Lwt_list.iter_p (fun (ic, oc) -> Client.propagate_set (ic, oc) cmd) replicas);
       Response.SIMPLE "OK", set state set_key set_value (timeout_to_int set_timeout))
  | Cmd.MASTER_SET { set_key; set_value; set_timeout } ->
    Response.QUIET, set state set_key set_value (timeout_to_int set_timeout)
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
  (* TODO: actually do something with these two *)
  | Cmd.REPL_CONF_PORT _ -> Response.SIMPLE "OK", state
  | Cmd.REPL_CONF_CAPA _ -> Response.SIMPLE "OK", state
  | Cmd.REPL_CONF_GET_ACK _ ->
    Response.strs_to_bulk_array [ "REPLCONF"; "ACK"; "0" ], state
  (* TODO: complete this *)
  | Cmd.PSYNC _ ->
    (match replication with
     | MASTER ({ replication_id; replicas; _ } as master) ->
       let rdb_contents = In_channel.read_all default_empty_rdb_file in
       ( Response.FULL_RESYNC (replication_id, rdb_contents)
       , { state with
           replication =
             MASTER { master with replicas = (client_ic, client_oc) :: replicas }
         } )
     | REPLICA _ -> Response.ERR "'PSYNC' not supported by replicas", state)
  | Cmd.INVALID s -> Response.ERR s, state
;;

(* | cmd -> *)
(*   Response.ERR (Printf.sprintf "%s command is not supported" @@ Cmd.show cmd), state *)

let run { mailbox } ~rdb_source ~replica_of =
  let rec inner context =
    let%lwt cmd, response_mailbox = Lwt_mvar.take mailbox in
    (* TODO: use a different handle message function depending on whether we are
       a master or replica node *)
    let resp, context = handle_message cmd context in
    Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
    inner context
  in
  let state = mk_state ~rdb_source ~replica_of in
  inner state
;;

let execute_cmd (cmd, ic, oc) { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox ((cmd, ic, oc), response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
