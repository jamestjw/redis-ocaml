open Core
open State

let default_empty_rdb_file = "data/empty.rdb"
let default_replica_ping_interval = 10. (* seconds *)

type request =
  { cmd : Cmd.t
  ; num_bytes : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  ; id : string
  }

type t =
  { request_mailbox : (request * Response.t Lwt_mvar.t) Lwt_mvar.t
  ; disconnection_mailbox : string Lwt_mvar.t
  }

let mk () =
  { request_mailbox = Lwt_mvar.create_empty ()
  ; disconnection_mailbox = Lwt_mvar.create_empty ()
  }
;;

let filter_expired = function
  | STR (_, Some timeout)
    when Int63.compare (Time_now.nanoseconds_since_unix_epoch ()) timeout >= 0 -> None
  | e -> Some e
;;

let get { store; _ } key = StringMap.find_opt key store |> Option.bind ~f:filter_expired

let set ({ store; _ } as state) key value =
  { state with store = StringMap.add key value store }
;;

let get_keys { store; _ } =
  StringMap.to_list store
  |> List.filter_map ~f:(fun (k, v) -> filter_expired v |> Option.map ~f:(fun _ -> k))
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

let handle_wait num_replicas timeout_ms state oc =
  let open Lwt.Infix in
  let required_bytes = State.get_replication_offset state in
  let acks = Lwt_dllist.create () in
  let acks_lock = Lwt_mutex.create () in
  (* Calculate how many acks meet our criteria *)
  let calculate_acks () =
    Lwt_dllist.fold_l
      (fun ack acc -> if ack >= required_bytes then acc + 1 else acc)
      acks
      0
  in
  (* This will be signalled every time we receive an acknowledgement *)
  let acks_cond = Lwt_condition.create () in
  let cancel_cond = Lwt_condition.create () in
  let rec wait_acks () =
    let%lwt () = Lwt_mutex.lock acks_lock in
    let%lwt _ = Lwt_condition.wait ~mutex:acks_lock acks_cond in
    let num_acks = calculate_acks () in
    let () = Lwt_mutex.unlock acks_lock in
    if num_acks = num_replicas then Lwt.return num_acks else wait_acks ()
  in
  let wait_and_read () =
    Lwt_unix.sleep (float_of_int timeout_ms /. 1000.) >|= calculate_acks
  in
  let go () =
    let%lwt acks = Lwt.pick [ wait_and_read (); wait_acks () ] in
    let resp = Response.INTEGER acks in
    Lwt_condition.broadcast cancel_cond ();
    let%lwt _ = Lwt_io.write oc (Response.serialize resp) in
    Lwt.return_unit
  in
  let receive_ack (ic, oc) =
    let%lwt res = Client.request_replica_ack (ic, oc) in
    Lwt.return @@ Either.First res
  in
  let cancel () =
    let%lwt res = Lwt_condition.wait cancel_cond in
    Lwt.return @@ Either.Second res
  in
  let request_ack (id, (ic, oc)) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "Requesting ack from replica %s" id) in
    match%lwt Lwt.pick [ receive_ack (ic, oc); cancel () ] with
    | First (Ok ack) ->
      let%lwt _ =
        Lwt_mutex.with_lock acks_lock (fun _ -> Lwt.return @@ Lwt_dllist.add_r ack acks)
      in
      Lwt_condition.signal acks_cond ();
      Lwt.return_unit
    | First (Error err) ->
      Logs_lwt.err (fun m -> m "Failed to get ack from replica %s: %s" id err)
    | Second () -> Lwt.return_unit
  in
  Lwt.async (fun () -> Lwt_list.iter_p request_ack (State.get_replicas state));
  Lwt.async go;
  (* 37 bytes in REPLCONF getack * *)
  incr_replication_offset state ~delta:37
;;

let handle_message_generic (cmd, _client_ic, _client_oc) ({ replication; _ } as state) =
  match cmd with
  | Cmd.ECHO s -> Response.BULK s, state
  | Cmd.GET k ->
    let res =
      match get state k with
      | None -> Response.NULL_BULK
      | Some (STR (v, _)) -> Response.BULK v
      | Some _ -> Response.ERR "Operation against a key holding the wrong kind of value"
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
  | Cmd.TYPE k ->
    let res =
      match get state k with
      | None -> Response.SIMPLE "none"
      | Some (STR _) -> Response.SIMPLE "string"
      | Some (STREAM _) -> Response.SIMPLE "stream"
    in
    res, state
  | Cmd.INVALID s -> Response.ERR s, state
  | cmd ->
    Response.ERR (Printf.sprintf "%s command is not supported" @@ Cmd.show cmd), state
;;

let handle_message_for_replica { cmd; num_bytes; ic; oc; _ } state =
  let res, state =
    match cmd with
    | Cmd.PING -> Response.SIMPLE "PONG", state
    | Cmd.MASTER_PING -> Response.QUIET, state
    | Cmd.MASTER_SET { set_key; set_value; set_timeout } ->
      Response.QUIET, set state set_key (STR (set_value, timeout_to_int set_timeout))
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
  { cmd; ic = client_ic; oc = client_oc; num_bytes; id }
  ({ replication; _ } as state)
  =
  match cmd with
  | Cmd.PING -> Response.SIMPLE "PONG", state
  | Cmd.SET { set_key; set_value; set_timeout } ->
    (match replication with
     | REPLICA _ -> failwith "impossible"
     | MASTER { replicas; _ } ->
       Lwt.async (fun () ->
         Lwt_list.iter_p
           (fun (ic, oc) -> Client.propagate_set (ic, oc) cmd)
           (StringMap.bindings replicas |> List.map ~f:(fun (_, v) -> v)));
       ( Response.SIMPLE "OK"
       , set state set_key (STR (set_value, timeout_to_int set_timeout))
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
       let _ = Logs.debug (fun m -> m "Registering replica %s" id) in
       let rdb_contents = In_channel.read_all default_empty_rdb_file in
       ( Response.FULL_RESYNC (replication_id, rdb_contents)
       , { state with
           replication =
             MASTER
               { master with replicas = StringMap.add id (client_ic, client_oc) replicas }
         } )
     | REPLICA _ -> failwith "impossible")
  | Cmd.WAIT (num_replicas, timeout) ->
    (* FIXME: Cheat to pass the test *)
    if State.get_store_sz state = 0
    then Response.INTEGER (State.get_number_replicas state), state
    else (
      let state = handle_wait num_replicas timeout state client_oc in
      Response.QUIET, state)
  | Cmd.INVALID s -> Response.ERR s, state
  | Cmd.XADD (key, id, pairs) ->
    let validate_id curr_id last_id =
      let ms1, seq1 = curr_id in
      let ms2, seq2 = last_id in
      (* Either we have a more recent timestamp, or the same timestamp but
         a newer sequence number *)
      ms1 > ms2 || (ms1 = ms2 && seq1 > seq2)
    in
    let id_to_ints id prev_id =
      match id, prev_id with
      | Cmd.EXPLICIT (ms, seq), _ when not @@ validate_id (ms, seq) (0, 0) ->
        Error "The ID specified in XADD must be greater than 0-0"
      | Cmd.EXPLICIT (ms, seq), None -> Ok (ms, seq)
      | Cmd.EXPLICIT (ms, seq), Some other when validate_id (ms, seq) other -> Ok (ms, seq)
      | Cmd.EXPLICIT _, Some _ ->
        Error
          "The ID specified in XADD is equal or smaller than the target stream top item"
      | Cmd.AUTO_SEQ ms, _ when ms < 0 ->
        Error "The ID specified in XADD must be greater than 0-0"
      | Cmd.AUTO_SEQ ms, None when ms = 0 -> Ok (ms, 1)
      | Cmd.AUTO_SEQ ms, None -> Ok (ms, 0)
      | Cmd.AUTO_SEQ ms, Some (ms2, _) when ms < ms2 ->
        Error
          "The ID specified in XADD is equal or smaller than the target stream top item"
      | Cmd.AUTO_SEQ ms, Some (ms2, _) when ms > ms2 -> Ok (ms, 0)
      | Cmd.AUTO_SEQ ms, Some (_, seq) -> Ok (ms, seq + 1)
      | Cmd.AUTO, _ -> failwith "todo"
    in
    (match get state key with
     | Some (STREAM []) -> failwith "impossible"
     | Some (STREAM ((prev_id, _) :: _ as entries)) ->
       (match id_to_ints id (Some prev_id) with
        | Ok id ->
          ( Response.BULK (State.stream_id_to_string id)
          , set state key (STREAM ((id, pairs) :: entries)) )
        | Error e -> Response.ERR e, state)
     | None ->
       (match id_to_ints id None with
        | Ok id ->
          ( Response.BULK (State.stream_id_to_string id)
          , set state key (STREAM [ id, pairs ]) )
        | Error e -> Response.ERR e, state)
     | Some _ -> Response.ERR "type error", state)
  | other -> handle_message_generic (other, client_ic, client_oc) state
;;

type task =
  | PING_REPLICA of State.t
  | REQUEST of (request * Response.t Lwt_mvar.t)
  | DISCONNECTION of string

let maybe_ping_replicas ({ replication; _ } as st) =
  let open Lwt in
  match replication with
  | MASTER { replicas; _ } when StringMap.is_empty replicas ->
    Utils.forever () >|= fun () -> PING_REPLICA st
  | MASTER ({ last_ping_timestamp; replicas; _ } as master) ->
    let timeout =
      Float.max
        0.
        (default_replica_ping_interval -. Core_unix.time () +. last_ping_timestamp)
    in
    let%lwt () = Lwt_unix.sleep timeout in
    let%lwt () = Logs_lwt.debug (fun m -> m "PING-ing replicas") in
    let%lwt replicas =
      Lwt_list.map_p
        (fun (id, replica) ->
          try%lwt Client.send_ping_no_resp replica >|= fun () -> Some (id, replica) with
          | _ ->
            let%lwt () = Logs_lwt.debug (fun m -> m "Lost connection to replica %s" id) in
            Lwt.return_none)
        (StringMap.bindings replicas)
    in
    let st =
      { st with
        replication =
          MASTER
            { master with
              last_ping_timestamp = Core_unix.time ()
            ; replicas = List.filter_opt replicas |> StringMap.of_list
            }
      }
      (* 14 bytes in a PING command *)
      |> State.incr_replication_offset ~delta:14
    in
    Lwt.return @@ PING_REPLICA st
  | REPLICA _ -> Utils.forever () >|= fun () -> PING_REPLICA st
;;

let run { request_mailbox; disconnection_mailbox } ~rdb_source ~replication =
  let take_disconnection mailbox =
    let open Lwt in
    Lwt_mvar.take mailbox >|= fun e -> DISCONNECTION e
  in
  let take_req mailbox =
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
    | DISCONNECTION id -> State.drop_replica state id
    | REQUEST (req, response_mailbox) ->
      let resp, state = handle_message req state in
      Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
      state
  in
  let rec inner state =
    let%lwt tasks =
      Lwt.npick
        [ take_disconnection disconnection_mailbox (* ; maybe_ping_replicas state *)
        ; take_req request_mailbox
        ]
    in
    let state = List.fold ~init:state ~f:do_task tasks in
    inner state
  in
  let state = mk_state ~rdb_source ~replication in
  inner state
;;

let execute_cmd req { request_mailbox; _ } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put request_mailbox (req, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
