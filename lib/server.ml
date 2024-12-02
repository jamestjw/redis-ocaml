open Base
module StringMap = Stdlib.Map.Make (String)

type t = { mailbox : (Cmd.t * Response.t Lwt_mvar.t) Lwt_mvar.t }

type state =
  { store : (string * float option) StringMap.t
  ; configs : string StringMap.t
  }

let mk_state ~rdb_dir ~rdb_filename =
  { store = StringMap.empty
  ; configs =
      StringMap.empty
      |> StringMap.add "dir" rdb_dir
      |> StringMap.add "dbfilename" rdb_filename
  }
;;

let mk () = { mailbox = Lwt_mvar.create_empty () }

let get { store; _ } key =
  let filter_expired (v, expiry) =
    match expiry with
    | Some timeout when Float.compare (Unix.time ()) timeout > 0 -> None
    | _ -> Some v
  in
  StringMap.find_opt key store |> Option.bind ~f:filter_expired
;;

let set ({ store; _ } as state) key value expiry =
  { state with store = StringMap.add key (value, expiry) store }
;;

let get_config { configs; _ } key = StringMap.find_opt key configs

let handle_message cmd state =
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
    let expiry =
      match set_timeout with
      | None -> None
      | Some (PX ms) -> Some (Unix.time () +. (Float.of_int ms /. 1000.0))
      | Some (EX secs) -> Some (Unix.time () +. Float.of_int secs)
    in
    Response.SIMPLE "OK", set state set_key set_value expiry
  | Cmd.GET_CONFIG keys ->
    let res =
      List.map ~f:(fun k -> get_config state k |> Option.map ~f:(fun v -> [ k; v ])) keys
      |> List.filter_map ~f:(fun x -> x)
      |> Stdlib.List.flatten
      |> List.map ~f:(fun e -> Response.BULK e)
    in
    Response.ARRAY res, state
  | Cmd.INVALID s -> Response.ERR s, state
;;

let run { mailbox } ~rdb_dir ~rdb_filename =
  let rec inner context =
    let%lwt cmd, response_mailbox = Lwt_mvar.take mailbox in
    let resp, context = handle_message cmd context in
    Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
    inner context
  in
  inner (mk_state ~rdb_dir ~rdb_filename)
;;

let execute_cmd cmd { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox (cmd, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
