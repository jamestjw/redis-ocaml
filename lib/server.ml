open Base
module StringMap = Stdlib.Map.Make (String)

type t = { mailbox : (Cmd.t * Response.t Lwt_mvar.t) Lwt_mvar.t }
type state = { store : (string * float option) StringMap.t }

let mk_state () = { store = StringMap.empty }
let mk () = { mailbox = Lwt_mvar.create_empty () }

let get { store } key =
  let filter_expired (v, expiry) =
    match expiry with
    | Some timeout when Float.compare (Unix.time ()) timeout > 0 -> None
    | _ -> Some v
  in
  StringMap.find_opt key store |> Option.bind ~f:filter_expired
;;

let set { store } key value expiry = { store = StringMap.add key (value, expiry) store }

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
;;

let run { mailbox } () =
  let rec inner context =
    let%lwt cmd, response_mailbox = Lwt_mvar.take mailbox in
    let resp, context = handle_message cmd context in
    Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
    inner context
  in
  inner (mk_state ())
;;

let execute_cmd cmd { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox (cmd, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
