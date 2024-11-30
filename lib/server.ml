module StringMap = Map.Make (String)

type t = { mailbox : (Cmd.t * Response.t Lwt_mvar.t) Lwt_mvar.t }
type state = { store : string StringMap.t }

let mk_state () = { store = StringMap.empty }
let mk () = { mailbox = Lwt_mvar.create_empty () }
let get { store } key = StringMap.find_opt key store
let set { store } key value = { store = StringMap.add key value store }

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
  | Cmd.SET (k, v) -> Response.SIMPLE "OK", set state k v
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
