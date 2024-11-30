type response =
  | SIMPLE of string
  | BULK of string
  | NULL_BULK

let handle_message cmd context =
  match cmd with
  | Cmd.PING -> SIMPLE "PONG"
  | Cmd.ECHO s -> BULK s
  | Cmd.GET k ->
    (match Context.get context k with
     | None -> NULL_BULK
     | Some v -> BULK v)
  | Cmd.SET (k, v) ->
    Context.set context k v;
    SIMPLE "OK"
;;

let serialize = function
  | SIMPLE s -> Printf.sprintf "+%s\r\n" s
  | BULK s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
  | NULL_BULK -> "$-1\r\n"
;;
