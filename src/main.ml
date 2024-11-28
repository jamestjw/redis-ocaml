open Lwt
open Lwt.Syntax

let default_backlog = 5 (* max number of concurrent clients *)
let default_address = "127.0.0.1"
let default_port = 6379
let num_args_regex = Str.regexp {|\*\([0-9]+\)|}
let arg_len_regex = Str.regexp {|\$\([0-9]+\)|}

type 'a parse_result =
  | Disconnected
  | InvalidFormat of string
  | Parsed of 'a

type command =
  | PING
  | ECHO of string
[@@deriving show { with_path = false }]

let args_to_cmd args =
  let lower_fst = function
    | [] -> []
    | x :: xs -> String.lowercase_ascii x :: xs
  in
  match lower_fst args with
  | [ "ping" ] -> Some PING
  | [ "echo"; arg ] -> Some (ECHO arg)
  | _ -> None
;;

let handle_message cmd =
  match cmd with
  | PING -> "+PONG\r\n"
  | ECHO s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
;;

(* Polls the input channel for a valid command, if this returns None this
   means that the connection has been dropped by the client. *)
let rec get_cmd ic =
  let parse_len regexp =
    let* msg = Lwt_io.read_line_opt ic in
    match msg with
    | None -> return Disconnected
    | Some msg ->
      if Str.string_match regexp msg 0
      then return @@ Parsed (int_of_string @@ Str.matched_group 1 msg)
      else return @@ InvalidFormat msg
  in
  let parse_args num_args =
    let rec parse_args' num_args acc =
      if num_args = 0
      then return @@ Parsed (List.rev acc)
      else
        let* arg_len = parse_len arg_len_regex in
        match arg_len with
        | Disconnected -> return Disconnected
        | InvalidFormat s ->
          let* _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
          return (InvalidFormat s)
        | Parsed arg_len ->
          let* msg = Lwt_io.read_line_opt ic in
          (match msg with
           | None -> return Disconnected
           | Some arg when String.length arg <> arg_len ->
             let* _ =
               Logs_lwt.err (fun m -> m "Argument (%s) length != %d" arg arg_len)
             in
             return @@ InvalidFormat arg
           | Some arg -> parse_args' (num_args - 1) (arg :: acc))
    in
    parse_args' num_args []
  in
  let* num_args = parse_len num_args_regex in
  match num_args with
  | Disconnected -> return None
  | InvalidFormat s ->
    let* _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
    get_cmd ic
  | Parsed num_args when num_args <= 0 -> get_cmd ic
  | Parsed num_args ->
    let* arg_list = parse_args num_args in
    (match arg_list with
     | Disconnected -> return None
     | InvalidFormat _ -> get_cmd ic
     | Parsed arg_list ->
       (match args_to_cmd arg_list with
        | None ->
          let* _ =
            Logs_lwt.err (fun m -> m "Unknown command %s" @@ String.concat "  " arg_list)
          in
          return None
        | Some cmd -> return @@ Some cmd))
;;

let rec handle_connection ic oc () =
  let* cmd = get_cmd ic in
  match cmd with
  | Some cmd ->
    let* _ = Logs_lwt.info (fun m -> m "Received command %s" @@ show_command cmd) in
    Lwt_io.write oc (handle_message cmd) >>= handle_connection ic oc
  | None -> Logs_lwt.info (fun m -> m "Connection closed")
;;

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let* () = Logs_lwt.info (fun m -> m "New connection") in
  Lwt.on_failure (handle_connection ic oc ()) (fun e ->
    Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  return ()
;;

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve
;;

let create_server_socket ~address ~port ~backlog =
  let open Lwt_unix in
  (* Create a TCP server socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  ignore @@ bind sock (ADDR_INET (Unix.inet_addr_of_string address, port));
  listen sock backlog;
  sock
;;

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let server_socket =
    create_server_socket
      ~address:default_address
      ~port:default_port
      ~backlog:default_backlog
  in
  let serve = create_server server_socket in
  Lwt_main.run @@ serve ()
;;
