(* Client that replicas use to communicate with master *)
open Lwt_unix

let connect_to_server ~host ~port =
  let%lwt _ = Logs_lwt.info (fun m -> m "Connecting to server...") in
  (* Create a socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* Prepare the server address *)
  let server_addr = ADDR_INET (Core_unix.Inet_addr.of_string_or_getbyname host, port) in
  (* Connect to the server *)
  let%lwt _ = Lwt_unix.connect sock server_addr in
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let%lwt _ = Logs_lwt.info (fun m -> m "Successfully connected to master.") in
  Lwt.return (sock, in_channel, out_channel)
;;

let cmd_to_str cmd =
  let args =
    match cmd with
    | Cmd.PING -> [ Response.BULK "PING" ]
    | _ -> failwith "not implemented yet"
  in
  Response.(serialize @@ ARRAY args)
;;

let send_request oc cmd =
  let%lwt _ = Lwt_io.write oc (cmd_to_str cmd) in
  Lwt.return_unit
;;

let send_ping (ic, oc) =
  let open Lwt in
  let%lwt () = send_request oc Cmd.PING in
  match%lwt Parser.parse_simple ic with
  | Parsed "PONG" -> return @@ Ok ()
  | Parsed msg ->
    return
    @@ Error (Printf.sprintf "invalid response, expected 'PONG' but got %s instead" msg)
  | Disconnected -> return @@ Error (Printf.sprintf "no response from server")
  | InvalidFormat s -> return @@ Error (Printf.sprintf "invalid response to PING: %s" s)
;;

let close_connection sock = Lwt_unix.close sock
