(* Client that replicas use to communicate with master *)
open Core
open Lwt_unix
open Lwt

let default_replication_capabilities = "psync2"

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
  return (sock, in_channel, out_channel)
;;

let cmd_to_str cmd =
  let arr =
    match cmd with
    | Cmd.PING -> Response.strs_to_bulk_array [ "PING" ]
    | Cmd.REPL_CONF_PORT port ->
      Response.strs_to_bulk_array [ "REPLCONF"; "listening-port"; string_of_int port ]
    | Cmd.REPL_CONF_CAPA capabilities ->
      Response.strs_to_bulk_array [ "REPLCONF"; "capa"; capabilities ]
    | Cmd.PSYNC (id, offset) ->
      Response.strs_to_bulk_array [ "PSYNC"; id; string_of_int offset ]
    | Cmd.SET { set_key; set_value; set_timeout } ->
      let timeout =
        match set_timeout with
        | None -> []
        | Some (PX i) -> [ "px"; string_of_int i ]
        | Some (EX i) -> [ "ex"; string_of_int i ]
      in
      Response.strs_to_bulk_array @@ [ "SET"; set_key; set_value ] @ timeout
    | _ -> failwith "not implemented yet"
  in
  Response.serialize arr
;;

let send_string oc str =
  let%lwt _ = Lwt_io.write oc str in
  Lwt.return_unit
;;

let send_request oc cmd = send_string oc @@ cmd_to_str cmd

let send_ping (ic, oc) =
  let%lwt () = send_request oc Cmd.PING in
  match%lwt Parser.parse_simple ic with
  | Parsed "PONG" -> Lwt_result.return ()
  | Parsed msg ->
    Lwt_result.fail
      (Printf.sprintf "invalid response, expected 'PONG' but got %s instead" msg)
  | Disconnected -> Lwt_result.fail (Printf.sprintf "no response from server")
  | InvalidFormat s -> Lwt_result.fail (Printf.sprintf "invalid response to PING: %s" s)
;;

let send_replication_config (ic, oc) port =
  let%lwt _ = Logs_lwt.info (fun m -> m "Sending listening port %d to master" port) in
  let%lwt () = send_request oc @@ Cmd.REPL_CONF_PORT port in
  match%lwt Parser.parse_simple ic with
  | Parsed "OK" ->
    let%lwt _ =
      Logs_lwt.info (fun m ->
        m "Sending replication capabilities %s to master" default_replication_capabilities)
    in
    let%lwt () = send_request oc @@ Cmd.REPL_CONF_CAPA default_replication_capabilities in
    (match%lwt Parser.parse_simple ic with
     | Parsed "OK" -> Lwt_result.return ()
     | Parsed msg ->
       Lwt_result.fail
         (Printf.sprintf "invalid response, expected 'OK' but got %s instead" msg)
     | Disconnected -> Lwt_result.fail (Printf.sprintf "no response from server")
     | InvalidFormat s ->
       Lwt_result.fail (Printf.sprintf "invalid response to REPLCONF capa: %s" s))
  | Parsed msg ->
    Lwt_result.fail
      (Printf.sprintf "invalid response, expected 'PONG' but got %s instead" msg)
  | Disconnected ->
    Lwt_result.fail @@ Printf.sprintf "No response from server to REPLCONF"
  | InvalidFormat s ->
    Lwt_result.fail @@ Printf.sprintf "invalid response to REPLCONF listening-port: %s" s
;;

let initiate_replication_stream (ic, oc) =
  let psync_response_regex = Str.regexp {|^FULLRESYNC \(.*\) \([0-9]+\)$|} in
  let%lwt _ = Logs_lwt.info (fun m -> m "Initiating replication stream") in
  let%lwt () = send_request oc @@ Cmd.PSYNC ("?", -1) in
  match%lwt Parser.parse_simple ic with
  | Parsed res when Str.string_match psync_response_regex res 0 ->
    (* TODO: actually do something with this *)
    Lwt_result.return ()
  | Parsed s -> Lwt_result.fail (Printf.sprintf "invalid response to PSYNC: %s" s)
  | Disconnected ->
    Lwt_result.fail
      (Printf.sprintf "No response from server to replication stream initiation")
  | InvalidFormat s -> Lwt_result.fail (Printf.sprintf "invalid response to PSYNC: %s" s)
;;

let receive_rdb_dump ic =
  let%lwt _ = Logs_lwt.info (fun m -> m "Waiting for RDB dump") in
  match%lwt Parser.parse_bulk_string_len ic with
  | Parsed num_bytes ->
    let buffer = Bytes.create num_bytes in
    let%lwt _ = Lwt_io.read_into_exactly ic buffer 0 num_bytes in
    Lwt_result.return (Bytes.to_list buffer |> List.map ~f:Char.to_int)
  | InvalidFormat s ->
    Lwt_result.fail @@ Printf.sprintf "Did not receive valid RDB dump from master: %s" s
  | Disconnected -> Lwt_result.fail "Master did not send RDB dump"
;;

let propagate_set (_ic, oc) set_cmd =
  match set_cmd with
  | Cmd.SET _ ->
    let%lwt () = send_request oc @@ set_cmd in
    Lwt.return_unit
  | _ -> failwith "must be called with a SET command"
;;

let close_connection sock = Lwt_unix.close sock
