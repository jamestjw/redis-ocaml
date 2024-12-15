open Core

let initiate_handshake replica_of listening_port =
  let open Lwt_result.Let_syntax in
  match replica_of with
  | None ->
    let%lwt _ =
      Logs_lwt.warn (fun m ->
        m "Initiating handshake without master information, aborting...")
    in
    failwith "shouldn't occur"
  | Some (addr, port) ->
    let%lwt _sock, ic, oc = Client.connect_to_server ~host:addr ~port in
    let%bind () = Client.send_ping (ic, oc) in
    let%bind () = Client.send_replication_config (ic, oc) listening_port in
    let%bind () = Client.initiate_replication_stream (ic, oc) in
    let%bind bytes = Client.receive_rdb_dump ic in
    Lwt_result.return (bytes, ic, oc)
;;

let listen_for_replication ic oc server =
  let open Lwt in
  let rec run () =
    let%lwt cmd = Parser.get_master_cmd ic in
    match cmd with
    | Some cmd ->
      let%lwt _ =
        Logs_lwt.info (fun m -> m "Received command %s from master" @@ Cmd.show cmd)
      in
      let%lwt resp = Server.execute_cmd (cmd, ic, oc) server in
      Lwt_io.write oc (Response.serialize resp) >>= run
    | None -> Logs_lwt.debug (fun m -> m "Connection closed")
  in
  Logs_lwt.info (fun m -> m "Listening for replication") >>= run
;;
