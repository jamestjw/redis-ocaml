open Core

let initiate_handshake (addr, port) listening_port =
  let open Lwt_result.Let_syntax in
  let%lwt _sock, ic, oc = Client.connect_to_server ~host:addr ~port in
  let%bind () = Client.send_ping (ic, oc) in
  let%bind () = Client.send_replication_config (ic, oc) listening_port in
  let%bind replication_id, offset = Client.initiate_replication_stream (ic, oc) in
  let%bind bytes = Client.receive_rdb_dump ic in
  Lwt_result.return (replication_id, offset, bytes, ic, oc)
;;

let listen_for_updates ic oc server =
  let open Lwt in
  let rec run () =
    match%lwt Parser.get_master_cmd ic with
    (* TODO: replica needs to save bytes read *)
    | Some (cmd, bytes_read) ->
      let%lwt _ =
        Logs_lwt.info (fun m -> m "Received command %s from master" @@ Cmd.show cmd)
      in
      let%lwt resp =
        Server.execute_cmd
          { cmd; num_bytes = bytes_read; ic; oc; id = Utils.mk_uuid () }
          server
      in
      Lwt_io.write oc (Response.serialize resp) >>= run
    | None -> Logs_lwt.debug (fun m -> m "Connection closed")
  in
  Logs_lwt.info (fun m -> m "Listening for replication") >>= run
;;
