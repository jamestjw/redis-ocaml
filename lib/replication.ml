open Core
open State

let fetch_info { replica_of; replication_id; offset } _ =
  let kv_pairs =
    match replica_of with
    | None ->
      [ "role", "master"
      ; "master_replid", replication_id
      ; "master_repl_offset", string_of_int offset
      ]
    | Some _ -> [ "role", "slave" ]
  in
  kv_pairs
  |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s:%s" k v)
  |> String.concat ~sep:"\r\n"
;;

let initiate_handshake { replica_of; _ } =
  match replica_of with
  | None ->
    let%lwt _ =
      Logs_lwt.warn (fun m -> m "initiating handshake without master information")
    in
    Lwt.return_unit
  | Some (addr, port) ->
    let%lwt sock, ic, oc = Client.connect_to_server ~host:addr ~port in
    let%lwt _ = Client.send_ping (ic, oc) in
    Client.close_connection sock
;;
