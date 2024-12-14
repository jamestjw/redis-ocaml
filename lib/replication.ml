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
    Lwt_result.return bytes
;;

(* let load_pdb bytes = *)
(*   let store = State.rdb_databases_to_store (failwith "add db")  in *)
(*   { store; configs=StringMap.empty; replication } *)
(* ;; *)
