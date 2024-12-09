open State

let fetch_info { replica_of; _ } _ =
  match replica_of with
  | None -> "role:master"
  | Some _ -> "role:slave"
;;
