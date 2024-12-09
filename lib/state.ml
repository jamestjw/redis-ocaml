open Core
module StringMap = Utils.StringMap

type replication =
  { replica_of : string option
  ; replication_id : string
  ; offset : int
  }

type t =
  { (* Storing the expiry in nanoseconds *)
    store : (string * Int63.t option) StringMap.t
  ; configs : string StringMap.t
  ; replication : replication
  }

let ms_to_ns ms = Int63.(of_int ms * of_int 1_000_000)
let s_to_ns s = Int63.(of_int s * of_int 1_000_000_000)

let mk_state ~rdb_dir ~rdb_filename ~replica_of =
  let do_key_value map key value expire_timestamp =
    let open Option.Let_syntax in
    let expire_timestamp =
      expire_timestamp
      >>| function
      | Parser.MILLISECS ms -> ms_to_ns ms
      | Parser.SECS s -> s_to_ns s
    in
    StringMap.add key (value, expire_timestamp) map
  in
  let do_db map (db : Parser.database) =
    List.fold
      ~init:map
      ~f:(fun map { key; value; expire_timestamp } ->
        do_key_value
          map
          (Parser.str_encoding_to_str key)
          (Parser.str_encoding_to_str value)
          expire_timestamp)
      db.kv_pairs
  in
  let rdb_full_filename = Filename.concat rdb_dir rdb_filename in
  let store =
    match Sys_unix.file_exists rdb_full_filename with
    | `No | `Unknown ->
      Logs.info (fun m ->
        m "RDB file (%s) does not exist, starting with blank database" rdb_full_filename);
      StringMap.empty
    | `Yes ->
      let rdb_bytes = Utils.read_all_bytes rdb_full_filename in
      (match Parser.parse_rdb rdb_bytes with
       | Ok pdb ->
         Logs.info (fun m -> m "RDB file (%s) loaded" rdb_full_filename);
         List.fold ~init:StringMap.empty ~f:do_db pdb.databases
       | Error e ->
         Logs.warn (fun m -> m "Couldn't parse RDB file (%s): %s" rdb_filename e);
         StringMap.empty)
  in
  { store
  ; configs =
      StringMap.empty
      |> StringMap.add "dir" rdb_dir
      |> StringMap.add "dbfilename" rdb_filename
  ; replication =
      { replica_of
      ; replication_id = "8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb"
      ; offset = 0
      }
  }
;;
