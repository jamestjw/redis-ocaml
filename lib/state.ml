open Core
module StringMap = Utils.StringMap

type replica_details =
  { replica_of : string * int
  ; replication_id : string
  ; offset : int
  }

type replication =
  | REPLICA of replica_details
  | MASTER of
      { replication_id : string
      ; offset : int
      ; replicas : (Lwt_io.input_channel * Lwt_io.output_channel) list
      }

type t =
  { (* Storing the expiry in nanoseconds *)
    store : (string * Int63.t option) StringMap.t
  ; configs : string StringMap.t
  ; replication : replication
  }

(* Either pass bytes or file dir and name *)
type rdb_source =
  | RDB_BYTES of int list
  | RDB_FILE of string * string

let mk_replica ~replica_of ~replication_id ~offset =
  REPLICA { replica_of; replication_id; offset }
;;

let mk_master () =
  MASTER
    { replication_id = "8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb"
    ; offset = 0
    ; replicas = []
    }
;;

let ms_to_ns ms = Int63.(of_int ms * of_int 1_000_000)
let s_to_ns s = Int63.(of_int s * of_int 1_000_000_000)

let rdb_databases_to_store databases =
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
  List.fold ~init:StringMap.empty ~f:do_db databases
;;

let mk_state ~rdb_source ~replication =
  let rdb_bytes, configs =
    match rdb_source with
    | RDB_FILE (rdb_dir, rdb_filename) ->
      let rdb_full_filename = Filename.concat rdb_dir rdb_filename in
      (match Sys_unix.file_exists rdb_full_filename with
       | `No | `Unknown ->
         Logs.info (fun m ->
           m
             "RDB file (%s) does not exist, starting with blank database"
             rdb_full_filename);
         ( None
         , StringMap.empty
           |> StringMap.add "dir" rdb_dir
           |> StringMap.add "dbfilename" rdb_filename )
       | `Yes ->
         ( Some (Utils.read_all_bytes rdb_full_filename)
         , StringMap.empty
           |> StringMap.add "dir" rdb_dir
           |> StringMap.add "dbfilename" rdb_filename ))
    | RDB_BYTES bytes -> Some bytes, StringMap.empty
  in
  let store =
    match rdb_bytes with
    | None -> StringMap.empty
    | Some rdb_bytes ->
      (match Parser.parse_rdb rdb_bytes with
       | Ok pdb ->
         Logs.info (fun m -> m "RDB file loaded");
         rdb_databases_to_store pdb.databases
       | Error e ->
         Logs.warn (fun m -> m "Couldn't parse RDB file: %s" e);
         StringMap.empty)
  in
  { store; configs; replication }
;;

let get_replication_offset { replication; _ } =
  match replication with
  | MASTER { offset; _ } -> offset
  | REPLICA { offset; _ } -> offset
;;

let incr_replication_offset ({ replication; _ } as state) delta =
  let replication =
    match replication with
    | MASTER ({ offset; _ } as rest) -> MASTER { rest with offset = offset + delta }
    | REPLICA ({ offset; _ } as rest) -> REPLICA { rest with offset = offset + delta }
  in
  { state with replication }
;;
