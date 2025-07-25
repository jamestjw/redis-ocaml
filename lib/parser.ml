open Core
open Lwt
module StringMap = Stdlib.Map.Make (String)

(* To parse RESP arrays *)
let array_length_regex = Str.regexp {|\*\([0-9]+\)|}

(* To parse BULK strings *)
let bulk_str_len_regex = Str.regexp {|\$\([0-9]+\)|}
let simple_regex = Str.regexp {|\+\(.*\)|}
let integer_regex = Str.regexp {|\:\([0-9]+\)|}

type 'a parse_result =
  | Disconnected
  | InvalidFormat of string
  | Parsed of 'a

let lower_fst = function
  | [] -> []
  | x :: xs -> String.lowercase x :: xs
;;

let parse_set_cmd = function
  | set_key :: set_value :: args ->
    let rec helper args (set_key, set_value, set_timeout) =
      match lower_fst args with
      | [] -> Cmd.SET { set_key; set_value; set_timeout }
      | "px" :: timeout :: rest ->
        (match Stdlib.int_of_string_opt timeout, set_timeout with
         | _, Some _ -> Cmd.INVALID "timeout set twice for 'SET' command"
         | Some timeout, None when timeout > 0 ->
           helper rest (set_key, set_value, Some (Cmd.PX timeout))
         | _ -> Cmd.INVALID "'SET' requires positive integer for timeout")
      | "ex" :: timeout :: rest ->
        (match Stdlib.int_of_string_opt timeout, set_timeout with
         | _, Some _ -> Cmd.INVALID "timeout set twice for 'SET' command"
         | Some timeout, None when timeout > 0 ->
           helper rest (set_key, set_value, Some (Cmd.EX timeout))
         | _ -> Cmd.INVALID "'SET' requires positive integer for timeout")
      | arg :: _ -> Cmd.INVALID (Printf.sprintf "unknown arg '%s' for 'SET'" arg)
    in
    helper args (set_key, set_value, None)
  | _ -> Cmd.INVALID "invalid args for set, key-value pair required"
;;

let parse_master_set_cmd args =
  match parse_set_cmd args with
  | Cmd.SET { set_key; set_value; set_timeout } ->
    Cmd.MASTER_SET { set_key; set_value; set_timeout }
  | cmd -> cmd
;;

let parse_ping_cmd = function
  | [] -> Cmd.PING
  | _ -> Cmd.INVALID "'PING' takes no args"
;;

let parse_echo_cmd = function
  | [ e ] -> Cmd.ECHO e
  | _ -> Cmd.INVALID "'ECHO' takes one arg"
;;

let parse_get_cmd = function
  | [ key ] -> Cmd.GET key
  | _ -> Cmd.INVALID "'GET' takes one arg"
;;

let parse_config_cmd args =
  match lower_fst args with
  | [ "get" ] -> Cmd.INVALID "wrong number of arguments for 'config|get' command"
  | "get" :: keys -> Cmd.GET_CONFIG keys
  | _ -> Cmd.INVALID "invalid 'CONFIG' subcommand"
;;

let parse_keys_cmd args =
  match lower_fst args with
  | [ query ] -> Cmd.KEYS query
  | _ -> Cmd.INVALID "'KEYS' takes one arg"
;;

let parse_info_cmd args = Cmd.INFO args

let parse_type_cmd args =
  match args with
  | [ key ] -> Cmd.TYPE key
  | _ -> Cmd.INVALID "'TYPE' takes one arg"
;;

let parse_replconf_cmd args =
  match lower_fst args with
  | [ "listening-port"; port ] ->
    (match int_of_string_opt port with
     | Some port -> Cmd.REPL_CONF_PORT port
     | None -> Cmd.INVALID "'REPLCONF' listening port isn't a valid integer")
  | [ "capa"; capa ] -> Cmd.REPL_CONF_CAPA capa
  | [ "getack"; query ] -> Cmd.REPL_CONF_GET_ACK query
  | _ -> Cmd.INVALID "'KEYS' takes one arg"
;;

let parse_psync_cmd args =
  match args with
  | [ replication_id; offset ] ->
    (match int_of_string_opt offset with
     | Some offset -> Cmd.PSYNC (replication_id, offset)
     | None -> Cmd.INVALID "'PSYNC' offset isn't a valid integer")
  | _ -> Cmd.INVALID "'PSYNC' takes 2 args"
;;

let parse_wait_cmd args =
  match args with
  | [ num_replicas; timeout ] ->
    (match int_of_string_opt num_replicas, int_of_string_opt timeout with
     | Some num_replicas, Some timeout -> Cmd.WAIT (num_replicas, timeout)
     | _ -> Cmd.INVALID "'WAIT' expects integer arguments")
  | _ -> Cmd.INVALID "'WAIT' takes 2 args"
;;

let parse_xadd_cmd args =
  let explicit_id_regex = Str.regexp {|\([0-9]+\)\-\([0-9]+\)|} in
  let partially_implicit_id_regex = Str.regexp {|\([0-9]+\)\-\*|} in
  let fully_implicit_id_regex = Str.regexp {|\*|} in
  let parse_id id =
    if Str.string_match explicit_id_regex id 0
    then
      Some
        (Cmd.EXPLICIT
           ( Stdlib.int_of_string (Str.matched_group 1 id)
           , Stdlib.int_of_string (Str.matched_group 2 id) ))
    else if Str.string_match partially_implicit_id_regex id 0
    then Some (Cmd.AUTO_SEQ (Stdlib.int_of_string (Str.matched_group 1 id)))
    else if Str.string_match fully_implicit_id_regex id 0
    then Some Cmd.AUTO
    else None
  in
  let rec take_kv_pairs args acc =
    match args with
    | [] -> Ok (List.rev acc)
    | k :: v :: rest -> take_kv_pairs rest ((k, v) :: acc)
    | _ -> Error "extra arguments passed to 'XADD'"
  in
  match args with
  | key :: id :: args ->
    (match parse_id id with
     | Some id ->
       (match take_kv_pairs args [] with
        | Ok pairs -> Cmd.XADD (key, id, pairs)
        | Error e -> Cmd.INVALID e)
     | None -> Cmd.INVALID "invalid entry ID")
  | _ -> Cmd.INVALID "wrong number of arguments for 'XADD' command"
;;

let parse_xrange_cmd args =
  let explicit_id_regex = Str.regexp {|\([0-9]+\)\-\([0-9]+\)|} in
  let time_only_regex = Str.regexp {|\([0-9]+\)|} in
  let str_to_id s wildcard_str =
    if Str.string_match explicit_id_regex s 0
    then
      Ok
        (Some
           ( Stdlib.int_of_string (Str.matched_group 1 s)
           , Stdlib.int_of_string (Str.matched_group 2 s) ))
    else if Str.string_match time_only_regex s 0
    then Ok (Some (Stdlib.int_of_string (Str.matched_group 1 s), 0))
    else if String.equal s wildcard_str
    then Ok None
    else Error "invalid ID format in 'XRANGE'"
  in
  match args with
  | [ key; lower; upper ] ->
    (match str_to_id lower "-", str_to_id upper "+" with
     | Ok (Some lower), Ok (Some upper) -> Cmd.XRANGE (key, Cmd.BTW (lower, upper))
     | Ok (Some lower), Ok None -> Cmd.XRANGE (key, Cmd.GTE lower)
     | Ok None, Ok (Some upper) -> Cmd.XRANGE (key, Cmd.LTE upper)
     | Ok None, Ok None -> Cmd.XRANGE (key, Cmd.ALL)
     | Error e, _ | _, Error e -> Cmd.INVALID e)
  | _ -> Cmd.INVALID "wrong number of arguments for 'XRANGE' command"
;;

let parse_xread_cmd args =
  let rec parse_ranges ranges acc =
    let explicit_id_regex = Str.regexp {|\([0-9]+\)\-\([0-9]+\)|} in
    match ranges with
    | [] -> Ok (List.rev acc)
    | range :: ranges ->
      if Str.string_match explicit_id_regex range 0
      then (
        let range =
          Cmd.FRESHER_THAN
            ( Stdlib.int_of_string (Str.matched_group 1 range)
            , Stdlib.int_of_string (Str.matched_group 2 range) )
        in
        parse_ranges ranges (range :: acc))
      else if String.equal range "$"
      then parse_ranges ranges (Cmd.LAST :: acc)
      else Error "invalid range format"
  in
  let args_block =
    match args with
    | "block" :: ms :: rest ->
      (match int_of_string_opt ms with
       | Some ms when ms < 0 -> Error "block timeout cannot be negative"
       | Some ms -> Ok (rest, Some ms)
       | None -> Error "block time has to be an integer")
    | _ -> Ok (args, None)
  in
  match args_block with
  | Ok ("streams" :: args, _) when List.length args % 2 <> 0 ->
    Cmd.INVALID
      "Unbalanced 'xread' list of streams: for each stream key an ID or '$' must be \
       specified."
  | Ok ([ "streams" ], _) -> Cmd.INVALID "wrong number of arguments for 'XREAD' command"
  | Ok ("streams" :: args, block) ->
    let stream_keys, ranges = List.split_n args (List.length args / 2) in
    (match parse_ranges ranges [] with
     | Error e -> Cmd.INVALID e
     | Ok ranges -> Cmd.XREAD { block; queries = List.zip_exn stream_keys ranges })
  | Error e -> Cmd.INVALID e
  | _ -> Cmd.INVALID "invalid arguments for 'XREAD' command"
;;

let parse_incr_cmd = function
  | [ e ] -> Cmd.INCR e
  | _ -> Cmd.INVALID "'INCR' takes one arg"
;;

let parse_multi_cmd = function
  | [] -> Cmd.MULTI
  | _ -> Cmd.INVALID "'MULTI' takes no args"
;;

let parse_exec_cmd = function
  | [] -> Cmd.EXEC
  | _ -> Cmd.INVALID "'EXEC' takes no args"
;;

let parse_discard_cmd = function
  | [] -> Cmd.DISCARD
  | _ -> Cmd.INVALID "'DISCARD' takes no args"
;;

let parse_push from_left = function
  | push_key :: push_values when List.length push_values > 0 ->
    Cmd.PUSH { from_left; push_key; push_values }
  | _ -> Cmd.INVALID "'RPUSH' takes 2 args"
;;

let parse_lrange = function
  | [ key; start_idx; end_idx ] ->
    (match int_of_string_opt start_idx, int_of_string_opt end_idx with
     | Some start_idx, Some end_idx -> Cmd.LRANGE { key; start_idx; end_idx }
     | _, _ -> Cmd.INVALID "'LRANGE' requires 2 integer arguments")
  | _ -> Cmd.INVALID "'LRANGE' takes 2 args"
;;

let args_to_cmd args =
  match lower_fst args with
  | "ping" :: args -> parse_ping_cmd args
  | "echo" :: args -> parse_echo_cmd args
  | "get" :: args -> parse_get_cmd args
  | "set" :: args -> parse_set_cmd args
  | "config" :: args -> parse_config_cmd args
  | "keys" :: args -> parse_keys_cmd args
  | "info" :: args -> parse_info_cmd args
  | "replconf" :: args -> parse_replconf_cmd args
  | "psync" :: args -> parse_psync_cmd args
  | "wait" :: args -> parse_wait_cmd args
  | "type" :: args -> parse_type_cmd args
  | "xadd" :: args -> parse_xadd_cmd args
  | "xrange" :: args -> parse_xrange_cmd args
  | "xread" :: args -> parse_xread_cmd args
  | "incr" :: args -> parse_incr_cmd args
  | "multi" :: args -> parse_multi_cmd args
  | "exec" :: args -> parse_exec_cmd args
  | "discard" :: args -> parse_discard_cmd args
  | "lpush" :: args -> parse_push true args
  | "rpush" :: args -> parse_push false args
  | "lrange" :: args -> parse_lrange args
  | cmd :: _ -> Cmd.INVALID (Printf.sprintf "unrecognised command %s" cmd)
  | _ -> Cmd.INVALID "invalid command"
;;

let parse_len regexp ic =
  match%lwt Utils.read_line_with_length_opt ic with
  | None -> return Disconnected
  | Some (msg, bytes_read) ->
    if Str.string_match regexp msg 0
    then return @@ Parsed (Stdlib.int_of_string @@ Str.matched_group 1 msg, bytes_read)
    else return @@ InvalidFormat (Printf.sprintf "%s is not a valid len" msg)
;;

let parse_bulk_string_len = parse_len bulk_str_len_regex
let parse_array_len = parse_len array_length_regex

let parse_bulk_string ic =
  match%lwt parse_bulk_string_len ic with
  | Disconnected -> return Disconnected
  | InvalidFormat s ->
    let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed bulk string length %s" s) in
    return (InvalidFormat s)
  | Parsed (arg_len, len_bytes_read) ->
    (match%lwt Utils.read_line_with_length_opt ic with
     | None -> return Disconnected
     | Some (arg, _) when String.length arg <> arg_len ->
       let%lwt _ = Logs_lwt.err (fun m -> m "Argument (%s) length != %d" arg arg_len) in
       return @@ InvalidFormat arg
     | Some (arg, bytes_read) -> return @@ Parsed (arg, len_bytes_read + bytes_read))
;;

(* Polls the input channel for a valid command, if this returns None this
   means that the connection has been dropped by the client. *)
let parse_resp_array ic =
  let parse_bulk_strings count =
    let rec parse_bulk_strings' count acc total_num_bytes =
      if count = 0
      then return @@ Parsed (List.rev acc, total_num_bytes)
      else (
        match%lwt parse_bulk_string ic with
        | Disconnected -> return Disconnected
        | InvalidFormat s ->
          let%lwt _ = Logs_lwt.err (fun m -> m "Couldn't parse string in array: %s" s) in
          return (InvalidFormat s)
        | Parsed (str, num_bytes) ->
          parse_bulk_strings' (count - 1) (str :: acc) (total_num_bytes + num_bytes))
    in
    parse_bulk_strings' count [] 0
  in
  match%lwt parse_array_len ic with
  | Disconnected -> return None
  | InvalidFormat s ->
    let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed array length %s" s) in
    return @@ Some ([], 0)
  | Parsed (num_args, len_bytes_read) when num_args <= 0 ->
    let%lwt _ =
      Logs_lwt.err (fun m -> m "Length %d of array is not greater than zero" num_args)
    in
    return @@ Some ([], len_bytes_read)
  | Parsed (num_args, len_bytes_read) ->
    (match%lwt parse_bulk_strings num_args with
     | Disconnected -> return None
     | InvalidFormat s ->
       let%lwt _ =
         Logs_lwt.err (fun m ->
           m "Expected resp array of length %d, but got error %s" num_args s)
       in
       return @@ Some ([], len_bytes_read)
     | Parsed (arg_list, array_bytes_read) ->
       return @@ Some (arg_list, len_bytes_read + array_bytes_read))
;;

let parse_simple ic =
  match%lwt Lwt_io.read_line_opt ic with
  | None -> return Disconnected
  | Some msg ->
    if Str.string_match simple_regex msg 0
    then return @@ Parsed (Str.matched_group 1 msg)
    else
      return
      @@ InvalidFormat
           (Printf.sprintf
              "expected SIMPLE string, i.e. starting with '+', but got %s instead"
              msg)
;;

let parse_integer ic =
  match%lwt Lwt_io.read_line_opt ic with
  | None -> return Disconnected
  | Some msg ->
    if Str.string_match integer_regex msg 0
    then return @@ Parsed (Str.matched_group 1 msg |> int_of_string)
    else
      return
      @@ InvalidFormat
           (Printf.sprintf
              "expected INTEGER, i.e. starting with ':', but got %s instead"
              msg)
;;

let get_cmd ic =
  match%lwt parse_resp_array ic with
  | None -> return None
  | Some ([], bytes_read) ->
    return @@ Some (Cmd.INVALID "missing or malformed command", bytes_read)
  | Some (args, bytes_read) -> return @@ Some (args_to_cmd args, bytes_read)
;;

let get_master_cmd ic =
  let args_to_cmd args =
    match lower_fst args with
    | "set" :: args -> parse_master_set_cmd args
    | "replconf" :: args -> parse_replconf_cmd args
    | "ping" :: [] -> Cmd.MASTER_PING
    | cmd :: _ -> Cmd.INVALID (Printf.sprintf "unrecognised master command %s" cmd)
    | _ -> Cmd.INVALID "invalid master command"
  in
  match%lwt parse_resp_array ic with
  | None -> return None
  | Some ([], bytes_read) ->
    return @@ Some (Cmd.INVALID "missing or malformed command", bytes_read)
  | Some (args, bytes_read) -> return @@ Some (args_to_cmd args, bytes_read)
;;

(* RDB file, parsed based on the specs in https://rdb.fnordig.de/file_format.html *)

type header = { rdb_version : int }

and str_encoding =
  | LP_STR of string
  | INTEGER of int
  (* bytes and uncompressed_len *)
  | COMPRESSED_STR of int list * int

(* TODO: implement rest *)
and metadata = (str_encoding StringMap.t[@opaque])

and expire_timestamp =
  | SECS of int
  | MILLISECS of int

and kv_pair =
  { key : str_encoding
  ; value : str_encoding
  ; expire_timestamp : expire_timestamp option
  }

and database =
  { db_index : int
  ; hash_tbl_sz : int
  ; expire_hash_tbl_sz : int
  ; kv_pairs : kv_pair list
  }

and rdb =
  { header : header
  ; metadata : metadata
  ; databases : database list
  ; checksum : string
  }
[@@deriving show { with_path = false }]

let str_encoding_to_str = function
  | LP_STR s -> s
  | INTEGER i -> string_of_int i
  | _ -> failwith "not implemented yet"
;;

let hex_str_to_int_opt s = Stdlib.int_of_string_opt (Printf.sprintf "0x%s" s)
let ascii_hex_to_str l = List.map ~f:(fun e -> Char.of_int_exn e) l |> String.of_list

let parse_length (bytes : int list) =
  match bytes with
  | [] -> Error "malformed length prefix"
  (* First 2 bits of the byte are 00 *)
  | b :: rest when Int.(b land 0b11000000) = 0 -> Ok (b, rest)
  (* First 2 bits of the first byte are 01 *)
  | b1 :: b2 :: rest when Int.(b1 land 0b11000000) = 0b01000000 ->
    (* Take the last 6 bits of the first byte, join them to the next byte *)
    Ok (Int.shift_left Int.(b1 land 0b111111) 8 + b2, rest)
  (* First 2 bits of the first byte are 10, the next 4 bytes are the length *)
  | b1 :: b2 :: b3 :: b4 :: b5 :: rest when Int.(b1 land 0b11000000) = 0b10000000 ->
    Ok (Int.((b2 lsl 24) + (b3 lsl 16) + (b4 lsl 8) + b5), rest)
  | _ -> Error "invalid prefix length format"
;;

let list_split_n_opt l n =
  let left, right = List.split_n l n in
  if List.length left <> n then None else Some (left, right)
;;

let parse_length_prefixed_string bytes =
  let open Result.Let_syntax in
  let%bind len, bytes = parse_length bytes in
  let%bind str_bytes, rest =
    Result.of_option
      ~error:"insufficient bytes in prefixed length string"
      (list_split_n_opt bytes len)
  in
  return (ascii_hex_to_str str_bytes, rest)
;;

let little_endian_to_int l =
  List.mapi ~f:(fun i v -> Int.(v lsl (8 * i))) l |> List.fold ~f:( + ) ~init:0
;;

let is_special_format bytes =
  match bytes with
  | byte :: _ when Int.(byte land 0b11000000) = 0b11000000 -> true
  | _ -> false
;;

let parse_special_format bytes =
  let open Result.Let_syntax in
  match bytes with
  (* Integers as String
     Read 1st byte, check if the first two bits are 11, then the remaining 6
     bits are read.

     If the value of those 6 bits is:
     - 0: indicates that an 8 bit integer follows
     - 1: indicates that a 16 bit integer follows
     - 2: indicates that a 32 bit integer follows *)
  | byte :: rest when Int.(byte land 0b11000000) = 0b11000000 ->
    (match Int.(byte land 0b111111), rest with
     | 0, b1 :: rest -> Ok (INTEGER b1, rest)
     | 1, b1 :: b2 :: rest -> Ok (INTEGER (little_endian_to_int [ b1; b2 ]), rest)
     | 2, b1 :: b2 :: b3 :: b4 :: rest ->
       Ok (INTEGER (little_endian_to_int [ b1; b2; b3; b4 ]), rest)
     | 3, _ ->
       let%bind compressed_len, bytes = parse_length bytes in
       let%bind uncompressed_len, bytes = parse_length bytes in
       let%bind str_bytes, rest =
         Result.of_option
           ~error:"insufficient bytes in compressed string"
           (list_split_n_opt bytes compressed_len)
       in
       Ok (COMPRESSED_STR (str_bytes, uncompressed_len), rest)
     | _ -> Error "invalid special format")
  | _ -> Error "not special format??"
;;

let parse_string_encoding bytes =
  let open Result.Let_syntax in
  if is_special_format bytes
  then parse_special_format bytes
  else parse_length_prefixed_string bytes >>| fun (str, bytes) -> LP_STR str, bytes
;;

let parse_header bytes =
  let open Result.Let_syntax in
  (* 52 45 44 49 53 30 30 31 31  // Magic string + version number (ASCII): "REDIS0011". *)
  let header_bytes, rest = List.split_n bytes 9 in
  if List.length header_bytes <> 9
  then Error "missing bytes in header"
  else (
    let magic_bytes, version_number_bytes = List.split_n header_bytes 5 in
    (* FIXME: Type system doesn't guarantee that this will always work *)
    let magic = ascii_hex_to_str magic_bytes in
    let version_number_str = ascii_hex_to_str version_number_bytes in
    let%bind version_num =
      Result.of_option
        ~error:"invalid version number"
        (Stdlib.int_of_string_opt version_number_str)
    in
    match magic with
    | "REDIS" -> Ok ({ rdb_version = version_num }, rest)
    | _ -> Error "invalid magic string")
;;

let byte_is_op_code = function
  | 0xFA | 0xFB | 0xFC | 0xFD | 0xFE | 0xFF -> true
  | _ -> false
;;

(* Also known as auxiliary fields in the spec *)
let parse_metadata bytes =
  let open Result.Let_syntax in
  let rec parse_kvs bytes map =
    match bytes with
    | 0xFA :: bytes ->
      (* TODO: we need to support other string formats too, i.e. integer strings
         and compressed LZF strings *)
      let%bind key, bytes = parse_length_prefixed_string bytes in
      let%bind value, bytes = parse_string_encoding bytes in
      parse_kvs bytes (StringMap.add key value map)
    | rest -> Ok (map, rest)
  in
  parse_kvs bytes StringMap.empty
;;

let ints_to_hex_str ints =
  List.map ~f:(fun i -> Printf.sprintf "%X" i) ints |> String.concat
;;

let parse_one bytes expected err =
  match bytes with
  | byte :: bytes when byte = expected -> Ok (byte, bytes)
  | _ -> Error err
;;

let parse_databases bytes =
  let open Result.Let_syntax in
  let rec parse_kvs bytes acc =
    match bytes with
    | 0xFC :: bytes ->
      let%bind timestamp_bytes, bytes =
        Result.of_option ~error:"missing bytes for timestamp" (list_split_n_opt bytes 8)
      in
      parse_kv (Some (MILLISECS (little_endian_to_int timestamp_bytes))) bytes acc
    | 0xFD :: bytes ->
      let%bind timestamp_bytes, bytes =
        Result.of_option ~error:"missing bytes for timestamp" (list_split_n_opt bytes 4)
      in
      parse_kv (Some (SECS (little_endian_to_int timestamp_bytes))) bytes acc
    | b :: _ when byte_is_op_code b ->
      (* We are probably in the EOF section *)
      return (List.rev acc, bytes)
    | bytes ->
      (* No expiry here, just move on *)
      parse_kv None bytes acc
  and parse_kv expire_timestamp bytes acc =
    match bytes with
    | 0 :: bytes ->
      (* dealing with a string *)
      let%bind key, bytes = parse_string_encoding bytes in
      let%bind value, bytes = parse_string_encoding bytes in
      parse_kvs bytes ({ key; value; expire_timestamp } :: acc)
    | t :: _ -> Fmt.failwith "can't handle type %d" t
    | _ -> Error "missing kv pair in database section"
  in
  let rec parse_database' bytes acc =
    match bytes with
    | 0xFE :: bytes ->
      let%bind db_index, bytes = parse_length bytes in
      let%bind _, bytes = parse_one bytes 0xFB "expected 0xFB in database section" in
      let%bind hash_tbl_sz, bytes = parse_length bytes in
      let%bind expire_hash_tbl_sz, bytes = parse_length bytes in
      let%bind kv_pairs, bytes = parse_kvs bytes [] in
      let database = { db_index; hash_tbl_sz; expire_hash_tbl_sz; kv_pairs } in
      parse_database' bytes (database :: acc)
    | _ -> Ok (acc, bytes)
  in
  parse_database' bytes []
;;

let parse_eof bytes =
  let open Result.Let_syntax in
  match bytes with
  | 0xFF :: bytes ->
    let%bind checksum_bytes, rest =
      Result.of_option ~error:"insufficient checksum bytes" (list_split_n_opt bytes 8)
    in
    Ok (ints_to_hex_str checksum_bytes, rest)
  | _ -> Error "malformed/missing end of file section"
;;

let parse_rdb bytes =
  let open Result.Let_syntax in
  let%bind header, bytes = parse_header bytes in
  let%bind metadata, bytes = parse_metadata bytes in
  let%bind databases, bytes = parse_databases bytes in
  let%bind checksum, bytes = parse_eof bytes in
  if not @@ List.is_empty bytes
  then Logs.warn (fun m -> m "trailing bytes found in RDB file, ignoring...");
  Ok { header; metadata; databases; checksum }
;;
