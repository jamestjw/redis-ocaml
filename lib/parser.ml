open Core
open Lwt
module StringMap = Stdlib.Map.Make (String)

let num_args_regex = Str.regexp {|\*\([0-9]+\)|}
let arg_len_regex = Str.regexp {|\$\([0-9]+\)|}

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

let args_to_cmd args =
  match lower_fst args with
  | "ping" :: args -> parse_ping_cmd args
  | "echo" :: args -> parse_echo_cmd args
  | "get" :: args -> parse_get_cmd args
  | "set" :: args -> parse_set_cmd args
  | "config" :: args -> parse_config_cmd args
  | cmd :: _ -> Cmd.INVALID (Printf.sprintf "unrecognised command %s" cmd)
  | _ -> Cmd.INVALID "invalid command"
;;

(* Polls the input channel for a valid command, if this returns None this
   means that the connection has been dropped by the client. *)
let rec get_cmd ic =
  let parse_len regexp =
    let%lwt msg = Lwt_io.read_line_opt ic in
    match msg with
    | None -> return Disconnected
    | Some msg ->
      if Str.string_match regexp msg 0
      then return @@ Parsed (Stdlib.int_of_string @@ Str.matched_group 1 msg)
      else return @@ InvalidFormat msg
  in
  let parse_args num_args =
    let rec parse_args' num_args acc =
      if num_args = 0
      then return @@ Parsed (List.rev acc)
      else (
        let%lwt arg_len = parse_len arg_len_regex in
        match arg_len with
        | Disconnected -> return Disconnected
        | InvalidFormat s ->
          let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
          return (InvalidFormat s)
        | Parsed arg_len ->
          let%lwt msg = Lwt_io.read_line_opt ic in
          (match msg with
           | None -> return Disconnected
           | Some arg when String.length arg <> arg_len ->
             let%lwt _ =
               Logs_lwt.err (fun m -> m "Argument (%s) length != %d" arg arg_len)
             in
             return @@ InvalidFormat arg
           | Some arg -> parse_args' (num_args - 1) (arg :: acc)))
    in
    parse_args' num_args []
  in
  let%lwt num_args = parse_len num_args_regex in
  match num_args with
  | Disconnected -> return None
  | InvalidFormat s ->
    let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
    get_cmd ic
  | Parsed num_args when num_args <= 0 -> get_cmd ic
  | Parsed num_args ->
    let%lwt arg_list = parse_args num_args in
    (match arg_list with
     | Disconnected -> return None
     | InvalidFormat s -> return @@ Some (Cmd.INVALID s)
     | Parsed arg_list -> return @@ Some (args_to_cmd arg_list))
;;

(* RDB file, parsed based on the specs in https://rdb.fnordig.de/file_format.html *)

type header = { rdb_version : int }
type metadata = string StringMap.t

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
      let%bind value, bytes = parse_length_prefixed_string bytes in
      parse_kvs bytes (StringMap.add key value map)
    | rest -> Ok (map, rest)
  in
  parse_kvs bytes StringMap.empty
;;

let ints_to_hex_str ints =
  List.map ~f:(fun i -> Printf.sprintf "%X" i) ints |> String.concat
;;

let parse_eof lines =
  let open Result.Let_syntax in
  match lines with
  | 0xFF :: bytes ->
    let%bind checksum_bytes, rest =
      Result.of_option ~error:"insufficient checksum bytes" (list_split_n_opt bytes 8)
    in
    Ok (ints_to_hex_str checksum_bytes, rest)
  | _ -> Error "malformed/missing end of file section"
;;

let parse_rdb str = failwith "todo"
