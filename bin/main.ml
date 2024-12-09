open Lwt
open Redis
open Core

let default_backlog = 5 (* max number of concurrent clients *)
let default_address = "127.0.0.1"
let default_port = 6379
let default_rdb_dir = "/tmp/redis-data"
let default_rdb_filename = "rdbfile"

let rec handle_connection ic oc server () =
  let%lwt cmd = Parser.get_cmd ic in
  match cmd with
  | Some cmd ->
    let%lwt _ = Logs_lwt.info (fun m -> m "Received command %s" @@ Cmd.show cmd) in
    let%lwt resp = Server.execute_cmd cmd server in
    Lwt_io.write oc (Response.serialize resp) >>= handle_connection ic oc server
  | None -> Logs_lwt.info (fun m -> m "Connection closed")
;;

let accept_connection server conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let%lwt () = Logs_lwt.info (fun m -> m "New connection") in
  Lwt.on_failure (handle_connection ic oc server ()) (fun e ->
    Logs.err (fun m -> m "%s" (Exn.to_string e)));
  return_unit
;;

let create_server_socket ~address ~port ~backlog =
  let open Lwt_unix in
  (* Create a TCP server socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  Lwt.async (fun _ -> bind sock (ADDR_INET (Core_unix.Inet_addr.of_string address, port)));
  listen sock backlog;
  sock
;;

let create_server ~sock ~rdb_dir ~rdb_filename ~replica_of =
  let server = Server.mk () in
  let rec loop () = Lwt_unix.accept sock >>= accept_connection server >>= loop in
  let start () =
    Lwt.on_failure (Server.run server ~rdb_dir ~rdb_filename ~replica_of) (fun e ->
      Logs.err (fun m -> m "%s" (Exn.to_string e)));
    loop ()
  in
  start
;;

let parse_replica_of str =
  if not @@ Str.string_match (Str.regexp {|^\([^ ]+\)[ ]+\([0-9]+\)$|}) str 0
  then failwith "master format incorrect: expected '<MASTER_HOST> <MASTER_PORT>'"
  else Str.matched_group 1 str, int_of_string @@ Str.matched_group 2 str
;;

let command =
  Command.basic
    ~summary:"Implementation of a Redis server in OCaml."
    (let%map_open.Command rdb_dir =
       flag
         "--dir"
         (optional string)
         ~doc:"path the path where the RDB file should be stored"
     and rdb_file_name =
       flag "--dbfilename" (optional string) ~doc:"filename the name of the RDB file"
     and port =
       flag
         "--port"
         (optional int)
         ~doc:"port_number the port that the TCP server should listen to"
     and replica_of =
       flag
         "--replicaof"
         (optional string)
         ~doc:
           "address_port the TCP server that hosts the master instance that we should \
            replicate"
     in
     let rdb_dir = Option.value rdb_dir ~default:default_rdb_dir in
     let rdb_filename = Option.value rdb_file_name ~default:default_rdb_filename in
     let port = Option.value port ~default:default_port in
     let replica_of = Option.map ~f:parse_replica_of replica_of in
     fun () ->
       let server_socket =
         create_server_socket ~address:default_address ~port ~backlog:default_backlog
       in
       let serve = create_server ~sock:server_socket ~rdb_dir ~rdb_filename ~replica_of in
       Lwt_main.run @@ serve ())
;;

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  Command_unix.run command
;;
