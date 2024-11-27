open Unix

let req_buffer_size = 50

let mk_server_socket () =
  (* Create a TCP server socket *)
  let server_socket = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  Unix.bind server_socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 6379));
  Unix.listen server_socket 1;
  server_socket
;;

let listen socket =
  let client_socket, _ = Unix.accept socket in
  client_socket
;;

let client_loop client_socket =
  let rec inner () =
    Fmt.pr "Listening...\n";
    let buffer = Bytes.create req_buffer_size in
    let bytes_read = Unix.recv client_socket buffer 0 req_buffer_size [] in
    (* Convert received bytes to a string *)
    let received_data = Bytes.sub_string buffer 0 bytes_read in
    Fmt.pr "Received %d bytes:\n %s" bytes_read received_data;
    let response = "+PONG\r\n" in
    let _ =
      Unix.send client_socket (Bytes.of_string response) 0 (String.length response) []
    in
    inner ()
  in
  inner ()
;;

let () =
  let server_socket = mk_server_socket () in
  let client_socket = listen server_socket in
  ignore @@ client_loop client_socket;
  close client_socket;
  close server_socket
;;
