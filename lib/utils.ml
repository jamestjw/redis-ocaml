open Core
open Lwt
module StringMap = Stdlib.Map.Make (String)

let read_all_bytes filename =
  let ic = In_channel.create ~binary:true filename in
  let rec read_all acc =
    match In_channel.input_byte ic with
    | None -> List.rev acc
    | Some b -> read_all (b :: acc)
  in
  read_all []
;;

(* TODO: don't blindly assume that we got a carriage return, or if there
   was a newline at all *)
let read_line_with_length_opt ic =
  let%lwt msg = Lwt_io.read_line_opt ic in
  Lwt.return @@ Option.map msg ~f:(fun msg -> msg, String.length msg + 2)
;;

(* TODO: is it better to sleep with Float.infinity? *)
let rec forever () = Lwt_unix.sleep 0.5 >>= forever
