open Core
open Lwt
module StringMap = Stdlib.Map.Make (String)

let uuid = Uuidm.v4_gen (Stdlib.Random.State.make_self_init ())
let mk_uuid () = Uuidm.to_string (uuid ())

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

module Time = struct
  let int_ms_to_ns ms = Int63.(of_int ms * of_int 1_000_000)
  let int_s_to_ns s = Int63.(of_int s * of_int 1_000_000_000)
  let ns_to_ms ns = Int63.(ns / of_int 1_000_000)
end
