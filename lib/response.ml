open Base

type t =
  | SIMPLE of string
  | BULK of string
  | NULL_BULK
  | ERR of string
  | ARRAY of t list
  | FULL_RESYNC of string * string
  | QUIET

let rec serialize = function
  | SIMPLE s -> Printf.sprintf "+%s\r\n" s
  | BULK s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
  | NULL_BULK -> "$-1\r\n"
  | ERR s -> Printf.sprintf "-ERR %s\r\n" s
  | ARRAY l ->
    Printf.sprintf
      "*%d\r\n%s"
      (List.length l)
      (List.map ~f:serialize l |> String.concat ~sep:"")
  | FULL_RESYNC (replication_id, contents) ->
    Printf.sprintf
      "+FULLRESYNC %s 0\r\n$%d\r\n%s"
      replication_id
      (String.length contents)
      contents
  | QUIET -> ""
;;
