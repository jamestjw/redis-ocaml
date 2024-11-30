type t =
  | SIMPLE of string
  | BULK of string
  | NULL_BULK

let serialize = function
  | SIMPLE s -> Printf.sprintf "+%s\r\n" s
  | BULK s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
  | NULL_BULK -> "$-1\r\n"
;;
