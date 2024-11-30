type command =
  | PING
  | ECHO of string
  | GET of string
  | SET of string * string
[@@deriving show { with_path = false }]
