type command =
  | PING
  | ECHO of string
[@@deriving show { with_path = false }]
