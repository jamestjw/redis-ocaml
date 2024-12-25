type set_timeout =
  | PX of int (* milliseconds until expiry *)
  | EX of int (* seconds until expiry *)
[@@deriving show { with_path = false }]

type stream_id =
  | EXPLICIT of int * int (* <millisecondsTime>-<sequenceNumber> *)
  | AUTO_SEQ of int (* auto generate the sequence number *)
  | AUTO (* auto generate both *)
[@@deriving show { with_path = false }]

type t =
  | PING
  | MASTER_PING
  | ECHO of string
  | GET of string
  | SET of
      { set_key : string
      ; set_value : string
      ; set_timeout : set_timeout option
      }
  | GET_CONFIG of string list
  | KEYS of string
  | INFO of string list
  | REPL_CONF_PORT of int
  | REPL_CONF_CAPA of string (* replication capabilities *)
  | REPL_CONF_GET_ACK of string
  | PSYNC of string * int
  | WAIT of int * int
  | INVALID of string
  | TYPE of string
  (* key name, entry id, kv pairs *)
  | XADD of string * stream_id * (string * string) list
  | MASTER_SET of
      { set_key : string
      ; set_value : string
      ; set_timeout : set_timeout option
      }
[@@deriving show { with_path = false }]
