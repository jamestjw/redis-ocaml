open Core

type set_timeout =
  | PX of int (* milliseconds until expiry *)
  | EX of int (* seconds until expiry *)
[@@deriving show { with_path = false }]

type stream_id =
  | EXPLICIT of int * int (* <millisecondsTime>-<sequenceNumber> *)
  | AUTO_SEQ of int (* auto generate the sequence number *)
  | AUTO (* auto generate both *)
[@@deriving show { with_path = false }]

type 'a range =
  | BTW of 'a * 'a
  | GTE of 'a
  | GT of 'a
  | LTE of 'a
  | ALL
[@@deriving show { with_path = false }]

type 'a freshness =
  | FRESHER_THAN of 'a
  | LAST
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
  | XRANGE of string * (int * int) range
  | XREAD of
      { block : int option
      ; queries : (string * (int * int) freshness) list
      }
  | INCR of string
  | MULTI
  | EXEC
  | DISCARD
  | MASTER_SET of
      { set_key : string
      ; set_value : string
      ; set_timeout : set_timeout option
      }
[@@deriving show { with_path = false }]

let is_in_range a = function
  | BTW (lower, upper) -> Poly.compare a lower >= 0 && Poly.compare a upper <= 0
  | GTE lower -> Poly.compare a lower >= 0
  | GT lower -> Poly.compare a lower > 0
  | LTE upper -> Poly.compare a upper <= 0
  | ALL -> true
;;

let is_txn_cmd = function
  | MULTI | EXEC | DISCARD -> true
  | _ -> false
;;
