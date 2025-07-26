Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "list_key"; push_values = ["foo"]}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "list_key"; push_values = ["bar"]}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "other_list";
    push_values = ["baz"; "bar"]}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "other_list";
    push_values = ["foo"; "bar"; "baz"]}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "range_list";
    push_values = ["a"; "b"; "c"; "d"; "e"]}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = 0; end_idx = 1}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = 2; end_idx = 4}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = -2; end_idx = -1}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = 0; end_idx = -3}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = true; push_key = "list_key_left";
    push_values = ["a"; "b"; "c"]}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "list_key_left"; start_idx = 0; end_idx = -1}
  redis_ocaml_server: [INFO] Received command (LLEN "new_list")
  redis_ocaml_server: [INFO] Received command (LLEN "list_key_left")
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "pop_list";
    push_values = ["a"; "b"; "c"; "d"]}
  redis_ocaml_server: [INFO] Received command LPOP {pop_key = "pop_list"; pop_count = None}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "pop_list"; start_idx = 0; end_idx = -1}
  redis_ocaml_server: [INFO] Received command LPOP {pop_key = "pop_list"; pop_count = (Some 2)}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "pop_list"; start_idx = 0; end_idx = -1}
  redis_ocaml_server: [INFO] Received command BLPOP {pop_key = "blpop_list"; pop_timeout = None}
  redis_ocaml_server: [INFO] Received command PUSH {from_left = false; push_key = "blpop_list";
    push_values = ["a"; "b"; "c"]}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "blpop_list"; start_idx = 0; end_idx = -1}
  redis_ocaml_server: [INFO] Received command BLPOP {pop_key = "blpop_list_2"; pop_timeout = (Some 1)}

Testing RPUSH create new list with one element:
  $ redis-cli rpush list_key "foo"
  1

Testing RPUSH append to existing list:
  $ redis-cli rpush list_key "bar"
  2

Testing RPUSH create new list with multiple element:
  $ redis-cli rpush other_list "baz" "bar"
  2

Testing RPUSH appending multiple elements to existing list:
  $ redis-cli rpush other_list "foo" "bar" "baz"
  5

Set up list for LRANGE
  $ redis-cli rpush range_list "a" "b" "c" "d" "e"
  5

Test LRANGE to list first 2 items
  $ redis-cli lrange range_list 0 1
  a
  b

Test LRANGE to list items from indexes 2-4
  $ redis-cli lrange range_list 2 4
  c
  d
  e

Test LRANGE to list last 2 items
  $ redis-cli lrange range_list -2 -1
  d
  e

Test LRANGE to list all items except last 2
  $ redis-cli lrange range_list 0 -3
  a
  b
  c

Testing LPUSH creating new list:
  $ redis-cli lpush list_key_left "a" "b" "c"
  3
  $ redis-cli lrange list_key_left 0 -1
  c
  b
  a

Testing LLEN on new list:
  $ redis-cli llen new_list
  0

Testing LLEN on existing list:
  $ redis-cli llen list_key_left
  3

Setting up list to LPOP:
  $ redis-cli rpush pop_list "a" "b" "c" "d"
  4

Testing LPOP with no argument:
  $ redis-cli lpop pop_list
  a
  $ redis-cli lrange pop_list 0 -1
  b
  c
  d

Testing LPOP with integer argument:
  $ redis-cli lpop pop_list 2
  b
  c
  $ redis-cli lrange pop_list 0 -1
  d

Testing BLPOP with no timeout:
  $ redis-cli blpop blpop_list 0 &
  blpop_list
  a
  $ redis-cli rpush blpop_list "a" "b" "c"
  3
  $ redis-cli lrange blpop_list 0 -1
  b
  c

Testing BLPOP with timeout:
  $ redis-cli blpop blpop_list_2 1
  

Kill redis:
  $ ./kill_redis.sh
