Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_values = ["foo"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_values = ["bar"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "other_list"; push_values = ["baz"; "bar"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "other_list"; push_values = ["foo"; "bar"; "baz"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "range_list"; push_values = ["a"; "b"; "c"; "d"; "e"]}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = 0; end_idx = 1}
  redis_ocaml_server: [INFO] Received command LRANGE {key = "range_list"; start_idx = 2; end_idx = 4}

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

Kill redis:
  $ ./kill_redis.sh
