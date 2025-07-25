Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_values = ["foo"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_values = ["bar"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "other_list"; push_values = ["baz"; "bar"]}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "other_list"; push_values = ["foo"; "bar"; "baz"]}

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

Kill redis:
  $ ./kill_redis.sh
