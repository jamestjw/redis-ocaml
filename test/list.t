Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_value = "foo"}
  redis_ocaml_server: [INFO] Received command RPUSH {push_key = "list_key"; push_value = "bar"}

Testing RPUSH create new list:
  $ redis-cli rpush list_key "foo"
  1

Testing RPUSH append to existing list:
  $ redis-cli rpush list_key "bar"
  2

Kill redis:
  $ ./kill_redis.sh
