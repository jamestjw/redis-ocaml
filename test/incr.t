Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command SET {set_key = "key"; set_value = "1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (INCR "key")
  redis_ocaml_server: [INFO] Received command (INCR "key")
  redis_ocaml_server: [INFO] Received command (INCR "key")
  redis_ocaml_server: [INFO] Received command SET {set_key = "not_int"; set_value = "foo"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (INCR "not_int")
  redis_ocaml_server: [INFO] Received command (GET "not_int")

INCR a valid key:
  $ redis-cli SET key 1
  OK
  $ redis-cli INCR key
  2
  $ redis-cli INCR key
  3
  $ redis-cli INCR key
  4

INCR an invalid key:
  $ redis-cli SET not_int foo
  OK
  $ redis-cli INCR not_int
  ERR value is not an integer or out of range
  
  $ redis-cli GET not_int
  foo

Kill redis:
  $ ./kill_redis.sh
