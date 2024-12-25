Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (TYPE "key")
  redis_ocaml_server: [INFO] Received command SET {set_key = "key"; set_value = "value"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (TYPE "key")

TYPE without setting:
  $ redis-cli TYPE key
  none

TYPE after setting:
  $ redis-cli SET key value
  OK
  $ redis-cli TYPE key
  string

Kill redis:
  $ ./kill_redis.sh
