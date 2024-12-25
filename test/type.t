Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (TYPE "key")
  redis_ocaml_server: [INFO] Received command SET {set_key = "key"; set_value = "value"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (TYPE "key")
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", "1526919030474-0",
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (TYPE "stream_key")

TYPE without setting:
  $ redis-cli TYPE key
  none

TYPE after setting string key:
  $ redis-cli SET key value
  OK
  $ redis-cli TYPE key
  string

TYPE after setting stream key:
  $ redis-cli XADD stream_key 1526919030474-0 temperature 36 humidity 95
  1526919030474-0
  $ redis-cli TYPE stream_key
  stream

Kill redis:
  $ ./kill_redis.sh
