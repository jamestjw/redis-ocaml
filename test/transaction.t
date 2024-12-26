Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command MULTI
  redis_ocaml_server: [INFO] Received command SET {set_key = "foo"; set_value = "41"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (INCR "foo")

Start and queue commands in transaction:
  $ printf 'MULTI\nSET foo 41\nINCR foo' | redis-cli 
  OK
  QUEUED
  QUEUED

Kill redis:
  $ ./kill_redis.sh
