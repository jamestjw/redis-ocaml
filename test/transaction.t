Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command MULTI
  redis_ocaml_server: [INFO] Received command SET {set_key = "bar"; set_value = "5"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (GET "bar")
  redis_ocaml_server: [INFO] Received command MULTI
  redis_ocaml_server: [INFO] Received command SET {set_key = "foo"; set_value = "41"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (INCR "foo")
  redis_ocaml_server: [INFO] Received command EXEC
  redis_ocaml_server: [INFO] Received command (GET "foo")
  redis_ocaml_server: [INFO] Received command EXEC

Cmd not executed without EXEC
  $ printf 'MULTI\nSET bar 5' | redis-cli
  OK
  QUEUED
  $ redis-cli GET bar
  

Start and queue commands in transaction:
  $ printf 'MULTI\nSET foo 41\nINCR foo\nEXEC' | redis-cli
  OK
  QUEUED
  QUEUED
  OK
  42
  $ redis-cli GET foo
  42

EXEC without starting transaction:
  $ redis-cli EXEC
  ERR EXEC without MULTI
  

Kill redis:
  $ ./kill_redis.sh
