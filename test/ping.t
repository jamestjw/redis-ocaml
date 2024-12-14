Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command (INVALID "'PING' takes no args")

Testing PING lowercase:
  $ redis-cli ping
  PONG

Testing PING uppercase:
  $ redis-cli PING
  PONG

Testing PING mixed-case:
  $ redis-cli Ping
  PONG

Testing PING multiple:
  $ printf 'PING\nPING\nping' | redis-cli 
  PONG
  PONG
  PONG

Testing PING concurrent:
  $ redis-cli ping & redis-cli ping & redis-cli ping & redis-cli ping & sleep 0.2
  PONG
  PONG
  PONG
  PONG

Testing PING too many args:
  $ redis-cli PING 123
  ERR 'PING' takes no args
  
Kill redis:
  $ ./kill_redis.sh
