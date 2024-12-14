Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (ECHO "test")
  redis_ocaml_server: [INFO] Received command (ECHO "123456")
  redis_ocaml_server: [INFO] Received command (ECHO "hello world")
  redis_ocaml_server: [INFO] Received command (INVALID "'ECHO' takes one arg")

Testing ECHO simple:
  $ redis-cli ECHO test
  test

Testing ECHO numbers:
  $ redis-cli ECHO 123456
  123456

Testing ECHO quoted:
  $ redis-cli ECHO "hello world"
  hello world

Testing ECHO wrong args:
  $ redis-cli ECHO arg1 arg2
  ERR 'ECHO' takes one arg
  
Kill redis:
  $ ./kill_redis.sh
