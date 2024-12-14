Start redis:
  $ flock /tmp/redis-test.lock ../run_redis.sh --dir $PWD --dbfilename dump.rdb
  redis_ocaml_server: [INFO] RDB file ($TESTCASE_ROOT/dump.rdb) loaded
  redis_ocaml_server: [INFO] Received command (GET "hello")


GET key from rdb file:
  $ redis-cli GET hello
  world1

Kill redis:
  $ ../kill_redis.sh
