Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (GET_CONFIG ["dir"])
  redis_ocaml_server: [INFO] Received command (GET_CONFIG ["dbfilename"])
  redis_ocaml_server: [INFO] Received command (GET_CONFIG ["dir"; "dbfilename"])
  redis_ocaml_server: [INFO] Received command (GET_CONFIG ["asdf"])
  redis_ocaml_server: [INFO] Received command (INVALID "wrong number of arguments for 'config|get' command")

Testing CONFIG GET dir:
  $ redis-cli CONFIG GET dir
  dir
  /tmp/redis-data

Testing CONFIG GET dbfilename:
  $ redis-cli CONFIG GET dbfilename
  dbfilename
  rdbfile

Testing CONFIG GET multiple keys:
  $ redis-cli CONFIG GET dir dbfilename
  dir
  /tmp/redis-data
  dbfilename
  rdbfile

Testing CONFIG GET key does not exist:
  $ redis-cli CONFIG GET asdf
  

Testing CONFIG GET no keys:
  $ redis-cli CONFIG GET
  ERR wrong number of arguments for 'config|get' command
  
Kill redis:
  $ ./kill_redis.sh
