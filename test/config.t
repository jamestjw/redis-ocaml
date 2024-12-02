Start redis:
  $ nohup redis_ocaml_server > /dev/null 2>&1 &
  $ sleep 0.5

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
  
