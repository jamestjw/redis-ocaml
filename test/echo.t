Start redis:
  $ nohup redis_ocaml_server > /dev/null 2>&1 &
  $ sleep 0.5

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
  
