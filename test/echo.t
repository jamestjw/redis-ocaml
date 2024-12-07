Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

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
