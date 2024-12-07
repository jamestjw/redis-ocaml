Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

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
