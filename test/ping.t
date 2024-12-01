Start redis:
  $ nohup redis_ocaml_server > /dev/null 2>&1 &
  $ sleep 0.5

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
  $ echo -e "PING\nPING\nping" | redis-cli 
  PONG
  PONG
  PONG

Testing PING concurrent:
  $ redis-cli ping & redis-cli ping & redis-cli ping & redis-cli ping & sleep 0.2
  PONG
  PONG
  PONG
  PONG
