Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

GET without setting:
  $ redis-cli GET token
  

GET after SETting:
  $ redis-cli SET token 42f3a00b71795be5c24fe891ea2fa78c
  OK
  $ redis-cli GET token
  42f3a00b71795be5c24fe891ea2fa78c

SETting with timeout (milliseconds):
  $ redis-cli SET promotion 20 px 100
  OK
  $ redis-cli GET promotion
  20
  $ sleep 1
  $ redis-cli GET promotion
  

SETting with timeout (seconds):
  $ redis-cli SET message "happy birthday" ex 1
  OK
  $ redis-cli GET message
  happy birthday
  $ sleep 2
  $ redis-cli GET message
  

GET too many args:
  $ redis-cli GET key value
  ERR 'GET' takes one arg
  

SET unknown args:
  $ redis-cli SET key value pxx 5
  ERR unknown arg 'pxx' for 'SET'
  

SET set timeout twice with different args:
  $ redis-cli SET key value px 1000 ex 1
  ERR timeout set twice for 'SET' command
  
SET set timeout twice with same args:
  $ redis-cli SET key value px 1000 px 1
  ERR timeout set twice for 'SET' command
  

SET set timeout with zero:
  $ redis-cli SET key value px 0
  ERR 'SET' requires positive integer for timeout
  

SET set timeout with negative value:
  $ redis-cli SET key value px -1000
  ERR 'SET' requires positive integer for timeout
  

Kill redis:
  $ ./kill_redis.sh
