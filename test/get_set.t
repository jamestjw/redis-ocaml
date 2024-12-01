Start redis:
  $ nohup redis_ocaml_server > /dev/null 2>&1 &
  $ sleep 0.5

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
  
