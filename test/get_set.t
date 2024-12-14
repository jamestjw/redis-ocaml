Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (GET "token")
  redis_ocaml_server: [INFO] Received command SET {set_key = "token"; set_value = "42f3a00b71795be5c24fe891ea2fa78c";
    set_timeout = None}
  redis_ocaml_server: [INFO] Received command (GET "token")
  redis_ocaml_server: [INFO] Received command SET {set_key = "promotion"; set_value = "20"; set_timeout = (Some (PX 100))}
  redis_ocaml_server: [INFO] Received command (GET "promotion")
  redis_ocaml_server: [INFO] Received command (GET "promotion")
  redis_ocaml_server: [INFO] Received command SET {set_key = "message"; set_value = "happy birthday";
    set_timeout = (Some (EX 1))}
  redis_ocaml_server: [INFO] Received command (GET "message")
  redis_ocaml_server: [INFO] Received command (GET "message")
  redis_ocaml_server: [INFO] Received command (INVALID "'GET' takes one arg")
  redis_ocaml_server: [INFO] Received command (INVALID "unknown arg 'pxx' for 'SET'")
  redis_ocaml_server: [INFO] Received command (INVALID "timeout set twice for 'SET' command")
  redis_ocaml_server: [INFO] Received command (INVALID "timeout set twice for 'SET' command")
  redis_ocaml_server: [INFO] Received command (INVALID "'SET' requires positive integer for timeout")
  redis_ocaml_server: [INFO] Received command (INVALID "'SET' requires positive integer for timeout")

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
  $ sleep 0.2
  $ redis-cli GET promotion
  

SETting with timeout (seconds):
  $ redis-cli SET message "happy birthday" ex 1
  OK
  $ redis-cli GET message
  happy birthday
  $ sleep 1
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
