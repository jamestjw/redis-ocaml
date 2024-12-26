Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (AUTO_SEQ 0),
     [("temperature", "24"); ("humidity", "75")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030474, 0)),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030474, 1)),
     [("temperature", "45"); ("humidity", "91")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030475, 0)),
     [("temperature", "24"); ("humidity", "81")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030476, 2)),
     [("temperature", "28"); ("humidity", "83")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (AUTO_SEQ 1526919030476),
     [("temperature", "29"); ("humidity", "85")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (AUTO_SEQ 1526919030476),
     [("temperature", "30"); ("humidity", "87")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key_2", (AUTO_SEQ 1726919030476), [("age", "10")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030474, 0)),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (AUTO_SEQ 1526919030474),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030473, 0)),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key", (EXPLICIT (1526919030476, 1)),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XADD ("stream_key2", (EXPLICIT (0, 0)),
     [("temperature", "36"); ("humidity", "95")]))
  redis_ocaml_server: [INFO] Received command (XRANGE ("stream_key", (BTW ((1526919030474, 1), (1526919030476, 3)))))
  redis_ocaml_server: [INFO] Received command (XRANGE ("stream_key", (GTE (1526919030475, 0))))
  redis_ocaml_server: [INFO] Received command (XRANGE ("stream_key", (LTE (1526919030474, 9))))
  redis_ocaml_server: [INFO] Received command XREAD {block = None;
    queries =
    [("stream_key", (FRESHER_THAN (1526919030476, 2)));
      ("stream_key_2", (FRESHER_THAN (0, 0)))]}
  redis_ocaml_server: [INFO] Received command XREAD {block = None; queries = [("unk_key", (FRESHER_THAN (0, 0)))]}
  redis_ocaml_server: [INFO] Received command XREAD {block = (Some 200); queries = [("async", (FRESHER_THAN (0, 0)))]}
  redis_ocaml_server: [INFO] Received command (XADD ("async", (EXPLICIT (0, 1)), [("added", "later")]))
  redis_ocaml_server: [INFO] Received command (XADD ("dollar", (EXPLICIT (0, 1)), [("already", "here")]))
  redis_ocaml_server: [INFO] Received command XREAD {block = (Some 200); queries = [("dollar", LAST)]}
  redis_ocaml_server: [INFO] Received command (XADD ("dollar", (EXPLICIT (0, 2)), [("added", "later")]))

XADD with valid entry id:
  $ redis-cli XADD stream_key 0-* temperature 24 humidity 75
  0-1
  $ redis-cli XADD stream_key 1526919030474-0 temperature 36 humidity 95
  1526919030474-0
  $ redis-cli XADD stream_key 1526919030474-1 temperature 45 humidity 91
  1526919030474-1
  $ redis-cli XADD stream_key 1526919030475-0 temperature 24 humidity 81
  1526919030475-0
  $ redis-cli XADD stream_key 1526919030476-2 temperature 28 humidity 83
  1526919030476-2
  $ redis-cli XADD stream_key 1526919030476-* temperature 29 humidity 85
  1526919030476-3
  $ redis-cli XADD stream_key 1526919030476-* temperature 30 humidity 87
  1526919030476-4
  $ redis-cli XADD stream_key_2 1726919030476-* age 10
  1726919030476-0

XADD with invalid entry id:
  $ redis-cli XADD stream_key 1526919030474-0 temperature 36 humidity 95
  ERR The ID specified in XADD is equal or smaller than the target stream top item
  
  $ redis-cli XADD stream_key 1526919030474-* temperature 36 humidity 95
  ERR The ID specified in XADD is equal or smaller than the target stream top item
  
  $ redis-cli XADD stream_key 1526919030473-0 temperature 36 humidity 95
  ERR The ID specified in XADD is equal or smaller than the target stream top item
  
  $ redis-cli XADD stream_key 1526919030476-1 temperature 36 humidity 95
  ERR The ID specified in XADD is equal or smaller than the target stream top item
  
  $ redis-cli XADD stream_key2 0-0 temperature 36 humidity 95
  ERR The ID specified in XADD must be greater than 0-0
  
XRANGE with lower and upper:
  $ redis-cli XRANGE stream_key 1526919030474-1 1526919030476-3
  1526919030474-1
  temperature
  45
  humidity
  91
  1526919030475-0
  temperature
  24
  humidity
  81
  1526919030476-2
  temperature
  28
  humidity
  83
  1526919030476-3
  temperature
  29
  humidity
  85

XRANGE with lower:
  $ redis-cli XRANGE stream_key 1526919030475 +
  1526919030475-0
  temperature
  24
  humidity
  81
  1526919030476-2
  temperature
  28
  humidity
  83
  1526919030476-3
  temperature
  29
  humidity
  85
  1526919030476-4
  temperature
  30
  humidity
  87

XRANGE with upper:
  $ redis-cli XRANGE stream_key - 1526919030474-9
  0-1
  temperature
  24
  humidity
  75
  1526919030474-0
  temperature
  36
  humidity
  95
  1526919030474-1
  temperature
  45
  humidity
  91

XREAD with multiple keys:
  $ redis-cli XREAD streams stream_key stream_key_2 1526919030476-2 0-0
  stream_key
  1526919030476-3
  temperature
  29
  humidity
  85
  1526919030476-4
  temperature
  30
  humidity
  87
  stream_key_2
  1726919030476-0
  age
  10

XREAD with key that does not exist:
  $ redis-cli XREAD streams unk_key 0-0
  

XREAD with block:
  $ redis-cli XREAD block 200 streams async 0-0 &
  async
  0-1
  added
  later
  $ redis-cli XADD async 0-1 added later
  0-1

XREAD with block demanding new entries only ($):
  $ redis-cli XADD dollar 0-1 already here
  0-1
  $ redis-cli XREAD block 200 streams dollar $ &
  dollar
  0-2
  added
  later
  $ redis-cli XADD dollar 0-2 added later
  0-2

Kill redis:
  $ ./kill_redis.sh
