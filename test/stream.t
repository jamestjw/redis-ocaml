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
  redis_ocaml_server: [INFO] Received command (XRANGE ("stream_key", (1526919030474, 1), (1526919030476, 3)))

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
  
XRANGE:
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

Kill redis:
  $ ./kill_redis.sh
