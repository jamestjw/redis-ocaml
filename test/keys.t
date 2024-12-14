KEYS fetches all keys from the store that are not expired

Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (KEYS "*")
  redis_ocaml_server: [INFO] Received command SET {set_key = "test-key"; set_value = "1997"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (KEYS "*")
  redis_ocaml_server: [INFO] Received command SET {set_key = "other-key1"; set_value = "some-value1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command SET {set_key = "other-key2"; set_value = "some-value2"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command SET {set_key = "other-key3"; set_value = "some-value3"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (KEYS "*")
  redis_ocaml_server: [INFO] Received command (KEYS "test*")
  redis_ocaml_server: [INFO] Received command (KEYS "*test*")
  redis_ocaml_server: [INFO] Received command (KEYS "*other*")
  redis_ocaml_server: [INFO] Received command (KEYS "*key")
  redis_ocaml_server: [INFO] Received command (KEYS "*key*")
  redis_ocaml_server: [INFO] Received command (KEYS "*k?y*")
  redis_ocaml_server: [INFO] Received command (KEYS "*key?")
  redis_ocaml_server: [INFO] Received command (KEYS "*key[12]")
  redis_ocaml_server: [INFO] Received command (KEYS "*key[^1]")
  redis_ocaml_server: [INFO] Received command (KEYS "*key[1-2]")
  redis_ocaml_server: [INFO] Received command SET {set_key = "key_a"; set_value = "1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command SET {set_key = "key_b"; set_value = "1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command SET {set_key = "key_c"; set_value = "1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command SET {set_key = "key_d"; set_value = "1"; set_timeout = None}
  redis_ocaml_server: [INFO] Received command (KEYS "key_[a-c]")

KEYS with empty store:
  $ redis-cli KEYS "*"
  

KEYS with after storing something:
  $ redis-cli SET test-key 1997
  OK
  $ redis-cli KEYS "*"
  test-key

KEYS with multiple keys:
  $ redis-cli SET other-key1 some-value1
  OK
  $ redis-cli SET other-key2 some-value2
  OK
  $ redis-cli SET other-key3 some-value3
  OK
  $ redis-cli KEYS "*"
  other-key1
  other-key2
  other-key3
  test-key

KEYS with star:
  $ redis-cli KEYS "test*"
  test-key
  $ redis-cli KEYS "*test*"
  test-key
  $ redis-cli KEYS "*other*"
  other-key1
  other-key2
  other-key3
  $ redis-cli KEYS "*key"
  test-key
  $ redis-cli KEYS "*key*"
  other-key1
  other-key2
  other-key3
  test-key

KEYS with wildcard
  $ redis-cli KEYS "*k?y*"
  other-key1
  other-key2
  other-key3
  test-key
  $ redis-cli KEYS "*key?"
  other-key1
  other-key2
  other-key3

KEYS with char set
  $ redis-cli KEYS "*key[12]"
  other-key1
  other-key2

KEYS with exclusion
  $ redis-cli KEYS "*key[^1]"
  other-key2
  other-key3

KEYS with char range using digits:
  $ redis-cli KEYS "*key[1-2]"
  other-key1
  other-key2

KEYS with char range using chars:
  $ redis-cli SET key_a 1
  OK
  $ redis-cli SET key_b 1
  OK
  $ redis-cli SET key_c 1
  OK
  $ redis-cli SET key_d 1
  OK
  $ redis-cli KEYS "key_[a-c]"
  key_a
  key_b
  key_c

Kill redis:
  $ ./kill_redis.sh
