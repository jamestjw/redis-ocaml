Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh
  redis_ocaml_server: [INFO] RDB file (/tmp/redis-data/rdbfile) does not exist, starting with blank database
  redis_ocaml_server: [INFO] Received command (INFO ["replication"])
  redis_ocaml_server: [INFO] Received command PING
  redis_ocaml_server: [INFO] Received command (REPL_CONF_PORT 6380)
  redis_ocaml_server: [INFO] Received command (REPL_CONF_CAPA "psync2")
  redis_ocaml_server: [INFO] Received command (PSYNC ("?", -1))
  redis_ocaml_server: [INFO] Received command SET {set_key = "chicken"; set_value = "bbq"; set_timeout = None}

Get replication info as master
  $ redis-cli INFO replication
  role:master
  master_replid:8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb
  master_repl_offset:0

Start replica:
  $ redis_ocaml_server --replicaof "localhost 6379" --port 6380 &
  redis_ocaml_server: [INFO] Connecting to server...
  redis_ocaml_server: [INFO] Successfully connected to master.
  redis_ocaml_server: [INFO] Sending listening port 6380 to master
  redis_ocaml_server: [INFO] Sending replication capabilities psync2 to master
  redis_ocaml_server: [INFO] Initiating replication stream
  redis_ocaml_server: [INFO] Waiting for RDB dump
  redis_ocaml_server: [INFO] Listening for replication
  redis_ocaml_server: [INFO] RDB file loaded
  redis_ocaml_server: [INFO] Received command MASTER_PING from master
  redis_ocaml_server: [INFO] Received command (INFO ["replication"])
  redis_ocaml_server: [INFO] Received command MASTER_SET {set_key = "chicken"; set_value = "bbq"; set_timeout = None} from master
  redis_ocaml_server: [INFO] Received command (GET "chicken")
  $ sleep 0.2

Get replication info as replica
  $ redis-cli -p 6380 INFO replication
  role:slave

Write to master
  $ redis-cli -p 6379 SET chicken bbq
  OK

Read from replica
  $ sleep 0.1 && redis-cli -p 6380 GET chicken
  bbq

Kill redis:
  $ ./kill_redis.sh
