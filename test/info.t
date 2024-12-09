Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

Get replication info as master
  $ redis-cli INFO replication
  role:master
  master_replid:8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb
  master_repl_offset:0

Start replica:
  $ nohup redis_ocaml_server --replicaof "localhost 6379" --port 6380 >/dev/null 2>&1 &
  $ sleep 0.2

Get replication info as slave
  $ redis-cli -p 6380 INFO replication
  role:slave

Kill redis:
  $ ./kill_redis.sh
