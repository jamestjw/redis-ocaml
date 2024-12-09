Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

Get replication info
  $ redis-cli INFO replication

Kill redis:
  $ ./kill_redis.sh
