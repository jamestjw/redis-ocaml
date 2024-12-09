#!/bin/bash

REDIS_PORT=6379
ARGS="$@"

# Loop until port is not active so we can launch our instance of Redis
while [ ! -z "$(netstat -lnp 2>/dev/null | grep $REDIS_PORT)" ]; do
  # Sleep between iterations to avoid overwhelming the system
  sleep 0.2
done

nohup redis_ocaml_server $ARGS >/dev/null 2>&1 &

while [ -z "$(netstat -lnp 2>/dev/null | grep $REDIS_PORT)" ]; do
  sleep 0.1 # Wait until server is ready before returning
done
