#!/bin/bash

REDIS_PORT=6379

result=$(netstat -lnp 2>/dev/null | grep $REDIS_PORT | awk '{ print $7 }' | awk -F'/' '{print $1}')

# Check if result is not empty
if [ ! -z "$result" ]; then
  kill -9 $result 2>/dev/null
fi
