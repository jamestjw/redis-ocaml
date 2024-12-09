#!/bin/bash

REDIS_PORT="${1:-6379}"
SEARCH_TERM="redis_ocaml"

result=$(netstat -lnp 2>/dev/null | grep $SEARCH_TERM | awk '{ print $7 }' | awk -F'/' '{print $1}')

if [ ! -z "$result" ]; then
  kill -9 $result 2>/dev/null
fi
