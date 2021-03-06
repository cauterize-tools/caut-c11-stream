#!/bin/sh

COMPILER="${COMPILER:-cc}"

stack exec crucible -- tester \
  --build-cmd="stack exec caut-c11-stream -- genall %s c11" \
  --build-cmd="cd c11 && ls *.c && $COMPILER -Os -Wall -Wextra -std=c11 -pedantic *.c -o test_client" \
  --run-cmd="./c11/test_client" \
  --schema-count=1 \
  --instance-count=1000 \
  --type-count=100 \
  --enc-size=1048576
