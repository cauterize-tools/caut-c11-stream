#!/bin/sh

stack exec crucible -- tester \
  --build-cmd="stack exec caut-c11-stream -- --spec=%s --output=c11" \
  --build-cmd="cd c11 && ls *.c && clang -Wall -Wextra -std=c11 -pedantic *.c -o test_client" \
  --run-cmd="./c11/test_client" \
  --schema-count=10 \
  --instance-count=1000 \
  --type-count=100 \
  --enc-size=4096 \
  --prototypes=synonym,array,vector,record,combination,union,enumeration,range
