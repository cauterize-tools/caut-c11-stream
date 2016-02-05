#!/bin/sh

OUT_BIN="caut_test"
SANITIZE=""
CFLAGS="-O3 -g3 -Wall -Wextra --std=c11 -pedantic ${SANITIZE}"
INCLUDES="-Igenerated/"

stack build
stack exec cauterize -- --schema caut_test.schema --output caut_test.spec
stack exec caut-c11-stream -- --spec caut_test.spec --output generated

clang ${CFLAGS} ${INCLUDES} -Os -o ${OUT_BIN} \
      generated/caut_test_descriptors.c \
      generated/cauterize.c \
      generated/cauterize_encode.c \
      generated/cauterize_decode.c \
      generated/cauterize_util.c \
      generated/cauterize_descriptors.c \
      generated/cauterize_iterators.c \
      test_main.c

if [ $? -eq 0 ]; then
    ./${OUT_BIN} $@;
    exit 0;
else
    echo Build failed.;
    exit 100;
fi
