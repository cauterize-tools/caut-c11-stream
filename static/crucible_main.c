#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>

#include "crucible_interface.h"

static bool read_exactly(FILE * fd, void * buf, size_t nbyte, size_t * rbyte);
static int run_client(FILE * si, FILE * so);
static int run_client_with_buffer(FILE * si, FILE * so, void * buffer, size_t buflen);

int main(int argc, char * argv[]) {
    (void)argc; (void)argv;



/*
    size_t const schema_length_word_size = #{slws};
    size_t const schema_tag_size = #{sts};
    size_t const schema_max_size = #{sms};
*/
    return run_client(stdin, stdout);
}

static int run_client(FILE * si, FILE * so) {
    uint8_t * buffer = calloc(sizeof(uint8_t), schema_max_size);
    int const ret = run_client_with_buffer(si, so, buffer, schema_max_size);
    free(buffer);
    return ret;
}

static int run_client_with_buffer(FILE * si, FILE * so, void * buffer, size_t buflen) {
    uint64_t length = 0;
    uint8_t type[20] = { 0 };
    size_t rlen;

    if (!read_exactly(si, &length, schema_length_word_size, &rlen)) {
        fprintf(stderr, "Length expected %lu bytes but got %lu bytes.\n",
                schema_length_word_size, rlen);
        return 1;
    }

    if (!read_exactly(si, type, schema_tag_size, &rlen)) {
        fprintf(stderr, "Type expected %lu bytes but got %lu bytes.\n",
                schema_tag_size, rlen);
        return 2;
    }

    if (length > buflen) {
        fprintf(stderr, "Header indicates length that exceeds buffer size: %llu > %lu.\n",
                length, buflen);
        return 3;
    }

    if (!read_exactly(si, buffer, length, &rlen)) {
        fprintf(stderr, "Header indicated %llu bytes are needed but only got %lu bytes.\n",
                length, rlen);
        return 3;
    }

    (void)so;

    return 0;
}

/*
 * read_exactly
 *
 *    Automatically re-reads a file descriptor until an error occurs or the
 *    expected number of bytes has been read.
 *
 *    fd - the file descriptor to read from.
 *    buf - the buffer to read into.
 *    nbyte - the number of bytes to read into the buffer.
 *    rbyte - the actual number of bytes read into the buffer. Will always
 *    equal nbyte if all reads were successful.
 *
 *    Returns true when no errors occurred and the proper number of bytes was
 *    read. Returns false otherwise.
 */
static bool read_exactly(FILE * fd, void * buf, size_t nbyte, size_t * rbyte) {
  uint8_t * bbuf = buf;
  size_t r = 0;

  while(r < nbyte) {
    uint8_t * next_pos = &(bbuf[r]);
    size_t l = fread(next_pos, 1, nbyte - r, fd);

    if (l == 0) {
      break;
    } else {
      r += (size_t)l;
    }
  }

  if (rbyte) {
    *rbyte = r;
  }

  return (r == nbyte);
}
