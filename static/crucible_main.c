#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>

#include "crucible_interface.h"
#include "cauterize.h"
#include "cauterize_info.h"

static bool read_exactly(FILE * fd, void * buf, size_t nbyte, size_t * rbyte);
static int run_client(FILE * si, FILE * so);
static struct type_descriptor const * find_type(uint8_t * tag, size_t taglen);
static int transcode(
    uint8_t const * in_buffer,
    uint8_t * out_buffer,
    size_t buflen,
    struct type_descriptor const * td);

int main(int argc, char * argv[]) {
    (void)argc; (void)argv;
    return run_client(stdin, stdout);
}

static int run_client(FILE * si, FILE * so) {
    uint64_t length = 0;
    uint8_t type[20] = { 0 };
    size_t rlen;

    if (!read_exactly(si, &length, schema_length_word_size, &rlen)) {
        fprintf(stderr, "Length expected %lu bytes but got %lu bytes.\n",
                schema_length_word_size, rlen);
        return __LINE__;
    }

    if (0 == length) {
        fprintf(stderr, "Length is 0.\n");
        return __LINE__;
    }

    if (!read_exactly(si, type, schema_tag_size, &rlen)) {
        fprintf(stderr, "Type expected %lu bytes but got %lu bytes.\n",
                schema_tag_size, rlen);
        return __LINE__;
    }

    void * const buffer = calloc(length, sizeof(uint8_t));

    if (!read_exactly(si, buffer, length, &rlen)) {
        fprintf(stderr, "Header indicated %llu bytes are needed but only got %lu bytes.\n",
                length, rlen);
        return __LINE__;
    }

    struct type_descriptor const * const t = find_type(type, schema_tag_size);

    if (NULL == t) {
        fprintf(stderr, "Could not find type matching tag.\n");
        return __LINE__;
    }

    void * out_buffer = calloc(length, sizeof(uint8_t));
    int const tret = transcode(buffer, out_buffer, length, t);

    if (0 != tret) {
        free(out_buffer);
        return tret;
    }

    if (schema_length_word_size != fwrite(&length, 1, schema_length_word_size, so)) {
        fprintf(stderr, "Could not completely write length.\n");
        return __LINE__;
    }
    if (schema_tag_size != fwrite(type, 1, schema_tag_size, so)) {
        fprintf(stderr, "Could not completely write tag.\n");
        return __LINE__;
    }
    if (length != fwrite(out_buffer, 1, length, so)) {
        fprintf(stderr, "Could not completely write output buffer.\n");
        return __LINE__;
    }

    free(out_buffer);

    return 0;
}

static struct type_descriptor const * find_type(uint8_t * tag, size_t taglen) {
    struct schema_descriptor const * const sd = schema_schema_descriptor;
    struct schema_info const * const si = schema_schema_info;

    for (size_t i = 0; i < sd->type_count; i++) {
        struct type_descriptor const * const td = &sd->types[i];
        struct type_info const * const ti = &si->types[i];

        if (0 == memcmp(tag, ti->fingerprint, taglen)) {
            return td;
        }
    }

    return NULL;
}

static int transcode(uint8_t const * in_buffer, uint8_t * out_buffer, size_t buflen, struct type_descriptor const * td) {
    void * const obj = calloc(1, td->obj_size);
    struct schema_descriptor const * const sd = schema_schema_descriptor;

    // decode
    {
        struct schema_decode_iterator sdi;
        struct type_decode_iterator * tdi = calloc(schema_depth, sizeof(*tdi));

        size_t decoded = 0;

        enum caut_status const init_stat =
            schema_decode_iterator_init(
                &sdi, sd, tdi,
                schema_depth,
                td->type_id,
                obj);

        if (caut_status_ok != init_stat) {
            fprintf(stderr, "Could not initialize decoding iterator (%d)\n", init_stat);
            return __LINE__;
        }

        enum caut_status const dec_status =
            caut_dec_put(&sdi, in_buffer, buflen, &decoded);

        if (caut_status_ok != dec_status) {
            fprintf(stderr, "Could not put buffer into object (%d)\n", dec_status);
            return __LINE__;
        }


        if (buflen != decoded) {
            fprintf(stderr, "Decoded an unexpected number of bytes (expected: %lu, actual: %lu).\n",
                    buflen, decoded);
        }

        free(tdi);
    }

    // encode
    {
        struct schema_encode_iterator sei;
        struct type_encode_iterator * tei = calloc(schema_depth, sizeof(*tei));

        size_t encoded = 0;

        enum caut_status const init_stat =
            schema_encode_iterator_init(
                &sei, sd, tei,
                schema_depth,
                td->type_id,
                obj);

        if (caut_status_ok != init_stat) {
            fprintf(stderr, "Could not initialize encoding iterator (%d)\n", init_stat);
            return __LINE__;
        }

        fprintf(stderr,
                "buflen (%lu)\n", buflen);
        enum caut_status const enc_status =
            caut_enc_get(&sei, out_buffer, buflen, &encoded);

        if (caut_status_ok != enc_status) {
            fprintf(stderr, "Could not put buffer into object (%d)\n", enc_status);
            return __LINE__;
        }


        if (buflen != encoded) {
            fprintf(stderr, "Decoded an unexpected number of bytes (expected: %lu, actual: %lu).\n",
                    buflen, encoded);
        }

        free(tei);
    }

    free(obj);

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
