#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "greatest.h"

#include "cauterize.h"
#include "cauterize_iterators.h"

#include "caut_test_types.h"
#include "caut_test_descriptors.h"

#define IGNORE_TEST(test) (void)test

static struct schema_descriptor const * const sd = &schema_descriptor_caut_test;

struct schema_encode_iterator * sei = NULL;
struct type_encode_iterator * tei = NULL;

struct schema_decode_iterator * sdi = NULL;
struct type_decode_iterator * tdi = NULL;

static uint8_t * enc_buffer = NULL;
static size_t buf_size = 0;

TEST test_encode_primitive(void) {
    uint32_t enc = 0xAABBCCDD;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_u32,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(4, encoded);

    PASS();
}

TEST test_encode_synonym(void) {
    syn enc = 0xAABBCCDD;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_syn,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(4, encoded);

    PASS();
}

TEST test_encode_range(void) {
    rng0 enc = -10;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rng0,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(2, encoded);

    PASS();
}

TEST test_encode_enumeration(void) {
    enum en0 enc = en0_en_c;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_en0,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(1, encoded);
    ASSERT_EQ(2, enc_buffer[0]);

    PASS();
}

TEST test_encode_array(void) {
    struct arr enc = { { 1, 2 } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_arr,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(8, encoded);

    PASS();
}

TEST test_encode_vector(void) {
    struct vec enc = { ._length = 1, .elems = { 1 } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_vec,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(5, encoded);

    PASS();
}

TEST test_encode_record(void) {
    struct rec enc = { .a = 1, .b = 2, .c = { ._length = 2, .elems = { 2, 3 } } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rec,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(15, encoded);

    PASS();
}

TEST test_encode_combination(void) {
    struct comb enc = { ._flags = 0x4, .c = 5 };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_comb,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(2, encoded);

    PASS();
}

TEST test_encode_union(void) {
    struct uni enc = { ._tag = uni_tag_b, .c = 5 };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_uni,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(3, encoded);

    PASS();
}

TEST test_decode_primitive(void) {
    uint8_t const buffer[] = { 0xAA, 0xBB, 0xCC, 0xDD };
    size_t decoded = 0;

    uint32_t dec = 0;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_u32,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ(caut_status_ok, dec_status);
    ASSERT_EQ(4, decoded);
    ASSERT_EQ(0xDDCCBBAA, dec);

    PASS();
}

TEST test_decode_synonym(void) {
    uint8_t const buffer[] = { 0xAA, 0xBB, 0xCC, 0xDD };
    size_t decoded = 0;

    syn dec = 0;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_syn,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ(caut_status_ok, dec_status);
    ASSERT_EQ(4, decoded);
    ASSERT_EQ(0xDDCCBBAA, dec);

    PASS();
}

TEST test_decode_range(void) {
    uint8_t const buffer[] = { 0xDE, 0x03 };
    size_t decoded = 0;

    rng0 dec = 0;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rng0,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ(caut_status_ok, dec_status);
    ASSERT_EQ(2, decoded);
    ASSERT_EQ(-10, dec);

    PASS();
}

TEST test_decode_enumeration(void) {
    uint8_t const buffer[] = { 0x01 };
    size_t decoded = 0;

    enum en0 dec = 0;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_en0,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ(1, decoded);
    ASSERT_EQ(en0_en_b, dec);

    PASS();
}

TEST test_decode_array(void) {
    uint8_t const buffer[] = { 0x01, 0x00, 0x00, 0x00
                             , 0x02, 0x00, 0x00, 0x00 };
    size_t decoded = 0;

    struct arr dec = { { 0, 0 } };

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_arr,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ(8, decoded);
    ASSERT_EQ(1, dec.elems[0]);
    ASSERT_EQ(2, dec.elems[1]);

    PASS();
}

TEST test_decode_vector(void) {
    uint8_t const buffer[] = { 0x01,
                               0x05, 0x00, 0x00, 0x00 };

    size_t decoded = 0;
    struct vec dec = { 0, { 0, 0 } };

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_vec,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ(5, decoded);
    ASSERT_EQ_FMT(1, dec._length, "%d");
    ASSERT_EQ(5, dec.elems[0]);

    PASS();
}

TEST test_decode_record(void) {
    uint8_t const buffer[] = {
        0xAA, 0xBB,
        0xCC, 0xDD, 0xEE, 0xFF,
        0x01, 0x09, 0x00, 0x00, 0x00
    };

    size_t decoded = 0;
    struct rec dec = { 0, 0, { 0, { 0, 0 } } };

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rec,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ(0xBBAA, dec.a);
    ASSERT_EQ(0xFFEEDDCC, dec.b);
    ASSERT_EQ(1, dec.c._length);
    ASSERT_EQ(9, dec.c.elems[0]);

    PASS();
}

TEST test_decode_combination(void) {
    uint8_t const buffer[] = { 0x02, 0xBB, 0xAA };
    size_t decoded = 0;
    struct comb dec = { 0, 0, 0, 0 };

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_comb,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ_FMT(2, dec._flags, "%d");
    ASSERT_EQ_FMT(0xAABB, dec.b, "%X");

    PASS();
}

TEST test_decode_union(void) {
    uint8_t const buffer[] = { 0x02, 0xBB };
    size_t decoded = 0;
    struct uni dec = { 0, { 0 } };

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_uni,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ_FMT(2, dec._tag, "%d");
    ASSERT_EQ_FMT(0xBB, dec.c, "%X");

    PASS();
}

TEST test_float_in_vector(void) {
    // encode
    struct floatvec enc = { ._length = 1, .elems = { -4.64 } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_floatvec,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ_FMT(9, encoded, "%lu");

    ASSERT_EQ_FMT(1, enc_buffer[0], "%02X");

    ASSERT_EQ_FMT(0x8F, enc_buffer[1], "%02X");
    ASSERT_EQ_FMT(0xC2, enc_buffer[2], "%02X");
    ASSERT_EQ_FMT(0xF5, enc_buffer[3], "%02X");
    ASSERT_EQ_FMT(0x28, enc_buffer[4], "%02X");
    ASSERT_EQ_FMT(0x5C, enc_buffer[5], "%02X");
    ASSERT_EQ_FMT(0x8F, enc_buffer[6], "%02X");
    ASSERT_EQ_FMT(0x12, enc_buffer[7], "%02X");
    ASSERT_EQ_FMT(0xC0, enc_buffer[8], "%02X");

    PASS();
}

TEST test_big_range_enc(void) {
    rng_big_unsigned const enc = 2624738754863070707;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            sei, sd, tei,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rng_big_unsigned,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(sei, enc_buffer, buf_size, &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(8, encoded);

    PASS();
}

TEST test_big_range_dec(void) {
    rng_big_unsigned const reference = 2624738754863070707;
    uint8_t const * const ref_buf = (uint8_t const *)&reference;
    size_t decoded = 0;
    rng_big_unsigned dec = 0;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            sdi, sd, tdi,
            SCHEMA_DEPTH_caut_test,
            type_id_caut_test_rng_big_unsigned,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(sdi, ref_buf, sizeof(reference), &decoded);

    ASSERT_EQ(caut_status_ok, dec_status);
    ASSERT_EQ(8, decoded);

    PASS();
}

TEST test_big_range_round_trip(void) {
    rng_big_unsigned const reference = 2624738754863070707;

    size_t encoded = 0;
    {
        rng_big_unsigned const enc = reference;

        enum caut_status const init_stat =
            schema_encode_iterator_init(
                sei, sd, tei,
                SCHEMA_DEPTH_caut_test,
                type_id_caut_test_rng_big_unsigned,
                &enc);

        ASSERT_EQ(caut_status_ok, init_stat);

        enum caut_status const enc_status =
            caut_enc_get(sei, enc_buffer, buf_size, &encoded);

        ASSERT_EQ(caut_status_ok, enc_status);
        ASSERT_EQ(8, encoded);
    }

    size_t decoded = 0;
    {
        rng_big_unsigned dec = 0;

        enum caut_status const init_stat =
            schema_decode_iterator_init(
                sdi, sd, tdi,
                SCHEMA_DEPTH_caut_test,
                type_id_caut_test_rng_big_unsigned,
                &dec);

        ASSERT_EQ(caut_status_ok, init_stat);

        enum caut_status const dec_status =
            caut_dec_put(sdi, enc_buffer, encoded, &decoded);

        ASSERT_EQ(caut_status_ok, dec_status);
        ASSERT_EQ(8, decoded);

        ASSERT_EQ(reference, dec);
    }

    ASSERT_EQ(encoded, decoded);

    PASS();
}

SUITE(encode) {
    RUN_TEST(test_encode_primitive);
    RUN_TEST(test_encode_synonym);
    RUN_TEST(test_encode_range);
    RUN_TEST(test_encode_enumeration);
    RUN_TEST(test_encode_array);
    RUN_TEST(test_encode_vector);
    RUN_TEST(test_encode_record);
    RUN_TEST(test_encode_combination);
    RUN_TEST(test_encode_union);
}

SUITE(decode) {
    RUN_TEST(test_decode_primitive);
    RUN_TEST(test_decode_synonym);
    RUN_TEST(test_decode_range);
    RUN_TEST(test_decode_enumeration);
    RUN_TEST(test_decode_array);
    RUN_TEST(test_decode_vector);
    RUN_TEST(test_decode_record);
    RUN_TEST(test_decode_combination);
    RUN_TEST(test_decode_union);
}

SUITE(corner) {
    RUN_TEST(test_float_in_vector);
    RUN_TEST(test_big_range_enc);
    RUN_TEST(test_big_range_dec);
    RUN_TEST(test_big_range_round_trip);
}

GREATEST_MAIN_DEFS();

int main(int argc, char * argv[]) {
    sei = calloc(sizeof(*sei), 1);
    tei = calloc(sizeof(*tei), SCHEMA_DEPTH_caut_test);

    sdi = calloc(sizeof(*sdi), 1);
    tdi = calloc(sizeof(*tdi), SCHEMA_DEPTH_caut_test);
    enc_buffer = calloc(sizeof(uint8_t), MAX_SIZE_caut_test);
    buf_size = MAX_SIZE_caut_test;

    GREATEST_MAIN_BEGIN();


    printf("Schema Encoding Iterators are %lu bytes in size.\n", sizeof(*sei));
    printf("Type Encoding Iterators are %lu bytes in size.\n", sizeof(*tei));
    printf("Schema Decoding Iterators are %lu bytes in size.\n", sizeof(*sdi));
    printf("Type Decoding Iterators are %lu bytes in size.\n", sizeof(*tdi));

    RUN_SUITE(encode);
    RUN_SUITE(decode);
    RUN_SUITE(corner);

    GREATEST_MAIN_END();

    free(sei);
    free(tei);
    free(sdi);
    free(tdi);
    free(enc_buffer);

    return 0;
}
