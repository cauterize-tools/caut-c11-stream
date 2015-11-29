#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "greatest.h"

#include "cauterize.h"
#include "cauterize_iterators.h"

#include "caut_test_types.h"
#include "caut_test_descriptors.h"

struct schema_encode_iterator sei;
struct type_encode_iterator tei[SCHEMA_DEPTH_caut_test];

struct schema_decode_iterator sdi;
struct type_decode_iterator tdi[SCHEMA_DEPTH_caut_test];

static struct schema_descriptor const * const sd = &schema_descriptor_caut_test;

static uint8_t enc_buffer[MAX_SIZE_caut_test];

static void dec_test(void);

TEST test_encode_primitive(void) {
    uint32_t enc = 0xAABBCCDD;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_u32,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(4, encoded);

    PASS();
}

TEST test_encode_synonym(void) {
    syn enc = 0xAABBCCDD;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_syn,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(4, encoded);

    PASS();
}

TEST test_encode_range(void) {
    rng0 enc = -10;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_rng0,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(2, encoded);

    PASS();
}

TEST test_encode_enumeration(void) {
    enum en0 enc = en0_en_c;
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_en0,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

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
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_arr,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(8, encoded);

    PASS();
}

TEST test_encode_vector(void) {
    struct vec enc = { ._length = 1, .elems = { 1 } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_vec,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(5, encoded);

    PASS();
}

TEST test_encode_record(void) {
    struct rec enc = { .a = -10, .b = 1050, .c = { ._length = 2, .elems = { 2, 3 } } };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_rec,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(12, encoded);

    PASS();
}

TEST test_encode_combination(void) {
    struct comb enc = { ._flags = 0x4, .c = 5 };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_comb,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

    ASSERT_EQ(caut_status_ok, enc_status);
    ASSERT_EQ(2, encoded);

    PASS();
}

TEST test_encode_union(void) {
    struct uni enc = { ._tag = uni_tag_b, .c = 5 };
    size_t encoded = 0;

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei, sd, tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_uni,
            &enc);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const enc_status =
        caut_enc_get(&sei, enc_buffer, sizeof(enc_buffer), &encoded);

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
            &sdi, sd, tdi,
            TYPE_COUNT_caut_test,
            type_id_caut_test_u32,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(&sdi, buffer, sizeof(buffer), &decoded);

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
            &sdi, sd, tdi,
            TYPE_COUNT_caut_test,
            type_id_caut_test_syn,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(&sdi, buffer, sizeof(buffer), &decoded);

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
            &sdi, sd, tdi,
            TYPE_COUNT_caut_test,
            type_id_caut_test_rng0,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(&sdi, buffer, sizeof(buffer), &decoded);

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
            &sdi, sd, tdi,
            TYPE_COUNT_caut_test,
            type_id_caut_test_en0,
            &dec);

    ASSERT_EQ(caut_status_ok, init_stat);

    enum caut_status const dec_status =
        caut_dec_put(&sdi, buffer, sizeof(buffer), &decoded);

    ASSERT_EQ_FMT(caut_status_ok, dec_status, "%d");
    ASSERT_EQ(1, decoded);
    ASSERT_EQ(en0_en_b, dec);

    PASS();
}

TEST test_decode_array(void) {
    FAIL();
}

TEST test_decode_vector(void) {
    FAIL();
}

TEST test_decode_record(void) {
    FAIL();
}

TEST test_decode_combination(void) {
    FAIL();
}

TEST test_decode_union(void) {
    FAIL();
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

GREATEST_MAIN_DEFS();

int main(int argc, char * argv[]) {
    GREATEST_MAIN_BEGIN();

    printf("Schema Encoding Iterators are %lu bytes in size.\n", sizeof(sei));
    printf("Type Encoding Iterators are %lu bytes in size.\n", sizeof(tei[0]));
    printf("Schema Decoding Iterators are %lu bytes in size.\n", sizeof(sdi));
    printf("Type Decoding Iterators are %lu bytes in size.\n", sizeof(tdi[0]));

    dec_test();

    RUN_SUITE(encode);
    RUN_SUITE(decode);

    GREATEST_MAIN_END();

    return 0;
}

static void dec_test(void) {
    uint8_t const buffer[] = { 0xAA, 0xBB, 0xCC, 0xDD };
    size_t const BSIZE = sizeof(buffer);
    uint32_t d = 0;
    uint8_t const * const dbuf = (uint8_t *)&d;

    enum caut_status const init_stat =
        schema_decode_iterator_init(
            &sdi,
            &schema_descriptor_caut_test,
            tdi,
            TYPE_COUNT_caut_test,
            type_id_caut_test_u32,
            &d);
    printf("init_stat: %u\n", init_stat);

    size_t decoded = 0;
    enum caut_status const dec_status =
        caut_dec_put(&sdi, buffer, BSIZE, &decoded);
    printf("dec_status: %u %lu\n", dec_status, decoded);

    for (size_t i = 0; i < BSIZE; i++) {
        printf("BYTE: %02X\n", dbuf[i]);
    }
}
