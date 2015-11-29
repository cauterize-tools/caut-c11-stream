#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "greatest.h"

#include "cauterize.h"
#include "cauterize_iterators.h"

#include "caut_test_types.h"
#include "caut_test_descriptors.h"

struct schema_encode_iterator sei;
struct schema_decode_iterator sdi;

struct type_encode_iterator tei[SCHEMA_DEPTH_caut_test];
struct type_decode_iterator tdi[SCHEMA_DEPTH_caut_test];

static void enc_test(void);
static void dec_test(void);

int main(int argc, char * argv[]) {
    (void)argc; (void)argv;

    printf("sizeof(sei) == %lu\n", sizeof(sei));
    printf("sizeof(tei) == %lu\n", sizeof(tei));
    printf("sizeof(sdi) == %lu\n", sizeof(sdi));
    printf("sizeof(tdi) == %lu\n", sizeof(tdi));

    enc_test();
    dec_test();

    return 0;
}

static void enc_test(void) {
    uint8_t buffer[32] = { 0 };

    struct complex const e = {
        .uni = {
            ._tag = uni_tag_a,
            .a = 0xAAAAAAAA,
        },
        .comb = {
            ._flags = 0x06,
            .b = 0xBBBB,
            .c = 0xCC,
        },
        .rec = {
            .a = 10,
            .b = 1002,
            .c = {
                ._length = 2,
                .elems = { 1, 2 },
            },
        },
        .rng1 = 1002,
        .en0 = en0_en_c,
    };

    enum caut_status const init_stat =
        schema_encode_iterator_init(
            &sei,
            &schema_descriptor_caut_test,
            tei,
            TYPE_COUNT_caut_test,
            type_id_caut_test_complex,
            &e);
    printf("init_stat: %u\n", init_stat);

    size_t encoded = 0;
    enum caut_status const enc_status =
        caut_enc_get(&sei, buffer, sizeof(buffer), &encoded);
    printf("enc_status: %u %lu\n", enc_status, encoded);

    for (size_t i = 0; i < encoded; i++) {
        printf("BYTE: %02X\n", buffer[i]);
    }
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
