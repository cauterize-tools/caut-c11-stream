#include "cauterize_types.h"
#include "cauterize_descriptors.h"
#include "cauterize_iterators.h"
#include "cauterize_util.h"

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define TD struct type_descriptor
#define TEI struct type_encode_iterator
#define TDI struct type_decode_iterator
#define SEI struct schema_encode_iterator
#define SDI struct schema_decode_iterator

static S caut_enc_get_byte(SEI * ei, uint8_t * byte);
static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);

static size_t caut_tag_size(enum caut_tag tag);

static S caut_enc_get_byte(SEI * ei, uint8_t * byte) {
    TD const * td = NULL;
    TEI * ti = NULL;

    RE(get_type_enc_iter(ei, &ti));
    RE(get_type_desc(ei->desc, ti->type_id, &td));

    switch (td->prototype_tag) {
    case caut_proto_primitive:
        return caut_enc_get_byte_primitive(ei, td, ti, byte);
    case caut_proto_synonym:
        return caut_enc_get_byte_synonym(ei, td, ti, byte);
    case caut_proto_range:
        return caut_enc_get_byte_range(ei, td, ti, byte);
    default:
        return caut_status_err_UNIMPLEMENTED;
    }

}

static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_primitive * iter = &ti->prototype.c_primitive;
    struct caut_primitive const * const desc = &td->prototype.c_primitive;
    uint8_t const * const type_bytes = ti->type;

    (void) ei;

    assert(type_bytes);
    assert(iter->word_position < desc->word_size);

    *byte = type_bytes[iter->word_position];
    iter->word_position += 1;

    if (iter->word_position >= desc->word_size) {
        return caut_status_ok_pop;
    } else {
        return caut_status_ok_busy;
    }
}

static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    (void)byte;

    struct iter_synonym * const iter = &ti->prototype.c_synonym;
    struct caut_synonym const * const desc = &td->prototype.c_synonym;
    TEI * new_ti = NULL;

    if (iter->done == false) {
        RE(push_type_enc_iter(ei, &new_ti, desc->ref_id, ti->type));
        iter->done = true;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_range * const iter = &ti->prototype.c_range;
    struct caut_range const * const desc = &td->prototype.c_range;
    union {
        uint64_t u;
        int64_t s;
        uint8_t b[sizeof(uint64_t)];
    } word = { .u = 0 };

    (void) ei;

    assert(iter->tag_iter.tag_position < caut_tag_size(desc->tag));
    memcpy(&word, ti->type, desc->word_size);

    if (desc->offset < 0) {
        int64_t const rmin = desc->offset;
        int64_t const rmax = desc->offset + desc->length;

        if (word.s < rmin || rmax < word.s) {
            return caut_status_err_invalid_range;
        } else {
            word.s -= desc->offset;
        }
    } else {
        uint64_t const rmin = desc->offset;
        uint64_t const rmax = desc->offset + desc->length;

        if (word.u < rmin || rmax < word.u) {
            return caut_status_err_invalid_range;
        } else {
            word.u -= (uint64_t)desc->offset;
        }
    }

    *byte = word.b[iter->tag_iter.tag_position];
    iter->tag_iter.tag_position += 1;

    printf("rbyte: %02X\n", *byte);

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        return caut_status_ok_busy;
    } else {
        return caut_status_ok_pop;
    }
}

static size_t caut_tag_size(enum caut_tag tag) {
    switch (tag) {
    case caut_tag_8: return 1;
    case caut_tag_16: return 2;
    case caut_tag_32: return 4;
    case caut_tag_64: return 8;
    default:
        assert(false);
        return 0;
    }
}

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    *enc_bytes = 0;

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i]);

        if (caut_status_ok_busy == s) {
            *i += 1;
            printf("busy (%lu)\n", *i);
        } else if (caut_status_ok_pop == s) {
            *i += 1;
            if (ei->iter_top == 0) {
                printf("done (%lu)\n", *i);
                return caut_status_ok;
            } else {
                printf("pop (%lu:%lu)\n", *i, ei->iter_top);
                ei->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            printf("pushed (%lu)\n", *i);
        } else {
            printf("other (%lu:%d)\n", *i, s);
            return caut_status_err;
        }
    }

    return caut_status_ok_busy;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    (void)di; (void)buf; (void)buf_size; (void)dec_bytes;
    return caut_status_err_invalid_type_id;
}
