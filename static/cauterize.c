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

#ifndef NDEBUG
#define DEBUG_CHAR(c) putchar(c)
#endif

static S caut_enc_get_byte(SEI * ei, uint8_t * byte);
static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_enumeration(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_array(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);

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
    case caut_proto_enumeration:
        return caut_enc_get_byte_enumeration(ei, td, ti, byte);
    case caut_proto_array:
        return caut_enc_get_byte_array(ei, td, ti, byte);
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

    if (iter->word_position < desc->word_size) {
        *byte = type_bytes[iter->word_position];
        iter->word_position += 1;

        return caut_status_ok_busy;
    } else {
        return caut_status_ok_pop;
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

    (void) ei;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        union {
            uint64_t u;
            int64_t s;
            uint8_t b[sizeof(uint64_t)];
        } word = { .u = 0 };

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

        return caut_status_ok_busy;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_enumeration(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_enumeration * const iter = &ti->prototype.c_enumeration;
    struct caut_enumeration const * const desc = &td->prototype.c_enumeration;

    (void) ei;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        uint64_t word = 0;
        memcpy(&word, ti->type, caut_tag_size(desc->tag));

        if (word >= desc->length) {
            return caut_status_err_invalid_enum;
        }

        *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_array(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_array * const iter = &ti->prototype.c_array;
    struct caut_array const * const desc = &td->prototype.c_array;
    TEI * new_ti = NULL;

    (void) byte;

    if (iter->elem_position < desc->length) {
        RE(push_type_enc_iter(ei, &new_ti, desc->ref_id, ti->type));

        uintptr_t const type_ptr = (uintptr_t)ti->type + desc->elem_span;
        ti->type = (void *)type_ptr;
        iter->elem_position += 1;

        return caut_status_ok_pushed;
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
    S ret = caut_status_ok_busy;

    *enc_bytes = 0;

    DEBUG_CHAR('+');

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i]);

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR('.');
            *i += 1;
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR('-');
            if (ei->iter_top == 0) {
                ret = caut_status_ok;
                break;
            } else {
                ei->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('+');
            // at the moment, do nothing. this might need to add a byte.
        } else {
            DEBUG_CHAR('!');
            ret = caut_status_err;
            break;
        }
    }

    DEBUG_CHAR('\n');

    return ret;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    (void)di; (void)buf; (void)buf_size; (void)dec_bytes;
    return caut_status_err_invalid_type_id;
}
