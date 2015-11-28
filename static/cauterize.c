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
#define DEBUG_FMT(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define DEBUG_CHAR(c)
#define DEBUG_FMT(fmt, ...)
#endif

static S caut_enc_get_byte(SEI * ei, uint8_t * byte);
static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_enumeration(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_array(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_vector(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_record(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_combination(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_union(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);

static S caut_dec_put_byte(SDI * di, uint8_t const * byte);
static S caut_dec_put_byte_primitive(SDI * di, TD const * td, TDI * ti, uint8_t const * byte);

static size_t caut_tag_size(enum caut_tag tag);
static void signed_promote(void const * in, size_t in_size, void * out, size_t out_size);

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
    case caut_proto_vector:
        return caut_enc_get_byte_vector(ei, td, ti, byte);
    case caut_proto_record:
        return caut_enc_get_byte_record(ei, td, ti, byte);
    case caut_proto_combination:
        return caut_enc_get_byte_combination(ei, td, ti, byte);
    case caut_proto_union:
        return caut_enc_get_byte_union(ei, td, ti, byte);
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
        uint8_t * b = NULL;

        if (desc->offset < 0) {
            int64_t const rmin = desc->offset;
            int64_t const rmax = desc->offset + desc->length;

            int64_t s = 0;
            signed_promote(ti->type, desc->word_size, &s, sizeof(s));

            if (s < rmin || rmax < s) {
                return caut_status_err_invalid_range;
            } else {
                s -= desc->offset;
            }

            b = (uint8_t *) &s;
        } else {
            uint64_t const rmin = desc->offset;
            uint64_t const rmax = desc->offset + desc->length;

            uint64_t u = 0;
            memcpy(&u, ti->type, desc->word_size);

            if (u < rmin || rmax < u) {
                return caut_status_err_invalid_range;
            } else {
                u -= (uint64_t)desc->offset;
            }

            b = (uint8_t *) &u;
        }

        assert(b);

        *byte = b[iter->tag_iter.tag_position];
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
        void const * base = ti->type + (desc->elem_span * iter->elem_position);

        RE(push_type_enc_iter(ei, &new_ti, desc->ref_id, base));
        iter->elem_position += 1;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_vector(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_vector * const iter = &ti->prototype.c_vector;
    struct caut_vector const * const desc = &td->prototype.c_vector;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        if (word > desc->max_length) {
            return caut_status_err_invalid_vector;
        }

        *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else if (iter->elem_position < word) {
        // accumulating elements
        TEI * new_ti = NULL;
        void const * base =
            ti->type +
            desc->elem_offset +
            (iter->elem_position * desc->elem_span);

        RE(push_type_enc_iter(ei, &new_ti, desc->ref_id, base));
        iter->elem_position += 1;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_record(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_record * const iter = &ti->prototype.c_record;
    struct caut_record const * const desc = &td->prototype.c_record;

    (void) byte;

    if (iter->field_position < desc->field_count) {
        struct caut_field const * const field = &desc->fields[iter->field_position];
        TEI * new_ti = NULL;
        void const * base = ti->type + field->offset;

        if (field->data == false) {
            return caut_status_err_invalid_record;
        } else {
            RE(push_type_enc_iter(ei, &new_ti, field->ref_id, base));
            iter->field_position += 1;
        }

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_combination(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_combination * const iter = &ti->prototype.c_combination;
    struct caut_combination const * const desc = &td->prototype.c_combination;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        if (word > ((1 << desc->field_count) - 1)) {
            return caut_status_err_invalid_combination;
        }

        *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else {
        while (iter->field_position < desc->field_count) {
            uint64_t const field_flag = 1 << iter->field_position;

            if (0 == (field_flag & word)) {
                iter->field_position += 1;
                continue;
            } else {
                struct caut_field const * const field = &desc->fields[iter->field_position];
                void const * base = ti->type + field->offset;
                TEI * new_ti = NULL;

                iter->field_position += 1;

                if (field->data) {
                    RE(push_type_enc_iter(ei, &new_ti, field->ref_id, base));
                    return caut_status_ok_pushed;
                } else {
                    continue;
                }
            }
        }

        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_union(SEI * ei, TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_union * const iter = &ti->prototype.c_union;
    struct caut_union const * const desc = &td->prototype.c_union;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        if (word > desc->field_count) {
            return caut_status_err_invalid_union;
        }

        *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else {
        struct caut_field const * const field = &desc->fields[word];
        void const * base = ti->type + field->offset;
        TEI * new_ti = NULL;

        if (iter->field_done || !field->data) {
            return caut_status_ok_pop;
        } else {
            RE(push_type_enc_iter(ei, &new_ti, field->ref_id, base));
            iter->field_done = true;

            return caut_status_ok_pushed;
        }
    }
}

static S caut_dec_put_byte(SDI * di, uint8_t const * byte) {
    TD const * td = NULL;
    TDI * ti = NULL;

    RE(get_type_dec_iter(di, &ti));
    RE(get_type_desc(di->desc, ti->type_id, &td));

    switch (td->prototype_tag) {
    case caut_proto_primitive:
        return caut_dec_put_byte_primitive(di, td, ti, byte);
    default:
        return caut_status_err_UNIMPLEMENTED;
#if 0
    case caut_proto_synonym:
        return caut_enc_get_byte_synonym(ei, td, ti, byte);
    case caut_proto_range:
        return caut_enc_get_byte_range(ei, td, ti, byte);
    case caut_proto_enumeration:
        return caut_enc_get_byte_enumeration(ei, td, ti, byte);
    case caut_proto_array:
        return caut_enc_get_byte_array(ei, td, ti, byte);
    case caut_proto_vector:
        return caut_enc_get_byte_vector(ei, td, ti, byte);
    case caut_proto_record:
        return caut_enc_get_byte_record(ei, td, ti, byte);
    case caut_proto_combination:
        return caut_enc_get_byte_combination(ei, td, ti, byte);
    case caut_proto_union:
        return caut_enc_get_byte_union(ei, td, ti, byte);
#endif
    }
}

static S caut_dec_put_byte_primitive(SDI * di, TD const * td, TDI * ti, uint8_t const * byte) {
    struct iter_primitive * iter = &ti->prototype.c_primitive;
    struct caut_primitive const * const desc = &td->prototype.c_primitive;
    uint8_t * const type_bytes = ti->type;

    (void) di;

    assert(type_bytes);

    if (iter->word_position < desc->word_size) {
        type_bytes[iter->word_position] = *byte;
        iter->word_position += 1;

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

static void signed_promote(void const * in, size_t in_size, void * out, size_t out_size) {
    // Assumes in and out are both properly aligned.

    assert(in_size <= out_size);
    assert(in_size == 1 || in_size == 2 || in_size == 4 || in_size == 8);
    assert(out_size == 1 || out_size == 2 || out_size == 4 || out_size == 8);

    int64_t word = 0;

    switch (in_size) {
    case sizeof(int8_t):  word = *(int8_t  *)in; break;
    case sizeof(int16_t): word = *(int16_t *)in; break;
    case sizeof(int32_t): word = *(int32_t *)in; break;
    case sizeof(int64_t): word = *(int64_t *)in; break;
    default: assert(false);
    }

    switch(out_size) {
    case sizeof(int8_t):  *((int8_t  *)out) = (int8_t )word; break;
    case sizeof(int16_t): *((int16_t *)out) = (int16_t)word; break;
    case sizeof(int32_t): *((int32_t *)out) = (int32_t)word; break;
    case sizeof(int64_t): *((int64_t *)out) = (int64_t)word; break;
    default: assert(false);
    }
}

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    S ret = caut_status_ok_busy;

    *enc_bytes = 0;

    DEBUG_CHAR('(');

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i]);

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR('.');
            *i += 1;
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR(')');
            if (ei->iter_top == 0) {
                ret = caut_status_ok;
                break;
            } else {
                ei->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('(');
        } else {
            assert(s >= ERRS_START);
            DEBUG_CHAR('!');
            ret = s;
            break;
        }
    }

    DEBUG_FMT("\nret = %d\n", ret);

    return ret;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    S ret = caut_status_ok_busy;

    *dec_bytes = 0;

    DEBUG_CHAR('(');

    size_t * i = dec_bytes;

    while (*i < buf_size) {
        uint8_t const * const bytes = buf;
        S const s = caut_dec_put_byte(di, &bytes[*i]);

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR('.');
            *i += 1;
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR(')');
            if (di->iter_top == 0) {
                ret = caut_status_ok;
                break;
            } else {
                di->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('(');
        } else {
            assert(s >= ERRS_START);
            DEBUG_CHAR('!');
            ret = s;
            break;
        }
    }

    DEBUG_FMT("\nret = %d\n", ret);

    return ret;
}
