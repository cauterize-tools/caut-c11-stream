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

#define STATE_CHECK(cond) do { if (!(cond)) { return caut_status_err_bad_state; } } while (0)

static S caut_enc_get_byte(SEI * ei, uint8_t * byte, bool * progress);
static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_enumeration(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_array(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_vector(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_record(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_combination(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);
static S caut_enc_get_byte_union(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte);

static S caut_dec_put_byte(SDI * di, uint8_t const * byte, bool * progress);
static S caut_dec_put_byte_primitive(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_synonym(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_range(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_enumeration(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_array(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_vector(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_record(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_combination(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);
static S caut_dec_put_byte_union(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte);

static size_t caut_tag_size(enum caut_tag tag);
static void signed_convert(void const * in, size_t in_size, void * out, size_t out_size);
static uint64_t mask_with_width(size_t width);
static uint64_t flag_set_at(size_t width);

static S caut_enc_get_byte(SEI * ei, uint8_t * byte, bool * progress) {
    TD const * td = NULL;
    TEI * ti = NULL;

    *progress = false;

    RE(get_type_enc_iter(ei, &ti));
    RE(get_type_desc(ei->desc, ti->type_id, &td));

    switch (td->prototype_tag) {
    case caut_proto_primitive:
        return caut_enc_get_byte_primitive(ei, td, ti, progress, byte);
    case caut_proto_synonym:
        return caut_enc_get_byte_synonym(ei, td, ti, progress, byte);
    case caut_proto_range:
        return caut_enc_get_byte_range(ei, td, ti, progress, byte);
    case caut_proto_enumeration:
        return caut_enc_get_byte_enumeration(ei, td, ti, progress, byte);
    case caut_proto_array:
        return caut_enc_get_byte_array(ei, td, ti, progress, byte);
    case caut_proto_vector:
        return caut_enc_get_byte_vector(ei, td, ti, progress, byte);
    case caut_proto_record:
        return caut_enc_get_byte_record(ei, td, ti, progress, byte);
    case caut_proto_combination:
        return caut_enc_get_byte_combination(ei, td, ti, progress, byte);
    case caut_proto_union:
        return caut_enc_get_byte_union(ei, td, ti, progress, byte);
    default:
        return caut_status_err_UNIMPLEMENTED;
    }
}

static S caut_enc_get_byte_primitive(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_primitive * iter = &ti->prototype.c_primitive;
    struct caut_primitive const * const desc = &td->prototype.c_primitive;
    uint8_t const * const type_bytes = ti->type;

    (void) ei;

    if (iter->word_position < desc->word_size) {
        if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = type_bytes[iter->word_position];
            iter->word_position += 1;

            return caut_status_ok_busy;
        }
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    (void)byte;

    struct iter_synonym * const iter = &ti->prototype.c_synonym;
    struct caut_synonym const * const desc = &td->prototype.c_synonym;

    *progress = false;

    if (iter->done == false) {
        RE(push_type_enc_iter(ei, desc->ref_id, ti->type));
        iter->done = true;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_range(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_range * const iter = &ti->prototype.c_range;
    struct caut_range const * const desc = &td->prototype.c_range;

    (void) ei;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        uint8_t * b = NULL;

        if (desc->offset < 0) {
            int64_t const rmin = desc->offset;
            int64_t const rmax = desc->offset + desc->length;

            int64_t s = 0;
            signed_convert(ti->type, desc->word_size, &s, sizeof(s));

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

        if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = b[iter->tag_iter.tag_position];
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_enumeration(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_enumeration * const iter = &ti->prototype.c_enumeration;
    struct caut_enumeration const * const desc = &td->prototype.c_enumeration;

    (void) ei;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        uint64_t word = 0;
        memcpy(&word, ti->type, caut_tag_size(desc->tag));

        if (word >= desc->length) {
            return caut_status_err_invalid_enum;
        } else if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_array(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_array * const iter = &ti->prototype.c_array;
    struct caut_array const * const desc = &td->prototype.c_array;

    *progress = false;
    (void) byte;

    if (iter->elem_position < desc->length) {
        void const * base =
            (void *)(
                ((uintptr_t)ti->type) +
                (desc->elem_span * iter->elem_position));

        RE(push_type_enc_iter(ei, desc->ref_id, base));
        iter->elem_position += 1;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_vector(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_vector * const iter = &ti->prototype.c_vector;
    struct caut_vector const * const desc = &td->prototype.c_vector;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        if (word > desc->max_length) {
            return caut_status_err_invalid_vector;
        } else if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else if (iter->elem_position < word) {
        // accumulating elements
        void const * base =
            (void *)(
                ((uintptr_t)ti->type) +
                desc->elem_offset +
                (desc->elem_span * iter->elem_position));

        RE(push_type_enc_iter(ei, desc->ref_id, base));
        iter->elem_position += 1;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_record(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_record * const iter = &ti->prototype.c_record;
    struct caut_record const * const desc = &td->prototype.c_record;

    (void) byte;
    *progress = false;

    if (iter->field_position < desc->field_count) {
        struct caut_field const * const field = &desc->fields[iter->field_position];
        void const * base = (void *)(((uintptr_t)ti->type) + field->offset);

        if (field->data == false) {
            return caut_status_err_invalid_record;
        } else {
            RE(push_type_enc_iter(ei, field->ref_id, base));
            iter->field_position += 1;
        }

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_combination(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_combination * const iter = &ti->prototype.c_combination;
    struct caut_combination const * const desc = &td->prototype.c_combination;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        uint64_t const mask = mask_with_width(desc->field_count);
        if (word > mask) {
            return caut_status_err_invalid_combination;
        } else if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else {
        while (iter->field_position < desc->field_count) {
            uint64_t const field_flag = flag_set_at(iter->field_position);

            if (0 == (field_flag & word)) {
                iter->field_position += 1;
                continue;
            } else {
                struct caut_field const * const field = &desc->fields[iter->field_position];
                void const * base = (void *)(((uintptr_t)ti->type) + field->offset);

                iter->field_position += 1;

                if (field->data) {
                    RE(push_type_enc_iter(ei, field->ref_id, base));
                    return caut_status_ok_pushed;
                } else {
                    continue;
                }
            }
        }

        return caut_status_ok_pop;
    }
}

static S caut_enc_get_byte_union(SEI * ei, TD const * td, TEI * ti, bool * progress, uint8_t * byte) {
    struct iter_union * const iter = &ti->prototype.c_union;
    struct caut_union const * const desc = &td->prototype.c_union;

    uint64_t word = 0;
    memcpy(&word, ti->type, caut_tag_size(desc->tag));

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // still accumulating tag
        if (word > desc->field_count) {
            return caut_status_err_invalid_union;
        } else if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            *progress = true;
            *byte = ((uint8_t *)&word)[iter->tag_iter.tag_position];
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else {
        struct caut_field const * const field = &desc->fields[word];
        void const * base = (void *)(((uintptr_t)ti->type) + field->offset);

        if (iter->field_done || !field->data) {
            return caut_status_ok_pop;
        } else {
            RE(push_type_enc_iter(ei, field->ref_id, base));
            iter->field_done = true;

            return caut_status_ok_pushed;
        }
    }
}

static S caut_dec_put_byte(SDI * di, uint8_t const * byte, bool * progress) {
    /* The parameter `byte` can be NULL. If it's not null, copy it
     * into the decoding structure. If it is null, pushing and popping
     * of the stack are allowed, but nothing else. If a byte is needed
     * to make progress, `caut_status_err_need_byte` must be returned
     * until a byte is provided. */

    TD const * td = NULL;
    TDI * ti = NULL;
    *progress = false;

    RE(get_type_dec_iter(di, &ti));
    RE(get_type_desc(di->desc, ti->type_id, &td));

    switch (td->prototype_tag) {
    case caut_proto_primitive:
        return caut_dec_put_byte_primitive(di, td, ti, progress, byte);
    case caut_proto_synonym:
        return caut_dec_put_byte_synonym(di, td, ti, progress, byte);
    case caut_proto_range:
        return caut_dec_put_byte_range(di, td, ti, progress, byte);
    case caut_proto_enumeration:
        return caut_dec_put_byte_enumeration(di, td, ti, progress, byte);
    case caut_proto_array:
        return caut_dec_put_byte_array(di, td, ti, progress, byte);
    case caut_proto_vector:
        return caut_dec_put_byte_vector(di, td, ti, progress, byte);
    case caut_proto_record:
        return caut_dec_put_byte_record(di, td, ti, progress, byte);
    case caut_proto_combination:
        return caut_dec_put_byte_combination(di, td, ti, progress, byte);
    case caut_proto_union:
        return caut_dec_put_byte_union(di, td, ti, progress, byte);
    default:
        return caut_status_err_UNIMPLEMENTED;
    }
}

static S caut_dec_put_byte_primitive(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_primitive * iter = &ti->prototype.c_primitive;
    struct caut_primitive const * const desc = &td->prototype.c_primitive;
    uint8_t * const type_bytes = ti->type;

    (void) di;

    STATE_CHECK(iter->word_position < desc->word_size);

    if (NULL == byte) {
        return caut_status_err_need_byte;
    }

    type_bytes[iter->word_position] = *byte;
    iter->word_position += 1;

    *progress = true;

    if (iter->word_position < desc->word_size) {
        return caut_status_ok_busy;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_synonym(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_synonym * const iter = &ti->prototype.c_synonym;
    struct caut_synonym const * const desc = &td->prototype.c_synonym;

    (void)byte;

    *progress = false;

    if (iter->done == false) {
        RE(push_type_dec_iter(di, desc->ref_id, ti->type));
        iter->done = true;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_range(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_range * const iter = &ti->prototype.c_range;
    struct caut_range const * const desc = &td->prototype.c_range;

    (void) di;

    assert(iter->tag_iter.tag_position < caut_tag_size(desc->tag));

    uint8_t * b = (uint8_t *)&iter->tag_iter.tag_buffer;

    if (NULL == byte) {
        return caut_status_err_need_byte;
    } else {
        *progress = true;
        b[iter->tag_iter.tag_position] = *byte;
        iter->tag_iter.tag_position += 1;
    }

    if (iter->tag_iter.tag_position >= caut_tag_size(desc->tag)) {
        if (desc->offset < 0) {
            int64_t const rmin = desc->offset;
            int64_t const rmax = desc->offset + desc->length;
            int64_t s = 0;

            signed_convert(
                &iter->tag_iter.tag_buffer, caut_tag_size(desc->tag),
                &s, sizeof(s));

            s += desc->offset;

            if (s < rmin || rmax < s) {
                return caut_status_err_invalid_range;
            } else {
                signed_convert(
                    &s, sizeof(s),
                    ti->type, desc->word_size);

                return caut_status_ok_pop;
            }
        } else {
            uint64_t const rmin = desc->offset;
            uint64_t const rmax = desc->offset + desc->length;

            uint64_t u = 0;
            memcpy(&u, &iter->tag_iter.tag_buffer, caut_tag_size(desc->tag));

            u += desc->offset;

            if (u < rmin || rmax < u) {
                return caut_status_err_invalid_range;
            } else {
                memcpy(ti->type, &u, desc->word_size);

                return caut_status_ok_pop;
            }
        }
    } else {
        return caut_status_ok_busy;
    }
}

static S caut_dec_put_byte_enumeration(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_enumeration * const iter = &ti->prototype.c_enumeration;
    struct caut_enumeration const * const desc = &td->prototype.c_enumeration;
    uint8_t * const b = (uint8_t *)&iter->tag_iter.tag_buffer;

    (void) di;

    assert(iter->tag_iter.tag_position < caut_tag_size(desc->tag));

    *progress = true;
    b[iter->tag_iter.tag_position] = *byte;
    iter->tag_iter.tag_position += 1;

    if (iter->tag_iter.tag_position >= caut_tag_size(desc->tag)) {
        if (iter->tag_iter.tag_buffer >= desc->length) {
            return caut_status_err_invalid_enum;
        } else {
            memcpy(ti->type, &iter->tag_iter.tag_buffer, caut_tag_size(desc->tag));
            return caut_status_ok_pop;
        }
    } else {
        return caut_status_ok_busy;
    }
}

static S caut_dec_put_byte_array(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_array * const iter = &ti->prototype.c_array;
    struct caut_array const * const desc = &td->prototype.c_array;

    (void) byte;

    *progress = false;

    if (iter->elem_position < desc->length) {
        void * const base =
            (void *)(
                ((uintptr_t)ti->type) +
                (desc->elem_span * iter->elem_position));
        RE(push_type_dec_iter(di, desc->ref_id, base));
        iter->elem_position += 1;
        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_vector(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_vector * const iter = &ti->prototype.c_vector;
    struct caut_vector const * const desc = &td->prototype.c_vector;
    uint8_t * const b = (uint8_t *)&iter->tag_iter.tag_buffer;
    uint8_t * const t = (uint8_t *)ti->type;

    *progress = false;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        // Copy the byte into both a tag buffer and the final tag
        // position. It goes in both places so that we can length
        // check using the buffer as a u64 and have the value in place
        // in the final tag memory.

        if (NULL == byte) {
            return caut_status_err_need_byte;
        } else {
            b[iter->tag_iter.tag_position] = *byte;
            t[iter->tag_iter.tag_position] = *byte;

            *progress = true;
            iter->tag_iter.tag_position += 1;

            return caut_status_ok_busy;
        }
    } else if (iter->tag_iter.tag_buffer > desc->max_length) {
        return caut_status_err_invalid_vector;
    } else if (iter->elem_position < iter->tag_iter.tag_buffer) {
        void * const base =
            (void *)(
                ((uintptr_t)ti->type) +
                desc->elem_offset +
                (desc->elem_span * iter->elem_position));

        RE(push_type_dec_iter(di, desc->ref_id, base));
        iter->elem_position += 1;

        return caut_status_ok_pushed;
    } else {
        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_record(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_record * const iter = &ti->prototype.c_record;
    struct caut_record const * const desc = &td->prototype.c_record;

    (void) byte;

    *progress = false;

    if (iter->field_position < desc->field_count) {
        struct caut_field const * const field = &desc->fields[iter->field_position];
        void * const base = (void *)(((uintptr_t)ti->type) + field->offset);

        if (field->data == false) {
            return caut_status_err_invalid_record;
        } else {
            RE(push_type_dec_iter(di, field->ref_id, base));
            iter->field_position += 1;
        }

        return caut_status_ok_pushed;

    } else {
        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_combination(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_combination * const iter = &ti->prototype.c_combination;
    struct caut_combination const * const desc = &td->prototype.c_combination;
    uint8_t * const b = (uint8_t *)&iter->tag_iter.tag_buffer;
    uint8_t * const t = (uint8_t *)ti->type;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        *progress = true;

        // still accumulating tag
        b[iter->tag_iter.tag_position] = *byte;
        t[iter->tag_iter.tag_position] = *byte;
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else {
        uint64_t const mask = mask_with_width(desc->field_count);
        if (iter->tag_iter.tag_buffer > mask) {
            return caut_status_err_invalid_combination;
        }

        while (iter->field_position < desc->field_count) {
            uint64_t const field_flag = flag_set_at(iter->field_position);

            if (0 == (field_flag & iter->tag_iter.tag_buffer)) {
                iter->field_position += 1;
                continue;
            } else {
                struct caut_field const * const field = &desc->fields[iter->field_position];
                void * const base = (void *)(((uintptr_t)ti->type) + field->offset);

                iter->field_position += 1;

                if (field->data) {
                    RE(push_type_dec_iter(di, field->ref_id, base));
                    return caut_status_ok_pushed;
                } else {
                    continue;
                }
            }
        }

        return caut_status_ok_pop;
    }
}

static S caut_dec_put_byte_union(SDI * di, TD const * td, TDI * ti, bool * progress, uint8_t const * byte) {
    struct iter_union * const iter = &ti->prototype.c_union;
    struct caut_union const * const desc = &td->prototype.c_union;
    uint8_t * const b = (uint8_t *)&iter->tag_iter.tag_buffer;
    uint8_t * const t = (uint8_t *)ti->type;

    if (iter->tag_iter.tag_position < caut_tag_size(desc->tag)) {
        *progress = true;

        // still accumulating tag
        b[iter->tag_iter.tag_position] = *byte;
        t[iter->tag_iter.tag_position] = *byte;
        iter->tag_iter.tag_position += 1;

        return caut_status_ok_busy;
    } else {
        struct caut_field const * const field = &desc->fields[iter->tag_iter.tag_buffer];
        void * const base = (void *)(((uintptr_t)ti->type) + field->offset);

        if (iter->tag_iter.tag_buffer > desc->field_count) {
            return caut_status_err_invalid_union;
        }


        if (iter->field_done || !field->data) {
            return caut_status_ok_pop;
        } else {
            RE(push_type_dec_iter(di, field->ref_id, base));
            iter->field_done = true;

            return caut_status_ok_pushed;
        }
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

static void signed_convert(void const * in, size_t in_size, void * out, size_t out_size) {
    // Assumes in and out are both properly aligned.

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
    case sizeof(int8_t):
        assert(INT8_MIN <= word && word <= INT8_MAX);
        *((int8_t  *)out) = (int8_t )word;
        break;
    case sizeof(int16_t):
        assert(INT16_MIN <= word && word <= INT16_MAX);
        *((int16_t *)out) = (int16_t)word;
        break;
    case sizeof(int32_t):
        assert(INT32_MIN <= word && word <= INT32_MAX);
        *((int32_t *)out) = (int32_t)word;
        break;
    case sizeof(int64_t):
        assert(INT64_MIN <= word && word <= INT64_MAX);
        *((int64_t *)out) = (int64_t)word;
        break;
    default:
        assert(false);
    }
}

static uint64_t mask_with_width(size_t width) {
    assert(0 < width && width < 65);

    uint64_t ret = 0;

    for (size_t i = 0; i < width; i++) {
        ret |= 1 << i;
    }

    return ret;
}

static uint64_t flag_set_at(size_t width) {
    assert(width < 64);

    uint64_t ret = 1;
    ret <<= width;

    return ret;
}

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    S ret = caut_status_ok_busy;

    *enc_bytes = 0;

    DEBUG_CHAR('(');

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        bool progress = false;
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i], &progress);

        if (progress) {
            *i += 1;
        }

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR('.');
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR(')');
            if (ei->iter_top == 0) {
                ret = caut_status_ok;
                ei->iter_top = SIZE_MAX;
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

    if (ret != caut_status_ok && ret < ERRS_START && *i >= buf_size) {
        // We ran out of bytes, did not error, but did not finish. Try
        // and unwind the stack in case we have a bunch of pops in a
        // row.

        while (true) {
            bool progress = false;
            S const s = caut_enc_get_byte(ei, NULL, &progress);

            assert(false == progress);
            assert(caut_status_ok_busy != s);

            if (caut_status_ok_pop == s) {
                DEBUG_CHAR(')');
                if (ei->iter_top == 0) {
                    ret = caut_status_ok;
                    ei->iter_top = SIZE_MAX;
                    break;
                } else {
                    ei->iter_top -= 1;
                }
            } else if (caut_status_ok_pushed == s) {
                DEBUG_CHAR('(');
            } else if (caut_status_err_need_byte == s) {
                ret = caut_status_ok_busy;
                break;
            } else {
                // unhandled value
                DEBUG_FMT("Unexpected return: %d\n", s);
                assert(false);
            }
        }
    }

    return ret;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    S ret = caut_status_ok_busy;

    *dec_bytes = 0;

    DEBUG_CHAR('(');

    size_t * const i = dec_bytes;

    while (*i < buf_size) {
        bool progress = false;
        uint8_t const * const bytes = buf;

        S const s = caut_dec_put_byte(di, &bytes[*i], &progress);

        if (progress) {
            *i += 1;
        }

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR_IF(progress, '.');
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR_IF(progress, '.');
            DEBUG_CHAR(')');
            if (di->iter_top == 0) {
                ret = caut_status_ok;
                di->iter_top = SIZE_MAX;
                break;
            } else {
                di->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('(');
            DEBUG_CHAR_IF(progress, '.');
        } else {
            assert(s >= ERRS_START);
            DEBUG_CHAR('!');
            ret = s;
            break;
        }
    }

    if (ret != caut_status_ok && ret < ERRS_START && *i >= buf_size) {
        // We ran out of bytes, did not error, but did not finish. Try
        // and unwind the stack in case we have a bunch of pops in a
        // row.

        while (true) {
            bool progress = false;
            S const s = caut_dec_put_byte(di, NULL, &progress);

            assert(false == progress);
            assert(caut_status_ok_busy != s);

            if (caut_status_ok_pop == s) {
                DEBUG_CHAR(')');
                if (di->iter_top == 0) {
                    ret = caut_status_ok;
                    di->iter_top = SIZE_MAX;
                    break;
                } else {
                    di->iter_top -= 1;
                }
            } else if (caut_status_ok_pushed == s) {
                DEBUG_CHAR('(');
            } else if (caut_status_err_need_byte == s) {
                ret = caut_status_ok_busy;
                break;
            } else {
                // unhandled value
                DEBUG_FMT("Unexpected return: %d\n", s);
                assert(false);
            }
        }
    }

    DEBUG_FMT("\nret = %d\n", ret);

    return ret;
}

S schema_encode_iterator_init(SEI * si, SD const * sd, TEI * ti, size_t ti_count, int type_id, void const * src_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->src_type = src_type;

    RE(type_encode_iterator_init(sd, ti, type_id, src_type));

    return caut_status_ok;
}

S schema_decode_iterator_init(SDI * si, SD const * sd, TDI * ti, size_t ti_count, int type_id, void * dst_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->dst_type = dst_type;

    RE(type_decode_iterator_init(sd, ti, type_id, dst_type));

    return caut_status_ok;
}
