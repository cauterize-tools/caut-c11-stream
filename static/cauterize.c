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
static S caut_enc_get_byte_primitive(TD const * td, TEI * ti, uint8_t * byte);
static S caut_enc_get_byte_synonym(SEI * ei, TD const * td, TEI * ti, uint8_t * byte);

static S caut_enc_get_byte(SEI * ei, uint8_t * byte) {
    TD const * td = NULL;
    TEI * ti = NULL;

    RE(get_type_enc_iter(ei, &ti));
    RE(get_type_desc(ei->desc, ti->type_id, &td));

    switch (td->prototype_tag) {
    case caut_proto_primitive:
        return caut_enc_get_byte_primitive(td, ti, byte);
    case caut_proto_synonym:
        return caut_enc_get_byte_synonym(ei, td, ti, byte);
    default:
        return caut_status_err_UNIMPLEMENTED;
    }

}

static S caut_enc_get_byte_primitive(TD const * td, TEI * ti, uint8_t * byte) {
    struct iter_primitive * iter = &ti->prototype.c_primitive;
    struct caut_primitive const * const desc = &td->prototype.c_primitive;
    uint8_t const * const type_bytes = ti->type;

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

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    *enc_bytes = 0;

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i]);

        if (caut_status_ok_busy == s) {
            printf("busy (%lu)\n", *i);
            *i += 1;
        } else if (caut_status_ok_pop == s) {
            if (ei->iter_top == 0) {
                printf("done (%lu)\n", *i);
                return caut_status_ok;
            } else {
                printf("pop (%lu:%lu)\n", *i, ei->iter_top);
                ei->iter_top -= 1;
                *i += 1;
            }
        } else if (caut_status_ok_pushed == s) {
            printf("pushed (%lu)\n", *i);
            // loop around again
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
