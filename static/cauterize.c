#include "cauterize_types.h"
#include "cauterize_descriptors.h"
#include "cauterize_iterators.h"
#include "cauterize_util.h"

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define TD struct type_descriptor
#define TI struct type_iterator
#define SEI struct schema_encode_iterator
#define SDI struct schema_decode_iterator

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    (void)buf; (void)buf_size; (void)enc_bytes;
    TD const * td = NULL;
    TI * ti = NULL;

    RE(get_type_desc(ei->desc, ei->type_id, &td));
    RE(get_type_iter(ei, &ti));

    switch(td->prototype_tag) {
    case caut_proto_primitive:
        break;
    default:
        return caut_status_err_UNIMPLEMENTED;
    }

    return caut_status_ok;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    (void)di; (void)buf; (void)buf_size; (void)dec_bytes;
    return caut_status_err_invalid_type_id;
}
