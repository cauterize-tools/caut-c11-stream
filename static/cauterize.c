#include "cauterize_types.h"
#include "cauterize_descriptors.h"
#include "cauterize_iterators.h"

#include <stdbool.h>
#include <string.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define TD struct type_descriptor
#define SEI struct schema_encode_iterator
#define SDI struct schema_decode_iterator

/* Allocate another type descriptor on the stack and dispatch into it. */
// ...

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    (void)ei; (void)buf; (void)buf_size; (void)enc_bytes;
    return caut_status_err_invalid_type_id;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    (void)di; (void)buf; (void)buf_size; (void)dec_bytes;
    return caut_status_err_invalid_type_id;
}
