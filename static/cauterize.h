#ifndef CAUTERIZE_H
#define CAUTERIZE_H

#include "cauterize_types.h"
#include "cauterize_descriptors.h"
#include "cauterize_iterators.h"

#include <stdint.h>

enum caut_status caut_enc_get(
    struct schema_encode_iterator * ei,
    void * buf,
    size_t buf_size,
    size_t * enc_bytes);

enum caut_status caut_dec_put(
    struct schema_decode_iterator * di,
    void const * buf,
    size_t buf_size,
    size_t * dec_bytes);

#endif /* CAUTERIZE_H */
