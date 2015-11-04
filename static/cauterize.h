#ifndef CAUTERIZE_H
#define CAUTERIZE_H

#include "cauterize_types.h"
#include "cauterize_descriptors.h"

#include <stdint.h>

enum caut_status caut_encode(
    struct schema_descriptor const * sd,
    int type_id,
    void const * src_type,
    void * dst_buf,
    size_t dst_size);

enum caut_status caut_decode(
    struct schema_descriptor const * sd,
    int type_id,
    void * dst_type,
    void const * src_buf,
    size_t src_size);

#endif /* CAUTERIZE_H */
