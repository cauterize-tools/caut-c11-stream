#ifndef CAUTERIZE_H
#define CAUTERIZE_H

#include <stdint.h>

enum caut_proto_tag {
    caut_proto_primitive,
    caut_proto_synonym,
    caut_proto_range,
    caut_proto_enumeration,
    caut_proto_array,
    caut_proto_vector,
    caut_proto_record,
    caut_proto_combination,
    caut_proto_union,
};

enum caut_tag {
    caut_tag_8,
    caut_tag_16,
    caut_tag_32,
    caut_tag_64,
};

typedef uint8_t caut_tag8_t;
typedef uint16_t caut_tag16_t;
typedef uint32_t caut_tag32_t;
typedef uint64_t caut_tag64_t;

#endif /* CAUTERIZE_H */
