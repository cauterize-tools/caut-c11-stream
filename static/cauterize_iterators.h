#ifndef CAUTERIZE_ITERATORS_H
#define CAUTERIZE_ITERATORS_H

#include "cauterize_types.h"

#include <stdbool.h>
#include <stddef.h>

struct tag_iter {
    uint64_t tag_buffer;
    size_t tag_position;
};


struct iter_primitive {
    size_t word_position;
};

struct iter_synonym {
    bool done;
};

struct iter_range {
    struct tag_iter tag_iter;
};

struct iter_enumeration {
    struct tag_iter tag_iter;
};

struct iter_array {
    size_t elem_position;
};

struct iter_vector {
    struct tag_iter tag_iter;
    size_t elem_position;
};

struct iter_record {
    size_t field_position;
};

struct iter_combination {
    struct tag_iter tag_iter;
    size_t field_position;
};

struct iter_union {
    struct tag_iter tag_iter;
};

union caut_proto_iter {
    struct iter_primitive c_primitive;
    struct iter_synonym c_synonym;
    struct iter_range c_range;
    struct iter_enumeration c_enumeration;
    struct iter_array c_array;
    struct iter_vector c_vector;
    struct iter_record c_record;
    struct iter_combination c_combination;
    struct iter_union c_union;
};

struct type_iterator {
    enum caut_proto_tag proto;
    union caut_proto_iter prototype;
};

struct schema_encode_iterator {
    struct schema_descriptor const * desc;
    struct type_iterator * iters;
    size_t iter_count;
    size_t iter_top;
    void const * src_type;
    void * dst_buf;
    size_t buf_size;
    size_t buf_pos;
};

struct schema_decode_iterator {
    struct schema_descriptor const * desc;
    struct type_iterator * iters;
    size_t iter_count;
    size_t iter_top;
    void * dst_type;
    void const * src_buf;
    size_t buf_size;
    size_t buf_pos;
};

enum caut_status schema_encode_iterator_init(
    struct schema_encode_iterator * si,
    struct schema_descriptor const * sd,
    struct type_iterator * ti,
    size_t ti_count,
    void const * src_type,
    void * dst_buf,
    size_t buf_size);

enum caut_status schema_decode_iterator_init(
    struct schema_decode_iterator * si,
    struct schema_descriptor const * sd,
    struct type_iterator * ti,
    size_t ti_count,
    void * dst_type,
    void const * src_buf,
    size_t buf_size);


#endif /* CAUTERIZE_ITERATORS_H */
