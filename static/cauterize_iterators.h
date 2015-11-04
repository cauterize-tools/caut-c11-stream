#ifndef CAUTERIZE_ITERATORS_H
#define CAUTERIZE_ITERATORS_H

#include "cauterize_types.h"

struct iter_primitive c_primitive;

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

#endif /* CAUTERIZE_ITERATORS_H */
