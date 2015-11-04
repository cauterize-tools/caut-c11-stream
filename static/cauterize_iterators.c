#include "cauterize_iterators.h"
#include "cauterize_descriptors.h"

#include <string.h>
#include <stdlib.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define EI struct schema_encode_iterator
#define DI struct schema_decode_iterator
#define TI struct type_iterator
#define TD struct type_descriptor

// return error if error
#define RE(EXP) \
    do { \
        S s; if (caut_status_ok != (s = EXP)) {  \
            return s; \
        } \
    } while(0)

int const min_prim_id = -11; // bool

/* Check that the type_id represents a real type. */
static S id_check(SD const * sd_set, int type_id);

/* Get a type descriptor out of the schema descriptor. */
static S get_desc(SD const * sd_set, int type_id, TD const ** sd_out);

void type_iterator_init(TI * ti) {
    memset(ti, 0, sizeof(*ti));
}

S schema_encode_iterator_init(EI * si, SD const * sd, TI * ti, size_t ti_count, int type_id, void const * src_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->type_id = type_id;
    si->src_type = src_type;

    return caut_status_ok;
}

S schema_decode_iterator_init(DI * si, SD const * sd, TI * ti, size_t ti_count, int type_id, void * dst_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->iters = ti;
    si->type_id = type_id;
    si->dst_type = dst_type;

    return caut_status_ok;
}

static S id_check(SD const * sd_set, int type_id) {
    if ((min_prim_id <= type_id) || ((size_t)type_id <= sd_set->type_count)) {
        return caut_status_ok;
    } else {
        return caut_status_err_invalid_type_id;
    }
}

static S get_desc(SD const * sd_set, int type_id, TD const ** sd_out) {
    if (0 > type_id) {
        int prim_idx = abs(type_id) - 1;
        *sd_out = &primitive_descriptors[prim_idx];
    } else if ((size_t)type_id < sd_set->type_count) {
        *sd_out = &sd_set->types[type_id];
    }

    return caut_status_ok;
}
