#include "cauterize_types.h"
#include "cauterize_descriptors.h"

#include <stdbool.h>
#include <string.h>

int const min_prim_id = -11; // bool

#define S enum caut_status
#define D struct schema_descriptor

// return error if error
#define RE(EXP) \
    do { \
        S s; if (caut_status_ok != (s = EXP)) {  \
            return s; \
        } \
    } while(0)

static S id_check(D const * sd, int type_id);

S caut_encode(D const * sd, int type_id, void const * src_type, void * dst_buf, size_t dst_size) {
    (void)src_type; (void)dst_buf; (void)dst_size;
    RE(id_check(sd, type_id));

    return caut_status_err;
}

S caut_decode(D const * sd, int type_id, void * dst_type, void const * src_buf, size_t src_size) {
    (void)dst_type; (void)src_buf; (void)src_size;
    RE(id_check(sd, type_id));

    return caut_status_err;
}

static S id_check(D const * sd, int type_id) {
    if ((min_prim_id <= type_id) || ((size_t)type_id <= sd->type_count)) {
        return caut_status_ok;
    } else {
        return caut_status_err_invalid_type_id;
    }
}
