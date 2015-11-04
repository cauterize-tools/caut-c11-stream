#include "cauterize_iterators.h"

#define S enum caut_status
#define SD struct schema_descriptor
#define EI struct schema_encode_iterator
#define DI struct schema_decode_iterator
#define TI struct type_iterator

S schema_encode_iterator_init(EI * si, SD const * sd, TI * ti, size_t ti_count, void const * src_type, void * dst_buf, size_t buf_size) {
    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->src_type = src_type;
    si->dst_buf = dst_buf;
    si->buf_size = buf_size;
    si->buf_pos = 0;

    return caut_status_ok;
}

S schema_decode_iterator_init(DI * si, SD const * sd, TI * ti, size_t ti_count, void * dst_type, void const * src_buf, size_t buf_size) {
    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->iters = ti;
    si->dst_type = dst_type;
    si->src_buf = src_buf;
    si->buf_size = buf_size;
    si->buf_pos = 0;

    return caut_status_ok;
}
