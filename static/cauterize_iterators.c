#include "cauterize_iterators.h"
#include "cauterize_descriptors.h"
#include "cauterize_util.h"

#include <string.h>
#include <stdlib.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define SEI struct schema_encode_iterator
#define SDI struct schema_decode_iterator
#define TEI struct type_encode_iterator
#define TDI struct type_decode_iterator
#define TD struct type_descriptor

S type_encode_iterator_init(SD const * sd, TEI * ti, int type_id, void const * type) {
    struct type_descriptor const * td = NULL;

    RE(id_check(sd, type_id));
    RE(get_type_desc(sd, type_id, &td));

    memset(ti, 0, sizeof(*ti));

    ti->proto = td->prototype_tag;
    ti->type = type;
    ti->type_id = type_id;

    return caut_status_ok;
}

S type_decode_iterator_init(SD const * sd, TDI * ti, int type_id, void * type) {
    struct type_descriptor const * td = NULL;

    RE(id_check(sd, type_id));
    RE(get_type_desc(sd, type_id, &td));

    memset(ti, 0, sizeof(*ti));

    ti->proto = td->prototype_tag;
    ti->type = type;
    ti->type_id = type_id;

    return caut_status_ok;
}

S schema_encode_iterator_init(SEI * si, SD const * sd, TEI * ti, size_t ti_count, int type_id, void const * src_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->src_type = src_type;

    RE(type_encode_iterator_init(sd, ti, type_id, src_type));

    return caut_status_ok;
}

S schema_decode_iterator_init(SDI * si, SD const * sd, TDI * ti, size_t ti_count, int type_id, void * dst_type) {
    RE(id_check(sd, type_id));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->dst_type = dst_type;

    RE(type_decode_iterator_init(sd, ti, type_id, dst_type));

    return caut_status_ok;
}

S get_type_enc_iter(SEI const * ei, TEI ** ti_out) {
    if (ei->iter_top < ei->iter_count) {
        *ti_out = &ei->iters[ei->iter_top];
        return caut_status_ok;
    } else {
        return caut_status_err_iter_stack_would_overflow;
    }
}

S get_type_dec_iter(SDI const * di, TDI ** ti_out) {
    if (di->iter_top < di->iter_count) {
        *ti_out = &di->iters[di->iter_top];
        return caut_status_ok;
    } else {
        return caut_status_err_iter_stack_would_overflow;
    }
}

S push_type_enc_iter(SEI * ei, TEI ** ti_out, int type_id, void const * type_base) {
    ei->iter_top += 1;
    RE(get_type_enc_iter(ei, ti_out));
    RE(type_encode_iterator_init(ei->desc, *ti_out, type_id, type_base));

    return caut_status_ok;
}

S push_type_dec_iter(SDI * di, TDI ** ti_out, int type_id, void * type_base) {
    di->iter_top += 1;
    RE(get_type_dec_iter(di, ti_out));
    RE(type_decode_iterator_init(di->desc, *ti_out, type_id, type_base));

    return caut_status_ok;
}
