#include "cauterize.h"
#include "cauterize_util.h"
#include "cauterize_encode.h"
#include "cauterize_decode.h"

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#define S enum caut_status
#define SD struct schema_descriptor
#define TD struct type_descriptor
#define TEI struct type_encode_iterator
#define TDI struct type_decode_iterator
#define SEI struct schema_encode_iterator
#define SDI struct schema_decode_iterator

S caut_enc_get(SEI * ei, void * buf, size_t buf_size, size_t * enc_bytes) {
    S ret = caut_status_ok_busy;

    *enc_bytes = 0;

    DEBUG_CHAR('(');

    size_t * i = enc_bytes;
    while (*i < buf_size) {
        bool progress = false;
        uint8_t * const bytes = buf;
        S const s = caut_enc_get_byte(ei, &bytes[*i], &progress);

        if (progress) {
            *i += 1;
        }

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR('.');
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR(')');
            if (ei->iter_top == 0) {
                ret = caut_status_ok;
                ei->iter_top = SIZE_MAX;
                break;
            } else {
                ei->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('(');
        } else {
            assert(s >= ERRS_START);
            DEBUG_CHAR('!');
            ret = s;
            break;
        }
    }

    if (ret != caut_status_ok && ret < ERRS_START && *i >= buf_size) {
        // We ran out of bytes, did not error, but did not finish. Try
        // and unwind the stack in case we have a bunch of pops in a
        // row.

        while (true) {
            bool progress = false;
            S const s = caut_enc_get_byte(ei, NULL, &progress);

            assert(false == progress);
            assert(caut_status_ok_busy != s);

            if (caut_status_ok_pop == s) {
                DEBUG_CHAR(')');
                if (ei->iter_top == 0) {
                    ret = caut_status_ok;
                    ei->iter_top = SIZE_MAX;
                    break;
                } else {
                    ei->iter_top -= 1;
                }
            } else if (caut_status_ok_pushed == s) {
                DEBUG_CHAR('(');
            } else if (caut_status_err_need_byte == s) {
                ret = caut_status_ok_busy;
                break;
            } else {
                // unhandled value
                DEBUG_FMT("Unexpected return: %d\n", s);
                assert(false);
            }
        }
    }

    return ret;
}

S caut_dec_put(SDI * di, void const * buf, size_t buf_size, size_t * dec_bytes) {
    S ret = caut_status_ok_busy;

    *dec_bytes = 0;

    DEBUG_CHAR('(');

    size_t * const i = dec_bytes;

    while (*i < buf_size) {
        bool progress = false;
        uint8_t const * const bytes = buf;

        S const s = caut_dec_put_byte(di, &bytes[*i], &progress);

        if (progress) {
            *i += 1;
        }

        if (caut_status_ok_busy == s) {
            DEBUG_CHAR_IF(progress, '.');
        } else if (caut_status_ok_pop == s) {
            DEBUG_CHAR_IF(progress, '.');
            DEBUG_CHAR(')');
            if (di->iter_top == 0) {
                ret = caut_status_ok;
                di->iter_top = SIZE_MAX;
                break;
            } else {
                di->iter_top -= 1;
            }
        } else if (caut_status_ok_pushed == s) {
            DEBUG_CHAR('(');
            DEBUG_CHAR_IF(progress, '.');
        } else {
            assert(s >= ERRS_START);
            DEBUG_CHAR('!');
            ret = s;
            break;
        }
    }

    if (ret != caut_status_ok && ret < ERRS_START && *i >= buf_size) {
        // We ran out of bytes, did not error, but did not finish. Try
        // and unwind the stack in case we have a bunch of pops in a
        // row.

        while (true) {
            bool progress = false;
            S const s = caut_dec_put_byte(di, NULL, &progress);

            assert(false == progress);
            assert(caut_status_ok_busy != s);

            if (caut_status_ok_pop == s) {
                DEBUG_CHAR(')');
                if (di->iter_top == 0) {
                    ret = caut_status_ok;
                    di->iter_top = SIZE_MAX;
                    break;
                } else {
                    di->iter_top -= 1;
                }
            } else if (caut_status_ok_pushed == s) {
                DEBUG_CHAR('(');
            } else if (caut_status_err_need_byte == s) {
                ret = caut_status_ok_busy;
                break;
            } else {
                // unhandled value
                DEBUG_FMT("Unexpected return: %d\n", s);
                assert(false);
            }
        }
    }

    DEBUG_FMT("\nret = %d\n", ret);

    return ret;
}

S schema_encode_iterator_init(SEI * si, SD const * sd, TEI * ti, size_t ti_count, int type_id, void const * src_type) {
    RE(id_check(sd, type_id));

    memset(si, 0, sizeof(*si));

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

    memset(si, 0, sizeof(*si));

    si->desc = sd;
    si->iters = ti;
    si->iter_count = ti_count;
    si->iter_top = 0;
    si->dst_type = dst_type;

    RE(type_decode_iterator_init(sd, ti, type_id, dst_type));

    return caut_status_ok;
}
