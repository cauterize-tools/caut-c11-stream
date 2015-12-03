#ifndef CAUTERIZE_UTIL_H
#define CAUTERIZE_UTIL_H

#include "cauterize_types.h"

// return error if error
#define RE(EXP) \
    do { \
        S s; if (caut_status_ok != (s = EXP)) {  \
            return s; \
        } \
    } while(0)

// #define DEBUG_QUIET 1

#if defined(NDEBUG) || defined(DEBUG_QUIET)
#define DEBUG_CHAR(c)
#define DEBUG_CHAR_IF(cond, c)
#define DEBUG_FMT(fmt, ...)
#else
#define DEBUG_CHAR(c) do { fputc(c, stderr); fflush(stderr); } while (0)
#define DEBUG_CHAR_IF(cond, c) do { if (cond) { DEBUG_CHAR(c); } } while (0)
#define DEBUG_FMT(fmt, ...) do { fprintf(stderr, fmt, __VA_ARGS__); fflush(stderr); } while (0)
#endif



#endif /* CAUTERIZE_UTIL_H */
