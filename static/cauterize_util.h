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

#endif /* CAUTERIZE_UTIL_H */
