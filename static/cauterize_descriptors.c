#include "cauterize_descriptors.h"

#define U8_ID (-1)
#define U16_ID (-2)
#define U32_ID (-3)
#define U64_ID (-4)
#define S8_ID (-5)
#define S16_ID (-6)
#define S32_ID (-7)
#define S64_ID (-8)
#define F32_ID (-9)
#define F64_ID (-10)
#define BOOL_ID (-11)

#define PROTO_DESC(NAME, ID, SIZE) \
    { \
        .name = NAME, \
        .type_id = ID, \
        .prototype_tag = caut_proto_primitive, \
        .prototype.c_primitive = { .word_size = SIZE }, \
    }

struct type_descriptor const primitive_descriptors[CAUT_PRIMITIVE_COUNT] = {
    PROTO_DESC("u8", U8_ID, 1),
    PROTO_DESC("u16", U16_ID, 2),
    PROTO_DESC("u32", U32_ID, 4),
    PROTO_DESC("u64", U64_ID, 8),
    PROTO_DESC("s8", S8_ID, 1),
    PROTO_DESC("s16", S16_ID, 2),
    PROTO_DESC("s32", S32_ID, 4),
    PROTO_DESC("s64", S64_ID, 8),
    PROTO_DESC("f32", F32_ID, 4),
    PROTO_DESC("f64", F64_ID, 8),
    PROTO_DESC("bool", BOOL_ID, 1),
};
