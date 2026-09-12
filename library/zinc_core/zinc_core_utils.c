#include "zinc_core_typemeta.h"
#include <string.h>

int32_t zinc_core_mem_compare(void* lhs, void* rhs, uint64_t size)  {
    return memcmp(lhs, rhs, size);
}
