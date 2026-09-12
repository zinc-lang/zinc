#include "zinc_core_time.h"

#include <time.h>
#include <stdio.h>

void zinc_core_instance_now(Instance * s) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    s->tv_sec = now.tv_sec;
    s->tv_nsec = now.tv_nsec;
}

int64_t zinc_core_instance_elapsed(Instance * from) {
    struct Instance now;
    zinc_core_instance_now(&now);

    int64_t diff_sec = now.tv_sec - from->tv_sec;
    int64_t diff_nsec = now.tv_nsec - from->tv_nsec;

    return (diff_sec * 1000) + (diff_nsec / 1000000);
}