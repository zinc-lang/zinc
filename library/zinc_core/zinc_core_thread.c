#include "zinc_core_thread.h"
#include <time.h>

void zinc_core_sleep_ms(unsigned int ms) {
    time_t seconds = ms / 1000;
    long nanoseconds = (ms % 1000) * 1000000;
    struct timespec req = {seconds, nanoseconds};
    struct timespec rem;

    if (nanosleep(&req, &rem) == -1) {
        
    }
    return;
}