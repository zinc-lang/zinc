#include "zinc_core_mutex.h"

int32_t sys_mutex_init(OsMutex* mtx) {
    return pthread_mutex_init(&mtx->inner, NULL);
}

int32_t sys_mutex_lock(OsMutex* mtx) {
    return pthread_mutex_lock(&mtx->inner);
}

int32_t sys_mutex_unlock(OsMutex* mtx) {
    return pthread_mutex_unlock(&mtx->inner);
}

int32_t sys_mutex_destroy(OsMutex* mtx) {
    return pthread_mutex_destroy(&mtx->inner);
}
