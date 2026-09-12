#include "zinc_core_map.h"
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
// todo 这个类型要保证线程安全，目前还没有做到
// todo 处理整数越界错误

typedef _Atomic(void*) Key;
typedef _Atomic(void*) Value;

#define TOMBSTONE ((void*)(size_t)-1)

typedef struct {
    Key k;
    Value v;
} KeyValue;

static void* alloc_buckets(zinc_ushort count) {
    void* buckets = zinc_core_alloc_meta(count * sizeof(KeyValue) + sizeof(zinc_uint));
    zinc_uint * head = (zinc_uint*) buckets;
    *head = 1;
    return buckets;
}

static void inc(void* bucket) {
    _Atomic zinc_uint * head = (_Atomic zinc_uint*) bucket;
    *head += 1;
}

static void dec(void* bucket) {
    _Atomic zinc_uint * head = (_Atomic zinc_uint *)bucket;
    zinc_uint c = atomic_fetch_sub_explicit(head, 1, memory_order_seq_cst);
    if (c == 1) {
        zinc_core_free_meta(bucket);
    }
}

static zinc_ushort next_power_of_2(zinc_ushort v) {
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}

static uint32_t psl_of_key(Key *k, zinc_ushort cap, uint32_t idx) {
    uint32_t ideal_idx = ((size_t)k) % cap;
    // 计算 idx 和 ideal_idx 之间的差距
    if (idx >= ideal_idx) {
        return idx - ideal_idx;
    } else {
        return cap + idx -ideal_idx;
    }
}

void zinc_core_map_init(HashMapPtr map) {
    // printf("[zinc_core] zinc_core_map_init %p \n", map);
    // 为了简单，key 的 hash 值就是简单的除以 cap 取余数
    // 保证 cap 永远是 2^N-1，这样对指针取模运算的时候分布比较均匀，默认 cap 是 7

    map->capacity = 7;
    map->count = 0;
    map->buckets = alloc_buckets(7);
    sys_mutex_init(&map->mutex);
}

void* zinc_core_map_get(HashMapPtr map, void* k) {
    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }
    if (map->count == 0) {
        return (Value)0;
    }

    uint32_t idx = ((size_t)k) % map->capacity;
    void * buckets = map->buckets;
    inc(buckets); // 避免在读取的过程中，被另外一个线程删除

    uint32_t n = idx;
    uint32_t psl = 0;
    KeyValue * kv = (KeyValue *)((char*)buckets + sizeof(zinc_uint));
    do {
        if (kv[n].k == NULL) {
            dec(buckets);
            return NULL; // 碰到了空白 bucket，说明没找到结果。
        }
        if (kv[n].k == k) { // 找到了
            dec(buckets);
            return kv[n].v;
        }
        // 如果psl已经过大，说明没找到，可以直接 return NULL
        // 或许可以直接省略这一步算了。在编译器没有 bug 的情况下，不会发生找不到的情况
        // 就算找不到，直接往后查找到 NULL 停止也应该没问题。
        // if (psl > psl_of_key(kv[n].k, map->capacity, n)) {
        //     return NULL;
        // }
        n++;
        psl++;
        n = n % map->capacity;
    } while (n != idx);

    dec(buckets);
    return NULL;
}

static void do_insert(KeyValue * kv, zinc_ushort cap, void* k, void* v) {
    zinc_ushort ideal_idx = ((size_t)k) % cap;

    zinc_ushort dst_idx = ideal_idx;
    zinc_ushort psl = 0;
    while(1) {
        void * e = kv[dst_idx].k;
        if (e == NULL || e == TOMBSTONE) {
            break;
        } else {
            zinc_ushort cur_psl = psl_of_key(kv[dst_idx].k, cap, dst_idx);
            if (psl > cur_psl) {
                break;
            }
        }
        dst_idx = (dst_idx+1) % cap;
        psl++;
    }

    zinc_ushort last_idx = dst_idx;
    while(1) {
        void* e = kv[last_idx].k;
        if (e == NULL || e == TOMBSTONE) {
            break;
        }
        last_idx = (last_idx+1) % cap;
    }

    for(zinc_ushort i = last_idx; i != dst_idx; i = (i==0) ? (cap-1) : (i-1)) {
        zinc_ushort prev = (i==0) ? (cap-1) : (i-1);
        kv[i].k = kv[prev].k;
        kv[i].v = kv[prev].v;
    }
    kv[dst_idx].k = k;
    kv[dst_idx].v = v;
}

// capacity 保持为 2^n - 1
static void hashmap_grow(HashMapPtr map) {
    zinc_ushort new_cap = map->capacity * 2 + 1; // 原来的 capacity 是 2^N-1，新的也确保满足这个规律
    void* new_buckets = alloc_buckets(new_cap);

    // 插入旧元素
    KeyValue * kv = (KeyValue *)((char*)map->buckets + sizeof(zinc_uint));
    KeyValue * new_kv = (KeyValue *)((char*)new_buckets + sizeof(zinc_uint));
    for(zinc_uint i = 0; i < map->capacity; i++) {
        if (kv[i].k != NULL) {
            do_insert(new_kv, new_cap, kv[i].k, kv[i].v);
        }
    }
    void* old_buckets = map->buckets;
    map->buckets = new_buckets;
    map->capacity = new_cap;
    dec(old_buckets);
}

static _Bool should_grow(HashMapPtr map) {
    return (map->count * 5 >= map->capacity * 4); // 超过 0.8 就扩容
}

void       zinc_core_map_put(HashMapPtr map, void* k, void* v) {
    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }

    sys_mutex_lock(&map->mutex); // TODO: handle error
    void* old_buckets = map->buckets;
    inc(old_buckets);

    // 重新查找是为了避免多个线程多次插入
    void* exists = zinc_core_map_get(map, k);
    if (exists != NULL) {
        dec(old_buckets);
        int32_t r = sys_mutex_unlock(&map->mutex); // TODO: handle error
        if (r != 0) {
            fprintf(stderr, "runtime mutex unlock failed.\n");
            abort();
        }
        return;
    }

    if (should_grow(map)) {
        hashmap_grow(map);
    }

    KeyValue * kv = (KeyValue *)((char*)map->buckets + sizeof(zinc_uint));
    do_insert(kv, map->capacity, k, v);
    map->count++;

    dec(old_buckets);
    int32_t r = sys_mutex_unlock(&map->mutex); // TODO: handle error
    if (r != 0) {
        fprintf(stderr, "runtime mutex unlock failed.\n");
        abort();
    }
}

void*      zinc_core_map_get_or_insert(HashMapPtr map, void* k, NewValueFn fn, void* arg) {

    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }

    uint32_t idx = ((size_t)k) % map->capacity;
    void* old_buckets = map->buckets;
    inc(old_buckets); // 避免在读取的过程中，被另外一个线程删除

    uint32_t n = idx;
    uint32_t psl = 0;
    KeyValue * kv = (KeyValue *)((char*)old_buckets + sizeof(zinc_uint));
    do {
        if (kv[n].k == NULL) {// 碰到了空白 bucket，说明没找到结果，需要 insert
            // 需要判断是否应该扩容, 这个地方需要处理重复插入问题
            sys_mutex_lock(&map->mutex);

            void* exists = zinc_core_map_get(map, k);
            if (exists != NULL) {
                dec(old_buckets);
                int32_t r = sys_mutex_unlock(&map->mutex); // TODO: handle error
                if (r != 0) {
                    fprintf(stderr, "runtime mutex unlock failed.\n");
                    abort();
                }
                return exists;
            }

            Value v = fn(arg);
            if (should_grow(map)) {
                // 慢速路径
                hashmap_grow(map);
                KeyValue * kv = (KeyValue *)((char*)map->buckets + sizeof(zinc_uint));
                do_insert(kv, map->capacity, k, v);
            } else {
                // 快速路径
                kv[n].v = v;
                kv[n].k = k;
            }
            map->count++;
            int32_t r = sys_mutex_unlock(&map->mutex);
            if (r != 0) {
                fprintf(stderr, "runtime mutex unlock failed.\n");
                abort();
            }
            dec(old_buckets);
            return v;
        }
        if (kv[n].k == k) { // 找到了
            dec(old_buckets);
            return kv[n].v;
        }

        n++;
        psl++;
        n = n % map->capacity;
    } while (n != idx);

    dec(old_buckets); // unreachable
    return NULL;
}

void zinc_core_map_iter(HashMapPtr map, void (*callback)(void* k, void* v, void* extra), void* extra) {
    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }
    if (map->count == 0) {
        return;
    }

    inc(map->buckets);

    KeyValue * kv = (KeyValue *)((char*)map->buckets + sizeof(zinc_uint));
    for(zinc_ushort i = 0; i < map->capacity; i++) {
        void* e = kv[i].k;
        if (e != NULL && e != TOMBSTONE) {
            callback(e, kv[i].v, extra);
        }
    }

    dec(map->buckets);
}

void* zinc_core_map_remove(HashMapPtr map, void* k) {
    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }
    if (map->count == 0) {
        return (Value)0;
    }

    uint32_t idx = ((size_t)k) % map->capacity;
    void * buckets = map->buckets;
    inc(buckets); // 避免在读取的过程中，被另外一个线程删除

    uint32_t n = idx;
    uint32_t psl = 0;
    KeyValue * kv = (KeyValue *)((char*)buckets + sizeof(zinc_uint));
    do {
        if (kv[n].k == NULL) {
            dec(buckets);
            return NULL; // 碰到了空白 bucket，说明没找到结果。
        }
        if (kv[n].k == k) { // 找到了
            kv[n].k = TOMBSTONE;
            void * e = kv[n].v;
            kv[n].v = NULL;
            map->count -= 1;
            dec(buckets);
            return e;
        }

        n++;
        psl++;
        n = n % map->capacity;
    } while (n != idx);

    dec(buckets);
    return NULL;
}

size_t zinc_core_map_calc_count(HashMapPtr map) {
    size_t count = 0;
    if (map->capacity == 0) {
        zinc_core_map_init(map);
    }
    if (map->count == 0) {
        return count;
    }

    inc(map->buckets);

    KeyValue * kv = (KeyValue *)((char*)map->buckets + sizeof(zinc_uint));
    for(zinc_ushort i = 0; i < map->capacity; i++) {
        void* e = kv[i].k;
        if (e != NULL && e != TOMBSTONE) {
            count++;
        }
    }

    dec(map->buckets);
    return count;
}
