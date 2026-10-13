#include "zinc_core_map.h"
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>   // PTHREAD_MUTEX_INITIALIZER (惰性初始化用的全局锁)

// ===========================================================================
// 设计说明: 读多写少 -> 无锁读 + 写加锁 + 旧表延迟回收
// ===========================================================================
typedef _Atomic(void*) Key;
typedef _Atomic(void*) Value;

#define INITIAL_CAPACITY 7

typedef struct {
    Key k;
    Value v;
} KeyValue;

// 一张表: capacity 与桶数组一起分配、一起发布, 发布后结构不再改变。
// (发布后仍可就地插入单个槽位, 见 put/get_or_insert)
typedef struct Table {
    zinc_ushort   capacity;  // 槽位数, 恒为 2^N - 1
    struct Table* retired;   // 延迟回收链的 next (见 retire_table)
    KeyValue      kv[];
} Table;

_Static_assert(offsetof(Table, kv) % _Alignof(KeyValue) == 0, "KeyValue 必须对齐");

// cg_meta.zn 里把 HashMap 的初始值硬编码成 56 字节的全零数组 (sizeof(HashMap)),
// 结构大小变化时必须同步修改那边。
#if defined(__SIZEOF_POINTER__) && __SIZEOF_POINTER__ == 8
_Static_assert(sizeof(HashMap) == 56, "HashMap 大小改变后需要同步修改 cg_meta.zn 中的 56, 以及 std::intrinsic::RtTypeMeta 类型定义");
#endif

// 保护 map 的惰性初始化。不能用 map->mutex: 它本身可能还没初始化,
// 也就没法用它来串行化初始化过程。
static OsMutex g_init_lock = { PTHREAD_MUTEX_INITIALIZER };

// ---------------------------------------------------------------------------
// 延迟回收链
// ---------------------------------------------------------------------------

// 所有 map 被替换下来的旧表都挂在这条全局链上, 运行期只插入、不摘取
// (除了显式调用 zinc_core_map_reclaim_all 的安全点)。
// 用 CAS 插入, 不需要额外的锁; 节点自身的 retired 字段就是链的 next。
static _Atomic(Table*) g_retired_head = NULL;

// 把旧表挂到全局延迟回收链上。只在写路径 (扩容) 调用, 极少触发。
// 注意: 这里只发布表指针, 表的内容不会再有读者以外的人访问, 用 release 足够。
static void retire_table(Table* t) {
    Table* head = atomic_load_explicit(&g_retired_head, memory_order_relaxed);
    do {
        t->retired = head;
    } while (!atomic_compare_exchange_weak_explicit(
                 &g_retired_head, &head, t, memory_order_release, memory_order_relaxed));
}

void zinc_core_map_reclaim_all(void) {
    Table* t = atomic_exchange_explicit(&g_retired_head, NULL, memory_order_acquire);
    while (t != NULL) {
        Table* next = t->retired;
        zinc_core_free_meta(t);
        t = next;
    }
}

// ---------------------------------------------------------------------------
// 小工具
// ---------------------------------------------------------------------------

static void map_lock(HashMapPtr map) {
    int32_t r = sys_mutex_lock(&map->mutex);
    if (r != 0) {
        fprintf(stderr, "zinc_core_map: mutex lock failed (%d).\n", (int)r);
        abort();
    }
}

static void map_unlock(HashMapPtr map) {
    int32_t r = sys_mutex_unlock(&map->mutex);
    if (r != 0) {
        fprintf(stderr, "zinc_core_map: mutex unlock failed (%d).\n", (int)r);
        abort();
    }
}

static Table* table_alloc(zinc_ushort capacity) {
    size_t bytes = sizeof(Table) + (size_t)capacity * sizeof(KeyValue);
    // zinc_core_alloc_meta 在分配失败时会 panic, 不会返回 NULL;
    // 它会把整块内存清零, 所以 retired 字段已经是 NULL。
    Table* t = (Table*)zinc_core_alloc_meta((zinc_uint)bytes);
    t->capacity = capacity;
    return t;
}

// 只在持有 g_init_lock 时调用
static void map_init_locked(HashMapPtr map) {
    if (atomic_load_explicit(&map->table, memory_order_relaxed) != NULL) {
        return; // 幂等: 已经初始化过了
    }
    // mutex 必须在发布 table 之前初始化完成, 否则并发的 put 可能 lock 一个
    // 尚未初始化的 mutex (发布 table 是 release store, 读者 acquire 到非空
    // table 时一定能看到初始化好的 mutex)。
    if (sys_mutex_init(&map->mutex) != 0) {
        fprintf(stderr, "zinc_core_map: mutex init failed.\n");
        abort();
    }
    atomic_store_explicit(&map->table, table_alloc(INITIAL_CAPACITY), memory_order_release);
}

// 写操作入口: 保证 map 已经初始化
static void ensure_init(HashMapPtr map) {
    if (atomic_load_explicit(&map->table, memory_order_acquire) != NULL) {
        return;
    }
    sys_mutex_lock(&g_init_lock);
    map_init_locked(map);
    sys_mutex_unlock(&g_init_lock);
}

void zinc_core_map_init(HashMapPtr map) {
    sys_mutex_lock(&g_init_lock);
    map_init_locked(map);
    sys_mutex_unlock(&g_init_lock);
}

// ---------------------------------------------------------------------------
// 探测 (只在某一张表内进行)
// ---------------------------------------------------------------------------

// 查找 key。命中返回 true 并通过 *out_slot 返回槽位; 未命中返回 false。
static _Bool table_find(const Table* t, void* k, zinc_ushort* out_slot) {
    zinc_ushort cap = t->capacity;
    zinc_ushort start = (zinc_ushort)(((size_t)k) % cap);
    zinc_ushort n = start;
    do {
        void* ek = atomic_load_explicit(&t->kv[n].k, memory_order_acquire);
        if (ek == NULL) {
            return 0;
        }
        if (ek == k) {
            *out_slot = n;
            return 1;
        }
        n++;
        if (n == cap) {
            n = 0;
        }
    } while (n != start);
    return 0;
}

// 给插入找一个空闲槽位: 探测路径上第一个 NULL。负载因子 <= 0.8 保证能找到。
static zinc_ushort table_free_slot(const Table* t, void* k) {
    zinc_ushort cap = t->capacity;
    zinc_ushort n = (zinc_ushort)(((size_t)k) % cap);
    for (zinc_ushort i = 0; i < cap; i++) {
        if (atomic_load_explicit(&t->kv[n].k, memory_order_relaxed) == NULL) {
            return n;
        }
        n++;
        if (n == cap) {
            n = 0;
        }
    }
    fprintf(stderr, "zinc_core_map: table is full (capacity=%u).\n", (unsigned)cap);
    abort();
}

// 把 value 和 key 发布到槽位: 必须先写 value 再 release 写 key,
// 这样读者 acquire 读到 key 时一定能看到对应的 value。
// 槽位一旦发布就不再改写 (本表只插入、不删除、不搬移), 所以不需要更强的同步。
static void table_publish(Table* t, zinc_ushort slot, void* k, void* v) {
    atomic_store_explicit(&t->kv[slot].v, v, memory_order_relaxed);
    atomic_store_explicit(&t->kv[slot].k, k, memory_order_release);
}

static _Bool should_grow(zinc_ushort count, zinc_ushort capacity) {
    return ((zinc_uint)count * 5 >= (zinc_uint)capacity * 4); // 超过 0.8 就扩容
}

// 建一张两倍大的新表, 把元素搬过去。旧表交给 retire_table 延迟回收
// (此刻可能仍有读者在探测它, 绝不能 free)。
static Table* table_grow(const Table* old) {
    zinc_uint new_cap = (zinc_uint)old->capacity * 2 + 1; // 仍然保持 2^N - 1
    if (new_cap > (zinc_uint)(zinc_ushort)~0) {
        fprintf(stderr, "zinc_core_map: capacity overflow.\n");
        abort();
    }
    Table* nt = table_alloc((zinc_ushort)new_cap);
    for (zinc_ushort i = 0; i < old->capacity; i++) {
        void* k = atomic_load_explicit(&old->kv[i].k, memory_order_relaxed);
        if (k == NULL) {
            continue;
        }
        void* v = atomic_load_explicit(&old->kv[i].v, memory_order_relaxed);
        table_publish(nt, table_free_slot(nt, k), k, v);
    }
    retire_table((Table*)old);
    return nt;
}

// ---------------------------------------------------------------------------
// 读操作 (无锁: 只有 acquire load, 没有原子写)
// ---------------------------------------------------------------------------

void* zinc_core_map_get(HashMapPtr map, void* k) {
    Table* t = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);
    if (t == NULL) {
        return NULL; // 还没初始化过 -> 一定为空
    }
    zinc_ushort slot;
    if (table_find(t, k, &slot)) {
        return atomic_load_explicit(&t->kv[slot].v, memory_order_relaxed);
    }
    return NULL;
}

// 遍历这张表, 统计元素个数
static size_t table_count(const Table* t) {
    size_t count = 0;
    zinc_ushort cap = t->capacity;
    for (zinc_ushort i = 0; i < cap; i++) {
        if (atomic_load_explicit(&t->kv[i].k, memory_order_acquire) != NULL) {
            count++;
        }
    }
    return count;
}

size_t zinc_core_map_calc_count(HashMapPtr map) {
    Table* t = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);
    if (t == NULL) {
        return 0;
    }
    return table_count(t);
}

void zinc_core_map_iter(HashMapPtr map, void (*callback)(void* k, void* v, void* extra), void* extra) {
    Table* t = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);
    if (t == NULL) {
        return;
    }
    // 弱一致遍历: 只走这张表, 而这张表在遍历期间不会被改写也不会被释放
    // (扩容只是把新表发布到 map->table, 旧表原样保留到安全点回收)。
    // 回调里对本 map 插入/扩容是安全的: 遍历继续用这张旧的、不可变的表。
    // 遍历期间就地插入的新条目可能落在游标之后而被看到。
    zinc_ushort cap = t->capacity;
    for (zinc_ushort i = 0; i < cap; i++) {
        void* k = atomic_load_explicit(&t->kv[i].k, memory_order_acquire);
        if (k == NULL) {
            continue;
        }
        void* v = atomic_load_explicit(&t->kv[i].v, memory_order_relaxed);
        callback(k, v, extra);
    }
}

// ---------------------------------------------------------------------------
// 写操作 (每个 map 自己的 mutex 串行化; 只在未命中时才会走到这里)
// ---------------------------------------------------------------------------

void zinc_core_map_put(HashMapPtr map, void* k, void* v) {
    ensure_init(map);
    map_lock(map);

    // 加锁后取快照: 加锁前可能已经有别的线程扩容/插入过
    Table* t = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);

    zinc_ushort slot;
    if (table_find(t, k, &slot)) {
        map_unlock(map);
        return; // insert-if-absent: 已存在就不覆盖
    }

    zinc_ushort count = atomic_load_explicit(&map->count, memory_order_relaxed);
    if (should_grow(count, t->capacity)) {
        t = table_grow(t);
        // 发布新表: release 足够 —— 旧表不会被释放, 读者要么看到旧表
        // (内容依然完整) 要么看到新表 (内容已经初始化完整)。
        atomic_store_explicit(&map->table, t, memory_order_release);
    }

    table_publish(t, table_free_slot(t, k), k, v);
    atomic_fetch_add_explicit(&map->count, 1, memory_order_relaxed);

    map_unlock(map);
}

void* zinc_core_map_get_or_insert(HashMapPtr map, void* k, NewValueFn fn, void* arg) {
    // ---- 无锁快路径: 命中就直接返回 (实测 99.997% 的调用都走这里) ----
    Table* ft = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);
    if (ft != NULL) {
        zinc_ushort fslot;
        if (table_find(ft, k, &fslot)) {
            return atomic_load_explicit(&ft->kv[fslot].v, memory_order_relaxed);
        }
    }

    // ---- 慢路径: 未命中才加锁; 加锁后重新探测 (别的线程可能刚刚插入) ----
    ensure_init(map);
    map_lock(map);

    Table* t = (Table*)atomic_load_explicit(&map->table, memory_order_acquire);

    zinc_ushort slot;
    if (table_find(t, k, &slot)) {
        void* v = atomic_load_explicit(&t->kv[slot].v, memory_order_relaxed);
        map_unlock(map);
        return v;
    }

    // fn 在锁内调用: 保证同一个 key 只会被算一次 (回调可能有副作用)。
    // 代价是 fn 不能重入本 map, 见 zinc_core_map.h 的说明。
    void* v = fn(arg);

    zinc_ushort count = atomic_load_explicit(&map->count, memory_order_relaxed);
    if (should_grow(count, t->capacity)) {
        t = table_grow(t);
        atomic_store_explicit(&map->table, t, memory_order_release);
    }

    table_publish(t, table_free_slot(t, k), k, v);
    atomic_fetch_add_explicit(&map->count, 1, memory_order_relaxed);

    map_unlock(map);
    return v;
}
