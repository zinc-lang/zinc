#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// reader

void * zno_open_db(const char * path, unsigned long len);
void zno_close_db(void * db);

const char * zno_read_dependency(void* db, const char * name, unsigned long len);
const char * zno_read_item_exact(void* db, const char * key, unsigned long key_len, unsigned char * out_kind);
const char * zno_read_item_similar(void* db, const char * key, unsigned long key_len);
const char * zno_read_impls_by_type(void* db, const char * key, unsigned long key_len);
const char * zno_read_impls_by_trait(void* db, const char * key, unsigned long key_len);

void zno_free_string(const char * s);

// writer

void * zno_create_db(const char * path, unsigned long len);
void zno_write_compilation(void * db, const char * key, const char * value);
void zno_write_mod(void * db, const char * key, const char * value);
void zno_write_global(void * db, const char * key, const char * value);
void zno_write_func(void * db, const char * key, const char * value);
void zno_write_type(void * db, const char * key, const char * value);
void zno_write_impl(void * db, const char * key, const char * v1, const char * v2);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif
