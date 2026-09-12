#include "sqlite3.h"

#include "sqlite_wrapper.h"

// todo: 需要注意目前的 schema 设计并不合理，有些需求无法完成
//    比如对于函数，应该单独用一个表来保存，其中有一列要保存函数体。
// 这些场景下，函数需要直接把函数体导出：
// 1. 函数有 inline 修饰
// 2. 函数有 const 修饰

#include <string.h>
#include <stddef.h>
#include <string>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <vector>

typedef struct DB {
    sqlite3 * db = nullptr;

    sqlite3_stmt *select_dependency = nullptr;
    sqlite3_stmt *select_item_equal = nullptr;
    sqlite3_stmt *select_item_like = nullptr;
    sqlite3_stmt *select_impl_by_type = nullptr;
    sqlite3_stmt *select_impl_by_trait = nullptr;
    void prepare_select_stmts();

    sqlite3_stmt *insert_compilation = nullptr;
    sqlite3_stmt *insert_dependency = nullptr;
    sqlite3_stmt *insert_mod = nullptr;
    sqlite3_stmt *insert_global = nullptr;
    sqlite3_stmt *insert_func = nullptr;
    sqlite3_stmt *insert_type = nullptr;
    sqlite3_stmt *insert_impl = nullptr;
    void prepare_insert_stmts();
} DB;

void * zno_open_db(const char * path, unsigned long len) {
    
    std::string db_path = std::string{path, len};

    std::filesystem::path fpath{db_path};
    if (!std::filesystem::exists(fpath)) {
        std::cerr << "文件不存在: " << fpath << std::endl;
        return nullptr;
    }

    sqlite3 * raw_db = nullptr;
    int rc = sqlite3_open(db_path.c_str(), &raw_db);
    if( rc ){
        std::cerr << "无法打开 zno 文件: " << fpath << " 错误信息：" << sqlite3_errmsg(raw_db) << std::endl;
        return nullptr;
    }

    DB * db = new DB;
    db->db = raw_db;
    db->prepare_select_stmts();

    return db;
}

void zno_close_db(void * db) {
    DB * p = (DB*)db;
    if (p) {
        sqlite3_close(p->db);
    }
    delete p;
}

static const char* zno_new_str(const std::string & s) {
    char* buf = new char[s.size() + 1];
    strcpy(buf, s.c_str());
    buf[s.size()] = '\0';
    return buf;
}

void zno_free_string(const char * s) {
    delete[] s;
}

const char * zno_read_item_exact(void* db, const char * key, unsigned long key_len, unsigned char * out_kind) {
    std::string export_key{key, key_len};
    DB * pdb = (DB*) db;
    int r = sqlite3_bind_text(pdb->select_item_equal, 1, export_key.c_str(), -1, nullptr);
    if (r != SQLITE_OK) {
        printf("sqlite3_bind_text error: %s\n", sqlite3_errmsg(pdb->db));
        return nullptr;
    }

    std::string res;
    if (sqlite3_step(pdb->select_item_equal) == SQLITE_ROW) {
        const unsigned char * k = sqlite3_column_text(pdb->select_item_equal, 0);
        *out_kind = *k;
        const unsigned char * s = sqlite3_column_text(pdb->select_item_equal, 1);
        res = std::string((const char *)s);
    } else {
    }
    sqlite3_reset(pdb->select_item_equal);
    return zno_new_str(res);
}

const char * zno_read_item_similar(void* db, const char * key, unsigned long key_len) {
    DB * pdb = (DB*) db;
    std::string export_key{key, key_len};
    std::string joined_name = export_key + "/%";

    int r = sqlite3_bind_text(pdb->select_item_like, 1, joined_name.c_str(), -1, nullptr);
    if (r != SQLITE_OK) {
        return nullptr;
    }

    std::string res;
    while (sqlite3_step(pdb->select_item_like) == SQLITE_ROW) {
        const unsigned char * k = sqlite3_column_text(pdb->select_item_like, 0);
        std::string kind = std::string((const char *)k);
        if (kind == "fn") {
            const unsigned char * decl = sqlite3_column_text(pdb->select_item_like, 1);
            res += std::string((const char *)decl);
        }
    }
    sqlite3_reset(pdb->select_item_like);
    return zno_new_str(res);
}

static std::string exec_select(sqlite3_stmt *stmt, std::string key) {
    sqlite3_bind_text(stmt, 1, key.c_str(), -1, nullptr);
    std::string res;
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const unsigned char * v = sqlite3_column_text(stmt, 0);
        res += std::string((const char *)v);
    }
    sqlite3_reset(stmt);
    return res;
}

const char * zno_read_dependency(void* db, const char * name, unsigned long len) {
    DB *pdb = (DB*)db;
    std::string dep{name, len};
    std::string res = exec_select(pdb->select_dependency, dep.c_str());
    return zno_new_str(res);
}

const char * zno_read_impls_by_type(void* db, const char * key, unsigned long key_len) {
    DB *pdb = (DB*)db;
    std::string name = std::string{key, key_len};
    std::string res = exec_select(pdb->select_impl_by_type, name);
    return zno_new_str(res);
}

const char * zno_read_impls_by_trait(void* db, const char * key, unsigned long key_len) {
    DB *pdb = (DB*)db;
    std::string name = std::string{key, key_len};
    std::string res = exec_select(pdb->select_impl_by_trait, name);
    return zno_new_str(res);
}

static sqlite3_stmt * prepare_stmt(sqlite3 *db, const char *sql) {
    sqlite3_stmt *stmt = nullptr;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        std::cerr << "[Compiler Internal Error] sqlite 执行 sqlite3_prepare_v2 错误 " << std::endl;
        return nullptr;
    }
    return stmt;
}

void DB::prepare_select_stmts() {
    select_dependency = prepare_stmt(db, "SELECT Path FROM Dependencies WHERE Dependency=? ;");
    select_item_equal = prepare_stmt(db, "SELECT Kind, Declaration FROM Items WHERE Name=? ;");
    select_item_like = prepare_stmt(db, "SELECT Kind, Declaration FROM Items WHERE Name LIKE ? ;");
    select_impl_by_type = prepare_stmt(db, "SELECT Declaration FROM Impls WHERE TypeName=? ;");
    select_impl_by_trait = prepare_stmt(db, "SELECT Declaration FROM Impls WHERE TraitName=? ;");
}

static void create_table(sqlite3 *db, const char * sql) {
    char *err_msg = nullptr;
    int rc = sqlite3_exec(db, sql, nullptr, nullptr, &err_msg);
    if( rc != SQLITE_OK ){
        std::cerr << "[Compiler Internal Error] 执行 SQL 语句失败：" << err_msg << std::endl;
    }
}

static void create_tables(sqlite3 *db) {
    create_table(db, 
        "CREATE TABLE Compilation("
            "Key TEXT PRIMARY KEY NOT NULL,"
            "Value TEXT"
        ");"
    );

    create_table(db, 
        "CREATE TABLE Items("
            "Name TEXT PRIMARY KEY NOT NULL,"
            "Kind CHAR(1),"
            "Declaration TEXT"
        ");"
    );

    create_table(db, 
        "CREATE TABLE Impls("
            "TypeName TEXT NOT NULL,"
            "TraitName TEXT NOT NULL,"
            "Declaration TEXT,"
            "PRIMARY KEY (TypeName, TraitName)"
        ");"
    );

    create_table(db, 
        "CREATE TABLE Dependencies("
            "Dependency TEXT PRIMARY KEY NOT NULL,"
            "Version TEXT,"
            "Path TEXT"
        ");"
    );
}

void DB::prepare_insert_stmts() {
    insert_compilation = prepare_stmt(db, "INSERT INTO Compilation VALUES(?, ?);");
    insert_dependency = prepare_stmt(db, "INSERT OR IGNORE INTO Dependencies VALUES(?, ?, ?);");
    insert_mod = prepare_stmt(db, "INSERT OR IGNORE INTO Items VALUES(?, 'mod', ?);");
    insert_global = prepare_stmt(db, "INSERT OR IGNORE INTO Items VALUES(?, 'global', ?);");
    insert_func = prepare_stmt(db, "INSERT OR IGNORE INTO Items VALUES(?, 'fn', ?);");
    insert_type = prepare_stmt(db, "INSERT OR IGNORE INTO Items VALUES(?, 'type', ?);");
    insert_impl = prepare_stmt(db, "INSERT OR IGNORE INTO Impls VALUES(?, ?, ?);");
}

void * zno_create_db(const char * path, unsigned long len) {
    std::string db_path = std::string{path, len};

    std::filesystem::path fpath{db_path};
    if (std::filesystem::exists(fpath)) {
        std::filesystem::remove(fpath);
    }

    sqlite3 * raw_db = nullptr;
    int rc = sqlite3_open(db_path.c_str(), &raw_db);
    if( rc ){
        std::cerr << "无法打开 zno 文件: " << fpath << " 错误信息：" << sqlite3_errmsg(raw_db) << std::endl;
        return nullptr;
    }

    DB * db = new DB;
    db->db = raw_db;
    create_tables(raw_db);

    db->prepare_insert_stmts();

    // 插入当前时间
    const char * sql = "INSERT INTO Compilation VALUES('datetime', datetime('now'))";
    char *err_msg = nullptr;
    int rc1 = sqlite3_exec(db->db, sql, nullptr, nullptr, &err_msg);
    if( rc1 != SQLITE_OK ){
        std::cerr << "[Compiler Internal Error] 执行 SQL 语句失败：" << err_msg << std::endl;
    }

    return db;
}

static void exec_insert2(sqlite3* db, sqlite3_stmt * stmt, const char* key, const char * value) {
    sqlite3_bind_text(stmt, 1, key, -1, nullptr);
    sqlite3_bind_text(stmt, 2, value, -1, nullptr);
    if (sqlite3_step(stmt) != SQLITE_DONE) {
        std::cerr << "[Compiler Internal Error] sqlite 执行 insert 错误: "  << sqlite3_errmsg(db)
                << "\n    key:" << key << "  value:" << value << std::endl;
    }
    sqlite3_reset(stmt);
}

void zno_write_compilation(void * db, const char * key, const char * value) {
    DB * pdb = (DB *)db;
    exec_insert2(pdb->db, pdb->insert_compilation, key, value);
}

void zno_write_mod(void * db, const char * key, const char * value) {
    DB * pdb = (DB *)db;
    exec_insert2(pdb->db, pdb->insert_mod, key, value);
}

void zno_write_global(void * db, const char * key, const char * value) {
    DB * pdb = (DB *)db;
    exec_insert2(pdb->db, pdb->insert_global, key, value);
}

void zno_write_func(void * db, const char * key, const char * value) {
    DB * pdb = (DB *)db;
    exec_insert2(pdb->db, pdb->insert_func, key, value);
}

void zno_write_type(void * db, const char * key, const char * value) {
    DB * pdb = (DB *)db;
    exec_insert2(pdb->db, pdb->insert_type, key, value);
}

void zno_write_impl(void * db, const char * key1, const char * key2, const char * v) {
    DB * pdb = (DB *)db;
    sqlite3_stmt * stmt = pdb->insert_impl;
    sqlite3_bind_text(stmt, 1, key1, -1, nullptr);
    sqlite3_bind_text(stmt, 2, key2, -1, nullptr);
    sqlite3_bind_text(stmt, 3, v, -1, nullptr);
    if (sqlite3_step(stmt) != SQLITE_DONE) {
        std::cerr << "[Compiler Internal Error] sqlite 执行 insert 错误 " << sqlite3_errmsg(pdb->db)
                << "\n    key1:" << key1 << "  key2:" << key2 << "  value:" << v << std::endl;
    }
    sqlite3_reset(stmt);
}
