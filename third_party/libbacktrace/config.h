/* config.h -- libbacktrace 的构建配置。
 *
 * 正常情况下这个文件由 autotools 的 configure 生成。这里没有把整套
 * configure 搬进来, 而是直接给出 Linux/glibc + GCC/Clang 目标下的取值。
 * 如果要支持 ELF 以外的目标 (Mach-O/PE/XCOFF), 需要按平台重新生成。
 */

#ifndef ZINC_LIBBACKTRACE_CONFIG_H
#define ZINC_LIBBACKTRACE_CONFIG_H

/* dl_iterate_phdr 等 GNU 扩展需要 */
#define _GNU_SOURCE 1

/* ELF size: 32 or 64 */
#define BACKTRACE_ELF_SIZE 64

#define HAVE_ATOMIC_FUNCTIONS 1
#define HAVE_CLOCK_GETTIME 1
#define HAVE_DECL_GETPAGESIZE 1
#define HAVE_DECL_STRNLEN 1
#define HAVE_DECL__PGMPTR 0
#define HAVE_DLFCN_H 1
#define HAVE_DL_ITERATE_PHDR 1
#define HAVE_FCNTL 1
#define HAVE_GETIPINFO 1
#define HAVE_INTTYPES_H 1
#define HAVE_LINK_H 1
#define HAVE_LSTAT 1
#define HAVE_MEMORY_H 1
#define HAVE_READLINK 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYNC_FUNCTIONS 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
#define STDC_HEADERS 1

/* 读取压缩的 debug section (.zdebug_* / zstd) 需要额外链接 zlib/zstd, 暂不启用 */
/* #undef HAVE_ZLIB */
/* #undef HAVE_ZSTD */
/* #undef HAVE_LIBLZMA */

#define PACKAGE_NAME "libbacktrace"
#define PACKAGE_VERSION "1.0"
#define PACKAGE_STRING "libbacktrace 1.0"
#define PACKAGE_TARNAME "libbacktrace"
#define PACKAGE_BUGREPORT ""
#define PACKAGE_URL ""

#endif /* ZINC_LIBBACKTRACE_CONFIG_H */
