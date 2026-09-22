/* backtrace-supported.h -- 正常情况下由 configure 生成, 这里直接给出
 * Linux/glibc + mmap 分配器目标下的取值。 */

#ifndef ZINC_LIBBACKTRACE_SUPPORTED_H
#define ZINC_LIBBACKTRACE_SUPPORTED_H

/* 1 表示 backtrace 库可用 */
#define BACKTRACE_SUPPORTED 1

/* 1 表示库内部用 malloc; 这里用 mmap.c, 所以是 0 */
#define BACKTRACE_USES_MALLOC 0

/* 1 表示支持多线程 (threaded 参数可以传 1) */
#define BACKTRACE_SUPPORTS_THREADS 1

/* 1 表示 backtrace_syminfo 对变量也有效 (ELF 下成立) */
#define BACKTRACE_SUPPORTS_DATA 1

/* 1 表示 backtrace_create_state 支持 MOREDATA flag */
#define BACKTRACE_SUPPORTS_MOREDATA 1

#endif /* ZINC_LIBBACKTRACE_SUPPORTED_H */
