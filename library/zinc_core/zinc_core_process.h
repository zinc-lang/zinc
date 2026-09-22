#pragma once

// 这两个函数由生成的 main 调用。zinc_core_protect_stack 也可以在每个线程里
// 调用一次 (可重复调用), 给那个线程装好栈溢出的备用信号栈。
void zinc_core_exit(int code);
void zinc_core_protect_stack(void);
