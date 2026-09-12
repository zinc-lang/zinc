#pragma once

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

void zinc_core_sleep_ms(unsigned int ms);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif
