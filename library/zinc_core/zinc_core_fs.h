#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

bool zinc_core_path_exists(const char * path, unsigned long len);

bool zinc_core_set_current_dir(const char * path, unsigned long len);

size_t zinc_core_current_exe(char * buf, size_t len);
