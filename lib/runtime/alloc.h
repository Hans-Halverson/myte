#include <stdint.h>

void myte_runtime_init() asm("__myte_runtime_init");

char *myte_alloc(uintptr_t size) asm("__myte_alloc");

uint64_t myte_get_heap_size() asm("__myte_get_heap_size");
