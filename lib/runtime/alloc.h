#include <stdint.h>

void myte_runtime_init() asm("_myte_runtime_init");

char *myte_alloc(uintptr_t size) asm("_myte_alloc");

uint64_t myte_get_heap_size() asm("_myte_get_heap_size");
