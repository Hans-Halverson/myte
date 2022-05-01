#include <stdint.h>

void mytec_runtime_init() asm("mytec_runtime_init");

char *mytec_alloc(uintptr_t size) asm("mytec_alloc");

uint64_t mytec_get_heap_size() asm("mytec_get_heap_size");

void mytec_collect() asm("mytec_collect");