#include "alloc.h"
#include "gc.h"
#include <stdlib.h>

#define OOM_EXIT_CODE 97

void mytec_runtime_init() { GC_INIT(); }

char *mytec_alloc(uintptr_t size) {
  char *ptr = GC_MALLOC(size);
  if (!ptr) {
    exit(OOM_EXIT_CODE);
  }

  return ptr;
}

uint64_t mytec_get_heap_size() { return (uint64_t)GC_get_heap_size(); }

void mytec_collect() { return GC_gcollect(); }