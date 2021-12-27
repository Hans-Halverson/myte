#include "alloc.h"
#include <sys/mman.h>

// Starting address of heap
char *heap_start;

// Top address of heap, next address to allocate object at
char *heap_current;

void myte_runtime_init() {
  char *heap = mmap(0 /* new memory region */, 0x20000000 /* 512M */,
                    PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,
                    -1 /* file descriptor */, 0 /* offset */);
  heap_start = heap;
  heap_current = heap;
}

char *myte_alloc(uintptr_t size) {
  // Align to 16-byte boundary
  uintptr_t aligned_heap_current = ((uintptr_t)heap_current + 15) & -16;
  heap_current = (char *)(aligned_heap_current + size);

  return (char *)aligned_heap_current;
}

uint64_t myte_get_heap_size() {
  return (uintptr_t)heap_current - (uintptr_t)heap_start;
}