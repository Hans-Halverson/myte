#include "alloc.h"

// Align stack when calling into libc builtin functions
#define LIBC_BUILTIN_WRAPPER(NAME)                                             \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "push %rbp\n"                                                        \
          "mov %rsp, %rbp\n"                                                   \
          "and $-16, %rsp\n"                                                   \
          "call _" #NAME "\n"                                                  \
          "mov %rbp, %rsp\n"                                                   \
          "pop %rbp\n"                                                         \
          "ret");

// Align stack when calling functions in the myte C runtime
//
// NAME is name of function that can be called from myte
// C_NAME is name of underlying C function
#define C_RUNTIME_FUNCTION(NAME, C_NAME)                                       \
  __asm__(".global __" #NAME "\n"                                              \
          "__" #NAME ":\n"                                                     \
          "push %rbp\n"                                                        \
          "mov %rsp, %rbp\n"                                                   \
          "and $-16, %rsp\n"                                                   \
          "call " #C_NAME "\n"                                                 \
          "mov %rbp, %rsp\n"                                                   \
          "pop %rbp\n"                                                         \
          "ret");

LIBC_BUILTIN_WRAPPER(close)
LIBC_BUILTIN_WRAPPER(exit)
LIBC_BUILTIN_WRAPPER(open)
LIBC_BUILTIN_WRAPPER(read)
LIBC_BUILTIN_WRAPPER(unlink)
LIBC_BUILTIN_WRAPPER(write)

C_RUNTIME_FUNCTION(myte_alloc, mytec_alloc)
C_RUNTIME_FUNCTION(myte_get_heap_size, mytec_get_heap_size)
C_RUNTIME_FUNCTION(myte_runtime_init, mytec_runtime_init)