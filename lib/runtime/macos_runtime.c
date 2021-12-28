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

LIBC_BUILTIN_WRAPPER(close)
LIBC_BUILTIN_WRAPPER(exit)
LIBC_BUILTIN_WRAPPER(open)
LIBC_BUILTIN_WRAPPER(read)
LIBC_BUILTIN_WRAPPER(unlink)
LIBC_BUILTIN_WRAPPER(write)