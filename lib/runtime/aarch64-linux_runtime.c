// Make a linux syscall, arguments must already be in correct registers
#define SYSCALL_WRAPPER(NAME, CODE)                                            \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "mov x8, #" #CODE "\n"                                               \
          "svc #0\n"                                                           \
          "ret");

// Make a linux syscall that prepends AT_FDCWD (-100) as an argument. This
// signals that the instruction should be interpreted from the current working
// directory, and is used for the openat and unlink at syscalls.
#define SYSCALL_PREPEND_DFD_WRAPPER(NAME, CODE)                                \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "mov x0, #-100\n"                                                    \
          "mov x8, #" #CODE "\n"                                               \
          "svc #0\n"                                                           \
          "ret");

#define SYSCALL_PREPEND_DFD_WRAPPER_APPEND_0(NAME, CODE)                       \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "mov x0, #-100\n"                                                    \
          "mov x2, xzr\n"                                                      \
          "mov x8, #" #CODE "\n"                                               \
          "svc #0\n"                                                           \
          "ret");

SYSCALL_WRAPPER(read, 63)
SYSCALL_WRAPPER(write, 64)
SYSCALL_WRAPPER(close, 57)
SYSCALL_WRAPPER(exit, 93)
SYSCALL_PREPEND_DFD_WRAPPER(open, 56)
SYSCALL_PREPEND_DFD_WRAPPER_APPEND_0(unlink, 35)