// Make a linux syscall, arguments must already be in correct registers
#define SYSCALL_WRAPPER(NAME, CODE)                                            \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "mov $" #CODE ", %eax\n"                                             \
          "syscall\n"                                                          \
          "ret");

SYSCALL_WRAPPER(read, 0)
SYSCALL_WRAPPER(write, 1)
SYSCALL_WRAPPER(open, 2)
SYSCALL_WRAPPER(close, 3)
SYSCALL_WRAPPER(exit, 60)
SYSCALL_WRAPPER(unlink, 87)