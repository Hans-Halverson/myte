// Make a linux syscall, arguments must already be in correct registers
#define SYSCALL_WRAPPER(NAME, CODE)                                            \
  __asm__(".global __myte_" #NAME "\n"                                         \
          "__myte_" #NAME ":\n"                                                \
          "mov $" #CODE ", %eax\n"                                             \
          "syscall\n"                                                          \
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

SYSCALL_WRAPPER(read, 0)
SYSCALL_WRAPPER(write, 1)
SYSCALL_WRAPPER(open, 2)
SYSCALL_WRAPPER(close, 3)
SYSCALL_WRAPPER(exit, 60)
SYSCALL_WRAPPER(unlink, 87)

C_RUNTIME_FUNCTION(myte_alloc, mytec_alloc)
C_RUNTIME_FUNCTION(myte_get_heap_size, mytec_get_heap_size)
C_RUNTIME_FUNCTION(myte_runtime_init, mytec_runtime_init)
C_RUNTIME_FUNCTION(myte_collect, mytec_collect)