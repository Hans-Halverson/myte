  .global _start
  .global __myte_copy
  .global mmap

  .text

_start:
  call __myte_runtime_init
  call _init
  mov (%rsp), %rdi
  lea 8(%rsp), %rsi
  lea 24(%rsp), %rdx
  call std.sys.init
  call _main
  mov %eax, %edi
  call __myte_exit

__myte_copy:
  mov %rdx, %rcx
  rep movsb
  ret

mmap:
  mov %rcx, %r10
  mov $9, %rax  # mmap
  syscall
  ret
