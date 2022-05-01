  .global main
  .global __myte_copy
  .global mmap

  .text

main:
  push %rdi
  push %rsi
  call __myte_runtime_init
  call _myte_init
  mov environ(%rip), %rdx
  pop %rsi
  pop %rdi
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
