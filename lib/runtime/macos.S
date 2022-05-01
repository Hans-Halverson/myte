  .global _start
  .global __myte_copy

  .text

_start:
  push %rdi
  push %rsi
  push %rdx
  call __myte_runtime_init
  call _myte_init
  pop %rdx
  pop %rsi
  pop %rdi
  call std.sys.init
  call _main
  ret

__myte_copy:
  mov %rdx, %rcx
  rep movsb
  ret
