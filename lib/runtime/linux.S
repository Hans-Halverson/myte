  .global _start
  .global __myte_close
  .global __myte_copy
  .global __myte_exit
  .global __myte_open
  .global __myte_read
  .global __myte_unlink
  .global __myte_write
  .global mmap

  .text

_start:
  call _myte_runtime_init
  call _init
  mov (%rsp), %rdi
  lea 8(%rsp), %rsi
  lea 24(%rsp), %rdx
  call std.sys.init
  call _main
  mov %eax, %edi
  call __myte_exit

__myte_close:
  mov $3, %rax  # close
  syscall
  ret

__myte_copy:
  mov %rdx, %rcx
  rep movsb
  ret

__myte_exit:
  mov $60, %rax  # exit
  syscall
  ret

__myte_open:
  mov $2, %rax  # open
  syscall
  ret

__myte_read:
  xor %rax, %rax  # read
  syscall
  ret

__myte_unlink:
  mov $87, %rax  # unlink
  syscall
  ret

__myte_write:
  mov $1, %rax  # write
  syscall
  ret

mmap:
  mov %rcx, %r10
  mov $9, %rax  # mmap
  syscall
  ret
