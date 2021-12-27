  .global _start
  .global __myte_close
  .global __myte_copy
  .global __myte_exit
  .global __myte_open
  .global __myte_read
  .global __myte_unlink
  .global __myte_write

  .text

_start:
  push %rdi
  push %rsi
  push %rdx
  call _myte_runtime_init
  call _init
  pop %rdx
  pop %rsi
  pop %rdi
  call std.sys.init
  call _main
  ret

__myte_close:
  mov $0x2000006, %rax   # close
  syscall
  jnc .myte_close_ret
  neg %rax
.myte_close_ret:
  ret

__myte_copy:
  mov %rdx, %rcx
  rep movsb
  ret

__myte_exit:
  mov $0x2000001, %rax   # exit
  syscall
  ret

__myte_open:
  mov $0x2000005, %rax   # open
  syscall
  jnc .myte_open_ret
  neg %rax
.myte_open_ret:
  ret

__myte_read:
  mov $0x2000003, %rax   # write
  syscall
  jnc .myte_read_ret
  neg %rax
.myte_read_ret:
  ret

__myte_unlink:
  mov $0x200000a, %rax   # unlink
  syscall
  jnc .myte_unlink_ret
  neg %rax
.myte_unlink_ret:
  ret

__myte_write:
  mov $0x2000004, %rax   # write
  syscall
  jnc .myte_write_ret
  neg %rax
.myte_write_ret:
  ret
