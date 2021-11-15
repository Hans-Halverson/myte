let file =
  {|
  .global _start
  .global __myte_alloc
  .global __myte_close
  .global __myte_copy
  .global __myte_exit
  .global __myte_open
  .global __myte_read
  .global __myte_runtime_init
  .global __myte_unlink
  .global __myte_write

  .data

__myte_heap_start:
  .quad 0

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

__myte_runtime_init:
  # Allocate memory region for heap
  mov $9, %rax          # mmap
  xor %edi, %edi        # addr = 0 for new memory region
  mov $0x20000000, %rsi # len = 512M
  mov $3, %rdx          # prot = PROT_READ | PROT_WRITE
  mov $0x22, %r10       # flags = MAP_PRIVATE | MAP_ANONYMOUS
  mov $-1, %r8          # fd
  xor %r9, %r9          # offset = 0
  syscall

  # Store heap start
  mov %rax, __myte_heap_start(%rip)
  ret

__myte_alloc:
  mov __myte_heap_start(%rip), %rdx
  # Align to 16-byte boundary
  add $15, %rdx
  and $-16, %rdx
  mov %rdx, %rax
  # Bump heap start pointer
  add %rdi, %rdx
  mov %rdx, __myte_heap_start(%rip)
  ret

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
|}
