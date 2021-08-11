  .global __myte_alloc
  .global __myte_copy
  .global __myte_exit
  .global __myte_runtime_init
  .global __myte_write

  .data

__myte_heap_start:
  .quad 0

  .text

__myte_runtime_init:
  # Allocate memory region for heap
  mov $0x20000C5, %rax  # mmap
  xor %edi, %edi        # addr = 0 for new memory region
  mov $0x20000000, %rsi # len = 512M
  mov $3, %rdx          # prot = PROT_READ | PROT_WRITE
  mov $0x1002, %r10     # flags = MAP_PRIVATE | MAP_ANONYMOUS
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

__myte_copy:
  mov %rdx, %rcx
  rep movsb
  ret

__myte_exit:
  mov $0x2000001, %rax   # exit
  syscall
  ret

__myte_write:
  mov $0x2000004, %rax   # write
  syscall
  ret
