  .global _start

  .data

__myte_heap_start:
  .quad 0

  .text

_start:
  call __myte_init
  call _main
  ret

__myte_init:
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
  # Bump heap start pointer
  mov __myte_heap_start(%rip), %rax
  add %rdi, __myte_heap_start(%rip)
  ret