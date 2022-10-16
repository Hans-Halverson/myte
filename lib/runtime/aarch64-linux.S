  .global main

  .text
main:
  mov X8, #64
  mov X0, #1
  ldr X1, =message
  mov X2, #14
  svc #0
  
  mov X8, #93
  mov X0, XZR
  svc #0

  ret

.data
message:
  .asciz "Hello World!\n"