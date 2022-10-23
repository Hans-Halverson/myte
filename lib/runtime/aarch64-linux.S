  .global main

  .p2align 2
  .text

main:
  # Call main
  stp x29, x30, [sp, #-16]!
  mov x29, sp
  bl _main
  ldp x29, x30, [sp], #16

  # Exit
  mov X8, #93
  mov X0, XZR
  svc #0
  