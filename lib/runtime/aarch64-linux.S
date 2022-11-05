  .global main

  .p2align 2
  .text

main:
  stp x29, x30, [sp, #-16]!
#  bl mytec_runtime_init
#  bl _myte_init
  bl _main
  bl __myte_exit
