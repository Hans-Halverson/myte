  .global main

  .p2align 2
  .text

main:
  stp x29, x30, [sp, #-32]!

  # Save argc and argv
  stp x0, x1, [sp, #16]

  # Initialize runtime and global variables
  bl mytec_runtime_init
  bl _myte_init

  # Restore argc and argv
  ldp x0, x1, [sp, #16]

  # Calculate envp
  add x2, x1, x0, lsl 3
  add x2, x2, #8

  # Initialize argc, argv, and envp
  bl std.sys.init

  # Call main function
  bl _main

  # Exit with main's return code
  bl __myte_exit
