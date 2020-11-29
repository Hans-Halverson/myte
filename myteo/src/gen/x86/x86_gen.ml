open X86_instructions

let rsquad r = RegisterRead (r, Quad)

let isquad i = Immediate (QuadImmediate i)

let rwquad r = RegisterWrite (r, Quad)

let msquad offset r index_and_scale = { base_register = (r, Quad); offset; index_and_scale }

let hello_world_executable =
  {
    rodata = [];
    data = [{ label = "msg"; value = AsciiData "Hello World!\n" }];
    text =
      [
        {
          label = "_main";
          instructions =
            [
              Push (rsquad BP);
              Mov (rsquad SP, rwquad BP);
              Mov (isquad (Int64.of_int 14), rwquad D);
              Lea (msquad (Some (LabelOffset "msg")) IP None, (SI, Quad));
              Mov (isquad Int64.one, rwquad DI);
              Mov (isquad (Int64.of_int 33554436), rwquad A);
              Syscall;
              Mov (isquad Int64.zero, rwquad A);
              Leave;
              Ret;
            ];
        };
      ];
  }

let gen_x86_executable _ir = hello_world_executable
