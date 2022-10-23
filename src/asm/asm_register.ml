open Basic_collections

type x86_64_register =
  (* General purpose integer registers *)
  [ `A
  | `B
  | `C
  | `D
  | `SI
  | `DI
  | `SP
  | `BP
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
  | (* SSE registers *)
    `XMM0
  | `XMM1
  | `XMM2
  | `XMM3
  | `XMM4
  | `XMM5
  | `XMM6
  | `XMM7
  | `XMM8
  | `XMM9
  | `XMM10
  | `XMM11
  | `XMM12
  | `XMM13
  | `XMM14
  | `XMM15
  ]

type aarch64_register =
  [ (* General purpose registers *)
    `R0
  | `R1
  | `R2
  | `R3
  | `R4
  | `R5
  | `R6
  | `R7
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
  | `R16
  | `R17
  | `R18
  | `R19
  | `R20
  | `R21
  | `R22
  | `R23
  | `R24
  | `R25
  | `R26
  | `R27
  | `R28
  | `R29
  | (* Link register *)
    `R30
  | (* Stack pointer / Zero register *)
    `R31
  | (* Floating point and vector registers *)
    `V0
  | `V1
  | `V2
  | `V3
  | `V4
  | `V5
  | `V6
  | `V7
  | `V8
  | `V9
  | `V10
  | `V11
  | `V12
  | `V13
  | `V14
  | `V15
  | `V16
  | `V17
  | `V18
  | `V19
  | `V20
  | `V21
  | `V22
  | `V23
  | `V24
  | `V25
  | `V26
  | `V27
  | `V28
  | `V29
  | `V30
  | `V31
  ]

module Register = struct
  type t =
    [ aarch64_register
    | x86_64_register
    ]

  let compare = Stdlib.compare
end

module RegisterCollection = MakeCollection (Register)

module RegSet = RegisterCollection.Set
module RegMap = RegisterCollection.Map
