open Basic_collections
open Mir
open Mir_builders
open Mir_type

type conversion_op =
  | TruncOp of Type.t
  | SExtOp of Type.t
  | ZExtOp of Type.t
  | IntToFloatOp of Type.t
  | FloatToIntOp of Type.t

let apply_unary_operation op (x : Literal.t) : Literal.t =
  match (op, x) with
  | (Instruction.Neg, Bool x) -> Bool x
  | (Neg, Byte x) -> Byte (-x)
  | (Neg, Int x) -> Int (Int32.neg x)
  | (Neg, Long x) -> Long (Int64.neg x)
  | (Neg, Double x) -> Double (Float.neg x)
  | (Not, Bool x) -> Bool (not x)
  | (Not, Byte x) -> Byte (lnot x)
  | (Not, Int x) -> Int (Int32.lognot x)
  | (Not, Long x) -> Long (Int64.lognot x)
  | _ -> failwith "Invalid operation"

let apply_binary_operation op (x : Literal.t) (y : Literal.t) : Literal.t =
  let trunc_integer_to_byte (x : Literal.t) : int =
    match x with
    | Byte x -> x
    | Int x -> Integers.trunc_int_to_byte x
    | Long x -> Integers.trunc_long_to_byte x
    | _ -> failwith "Invalid operation"
  in
  match (op, x, y) with
  | (Instruction.Add, Bool x, Bool y) -> Bool (x <> y)
  | (Add, Byte x, Byte y) -> Byte (x + y)
  | (Add, Int x, Int y) -> Int (Int32.add x y)
  | (Add, Long x, Long y) -> Long (Int64.add x y)
  | (Add, Double x, Double y) -> Double (Float.add x y)
  | (Sub, Bool x, Bool y) -> Bool (x <> y)
  | (Sub, Byte x, Byte y) -> Byte (x - y)
  | (Sub, Int x, Int y) -> Int (Int32.sub x y)
  | (Sub, Long x, Long y) -> Long (Int64.sub x y)
  | (Sub, Double x, Double y) -> Double (Float.sub x y)
  | (Mul, Bool x, Bool y) -> Bool (x && y)
  | (Mul, Byte x, Byte y) -> Byte (x * y)
  | (Mul, Int x, Int y) -> Int (Int32.mul x y)
  | (Mul, Long x, Long y) -> Long (Int64.mul x y)
  | (Mul, Double x, Double y) -> Double (Float.mul x y)
  | (Div, Bool x, Bool y) ->
    if not y then
      failwith "Division by zero"
    else
      Bool x
  | (Div, Byte x, Byte y) -> Byte (x / y)
  | (Div, Int x, Int y) -> Int (Int32.div x y)
  | (Div, Long x, Long y) -> Long (Int64.div x y)
  | (Div, Double x, Double y) -> Double (Float.div x y)
  | (Rem, Bool _, Bool y) ->
    if not y then
      failwith "Division by zero"
    else
      Bool false
  | (Rem, Byte x, Byte y) -> Byte (x mod y)
  | (Rem, Int x, Int y) -> Int (Int32.rem x y)
  | (Rem, Long x, Long y) -> Long (Int64.rem x y)
  | (And, Bool x, Bool y) -> Bool (x && y)
  | (And, Byte x, Byte y) -> Byte (x land y)
  | (And, Int x, Int y) -> Int (Int32.logand x y)
  | (And, Long x, Long y) -> Long (Int64.logand x y)
  | (Or, Bool x, Bool y) -> Bool (x || y)
  | (Or, Byte x, Byte y) -> Byte (x lor y)
  | (Or, Int x, Int y) -> Int (Int32.logor x y)
  | (Or, Long x, Long y) -> Long (Int64.logor x y)
  | (Xor, Bool x, Bool y) -> Bool (x <> y)
  | (Xor, Byte x, Byte y) -> Byte (x lxor y)
  | (Xor, Int x, Int y) -> Int (Int32.logxor x y)
  | (Xor, Long x, Long y) -> Long (Int64.logxor x y)
  | (Shl, Bool x, Bool y) -> Bool (x && not y)
  | (Shl, Byte x, y) -> Byte (x lsl trunc_integer_to_byte y)
  | (Shl, Int x, y) -> Int (Int32.shift_left x (trunc_integer_to_byte y))
  | (Shl, Long x, y) -> Long (Int64.shift_left x (trunc_integer_to_byte y))
  | (Shr, Bool x, Bool _) -> Bool x
  | (Shr, Byte x, y) -> Byte (x asr trunc_integer_to_byte y)
  | (Shr, Int x, y) -> Int (Int32.shift_right x (trunc_integer_to_byte y))
  | (Shr, Long x, y) -> Long (Int64.shift_right x (trunc_integer_to_byte y))
  | (Shrl, Bool x, Bool y) -> Bool (x && not y)
  | (Shrl, Byte x, y) -> Byte (Int.logand x 0xFF lsr trunc_integer_to_byte y)
  | (Shrl, Int x, y) -> Int (Int32.shift_right_logical x (trunc_integer_to_byte y))
  | (Shrl, Long x, y) -> Long (Int64.shift_right_logical x (trunc_integer_to_byte y))
  | _ -> failwith "Invalid operation"

let apply_conversion op (x : Literal.t) : Literal.t =
  match (op, x) with
  | (TruncOp Bool, Byte x) -> Bool (x land 1 = 1)
  | (TruncOp Bool, Int x) -> Bool (Integers.trunc_int_to_bool x)
  | (TruncOp Bool, Long x) -> Bool (Integers.trunc_long_to_bool x)
  | (TruncOp Byte, Int x) -> Byte (Integers.trunc_int_to_byte x)
  | (TruncOp Byte, Long x) -> Byte (Integers.trunc_long_to_byte x)
  | (TruncOp Int, Long x) -> Int (Integers.trunc_long_to_int x)
  | ((SExtOp Byte | ZExtOp Byte), Bool x) -> Int (Integers.zext_bool_to_int x)
  | ((SExtOp Int | ZExtOp Int), Bool x) -> Int (Integers.zext_bool_to_int x)
  | ((SExtOp Long | ZExtOp Long), Bool x) -> Long (Integers.zext_bool_to_long x)
  | (SExtOp Int, Byte x) -> Int (Int32.of_int x)
  | (SExtOp Long, Byte x) -> Long (Int64.of_int x)
  | (SExtOp Long, Int x) -> Long (Int64.of_int32 x)
  | (ZExtOp Int, Byte x) -> Int (Integers.zext_byte_to_int x)
  | (ZExtOp Long, Byte x) -> Long (Integers.zext_byte_to_long x)
  | (ZExtOp Long, Int x) -> Long (Integers.zext_int_to_long x)
  | (IntToFloatOp Double, Byte x) -> Double (Float.of_int x)
  | (IntToFloatOp Double, Int x) -> Double (Int32.to_float x)
  | (IntToFloatOp Double, Long x) -> Double (Int64.to_float x)
  | (FloatToIntOp Byte, Double x) -> Byte (Integers.trunc_int_to_byte (Int32.of_float x))
  | (FloatToIntOp Int, Double x) -> Int (Int32.of_float x)
  | (FloatToIntOp Long, Double x) ->
    (* Largest  *)
    if x > 9223372036854775295.0 then
      Long Int64.max_int
    else
      Long (Int64.of_float x)
  | _ -> failwith "Invalid operation"

let fold_constants_compare (x : Literal.t) (y : Literal.t) : int =
  match (x, y) with
  | (Bool x, Bool y) -> Bool.compare x y
  | (Byte x, Byte y) -> Int.compare x y
  | (Int x, Int y) -> Int32.compare x y
  | (Long x, Long y) -> Int64.compare x y
  | (Double x, Double y) -> Float.compare x y
  | _ -> failwith "Invalid operation"

class constant_folding_transform ~(program : Program.t) =
  object (this)
    val mutable values_queue : VSet.t = VSet.empty

    method run () =
      (* Initially visit all globals and instructions for an initial round of constant folding *)
      SMap.iter (fun _ global -> this#visit_global global) program.globals;
      program_iter_blocks program (fun block ->
          iter_instructions block (fun instr_val instr -> this#visit_instruction instr_val instr));

      (* Pop values off the queue and try to fold constants until there are no values on queue left *)
      while not (VSet.is_empty values_queue) do
        let next_value = this#pop_next_value () in
        match next_value.value with
        | Lit (Global global) -> this#visit_global global
        | Instr instr -> this#visit_instruction next_value instr
        | _ -> ()
      done

    method enqueue_value value = values_queue <- VSet.add value values_queue

    (* Enqueue all values where this value was used to check if they can be folded *)
    method enqueue_value_uses value =
      value_iter_uses ~value (fun use -> this#enqueue_value use.user)

    method pop_next_value () : Value.t =
      let next_value = VSet.choose values_queue in
      values_queue <- VSet.remove next_value values_queue;
      next_value

    method get_constant_opt (use : Use.t) : Literal.t option =
      match use.value.value with
      | Value.Lit ((Bool _ | Byte _ | Int _ | Long _ | Double _ | Function _) as lit) -> Some lit
      | _ -> None

    method visit_global global =
      if global.is_constant then
        match Option_utils.flat_map this#get_constant_opt global.init_val with
        | None -> ()
        | Some _ ->
          this#enqueue_value_uses global.value;
          program.globals <- SMap.remove global.name program.globals

    method visit_instruction instr_val instr =
      match this#try_fold_instruction instr_val instr with
      | None -> ()
      | Some constant ->
        (* Replace this instruction in all its uses with the new constant *)
        this#enqueue_value_uses instr_val;
        let folded_val = mk_value (Lit constant) in
        replace_instruction ~from:instr_val ~to_:folded_val

    method try_fold_instruction instr_val instr =
      let try_fold_conversion arg op =
        match this#get_constant_opt arg with
        | None -> None
        | Some arg -> Some (apply_conversion op arg)
      in
      match instr.instr with
      | Unary (op, arg) ->
        (match this#get_constant_opt arg with
        | None -> None
        | Some arg -> Some (apply_unary_operation op arg))
      | Binary (op, left, right) ->
        (match (this#get_constant_opt left, this#get_constant_opt right) with
        | (Some left, Some right) -> Some (apply_binary_operation op left right)
        | _ -> None)
      | Cmp (cmp, left, right) ->
        let cmp_f =
          match cmp with
          | Eq -> ( == )
          | Neq -> ( <> )
          | Lt -> ( < )
          | LtEq -> ( <= )
          | Gt -> ( > )
          | GtEq -> ( >= )
        in
        (match (this#get_constant_opt left, this#get_constant_opt right) with
        | (Some left, Some right) -> Some (Bool (cmp_f (fold_constants_compare left right) 0))
        | _ -> None)
      | Trunc arg -> try_fold_conversion arg (TruncOp instr.type_)
      | SExt arg -> try_fold_conversion arg (SExtOp instr.type_)
      | ZExt arg -> try_fold_conversion arg (ZExtOp instr.type_)
      | IntToFloat arg -> try_fold_conversion arg (IntToFloatOp instr.type_)
      | FloatToInt arg -> try_fold_conversion arg (FloatToIntOp instr.type_)
      (* Propagate global constants through pointers *)
      | Load { value = { value = Lit (Global { is_constant; init_val = Some init_val; _ }); _ }; _ }
        when is_constant ->
        this#get_constant_opt init_val
      (* Stores in the init function of constants can be converted to constant global initializers *)
      | Store ({ value = { value = Lit (Global global); _ }; _ }, stored_val)
        when instr.block.func.name == init_func_name ->
        (match this#get_constant_opt stored_val with
        | None -> ()
        | Some constant ->
          global_set_init ~global ~init:(Some (mk_value (Lit constant)));
          remove_instruction instr_val;
          this#visit_global global);
        None
      (* Phis where all branches have the same literal argument are replaced with that literal *)
      | Phi phi ->
        (match phi_get_single_arg_value phi with
        | Some { value = Lit arg_literal; _ } -> Some arg_literal
        | _ -> None)
      (* A constant test means the other branch is pruned *)
      | Branch { test; _ } ->
        (match this#get_constant_opt test with
        | Some (Bool to_keep) -> prune_branch to_keep instr.block
        | _ -> ());
        None
      | _ -> None
  end

let fold_constants_and_prune ~program =
  let transform = new constant_folding_transform ~program in
  transform#run ()
