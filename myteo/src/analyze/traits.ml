open Basic_collections

module Method = struct
  type t = {
    (* Type parameters for this function. During type instantiation for the function (where it is
       identified - whether at a direct call or another use) type arguments will be bound to these
       type parameters in the param and return signatures of the type. *)
    mutable type_params: Types.TypeParam.t list;
    (* Parameter types for the function. If the function has type params, this is a signature *)
    mutable params: Types.t list;
    (* Return types for the function. If the function has type params, this is a signature *)
    mutable return: Types.t;
    (* Whether this function is a builtin *)
    is_builtin: bool;
  }
end

module Trait = struct
  type id = int

  type t = {
    id: id;
    mutable type_params: Types.TypeParam.t list;
    mutable methods: Method.t SMap.t;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := !max_id + 1;
    id
end
