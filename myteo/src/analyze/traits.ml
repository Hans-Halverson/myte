open Basic_collections

module Method = struct
  type t = {
    name: string;
    loc: Loc.t;
    is_builtin: bool;
    is_static: bool;
    is_override: bool;
    is_signature: bool;
    (* Type parameters for this function. During type instantiation for the function (where it is
       identified - whether at a direct call or another use) type arguments will be bound to these
       type parameters in the param and return signatures of the type. *)
    mutable type_params: Types.TypeParam.t list;
    (* Parameter types for the function. If the function has type params, this is a signature *)
    mutable params: Types.t list;
    (* Return types for the function. If the function has type params, this is a signature *)
    mutable return: Types.t;
  }

  let mk ~name ~loc ~is_builtin ~is_static ~is_override ~is_signature =
    {
      name;
      loc;
      is_builtin;
      is_static;
      is_override;
      is_signature;
      type_params = [];
      params = [];
      return = Any;
    }
end

module Trait = struct
  type id = int

  type t = {
    id: id;
    name: string;
    loc: Loc.t;
    mutable type_params: Types.TypeParam.t list;
    mutable methods: Method.t SMap.t;
    mutable implemented: implemented_trait LocMap.t;
  }

  and implemented_trait = {
    mutable implemented_trait: t;
    mutable implemented_loc: Loc.t;
    mutable implemented_type_params: Types.TypeParam.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := !max_id + 1;
    id

  let mk ~name ~loc =
    { id = mk_id (); name; loc; type_params = []; methods = SMap.empty; implemented = LocMap.empty }
end
