type tvar_id = int

type t =
  | TVar of tvar_id
  | Unit
  | Bool
  | Int
  | String
  | Function of {
      params: t list;
      return: t;
    }

let max_tvar_id = ref 0

let mk_tvar () =
  let tvar_id = !max_tvar_id in
  max_tvar_id := tvar_id + 1;
  TVar tvar_id
