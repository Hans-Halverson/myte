open Ast

let unchanged f n n' mk =
  let n'' = f n in
  if n'' == n then
    n'
  else
    mk n''

let unchanged_opt f x =
  match x with
  | None -> x
  | Some x' ->
    let x'' = f x' in
    if x'' == x' then
      x
    else
      Some x''

let unchanged_list f lst =
  let rec helper rest acc changed =
    match rest with
    | [] -> (acc, changed)
    | item :: tl ->
      let item' = f item in
      let item_changed = item != item' in
      helper tl (f item :: acc) (item_changed || changed)
  in
  let (lst', changed) = helper lst [] false in
  if not changed then
    lst
  else
    lst'

class ['a, 'b] ast_mapper =
  object (this)
    method decorate : 'a -> 'b = (fun i -> i)

    method loc : Loc.t -> Loc.t = (fun l -> l)

    method module_ : 'a Module.t -> 'b Module.t =
      fun mod_ ->
        let { Module.t; loc; name; imports; toplevels } = mod_ in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let name' = this#scoped_identifier name in
        let imports' = unchanged_list this#import imports in
        let toplevels' = unchanged_list this#toplevel toplevels in
        if t == t' && loc == loc' && name == name' && imports == imports' && toplevels == toplevels'
        then
          mod_
        else
          { Module.t = t'; loc = loc'; name = name'; imports; toplevels = toplevels' }

    method toplevel : 'a Module.toplevel -> 'b Module.toplevel =
      fun toplevel ->
        let open Module in
        match toplevel with
        | VariableDeclaration t ->
          unchanged this#variable_declaration t toplevel (fun t' -> VariableDeclaration t')
        | FunctionDeclaration t ->
          unchanged this#function_ t toplevel (fun t' -> FunctionDeclaration t')

    method statement : 'a Statement.t -> 'b Statement.t =
      fun stmt ->
        let open Statement in
        match stmt with
        | Expression s -> unchanged this#expression_statement s stmt (fun s' -> Expression s')
        | Block s -> unchanged this#block s stmt (fun s' -> Block s')
        | If s -> unchanged this#if_ s stmt (fun s' -> If s')
        | Return s -> unchanged this#return s stmt (fun s' -> Return s')
        | VariableDeclaration s ->
          unchanged this#variable_declaration s stmt (fun s' -> VariableDeclaration s')
        | FunctionDeclaration s ->
          unchanged this#function_ s stmt (fun s' -> FunctionDeclaration s')

    method expression : 'a Expression.t -> 'b Expression.t =
      fun expr ->
        let open Expression in
        match expr with
        | Unit e -> unchanged this#unit e expr (fun e' -> Unit e')
        | IntLiteral e -> unchanged this#int_literal e expr (fun e' -> IntLiteral e')
        | StringLiteral e -> unchanged this#string_literal e expr (fun e' -> StringLiteral e')
        | BoolLiteral e -> unchanged this#bool_literal e expr (fun e' -> BoolLiteral e')
        | Identifier e -> unchanged this#identifier e expr (fun e' -> Identifier e')
        | UnaryOperation e -> unchanged this#unary_operation e expr (fun e' -> UnaryOperation e')
        | BinaryOperation e -> unchanged this#binary_operation e expr (fun e' -> BinaryOperation e')
        | LogicalAnd e -> unchanged this#logical_and e expr (fun e' -> LogicalAnd e')
        | LogicalOr e -> unchanged this#logical_or e expr (fun e' -> LogicalOr e')
        | Call e -> unchanged this#call e expr (fun e' -> Call e')
        | Access e -> unchanged this#access e expr (fun e' -> Access e')

    method pattern : 'a Pattern.t -> 'b Pattern.t =
      fun pat ->
        let open Pattern in
        match pat with
        | Identifier p -> unchanged this#identifier p pat (fun p' -> Identifier p')

    method type_ : 'a Type.t -> 'b Type.t =
      fun ty ->
        let open Type in
        match ty with
        | Primitive t -> unchanged this#primitive_type t ty (fun t' -> Primitive t')
        | Function t -> unchanged this#function_type t ty (fun t' -> Function t')

    method identifier : 'a Identifier.t -> 'b Identifier.t =
      fun id ->
        let open Identifier in
        let { t; loc; name } = id in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          id
        else
          { t = t'; loc = loc'; name }

    method scoped_identifier : 'a ScopedIdentifier.t -> 'b ScopedIdentifier.t =
      fun id ->
        let open ScopedIdentifier in
        let { t; loc; scopes; name } = id in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let scopes' = unchanged_list this#identifier scopes in
        let name' = this#identifier name in
        if t == t' && loc == loc' && scopes == scopes' && name == name' then
          id
        else
          { t = t'; loc = loc'; scopes = scopes'; name = name' }

    method import : 'a Module.Import.t -> 'b Module.Import.t =
      fun import ->
        let open Module.Import in
        match import with
        | Simple i -> unchanged this#scoped_identifier i import (fun i' -> Simple i')
        | Complex i -> unchanged this#complex_import i import (fun i' -> Complex i')

    method complex_import : 'a Module.Import.Complex.t -> 'b Module.Import.Complex.t =
      fun import ->
        let open Module.Import.Complex in
        let { t; loc; scopes; aliases } = import in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let scopes' = unchanged_list this#identifier scopes in
        let aliases' = unchanged_list this#import_alias aliases in
        if t == t' && loc == loc' && scopes == scopes' && aliases == aliases' then
          import
        else
          { t = t'; loc = loc'; scopes = scopes'; aliases = aliases' }

    method import_alias : 'a Module.Import.Alias.t -> 'b Module.Import.Alias.t =
      fun import_alias ->
        let open Module.Import.Alias in
        let { t; loc; name; alias } = import_alias in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let name' = this#identifier name in
        let alias' = unchanged_opt this#identifier alias in
        if t == t' && loc == loc' && name == name' && alias == alias' then
          import_alias
        else
          { t = t'; loc = loc'; name = name'; alias = alias' }

    method unit : 'a Expression.Unit.t -> 'b Expression.Unit.t =
      fun unit ->
        let open Expression.Unit in
        let { t; loc } = unit in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          unit
        else
          { t = t'; loc = loc' }

    method int_literal : 'a Expression.IntLiteral.t -> 'b Expression.IntLiteral.t =
      fun lit ->
        let open Expression.IntLiteral in
        let { t; loc; value; raw } = lit in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          lit
        else
          { t = t'; loc = loc'; value; raw }

    method string_literal : 'a Expression.StringLiteral.t -> 'b Expression.StringLiteral.t =
      fun lit ->
        let open Expression.StringLiteral in
        let { t; loc; value } = lit in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          lit
        else
          { t = t'; loc = loc'; value }

    method bool_literal : 'a Expression.BoolLiteral.t -> 'b Expression.BoolLiteral.t =
      fun lit ->
        let open Expression.BoolLiteral in
        let { t; loc; value } = lit in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          lit
        else
          { t = t'; loc = loc'; value }

    method unary_operation : 'a Expression.UnaryOperation.t -> 'b Expression.UnaryOperation.t =
      fun unary ->
        let open Expression.UnaryOperation in
        let { t; loc; op; operand } = unary in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let operand' = this#expression operand in
        if t == t' && loc == loc' && operand == operand' then
          unary
        else
          { t = t'; loc = loc'; op; operand = operand' }

    method binary_operation : 'a Expression.BinaryOperation.t -> 'b Expression.BinaryOperation.t =
      fun binary ->
        let open Expression.BinaryOperation in
        let { t; loc; op; left; right } = binary in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let left' = this#expression left in
        let right' = this#expression right in
        if t == t' && loc == loc' && left == left' && right == right' then
          binary
        else
          { t = t'; loc = loc'; op; left = left'; right = right' }

    method logical_and : 'a Expression.LogicalAnd.t -> 'b Expression.LogicalAnd.t =
      fun logical ->
        let open Expression.LogicalAnd in
        let { t; loc; left; right } = logical in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let left' = this#expression left in
        let right' = this#expression right in
        if t == t' && loc == loc' && left == left' && right == right' then
          logical
        else
          { t = t'; loc = loc'; left = left'; right = right' }

    method logical_or : 'a Expression.LogicalOr.t -> 'b Expression.LogicalOr.t =
      fun logical ->
        let open Expression.LogicalOr in
        let { t; loc; left; right } = logical in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let left' = this#expression left in
        let right' = this#expression right in
        if t == t' && loc == loc' && left == left' && right == right' then
          logical
        else
          { t = t'; loc = loc'; left = left'; right = right' }

    method call : 'a Expression.Call.t -> 'b Expression.Call.t =
      fun call ->
        let open Expression.Call in
        let { t; loc; func; args } = call in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let func' = this#expression func in
        let args' = List.map this#expression args in
        if t == t' && loc == loc' && func == func' && args == args' then
          call
        else
          { t = t'; loc = loc'; func = func'; args = args' }

    method access : 'a Expression.Access.t -> 'b Expression.Access.t =
      fun access ->
        let open Expression.Access in
        let { t; loc; left; right } = access in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let left' = this#expression left in
        let right' = this#identifier right in
        if t == t' && loc == loc' && left == left' && right == right' then
          access
        else
          { t = t'; loc = loc'; left = left'; right = right' }

    method function_ : 'a Function.t -> 'b Function.t =
      fun func ->
        let open Function in
        let { t; loc; name; params; body; return } = func in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let name' = this#identifier name in
        let params' = unchanged_list this#function_param params in
        let body' = this#function_body body in
        let return' = unchanged_opt this#type_ return in
        if
          t == t'
          && loc == loc'
          && name == name'
          && params == params'
          && body == body'
          && return == return'
        then
          func
        else
          { t = t'; loc = loc'; name = name'; params = params'; body = body'; return = return' }

    method function_param : 'a Function.Param.t -> 'b Function.Param.t =
      fun param ->
        let open Function.Param in
        let { t; loc; name; annot } = param in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let name' = this#identifier name in
        let annot' = this#type_ annot in
        if t == t' && loc == loc' && name == name' && annot == annot' then
          param
        else
          { t = t'; loc = loc'; name = name'; annot = annot' }

    method function_body : 'a Function.body -> 'b Function.body =
      fun body ->
        let open Function in
        match body with
        | Block block -> unchanged this#block block body (fun block' -> Block block')
        | Expression expr -> unchanged this#expression expr body (fun expr' -> Expression expr')

    method expression_statement : Loc.t * 'a Expression.t -> Loc.t * 'b Expression.t =
      fun ((loc, expr) as stmt) ->
        let loc' = this#loc loc in
        let expr' = this#expression expr in
        if loc == loc' && expr == expr' then
          stmt
        else
          (loc', expr')

    method block : 'a Statement.Block.t -> 'b Statement.Block.t =
      fun block ->
        let open Statement.Block in
        let { t; loc; statements } = block in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let statements' = unchanged_list this#statement statements in
        if t == t' && loc == loc' && statements == statements' then
          block
        else
          { t = t'; loc = loc'; statements = statements' }

    method if_ : 'a Statement.If.t -> 'b Statement.If.t =
      fun if_ ->
        let open Statement.If in
        let { t; loc; test; conseq; altern } = if_ in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let test' = this#expression test in
        let conseq' = this#statement conseq in
        let altern' = unchanged_opt this#statement altern in
        if t == t' && loc == loc' && test == test' && conseq == conseq' && altern == altern' then
          if_
        else
          { t = t'; loc = loc'; test = test'; conseq = conseq'; altern = altern' }

    method return : 'a Statement.Return.t -> 'b Statement.Return.t =
      fun return ->
        let open Statement.Return in
        let { t; loc; arg } = return in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let arg' = this#expression arg in
        if t == t' && loc == loc' && arg == arg' then
          return
        else
          { t = t'; loc = loc'; arg = arg' }

    method variable_declaration
        : 'a Statement.VariableDeclaration.t -> 'b Statement.VariableDeclaration.t =
      fun decl ->
        let open Statement.VariableDeclaration in
        let { t; loc; kind; pattern; init; annot } = decl in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let pattern' = this#pattern pattern in
        let init' = this#expression init in
        let annot' = unchanged_opt this#type_ annot in
        if t == t' && loc == loc' && pattern == pattern' && init == init' && annot == annot' then
          decl
        else
          { t = t'; loc = loc'; kind; pattern = pattern'; init = init'; annot = annot' }

    method primitive_type : 'a Type.Primitive.t -> 'b Type.Primitive.t =
      fun prim ->
        let open Type.Primitive in
        let { t; loc; kind } = prim in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        if t == t' && loc == loc' then
          prim
        else
          { t = t'; loc = loc'; kind }

    method function_type : 'a Type.Function.t -> 'b Type.Function.t =
      fun func ->
        let open Type.Function in
        let { t; loc; params; return } = func in
        let t' = this#decorate t in
        let loc' = this#loc loc in
        let params' = unchanged_list this#type_ params in
        let return' = this#type_ return in
        if t == t' && loc == loc' && params == params' && return == return' then
          func
        else
          { t = t'; loc = loc'; params = params'; return = return' }
  end
