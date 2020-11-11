open Ast
open Immutable_utils

let id x = x

class mapper =
  object (this)
    method module_ : Module.t -> Module.t =
      fun mod_ ->
        let { Module.loc; module_; imports; toplevels } = mod_ in
        let module' = this#module_module module_ in
        let imports' = id_map_list this#import imports in
        let toplevels' = id_map_list this#toplevel toplevels in
        if module_ == module' && imports == imports' && toplevels == toplevels' then
          mod_
        else
          { Module.loc; module_ = module'; imports = imports'; toplevels = toplevels' }

    method module_module : Module.Module.t -> Module.Module.t =
      fun module_ ->
        let open Module.Module in
        let { loc; name } = module_ in
        let name' = this#scoped_identifier name in
        if name == name' then
          module_
        else
          { loc; name = name' }

    method toplevel : Module.toplevel -> Module.toplevel =
      fun toplevel ->
        let open Module in
        match toplevel with
        | VariableDeclaration t ->
          id_map this#variable_declaration t toplevel (fun t' -> VariableDeclaration t')
        | FunctionDeclaration t ->
          id_map this#function_ t toplevel (fun t' -> FunctionDeclaration t')
        | TypeDeclaration t ->
          id_map this#type_declaration t toplevel (fun t' -> TypeDeclaration t')

    method statement : Statement.t -> Statement.t =
      fun stmt ->
        let open Statement in
        match stmt with
        | VariableDeclaration s ->
          id_map this#variable_declaration s stmt (fun s' -> VariableDeclaration s')
        | FunctionDeclaration s -> id_map this#function_ s stmt (fun s' -> FunctionDeclaration s')
        | Expression s -> id_map this#expression_statement s stmt (fun s' -> Expression s')
        | Block s -> id_map this#block s stmt (fun s' -> Block s')
        | If s -> id_map this#if_ s stmt (fun s' -> If s')
        | While s -> id_map this#while_ s stmt (fun s' -> While s')
        | Return s -> id_map this#return s stmt (fun s' -> Return s')
        | Assignment s -> id_map this#assignment s stmt (fun s' -> Assignment s')

    method expression : Expression.t -> Expression.t =
      fun expr ->
        let open Expression in
        match expr with
        | Unit e -> id_map this#unit e expr (fun e' -> Unit e')
        | IntLiteral e -> id_map this#int_literal e expr (fun e' -> IntLiteral e')
        | StringLiteral e -> id_map this#string_literal e expr (fun e' -> StringLiteral e')
        | BoolLiteral e -> id_map this#bool_literal e expr (fun e' -> BoolLiteral e')
        | Identifier e -> id_map this#identifier e expr (fun e' -> Identifier e')
        | ScopedIdentifier e -> id_map this#scoped_identifier e expr (fun e' -> ScopedIdentifier e')
        | TypeCast e -> id_map this#type_cast e expr (fun e' -> TypeCast e')
        | UnaryOperation e -> id_map this#unary_operation e expr (fun e' -> UnaryOperation e')
        | BinaryOperation e -> id_map this#binary_operation e expr (fun e' -> BinaryOperation e')
        | LogicalAnd e -> id_map this#logical_and e expr (fun e' -> LogicalAnd e')
        | LogicalOr e -> id_map this#logical_or e expr (fun e' -> LogicalOr e')
        | Call e -> id_map this#call e expr (fun e' -> Call e')
        | Access e -> id_map this#access e expr (fun e' -> Access e')

    method pattern : Pattern.t -> Pattern.t =
      fun pat ->
        let open Pattern in
        match pat with
        | Identifier p -> id_map this#identifier p pat (fun p' -> Identifier p')

    method type_ : Type.t -> Type.t =
      fun ty ->
        let open Type in
        match ty with
        | Primitive t -> id_map this#primitive_type t ty (fun t' -> Primitive t')
        | Custom t -> id_map this#custom_type t ty (fun t' -> Custom t')
        | Function t -> id_map this#function_type t ty (fun t' -> Function t')

    method identifier id = id

    method scoped_identifier id =
      let open ScopedIdentifier in
      let { loc; name; scopes } = id in
      let name' = this#identifier name in
      let scopes' = id_map_list this#identifier scopes in
      if name == name' && scopes == scopes' then
        id
      else
        { loc; name = name'; scopes = scopes' }

    method import import =
      let open Module.Import in
      match import with
      | Simple i -> id_map this#scoped_identifier i import (fun i' -> Simple i')
      | Complex i -> id_map this#complex_import i import (fun i' -> Complex i')

    method complex_import import =
      let open Module.Import.Complex in
      let { loc; scopes; aliases } = import in
      let scopes' = id_map_list this#identifier scopes in
      let aliases' = id_map_list this#import_alias aliases in
      if scopes == scopes' && aliases == aliases' then
        import
      else
        { loc; scopes = scopes'; aliases = aliases' }

    method import_alias alias_ =
      let open Module.Import.Alias in
      let { loc; name; alias } = alias_ in
      let name' = this#identifier name in
      let alias' = id_map_opt this#identifier alias in
      if name == name' && alias == alias' then
        alias_
      else
        { loc; name = name'; alias = alias' }

    method unit unit = unit

    method int_literal lit = lit

    method string_literal lit = lit

    method bool_literal lit = lit

    method type_cast cast =
      let open Expression.TypeCast in
      let { loc; expr; ty } = cast in
      let expr' = this#expression expr in
      let ty' = this#type_ ty in
      if expr == expr' && ty == ty' then
        cast
      else
        { loc; expr = expr'; ty = ty' }

    method unary_operation unary =
      let open Expression.UnaryOperation in
      let { loc; op; operand } = unary in
      let operand' = this#expression operand in
      if operand == operand' then
        unary
      else
        { loc; op; operand = operand' }

    method binary_operation binary =
      let open Expression.BinaryOperation in
      let { loc; op; left; right } = binary in
      let left' = this#expression left in
      let right' = this#expression right in
      if left == left' && right == right' then
        binary
      else
        { loc; op; left = left'; right = right' }

    method logical_and logical =
      let open Expression.LogicalAnd in
      let { loc; left; right } = logical in
      let left' = this#expression left in
      let right' = this#expression right in
      if left == left' && right == right' then
        logical
      else
        { loc; left = left'; right = right' }

    method logical_or logical =
      let open Expression.LogicalOr in
      let { loc; left; right } = logical in
      let left' = this#expression left in
      let right' = this#expression right in
      if left == left' && right == right' then
        logical
      else
        { loc; left = left'; right = right' }

    method call call =
      let open Expression.Call in
      let { loc; func; args } = call in
      let func' = this#expression func in
      let args' = id_map_list this#expression args in
      if func == func' && args == args' then
        call
      else
        { loc; func = func'; args = args' }

    method access access =
      let open Expression.Access in
      let { loc; left; right } = access in
      let left' = this#expression left in
      let right' = this#identifier right in
      if left == left' && right == right' then
        access
      else
        { loc; left = left'; right = right' }

    method function_ func =
      let open Function in
      let { loc; name; params; body; return; type_params } = func in
      let name' = this#identifier name in
      let params' = id_map_list this#function_param params in
      let body' = this#function_body body in
      let return' = id_map_opt this#type_ return in
      let type_params' = id_map_list this#identifier type_params in
      if
        name == name'
        && params == params'
        && body == body'
        && return == return'
        && type_params == type_params'
      then
        func
      else
        {
          loc;
          name = name';
          params = params';
          body = body';
          return = return';
          type_params = type_params';
        }

    method function_param param =
      let open Function.Param in
      let { loc; name; annot } = param in
      let name' = this#identifier name in
      let annot' = this#type_ annot in
      if name == name' && annot == annot' then
        param
      else
        { loc; name = name'; annot = annot' }

    method function_body body =
      let open Function in
      match body with
      | Block block -> id_map this#block block body (fun block' -> Block block')
      | Expression expr -> id_map this#expression expr body (fun expr' -> Expression expr')

    method expression_statement stmt =
      let (loc, expr) = stmt in
      let expr' = this#expression expr in
      if expr == expr' then
        stmt
      else
        (loc, expr')

    method block block =
      let open Statement.Block in
      let { loc; statements } = block in
      let statements' = id_map_list this#statement statements in
      if statements == statements' then
        block
      else
        { loc; statements = statements' }

    method if_ if_ =
      let open Statement.If in
      let { loc; test; conseq; altern } = if_ in
      let test' = this#expression test in
      let conseq' = this#statement conseq in
      let altern' = id_map_opt this#statement altern in
      if test == test' && conseq == conseq' && altern == altern' then
        if_
      else
        { loc; test = test'; conseq = conseq'; altern = altern' }

    method while_ while_ =
      let open Statement.While in
      let { loc; test; body } = while_ in
      let test' = this#expression test in
      let body' = this#statement body in
      if test == test' && body == body' then
        while_
      else
        { loc; test = test'; body = body' }

    method return return =
      let open Statement.Return in
      let { loc; arg } = return in
      let arg' = id_map_opt this#expression arg in
      if arg == arg' then
        return
      else
        { loc; arg = arg' }

    method assignment assign =
      let open Statement.Assignment in
      let { loc; pattern; expr } = assign in
      let pattern' = this#pattern pattern in
      let expr' = this#expression expr in
      if pattern == pattern' && expr == expr' then
        assign
      else
        { loc; pattern = pattern'; expr = expr' }

    method variable_declaration decl =
      let open Statement.VariableDeclaration in
      let { loc; kind; pattern; init; annot } = decl in
      let pattern' = this#pattern pattern in
      let init' = this#expression init in
      let annot' = id_map_opt this#type_ annot in
      if pattern == pattern' && init == init' && annot == annot' then
        decl
      else
        { loc; kind; pattern = pattern'; init = init'; annot = annot' }

    method type_declaration decl =
      let open TypeDeclaration in
      let { loc; name; ty } = decl in
      let name' = this#identifier name in
      let ty' = this#type_ ty in
      if name == name' && ty == ty' then
        decl
      else
        { loc; name = name'; ty = ty' }

    method primitive_type prim = prim

    method custom_type custom =
      let open Type.Custom in
      let { loc; name } = custom in
      let name' = this#scoped_identifier name in
      if name == name' then
        custom
      else
        { loc; name = name' }

    method function_type func =
      let open Type.Function in
      let { loc; params; return; type_params } = func in
      let params' = id_map_list this#type_ params in
      let return' = this#type_ return in
      let type_params' = id_map_list this#identifier type_params in
      if params == params' && return == return' && type_params == type_params' then
        func
      else
        { loc; params = params'; return = return'; type_params = type_params' }
  end
