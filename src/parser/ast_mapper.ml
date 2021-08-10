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
        | TraitDeclaration t ->
          id_map this#trait_declaration t toplevel (fun t' -> TraitDeclaration t')

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
        | Break s -> id_map this#break s stmt (fun s' -> Break s')
        | Continue s -> id_map this#continue s stmt (fun s' -> Continue s')
        | Assignment s -> id_map this#assignment s stmt (fun s' -> Assignment s')
        | Match s -> id_map this#match_ s stmt (fun s' -> Match s')

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
        | InterpolatedString e ->
          id_map this#interpolated_string e expr (fun e' -> InterpolatedString e')
        | Record e -> id_map this#record_expression e expr (fun e' -> Record e')
        | Tuple e -> id_map this#tuple_expression e expr (fun e' -> Tuple e')
        | TypeCast e -> id_map this#type_cast e expr (fun e' -> TypeCast e')
        | UnaryOperation e -> id_map this#unary_operation e expr (fun e' -> UnaryOperation e')
        | BinaryOperation e -> id_map this#binary_operation e expr (fun e' -> BinaryOperation e')
        | LogicalAnd e -> id_map this#logical_and e expr (fun e' -> LogicalAnd e')
        | LogicalOr e -> id_map this#logical_or e expr (fun e' -> LogicalOr e')
        | Ternary e -> id_map this#ternary e expr (fun e' -> Ternary e')
        | Call e -> id_map this#call e expr (fun e' -> Call e')
        | IndexedAccess e -> id_map this#indexed_access e expr (fun e' -> IndexedAccess e')
        | NamedAccess e -> id_map this#named_access e expr (fun e' -> NamedAccess e')
        | Match e -> id_map this#match_ e expr (fun e' -> Match e')
        | Super _ -> expr

    method pattern : Pattern.t -> Pattern.t =
      fun pat ->
        let open Pattern in
        match pat with
        | Wildcard _ -> pat
        | Identifier p -> id_map this#scoped_identifier p pat (fun p' -> Identifier p')
        | NamedWildcard p -> id_map this#named_wildcard_pattern p pat (fun p' -> NamedWildcard p')
        | Binding b -> id_map this#binding_pattern b pat (fun b' -> Binding b')
        | Or o -> id_map this#or_pattern o pat (fun o' -> Or o')
        | Tuple e -> id_map this#tuple_pattern e pat (fun e' -> Tuple e')
        | Record e -> id_map this#record_pattern e pat (fun e' -> Record e')
        | Literal e -> id_map this#literal_pattern e pat (fun e' -> Literal e')

    method type_ : Type.t -> Type.t =
      fun ty ->
        let open Type in
        match ty with
        | Identifier t -> id_map this#identifier_type t ty (fun t' -> Identifier t')
        | Tuple t -> id_map this#tuple_type t ty (fun t' -> Tuple t')
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

    method interpolated_string str =
      let open Expression.InterpolatedString in
      let { loc; parts } = str in
      let parts' = id_map_list this#interpolated_string_part parts in
      if parts == parts' then
        str
      else
        { loc; parts = parts' }

    method interpolated_string_part part =
      let open Expression.InterpolatedString in
      match part with
      | String p -> id_map this#string_literal p part (fun p' -> String p')
      | Expression p -> id_map this#expression p part (fun p' -> Expression p')

    method record_expression record =
      let open Expression.Record in
      let { loc; name; fields; rest } = record in
      let name' = this#expression name in
      let fields' = id_map_list this#record_expression_field fields in
      if name == name' && fields == fields' then
        record
      else
        { loc; name = name'; fields = fields'; rest }

    method record_expression_field field =
      let open Expression.Record.Field in
      let { loc; name; value } = field in
      let name' = this#identifier name in
      let value' = id_map_opt this#expression value in
      if name == name' && value == value' then
        field
      else
        { loc; name = name'; value = value' }

    method tuple_expression tuple =
      let open Expression.Tuple in
      let { loc; elements } = tuple in
      let elements' = id_map_list this#expression elements in
      if elements == elements' then
        tuple
      else
        { loc; elements = elements' }

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

    method ternary ternary =
      let open Expression.Ternary in
      let { loc; test; conseq; altern } = ternary in
      let test' = this#expression test in
      let conseq' = this#expression conseq in
      let altern' = this#expression altern in
      if test == test' && conseq == conseq' && altern == altern' then
        ternary
      else
        { loc; test = test'; conseq = conseq'; altern = altern' }

    method call call =
      let open Expression.Call in
      let { loc; func; args } = call in
      let func' = this#expression func in
      let args' = id_map_list this#expression args in
      if func == func' && args == args' then
        call
      else
        { loc; func = func'; args = args' }

    method indexed_access access =
      let open Expression.IndexedAccess in
      let { loc; target; index } = access in
      let target' = this#expression target in
      let index' = this#expression index in
      if target == target' && index == index' then
        access
      else
        { loc; target = target'; index = index' }

    method named_access access =
      let open Expression.NamedAccess in
      let { loc; target; name } = access in
      let target' = this#expression target in
      let name' = this#identifier name in
      if target == target' && name == name' then
        access
      else
        { loc; target = target'; name = name' }

    method record_pattern record =
      let open Pattern.Record in
      let { loc; name; fields; rest } = record in
      let name' = this#scoped_identifier name in
      let fields' = id_map_list this#record_pattern_field fields in
      if name == name' && fields == fields' then
        record
      else
        { loc; name = name'; fields = fields'; rest }

    method record_pattern_field field =
      let open Pattern.Record.Field in
      let { loc; name; value } = field in
      let name' = id_map_opt this#identifier name in
      let value' = this#pattern value in
      if name == name' && value == value' then
        field
      else
        { loc; name = name'; value = value' }

    method literal_pattern lit =
      let open Pattern.Literal in
      match lit with
      | Unit l -> id_map this#unit l lit (fun l' -> Unit l')
      | Bool l -> id_map this#bool_literal l lit (fun l' -> Bool l')
      | Int l -> id_map this#int_literal l lit (fun l' -> Int l')
      | String l -> id_map this#string_literal l lit (fun l' -> String l')

    method named_wildcard_pattern named =
      let open Pattern.NamedWildcard in
      let { loc; name } = named in
      let name' = this#scoped_identifier name in
      if name == name' then
        named
      else
        { loc; name = name' }

    method binding_pattern binding =
      let open Pattern.Binding in
      let { loc; pattern; name } = binding in
      let pattern' = this#pattern pattern in
      let name' = this#identifier name in
      if pattern == pattern' && name == name' then
        binding
      else
        { loc; pattern = pattern'; name = name' }

    method or_pattern or_ =
      let open Pattern.Or in
      let { loc; left; right } = or_ in
      let left' = this#pattern left in
      let right' = this#pattern right in
      if left == left' && right == right' then
        or_
      else
        { loc; left = left'; right = right' }

    method tuple_pattern tuple =
      let open Pattern.Tuple in
      let { loc; name; elements } = tuple in
      let name' = id_map_opt this#scoped_identifier name in
      let elements' = id_map_list this#pattern elements in
      if name == name' && elements == elements' then
        tuple
      else
        { loc; name = name'; elements = elements' }

    method function_ func =
      let open Function in
      let { loc; name; params; body; return; type_params; builtin; static; override } = func in
      let name' = this#identifier name in
      let params' = id_map_list this#function_param params in
      let body' = this#function_body body in
      let return' = id_map_opt this#type_ return in
      let type_params' = id_map_list this#type_parameter type_params in
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
          builtin;
          static;
          override;
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
      | Signature -> body

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

    method break break = break

    method continue continue = continue

    method assignment assign =
      let open Statement.Assignment in
      let { loc; lvalue; expr } = assign in
      let lvalue' =
        match lvalue with
        | Pattern pattern -> id_map this#pattern pattern lvalue (fun p' -> Pattern p')
        | Expression expr -> id_map this#expression expr lvalue (fun e' -> Expression e')
      in
      let expr' = this#expression expr in
      if lvalue == lvalue' && expr == expr' then
        assign
      else
        { loc; lvalue = lvalue'; expr = expr' }

    method match_ match_ =
      let open Match in
      let { loc; args; cases } = match_ in
      let args' = id_map_list this#expression args in
      let cases' = id_map_list this#match_case cases in
      if args == args' && cases == cases' then
        match_
      else
        { loc; args = args'; cases = cases' }

    method match_case case =
      let open Match.Case in
      let { loc; pattern; guard; right } = case in
      let pattern' = this#pattern pattern in
      let guard' = id_map_opt this#expression guard in
      let right' =
        match right with
        | Expression e -> id_map this#expression e right (fun e' -> Expression e')
        | Statement s -> id_map this#statement s right (fun s' -> Statement s')
      in
      if pattern == pattern' && guard == guard' && right == right' then
        case
      else
        { loc; pattern = pattern'; guard = guard'; right = right' }

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

    method trait_declaration decl =
      let open TraitDeclaration in
      let { loc; kind; name; type_params; implemented; methods } = decl in
      let name' = this#identifier name in
      let type_params' = id_map_list this#type_parameter type_params in
      let implemented' = id_map_list this#identifier_type implemented in
      let methods' = id_map_list this#function_ methods in
      if name == name' && type_params == type_params' && methods == methods' then
        decl
      else
        {
          loc;
          kind;
          name = name';
          type_params = type_params';
          implemented = implemented';
          methods = methods';
        }

    method type_parameter param =
      let open TypeParameter in
      let { loc; name; bounds } = param in
      let name' = this#identifier name in
      let bounds' = id_map_list this#identifier_type bounds in
      if name == name' && bounds == bounds' then
        param
      else
        { loc; name = name'; bounds = bounds' }

    method type_declaration declaration =
      let open TypeDeclaration in
      let { loc; name; type_params; decl } = declaration in
      let name' = this#identifier name in
      let type_params' = id_map_list this#type_parameter type_params in
      let decl' =
        match decl with
        | Builtin -> decl
        | Alias a -> id_map this#type_ a decl (fun a' -> Alias a')
        | Record r -> id_map this#record_variant r decl (fun r' -> Record r')
        | Tuple t -> id_map this#tuple_variant t decl (fun t' -> Tuple t')
        | Variant variants ->
          let variants' = id_map_list this#type_declaration_variant variants in
          if variants == variants' then
            decl
          else
            Variant variants'
      in
      if name == name' && type_params == type_params' && decl == decl' then
        declaration
      else
        { loc; name = name'; decl = decl'; type_params = type_params' }

    method type_declaration_variant variant =
      let open TypeDeclaration in
      match variant with
      | RecordVariant r -> id_map this#record_variant r variant (fun r' -> RecordVariant r')
      | TupleVariant t -> id_map this#tuple_variant t variant (fun t' -> TupleVariant t')
      | EnumVariant i -> id_map this#enum_variant i variant (fun i' -> EnumVariant i')

    method enum_variant id = this#identifier id

    method record_variant record =
      let open TypeDeclaration.Record in
      let { loc; name; fields } = record in
      let name' = this#identifier name in
      let fields' = id_map_list this#record_variant_field fields in
      if name == name' && fields == fields' then
        record
      else
        { loc; name = name'; fields = fields' }

    method record_variant_field field =
      let open TypeDeclaration.Record.Field in
      let { loc; name; ty } = field in
      let name' = this#identifier name in
      let ty' = this#type_ ty in
      if name == name' && ty == ty' then
        field
      else
        { loc; name = name'; ty = ty' }

    method tuple_variant tuple =
      let open TypeDeclaration.Tuple in
      let { loc; name; elements } = tuple in
      let name' = this#identifier name in
      let elements' = id_map_list this#type_ elements in
      if name == name' && elements == elements' then
        tuple
      else
        { loc; name = name'; elements = elements' }

    method identifier_type id =
      let open Type.Identifier in
      let { loc; name; type_args } = id in
      let name' = this#scoped_identifier name in
      let type_args' = id_map_list this#type_ type_args in
      if name == name' && type_args == type_args' then
        id
      else
        { loc; name = name'; type_args = type_args' }

    method tuple_type tuple =
      let open Type.Tuple in
      let { loc; elements } = tuple in
      let elements' = id_map_list this#type_ elements in
      if elements == elements' then
        tuple
      else
        { loc; elements = elements' }

    method function_type func =
      let open Type.Function in
      let { loc; params; return } = func in
      let params' = id_map_list this#type_ params in
      let return' = this#type_ return in
      if params == params' && return == return' then
        func
      else
        { loc; params = params'; return = return' }
  end
