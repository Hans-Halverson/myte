open Ast

class ['a] visitor =
  object (this)
    method module_ : 'a -> Module.t -> unit =
      fun acc mod_ ->
        let { Module.loc = _; module_; imports; toplevels } = mod_ in
        this#module_module acc module_;
        List.iter (this#import acc) imports;
        List.iter (this#toplevel acc) toplevels

    method module_module : 'a -> Module.Module.t -> unit =
      fun acc module_ ->
        let open Module.Module in
        let { loc = _; name } = module_ in
        this#scoped_identifier acc name

    method toplevel : 'a -> Module.toplevel -> unit =
      fun acc toplevel ->
        let open Module in
        match toplevel with
        | VariableDeclaration t -> this#variable_declaration acc t
        | FunctionDeclaration t -> this#function_ acc t

    method statement : 'a -> Statement.t -> unit =
      fun acc stmt ->
        let open Statement in
        match stmt with
        | Expression s -> this#expression_statement acc s
        | Block s -> this#block acc s
        | If s -> this#if_ acc s
        | Return s -> this#return acc s
        | VariableDeclaration s -> this#variable_declaration acc s
        | FunctionDeclaration s -> this#function_ acc s

    method expression : 'a -> Expression.t -> unit =
      fun acc expr ->
        let open Expression in
        match expr with
        | Unit e -> this#unit acc e
        | IntLiteral e -> this#int_literal acc e
        | StringLiteral e -> this#string_literal acc e
        | BoolLiteral e -> this#bool_literal acc e
        | Identifier e -> this#identifier acc e
        | ScopedIdentifier e -> this#scoped_identifier acc e
        | UnaryOperation e -> this#unary_operation acc e
        | BinaryOperation e -> this#binary_operation acc e
        | LogicalAnd e -> this#logical_and acc e
        | LogicalOr e -> this#logical_or acc e
        | Call e -> this#call acc e
        | Access e -> this#access acc e

    method pattern : 'a -> Pattern.t -> unit =
      fun acc pat ->
        let open Pattern in
        match pat with
        | Identifier p -> this#identifier acc p

    method type_ : 'a -> Type.t -> unit =
      fun acc ty ->
        let open Type in
        match ty with
        | Primitive t -> this#primitive_type acc t
        | Function t -> this#function_type acc t

    method identifier _acc _id = ()

    method scoped_identifier acc id =
      let open ScopedIdentifier in
      let { loc = _; name; scopes } = id in
      this#identifier acc name;
      List.iter (this#identifier acc) scopes

    method import acc import =
      let open Module.Import in
      match import with
      | Simple i -> this#scoped_identifier acc i
      | Complex i -> this#complex_import acc i

    method complex_import acc import =
      let open Module.Import.Complex in
      let { loc = _; scopes; aliases } = import in
      List.iter (this#identifier acc) scopes;
      List.iter (this#import_alias acc) aliases

    method import_alias acc alias =
      let open Module.Import.Alias in
      let { loc = _; name; alias } = alias in
      this#identifier acc name;
      Option.iter (this#identifier acc) alias

    method unit _acc _unit = ()

    method int_literal _acc _lit = ()

    method string_literal _acc _lit = ()

    method bool_literal _acc _lit = ()

    method unary_operation acc unary =
      let open Expression.UnaryOperation in
      let { loc = _; op = _; operand } = unary in
      this#expression acc operand

    method binary_operation acc binary =
      let open Expression.BinaryOperation in
      let { loc = _; op = _; left; right } = binary in
      this#expression acc left;
      this#expression acc right

    method logical_and acc logical =
      let open Expression.LogicalAnd in
      let { loc = _; left; right } = logical in
      this#expression acc left;
      this#expression acc right

    method logical_or acc logical =
      let open Expression.LogicalOr in
      let { loc = _; left; right } = logical in
      this#expression acc left;
      this#expression acc right

    method call acc call =
      let open Expression.Call in
      let { loc = _; func; args } = call in
      this#expression acc func;
      List.iter (this#expression acc) args

    method access acc access =
      let open Expression.Access in
      let { loc = _; left; right } = access in
      this#expression acc left;
      this#identifier acc right

    method function_ acc func =
      let open Function in
      let { loc = _; name; params; body; return } = func in
      this#identifier acc name;
      List.iter (this#function_param acc) params;
      this#function_body acc body;
      Option.iter (this#type_ acc) return

    method function_param acc param =
      let open Function.Param in
      let { loc = _; name; annot } = param in
      this#identifier acc name;
      this#type_ acc annot

    method function_body acc body =
      let open Function in
      match body with
      | Block block -> this#block acc block
      | Expression expr -> this#expression acc expr

    method expression_statement acc stmt =
      let (_, expr) = stmt in
      this#expression acc expr

    method block acc block =
      let open Statement.Block in
      let { loc = _; statements } = block in
      List.iter (this#statement acc) statements

    method if_ acc if_ =
      let open Statement.If in
      let { loc = _; test; conseq; altern } = if_ in
      this#expression acc test;
      this#statement acc conseq;
      Option.iter (this#statement acc) altern

    method return acc return =
      let open Statement.Return in
      let { loc = _; arg } = return in
      this#expression acc arg

    method variable_declaration acc decl =
      let open Statement.VariableDeclaration in
      let { loc = _; kind = _; pattern; init; annot } = decl in
      this#pattern acc pattern;
      this#expression acc init;
      Option.iter (this#type_ acc) annot

    method primitive_type _acc _prim = ()

    method function_type acc func =
      let open Type.Function in
      let { loc = _; params; return } = func in
      List.iter (this#type_ acc) params;
      this#type_ acc return
  end
