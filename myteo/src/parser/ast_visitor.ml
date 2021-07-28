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
        | TypeDeclaration t -> this#type_declaration acc t
        | TraitDeclaration t -> this#trait_declaration acc t

    method statement : 'a -> Statement.t -> unit =
      fun acc stmt ->
        let open Statement in
        match stmt with
        | VariableDeclaration s -> this#variable_declaration acc s
        | FunctionDeclaration s -> this#function_ acc s
        | Expression s -> this#expression_statement acc s
        | Block s -> this#block acc s
        | If s -> this#if_ acc s
        | While s -> this#while_ acc s
        | Return s -> this#return acc s
        | Break s -> this#break acc s
        | Continue s -> this#continue acc s
        | Assignment s -> this#assignment acc s
        | Match s -> this#match_ acc s

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
        | InterpolatedString e -> this#interpolated_string acc e
        | Record e -> this#record_expression acc e
        | Tuple e -> this#tuple_expression acc e
        | TypeCast e -> this#type_cast acc e
        | UnaryOperation e -> this#unary_operation acc e
        | BinaryOperation e -> this#binary_operation acc e
        | LogicalAnd e -> this#logical_and acc e
        | LogicalOr e -> this#logical_or acc e
        | Ternary e -> this#ternary acc e
        | Call e -> this#call acc e
        | IndexedAccess e -> this#indexed_access acc e
        | NamedAccess e -> this#named_access acc e
        | Match e -> this#match_ acc e
        | Super e -> this#super acc e

    method pattern : 'a -> Pattern.t -> unit =
      fun acc pat ->
        let open Pattern in
        match pat with
        | Wildcard _ -> ()
        | Identifier p -> this#scoped_identifier acc p
        | Tuple t -> this#tuple_pattern acc t
        | Record r -> this#record_pattern acc r
        | Literal l -> this#literal_pattern acc l

    method type_ : 'a -> Type.t -> unit =
      fun acc ty ->
        let open Type in
        match ty with
        | Identifier t -> this#identifier_type acc t
        | Tuple t -> this#tuple_type acc t
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

    method interpolated_string acc str =
      let open Expression.InterpolatedString in
      let { loc = _; parts } = str in
      List.iter
        (fun part ->
          match part with
          | String lit -> this#string_literal acc lit
          | Expression expr -> this#expression acc expr)
        parts

    method record_expression acc record =
      let open Expression.Record in
      let { loc = _; name; fields } = record in
      this#expression acc name;
      List.iter (this#record_expression_field acc) fields

    method record_expression_field acc field =
      let open Expression.Record.Field in
      let { loc = _; name; value } = field in
      this#identifier acc name;
      Option.iter (this#expression acc) value

    method tuple_expression acc tuple =
      let open Expression.Tuple in
      let { loc = _; elements } = tuple in
      List.iter (this#expression acc) elements

    method type_cast acc cast =
      let open Expression.TypeCast in
      let { loc = _; expr; ty } = cast in
      this#expression acc expr;
      this#type_ acc ty

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

    method ternary acc ternary =
      let open Expression.Ternary in
      let { loc = _; test; conseq; altern } = ternary in
      this#expression acc test;
      this#expression acc conseq;
      this#expression acc altern

    method call acc call =
      let open Expression.Call in
      let { loc = _; func; args } = call in
      this#expression acc func;
      List.iter (this#expression acc) args

    method indexed_access acc access =
      let open Expression.IndexedAccess in
      let { loc = _; target; index } = access in
      this#expression acc target;
      this#expression acc index

    method named_access acc access =
      let open Expression.NamedAccess in
      let { loc = _; target; name } = access in
      this#expression acc target;
      this#identifier acc name

    method record_pattern acc record =
      let open Pattern.Record in
      let { loc = _; name; fields } = record in
      this#scoped_identifier acc name;
      List.iter (this#record_pattern_field acc) fields

    method record_pattern_field acc field =
      let open Pattern.Record.Field in
      let { loc = _; name; value } = field in
      Option.iter (this#identifier acc) name;
      this#pattern acc value

    method literal_pattern acc lit =
      let open Pattern.Literal in
      match lit with
      | Unit lit -> this#unit acc lit
      | Bool lit -> this#bool_literal acc lit
      | Int lit -> this#int_literal acc lit
      | String lit -> this#string_literal acc lit

    method tuple_pattern acc tuple =
      let open Pattern.Tuple in
      let { loc = _; name; elements } = tuple in
      Option.iter (this#scoped_identifier acc) name;
      List.iter (this#pattern acc) elements

    method function_ acc func =
      let open Function in
      let {
        loc = _;
        name;
        params;
        body;
        return;
        type_params;
        builtin = _;
        static = _;
        override = _;
      } =
        func
      in
      this#identifier acc name;
      List.iter (this#function_param acc) params;
      this#function_body acc body;
      Option.iter (this#type_ acc) return;
      List.iter (this#type_parameter acc) type_params

    method function_param acc param =
      let open Function.Param in
      let { loc = _; name; annot } = param in
      this#identifier acc name;
      this#type_ acc annot

    method type_parameter acc param =
      let open TypeParameter in
      let { loc = _; name; bounds } = param in
      this#identifier acc name;
      List.iter (this#identifier_type acc) bounds

    method function_body acc body =
      let open Function in
      match body with
      | Block block -> this#block acc block
      | Expression expr -> this#expression acc expr
      | Signature -> ()

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

    method while_ acc while_ =
      let open Statement.While in
      let { loc = _; test; body } = while_ in
      this#expression acc test;
      this#statement acc body

    method return acc return =
      let open Statement.Return in
      let { loc = _; arg } = return in
      Option.iter (this#expression acc) arg

    method break _acc _break = ()

    method continue _acc _return = ()

    method assignment acc assign =
      let open Statement.Assignment in
      let { loc = _; lvalue; expr } = assign in
      (match lvalue with
      | Pattern patt -> this#pattern acc patt
      | Expression epxr -> this#expression acc epxr);
      this#expression acc expr

    method match_ acc match_ =
      let open Match in
      let { loc = _; args; cases } = match_ in
      List.iter (this#expression acc) args;
      List.iter (this#match_case acc) cases

    method super _acc _loc = ()

    method match_case acc case =
      let open Match.Case in
      let { loc = _; pattern; guard; right } = case in
      this#pattern acc pattern;
      Option.iter (this#expression acc) guard;
      match right with
      | Expression expr -> this#expression acc expr
      | Statement stmt -> this#statement acc stmt

    method variable_declaration acc decl =
      let open Statement.VariableDeclaration in
      let { loc = _; kind = _; pattern; init; annot } = decl in
      this#pattern acc pattern;
      this#expression acc init;
      Option.iter (this#type_ acc) annot

    method trait_declaration acc decl =
      let open TraitDeclaration in
      let { loc = _; kind = _; name; type_params; implemented; methods } = decl in
      this#identifier acc name;
      List.iter (this#type_parameter acc) type_params;
      List.iter (this#identifier_type acc) implemented;
      List.iter (this#function_ acc) methods

    method type_declaration acc decl =
      let open TypeDeclaration in
      let { loc = _; name; type_params; decl } = decl in
      this#identifier acc name;
      List.iter (this#type_parameter acc) type_params;
      this#type_declaration_declaration acc decl

    method type_declaration_declaration acc decl =
      let open TypeDeclaration in
      match decl with
      | Builtin -> ()
      | Alias ty -> this#type_ acc ty
      | Record record -> this#record_variant acc record
      | Tuple tuple -> this#tuple_variant acc tuple
      | Variant variants -> List.iter (this#variant_declaration acc) variants

    method variant_declaration acc variant =
      let open TypeDeclaration in
      match variant with
      | RecordVariant record -> this#record_variant acc record
      | TupleVariant tuple -> this#tuple_variant acc tuple
      | EnumVariant id -> this#identifier acc id

    method record_variant acc record =
      let open TypeDeclaration.Record in
      let { loc = _; name; fields } = record in
      this#identifier acc name;
      List.iter (this#record_variant_field acc) fields

    method record_variant_field acc field =
      let open TypeDeclaration.Record.Field in
      let { loc = _; name; ty } = field in
      this#identifier acc name;
      this#type_ acc ty

    method tuple_variant acc tuple =
      let open TypeDeclaration.Tuple in
      let { loc = _; name; elements } = tuple in
      this#identifier acc name;
      List.iter (this#type_ acc) elements

    method identifier_type acc id =
      let open Type.Identifier in
      let { loc = _; name; type_args } = id in
      this#scoped_identifier acc name;
      List.iter (this#type_ acc) type_args

    method tuple_type acc tuple =
      let open Type.Tuple in
      let { loc = _; elements } = tuple in
      List.iter (this#type_ acc) elements

    method function_type acc func =
      let open Type.Function in
      let { loc = _; params; return } = func in
      List.iter (this#type_ acc) params;
      this#type_ acc return
  end
