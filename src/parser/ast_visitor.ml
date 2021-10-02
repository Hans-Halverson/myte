open Ast

class visitor =
  object (this)
    method module_ : Module.t -> unit =
      fun mod_ ->
        let { Module.loc = _; module_; imports; toplevels } = mod_ in
        this#module_module module_;
        List.iter this#import imports;
        List.iter this#toplevel toplevels

    method module_module : Module.Module.t -> unit =
      fun module_ ->
        let open Module.Module in
        let { loc = _; name } = module_ in
        this#scoped_identifier name

    method toplevel : Module.toplevel -> unit =
      fun toplevel ->
        let open Module in
        match toplevel with
        | VariableDeclaration t -> this#variable_declaration t
        | FunctionDeclaration t -> this#function_ t
        | TypeDeclaration t -> this#type_declaration t
        | TraitDeclaration t -> this#trait_declaration t

    method statement : Statement.t -> unit =
      fun stmt ->
        let open Statement in
        match stmt with
        | VariableDeclaration s -> this#variable_declaration s
        | FunctionDeclaration s -> this#function_ s
        | Expression s -> this#expression_statement s
        | Block s -> this#block s
        | If s -> this#if_ s
        | While s -> this#while_ s
        | For s -> this#for_ s
        | Return s -> this#return s
        | Break s -> this#break s
        | Continue s -> this#continue s
        | Assignment s -> this#assignment s
        | Match s -> this#match_ s

    method expression : Expression.t -> unit =
      fun expr ->
        let open Expression in
        match expr with
        | Unit e -> this#unit e
        | IntLiteral e -> this#int_literal e
        | StringLiteral e -> this#string_literal e
        | BoolLiteral e -> this#bool_literal e
        | Identifier e -> this#identifier e
        | ScopedIdentifier e -> this#scoped_identifier e
        | InterpolatedString e -> this#interpolated_string e
        | Record e -> this#record_expression e
        | Tuple e -> this#tuple_expression e
        | TypeCast e -> this#type_cast e
        | UnaryOperation e -> this#unary_operation e
        | BinaryOperation e -> this#binary_operation e
        | LogicalAnd e -> this#logical_and e
        | LogicalOr e -> this#logical_or e
        | Ternary e -> this#ternary e
        | Call e -> this#call e
        | IndexedAccess e -> this#indexed_access e
        | NamedAccess e -> this#named_access e
        | Match e -> this#match_ e
        | Super e -> this#super e
        | VecLiteral e -> this#vec_literal e

    method pattern : Pattern.t -> unit =
      fun pat ->
        let open Pattern in
        match pat with
        | Wildcard _ -> ()
        | Identifier p -> this#scoped_identifier p
        | NamedWildcard n -> this#named_wildcard_pattern n
        | Binding b -> this#binding_pattern b
        | Or o -> this#or_pattern o
        | Tuple t -> this#tuple_pattern t
        | Record r -> this#record_pattern r
        | Literal l -> this#literal_pattern l

    method type_ : Type.t -> unit =
      fun ty ->
        let open Type in
        match ty with
        | Identifier t -> this#identifier_type t
        | Tuple t -> this#tuple_type t
        | Function t -> this#function_type t

    method identifier _id = ()

    method scoped_identifier id =
      let open ScopedIdentifier in
      let { loc = _; name; scopes } = id in
      this#identifier name;
      List.iter this#identifier scopes

    method import import =
      let open Module.Import in
      match import with
      | Simple i -> this#scoped_identifier i
      | Complex i -> this#complex_import i

    method complex_import import =
      let open Module.Import.Complex in
      let { loc = _; scopes; aliases } = import in
      List.iter this#identifier scopes;
      List.iter this#import_alias aliases

    method import_alias alias =
      let open Module.Import.Alias in
      let { loc = _; name; alias } = alias in
      this#identifier name;
      Option.iter this#identifier alias

    method unit _unit = ()

    method int_literal _lit = ()

    method string_literal _lit = ()

    method bool_literal _lit = ()

    method interpolated_string str =
      let open Expression.InterpolatedString in
      let { loc = _; parts } = str in
      List.iter
        (fun part ->
          match part with
          | String lit -> this#string_literal lit
          | Expression expr -> this#expression expr)
        parts

    method record_expression record =
      let open Expression.Record in
      let { loc = _; name; fields; rest = _ } = record in
      this#expression name;
      List.iter this#record_expression_field fields

    method record_expression_field field =
      let open Expression.Record.Field in
      let { loc = _; name; value } = field in
      this#identifier name;
      Option.iter this#expression value

    method tuple_expression tuple =
      let open Expression.Tuple in
      let { loc = _; elements } = tuple in
      List.iter this#expression elements

    method type_cast cast =
      let open Expression.TypeCast in
      let { loc = _; expr; ty } = cast in
      this#expression expr;
      this#type_ ty

    method unary_operation unary =
      let open Expression.UnaryOperation in
      let { loc = _; op = _; operand } = unary in
      this#expression operand

    method binary_operation binary =
      let open Expression.BinaryOperation in
      let { loc = _; op = _; left; right } = binary in
      this#expression left;
      this#expression right

    method logical_and logical =
      let open Expression.LogicalAnd in
      let { loc = _; left; right } = logical in
      this#expression left;
      this#expression right

    method logical_or logical =
      let open Expression.LogicalOr in
      let { loc = _; left; right } = logical in
      this#expression left;
      this#expression right

    method ternary ternary =
      let open Expression.Ternary in
      let { loc = _; test; conseq; altern } = ternary in
      this#expression test;
      this#expression conseq;
      this#expression altern

    method call call =
      let open Expression.Call in
      let { loc = _; func; args } = call in
      this#expression func;
      List.iter this#expression args

    method indexed_access access =
      let open Expression.IndexedAccess in
      let { loc = _; target; index } = access in
      this#expression target;
      this#expression index

    method named_access access =
      let open Expression.NamedAccess in
      let { loc = _; target; name } = access in
      this#expression target;
      this#identifier name

    method vec_literal lit =
      let open Expression.VecLiteral in
      let { loc = _; elements } = lit in
      List.iter this#expression elements

    method record_pattern record =
      let open Pattern.Record in
      let { loc = _; name; fields; rest = _ } = record in
      this#scoped_identifier name;
      List.iter this#record_pattern_field fields

    method record_pattern_field field =
      let open Pattern.Record.Field in
      let { loc = _; name; value } = field in
      Option.iter this#identifier name;
      this#pattern value

    method literal_pattern lit =
      let open Pattern.Literal in
      match lit with
      | Unit lit -> this#unit lit
      | Bool lit -> this#bool_literal lit
      | Int lit -> this#int_literal lit
      | String lit -> this#string_literal lit

    method named_wildcard_pattern named =
      let open Pattern.NamedWildcard in
      let { loc = _; name } = named in
      this#scoped_identifier name

    method binding_pattern binding =
      let open Pattern.Binding in
      let { loc = _; pattern; name } = binding in
      this#pattern pattern;
      this#identifier name

    method or_pattern or_ =
      let open Pattern.Or in
      let { loc = _; left; right } = or_ in
      this#pattern left;
      this#pattern right

    method tuple_pattern tuple =
      let open Pattern.Tuple in
      let { loc = _; name; elements } = tuple in
      Option.iter this#scoped_identifier name;
      List.iter this#pattern elements

    method function_ func =
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
      this#identifier name;
      List.iter this#function_param params;
      this#function_body body;
      Option.iter this#type_ return;
      List.iter this#type_parameter type_params

    method function_param param =
      let open Function.Param in
      let { loc = _; name; annot } = param in
      this#identifier name;
      this#type_ annot

    method type_parameter param =
      let open TypeParameter in
      let { loc = _; name; bounds } = param in
      this#identifier name;
      List.iter this#identifier_type bounds

    method function_body body =
      let open Function in
      match body with
      | Block block -> this#block block
      | Expression expr -> this#expression expr
      | Signature -> ()

    method expression_statement stmt =
      let (_, expr) = stmt in
      this#expression expr

    method block block =
      let open Statement.Block in
      let { loc = _; statements } = block in
      List.iter this#statement statements

    method if_ if_ =
      let open Statement.If in
      let { loc = _; test; conseq; altern } = if_ in
      this#expression test;
      this#statement conseq;
      Option.iter this#statement altern

    method while_ while_ =
      let open Statement.While in
      let { loc = _; test; body } = while_ in
      this#expression test;
      this#statement body

    method for_ for_ =
      let open Statement.For in
      let { loc = _; pattern; annot; iterator; body } = for_ in
      this#pattern pattern;
      Option.iter this#type_ annot;
      this#expression iterator;
      this#statement body

    method return return =
      let open Statement.Return in
      let { loc = _; arg } = return in
      Option.iter this#expression arg

    method break _break = ()

    method continue _return = ()

    method assignment assign =
      let open Statement.Assignment in
      let { loc = _; lvalue; expr } = assign in
      (match lvalue with
      | Pattern patt -> this#pattern patt
      | Expression epxr -> this#expression epxr);
      this#expression expr

    method match_ match_ =
      let open Match in
      let { loc = _; args; cases } = match_ in
      List.iter this#expression args;
      List.iter this#match_case cases

    method super _loc = ()

    method match_case case =
      let open Match.Case in
      let { loc = _; pattern; guard; right } = case in
      this#pattern pattern;
      Option.iter this#expression guard;
      match right with
      | Expression expr -> this#expression expr
      | Statement stmt -> this#statement stmt

    method variable_declaration decl =
      let open Statement.VariableDeclaration in
      let { loc = _; kind = _; pattern; init; annot } = decl in
      this#pattern pattern;
      this#expression init;
      Option.iter this#type_ annot

    method trait_declaration decl =
      let open TraitDeclaration in
      let { loc = _; kind = _; name; type_params; implemented; methods } = decl in
      this#identifier name;
      List.iter this#type_parameter type_params;
      List.iter this#identifier_type implemented;
      List.iter this#function_ methods

    method type_declaration decl =
      let open TypeDeclaration in
      let { loc = _; name; type_params; decl } = decl in
      this#identifier name;
      List.iter this#type_parameter type_params;
      this#type_declaration_declaration decl

    method type_declaration_declaration decl =
      let open TypeDeclaration in
      match decl with
      | Builtin -> ()
      | Alias ty -> this#type_ ty
      | Record record -> this#record_variant record
      | Tuple tuple -> this#tuple_variant tuple
      | Variant variants -> List.iter this#variant_declaration variants

    method variant_declaration variant =
      let open TypeDeclaration in
      match variant with
      | RecordVariant record -> this#record_variant record
      | TupleVariant tuple -> this#tuple_variant tuple
      | EnumVariant id -> this#identifier id

    method record_variant record =
      let open TypeDeclaration.Record in
      let { loc = _; name; fields } = record in
      this#identifier name;
      List.iter this#record_variant_field fields

    method record_variant_field field =
      let open TypeDeclaration.Record.Field in
      let { loc = _; name; ty } = field in
      this#identifier name;
      this#type_ ty

    method tuple_variant tuple =
      let open TypeDeclaration.Tuple in
      let { loc = _; name; elements } = tuple in
      this#identifier name;
      List.iter this#type_ elements

    method identifier_type id =
      let open Type.Identifier in
      let { loc = _; name; type_args } = id in
      this#scoped_identifier name;
      List.iter this#type_ type_args

    method tuple_type tuple =
      let open Type.Tuple in
      let { loc = _; elements } = tuple in
      List.iter this#type_ elements

    method function_type func =
      let open Type.Function in
      let { loc = _; params; return } = func in
      List.iter this#type_ params;
      this#type_ return
  end
