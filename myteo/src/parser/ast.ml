module rec Module : sig
  type toplevel =
    | VariableDeclaration of Statement.VariableDeclaration.t
    | FunctionDeclaration of Function.t
    | TypeDeclaration of TypeDeclaration.t
    | TraitDeclaration of TraitDeclaration.t

  module Module : sig
    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t;
    }
  end

  module Import : sig
    module Alias : sig
      type t = {
        loc: Loc.t;
        name: Identifier.t;
        alias: Identifier.t option;
      }
    end

    module Complex : sig
      type t = {
        loc: Loc.t;
        scopes: Identifier.t list;
        aliases: Alias.t list;
      }
    end

    type t =
      | Simple of ScopedIdentifier.t
      | Complex of Complex.t
  end

  type t = {
    loc: Loc.t;
    module_: Module.t;
    imports: Import.t list;
    toplevels: toplevel list;
  }
end =
  Module

and Statement : sig
  module Block : sig
    type t = {
      loc: Loc.t;
      statements: Statement.t list;
    }
  end

  module If : sig
    type t = {
      loc: Loc.t;
      test: Expression.t;
      conseq: Statement.t;
      altern: Statement.t option;
    }
  end

  module While : sig
    type t = {
      loc: Loc.t;
      test: Expression.t;
      body: Statement.t;
    }
  end

  module Return : sig
    type t = {
      loc: Loc.t;
      arg: Expression.t option;
    }
  end

  module Break : sig
    type t = { loc: Loc.t }
  end

  module Continue : sig
    type t = { loc: Loc.t }
  end

  module Assignment : sig
    type lvalue =
      | Pattern of Pattern.t
      | Expression of Expression.t

    type t = {
      loc: Loc.t;
      lvalue: lvalue;
      expr: Expression.t;
    }
  end

  module VariableDeclaration : sig
    type kind =
      | Immutable
      | Mutable

    type t = {
      loc: Loc.t;
      kind: kind;
      pattern: Pattern.t;
      init: Expression.t;
      annot: Type.t option;
    }
  end

  type t =
    | VariableDeclaration of VariableDeclaration.t
    | FunctionDeclaration of Function.t
    | Expression of (Loc.t * Expression.t)
    | Block of Block.t
    | If of If.t
    | While of While.t
    | Return of Return.t
    | Break of Break.t
    | Continue of Continue.t
    | Assignment of Assignment.t
    | Match of Match.t
end =
  Statement

and Expression : sig
  module Unit : sig
    type t = { loc: Loc.t }
  end

  module IntLiteral : sig
    type t = {
      loc: Loc.t;
      raw: string;
      base: Integers.base;
    }
  end

  module StringLiteral : sig
    type t = {
      loc: Loc.t;
      value: string;
    }
  end

  module BoolLiteral : sig
    type t = {
      loc: Loc.t;
      value: bool;
    }
  end

  module InterpolatedString : sig
    type part =
      | String of StringLiteral.t
      | Expression of Expression.t

    type t = {
      loc: Loc.t;
      parts: part list;
    }
  end

  module Record : sig
    module Field : sig
      type t = {
        loc: Loc.t;
        name: Identifier.t;
        value: Expression.t option;
      }
    end

    type t = {
      loc: Loc.t;
      name: Expression.t;
      fields: Field.t list;
    }
  end

  module Tuple : sig
    type t = {
      loc: Loc.t;
      elements: Expression.t list;
    }
  end

  module TypeCast : sig
    type t = {
      loc: Loc.t;
      expr: Expression.t;
      ty: Type.t;
    }
  end

  module UnaryOperation : sig
    type op =
      | Plus
      | Minus
      | Not

    and t = {
      loc: Loc.t;
      operand: Expression.t;
      op: op;
    }
  end

  module BinaryOperation : sig
    type op =
      | Add
      | Subtract
      | Multiply
      | Divide
      | Remainder
      | Is
      | Equal
      | NotEqual
      | LessThan
      | GreaterThan
      | LessThanOrEqual
      | GreaterThanOrEqual
      | BitwiseAnd
      | BitwiseOr
      | BitwiseXor
      | LeftShift
      | ArithmeticRightShift
      | LogicalRightShift

    and t = {
      loc: Loc.t;
      left: Expression.t;
      right: Expression.t;
      op: op;
    }
  end

  module LogicalAnd : sig
    type t = {
      loc: Loc.t;
      left: Expression.t;
      right: Expression.t;
    }
  end

  module LogicalOr : sig
    type t = {
      loc: Loc.t;
      left: Expression.t;
      right: Expression.t;
    }
  end

  module Ternary : sig
    type t = {
      loc: Loc.t;
      test: Expression.t;
      conseq: Expression.t;
      altern: Expression.t;
    }
  end

  module Call : sig
    type t = {
      loc: Loc.t;
      func: Expression.t;
      args: Expression.t list;
    }
  end

  module IndexedAccess : sig
    type t = {
      loc: Loc.t;
      target: Expression.t;
      index: Expression.t;
    }
  end

  module NamedAccess : sig
    type t = {
      loc: Loc.t;
      target: Expression.t;
      name: Identifier.t;
    }
  end

  type t =
    | Unit of Unit.t
    | IntLiteral of IntLiteral.t
    | StringLiteral of StringLiteral.t
    | BoolLiteral of BoolLiteral.t
    | Identifier of Identifier.t
    | ScopedIdentifier of ScopedIdentifier.t
    | InterpolatedString of InterpolatedString.t
    | Tuple of Tuple.t
    | Record of Record.t
    | TypeCast of TypeCast.t
    | UnaryOperation of UnaryOperation.t
    | BinaryOperation of BinaryOperation.t
    | LogicalAnd of LogicalAnd.t
    | LogicalOr of LogicalOr.t
    | Ternary of Ternary.t
    | Call of Call.t
    | IndexedAccess of IndexedAccess.t
    | NamedAccess of NamedAccess.t
    | Match of Match.t
    | Super of Loc.t
end =
  Expression

and Pattern : sig
  module Tuple : sig
    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t option;
      elements: Pattern.t list;
    }
  end

  module Record : sig
    module Field : sig
      type t = {
        loc: Loc.t;
        name: Identifier.t option;
        value: Pattern.t;
      }
    end

    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t;
      fields: Field.t list;
    }
  end

  module Literal : sig
    type t =
      | Unit of Expression.Unit.t
      | Bool of Expression.BoolLiteral.t
      | Int of Expression.IntLiteral.t
      | String of Expression.StringLiteral.t
  end

  type t =
    | Identifier of Identifier.t
    | Wildcard of Loc.t
    | Tuple of Tuple.t
    | Record of Record.t
    | Literal of Literal.t
end =
  Pattern

and Type : sig
  module Identifier : sig
    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t;
      type_params: Type.t list;
    }
  end

  module Tuple : sig
    type t = {
      loc: Loc.t;
      elements: Type.t list;
    }
  end

  module Function : sig
    type t = {
      loc: Loc.t;
      params: Type.t list;
      return: Type.t;
    }
  end

  type t =
    | Identifier of Identifier.t
    | Tuple of Tuple.t
    | Function of Function.t
end =
  Type

and TypeParameter : sig
  type t = {
    loc: Loc.t;
    name: Identifier.t;
    bounds: Type.Identifier.t list;
  }
end =
  TypeParameter

and Function : sig
  module Param : sig
    type t = {
      loc: Loc.t;
      name: Identifier.t;
      annot: Type.t;
    }
  end

  type body =
    | Block of Statement.Block.t
    | Expression of Expression.t
    | Signature

  and t = {
    loc: Loc.t;
    name: Identifier.t;
    params: Param.t list;
    body: body;
    return: Type.t option;
    type_params: TypeParameter.t list;
    builtin: bool;
    static: bool;
    override: bool;
  }
end =
  Function

and TypeDeclaration : sig
  module Record : sig
    module Field : sig
      type t = {
        loc: Loc.t;
        name: Identifier.t;
        ty: Type.t;
      }
    end

    type t = {
      loc: Loc.t;
      name: Identifier.t;
      fields: Field.t list;
    }
  end

  module Tuple : sig
    type t = {
      loc: Loc.t;
      name: Identifier.t;
      elements: Type.t list;
    }
  end

  type variant =
    | RecordVariant of Record.t
    | TupleVariant of Tuple.t
    | EnumVariant of Identifier.t

  type decl =
    | Alias of Type.t
    | Record of Record.t
    | Tuple of Tuple.t
    | Variant of variant list
    | Builtin

  type t = {
    loc: Loc.t;
    name: Identifier.t;
    type_params: TypeParameter.t list;
    decl: decl;
  }
end =
  TypeDeclaration

and TraitDeclaration : sig
  module ImplementedTrait : sig
    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t;
      type_args: Type.t list;
    }
  end

  type kind =
    | Methods
    | Trait

  type t = {
    loc: Loc.t;
    kind: kind;
    name: Identifier.t;
    type_params: TypeParameter.t list;
    implemented: ImplementedTrait.t list;
    methods: Function.t list;
  }
end =
  TraitDeclaration

and Identifier : sig
  type t = {
    loc: Loc.t;
    name: string;
  }
end =
  Identifier

and Match : sig
  module Case : sig
    type right =
      | Expression of Expression.t
      | Statement of Statement.t

    and t = {
      loc: Loc.t;
      pattern: Pattern.t;
      guard: Expression.t option;
      right: right;
    }
  end

  type t = {
    loc: Loc.t;
    args: Expression.t list;
    cases: Case.t list;
  }
end =
  Match

and ScopedIdentifier : sig
  type t = {
    loc: Loc.t;
    scopes: Identifier.t list;
    name: Identifier.t;
  }
end =
  ScopedIdentifier
