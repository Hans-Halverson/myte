module rec Module : sig
  type toplevel =
    | VariableDeclaration of Statement.VariableDeclaration.t
    | FunctionDeclaration of Function.t
    | TypeDeclaration of TypeDeclaration.t

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

  module Return : sig
    type t = {
      loc: Loc.t;
      arg: Expression.t option;
    }
  end

  module Assignment : sig
    type t = {
      loc: Loc.t;
      pattern: Pattern.t;
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
    | Return of Return.t
    | Assignment of Assignment.t
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
      value: int;
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
      | LogicalNot

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
      | Equal
      | NotEqual
      | LessThan
      | GreaterThan
      | LessThanOrEqual
      | GreaterThanOrEqual

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

  module Call : sig
    type t = {
      loc: Loc.t;
      func: Expression.t;
      args: Expression.t list;
    }
  end

  module Access : sig
    type t = {
      loc: Loc.t;
      left: Expression.t;
      right: Identifier.t;
    }
  end

  type t =
    | Unit of Unit.t
    | IntLiteral of IntLiteral.t
    | StringLiteral of StringLiteral.t
    | BoolLiteral of BoolLiteral.t
    | Identifier of Identifier.t
    | ScopedIdentifier of ScopedIdentifier.t
    | TypeCast of TypeCast.t
    | UnaryOperation of UnaryOperation.t
    | BinaryOperation of BinaryOperation.t
    | LogicalAnd of LogicalAnd.t
    | LogicalOr of LogicalOr.t
    | Call of Call.t
    | Access of Access.t
end =
  Expression

and Pattern : sig
  type t = Identifier of Identifier.t
end =
  Pattern

and Type : sig
  module Primitive : sig
    type kind =
      | Unit
      | Int
      | String
      | Bool

    type t = {
      loc: Loc.t;
      kind: kind;
    }
  end

  module Custom : sig
    type t = {
      loc: Loc.t;
      name: ScopedIdentifier.t;
    }
  end

  module Function : sig
    type t = {
      loc: Loc.t;
      params: Type.t list;
      return: Type.t;
      type_params: Identifier.t list;
    }
  end

  type t =
    | Primitive of Primitive.t
    | Custom of Custom.t
    | Function of Function.t
end =
  Type

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

  and t = {
    loc: Loc.t;
    name: Identifier.t;
    params: Param.t list;
    body: body;
    return: Type.t option;
    type_params: Identifier.t list;
  }
end =
  Function

and TypeDeclaration : sig
  type t = {
    loc: Loc.t;
    name: Identifier.t;
    ty: Type.t;
  }
end =
  TypeDeclaration

and Identifier : sig
  type t = {
    loc: Loc.t;
    name: string;
  }
end =
  Identifier

and ScopedIdentifier : sig
  type t = {
    loc: Loc.t;
    scopes: Identifier.t list;
    name: Identifier.t;
  }
end =
  ScopedIdentifier
