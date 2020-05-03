module rec Program : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    statements: 'T Statement.t list;
  }
end =
  Program

and Statement : sig
  module Block : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      statements: 'T Statement.t list;
    }
  end

  module If : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      test: 'T Expression.t;
      conseq: 'T Statement.t;
      altern: 'T Statement.t option;
    }
  end

  module Return : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      arg: 'T Expression.t;
    }
  end

  module VariableDeclaration : sig
    type kind =
      | Immutable
      | Mutable

    type 'T t = {
      t: 'T;
      loc: Loc.t;
      kind: kind;
      pattern: 'T Pattern.t;
      init: 'T Expression.t;
      annot: 'T Type.t option;
    }
  end

  type 'T t =
    | Expression of (Loc.t * 'T Expression.t)
    | Block of 'T Block.t
    | If of 'T If.t
    | Return of 'T Return.t
    | VariableDeclaration of 'T VariableDeclaration.t
    | FunctionDeclaration of 'T Function.t
end =
  Statement

and Expression : sig
  module Unit : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
    }
  end

  module IntLiteral : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      raw: string;
      value: int;
    }
  end

  module StringLiteral : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      value: string;
    }
  end

  module BoolLiteral : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      value: bool;
    }
  end

  module UnaryOperation : sig
    type op =
      | Plus
      | Minus
      | LogicalNot

    and 'T t = {
      t: 'T;
      loc: Loc.t;
      operand: 'T Expression.t;
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

    and 'T t = {
      t: 'T;
      loc: Loc.t;
      left: 'T Expression.t;
      right: 'T Expression.t;
      op: op;
    }
  end

  module LogicalAnd : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      left: 'T Expression.t;
      right: 'T Expression.t;
    }
  end

  module LogicalOr : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      left: 'T Expression.t;
      right: 'T Expression.t;
    }
  end

  module Call : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      func: 'T Expression.t;
      args: 'T Expression.t list;
    }
  end

  type 'T t =
    | Unit of 'T Unit.t
    | IntLiteral of 'T IntLiteral.t
    | StringLiteral of 'T StringLiteral.t
    | BoolLiteral of 'T BoolLiteral.t
    | Identifier of 'T Identifier.t
    | UnaryOperation of 'T UnaryOperation.t
    | BinaryOperation of 'T BinaryOperation.t
    | LogicalAnd of 'T LogicalAnd.t
    | LogicalOr of 'T LogicalOr.t
    | Call of 'T Call.t
end =
  Expression

and Pattern : sig
  type 'T t = Identifier of 'T Identifier.t
end =
  Pattern

and Type : sig
  module Primitive : sig
    type kind =
      | Unit
      | Int
      | String
      | Bool

    type 'T t = {
      t: 'T;
      loc: Loc.t;
      kind: kind;
    }
  end

  module Function : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      params: 'T Type.t list;
      return: 'T Type.t;
    }
  end

  type 'T t =
    | Primitive of 'T Primitive.t
    | Function of 'T Function.t
end =
  Type

and Function : sig
  module Param : sig
    type 'T t = {
      t: 'T;
      loc: Loc.t;
      name: 'T Identifier.t;
      annot: 'T Type.t;
    }
  end

  type 'T body =
    | Block of 'T Statement.Block.t
    | Expression of 'T Expression.t

  and 'T t = {
    t: 'T;
    loc: Loc.t;
    name: 'T Identifier.t;
    params: 'T Param.t list;
    body: 'T body;
    return: 'T Type.t option;
  }
end =
  Function

and Identifier : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    name: string;
  }
end =
  Identifier
