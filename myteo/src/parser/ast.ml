module rec Program : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    statements: 'T Statement.t list;
  }
end = Program

and Statement : sig
  type 'T t =
      Expression of (Loc.t * 'T Expression.t)
end = Statement

and Expression : sig
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

  module BinaryOperation : sig
    type op =
      | Add
      | Subtract
      | Multiply
      | Divide

    and 'T t = {
      t: 'T;
      loc: Loc.t;
      left: 'T Expression.t;
      right: 'T Expression.t;
      op: op;
    }
  end

  type 'T t =
    | IntLiteral of 'T IntLiteral.t
    | StringLiteral of 'T StringLiteral.t
    | BoolLiteral of 'T BoolLiteral.t
    | Identifier of 'T Identifier.t
    | BinaryOperation of 'T BinaryOperation.t
end = Expression

and Identifier : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    name: string;
  }
end = Identifier
