module rec Program : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    statements: 'T Statement.t list;
  }
end = Program

and Statement : sig
  type 'T t =
      Expression of 'T Expression.t
end = Statement

and Expression : sig
  type 'T t =
      Identifier of 'T Identifier.t
end = Expression

and Identifier : sig
  type 'T t = {
    t: 'T;
    loc: Loc.t;
    name: string;
  }
end = Identifier
