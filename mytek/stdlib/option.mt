package std::option

import std::compare::Equal

type Option<a> =
| Some(a)
| None

implement Option<a> extends Equal<Option<a>> {
    def getOrElse(other: a): a = match this
        | Some(x) -> x
        | None -> other

    def map(f: a -> b): Option<b> = match this
        | Some(x) -> Some(f(x))
        | None -> None

    def flatMap(f: a -> Option<b>): Option<b> = match this
        | Some(x) -> f(x)
        | None -> None

    def filter(p: a -> bool): Option<a> = match this
        | Some(x) when p(x) -> Some(x)
        | Some(_) -> None
        | None -> None

    def equals(other: Option<a>): bool = match (this, other)
        | (None, None) -> true
        | (Some(x), Some(y)) when x == y -> true
        | _ -> false
}
