package std::option

import std::compare::Equal

type Option<a> =
| Some(a)
| None

implement Option<a> extends Equal<Option<a>> {
    def getOrElse(other: a) = match this
        | Some(x) -> x
        | None -> other

    def map(f) = match this
        | Some(x) -> Some(f(x))
        | None -> None

    def flatMap(f) = match this
        | Some(x) -> f(x)
        | None -> None

    def filter(p) = match this
        | Some(x) when p(x) -> Some(x)
        | Some(_) -> None
        | None -> None

    def equals(other) = match (this, other)
        | (None, None) -> true
        | (Some(x), Some(y)) when x == y -> true
        | _ -> false
}
