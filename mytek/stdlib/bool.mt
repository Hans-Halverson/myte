package std::bool

import std::compare::{Comparable, Comparison, Equal, EqualComparison, GreaterComparison,
    LessComparison}

implement bool extends Equal<bool>, Comparable<bool> {
    def compare(other: bool): Comparison = match (this, other)
        | (true, true) -> EqualComparison
        | (true, false) -> GreaterComparison
        | (false, true) -> LessComparison
        | (false, false) -> EqualComparison

    def equals(other: bool): bool = match (this, other)
        | (true, true) -> true
        | (false, false) -> true
        | _ -> false

    def toString(): string = match this
        | true -> "true"
        | false -> "false"
}
