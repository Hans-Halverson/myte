package std::compare

type Comparison =
    | EqualComparison
    | LessComparison
    | GreaterComparison

trait Comparable<a> {
    sig compare(a): Comparison

    def lessThan(x: a): bool = match this.compare(x)
        | LessComparison -> true
        | _ -> false

    def greaterThan(x: a): bool = match this.compare(x)
        | GreaterComparison -> true
        | _ -> false

    def lessThanOrEquals(x: a): bool = match this.compare(x)
        | GreaterComparison -> false
        | _ -> true

    def greaterThanOrEquals(x: a): bool = match this.compare(x)
        | LessComparison -> false
        | _ -> true
}

trait Equal<a> {
    sig equals(a): bool
}
