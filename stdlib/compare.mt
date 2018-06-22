package std::compare

type Comparison =
    | EqualComparison
    | LessComparison
    | GreaterComparison

trait Comparable<a> {
    sig compare(a): Comparison

    def lessThan(x) = match this.compare(x)
        | LessComparison -> true
        | _ -> false

    def greaterThan(x) = match this.compare(x)
        | GreaterComparison -> true
        | _ -> false

    def lessThanOrEquals(x) = match this.compare(x)
        | GreaterComparison -> false
        | _ -> true

    def greaterThanOrEquals(x) = match this.compare(x)
        | LessComparison -> false
        | _ -> true
}

trait Equal<a> {
    sig equals(a): bool
}
