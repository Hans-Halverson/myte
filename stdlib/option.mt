package std::option

type Option<a> =
    | Some(a)
    | None

implement Option<a> {
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
}
