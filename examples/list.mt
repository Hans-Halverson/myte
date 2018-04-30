package List

type List<a> =
| Cons(a, List<a>)
| Nil

implement List<a> {
    def length() = match this
        | Nil -> 0
        | Cons(_, tl) -> 1 + tl.length()

    def map(f) = match this
        | Nil -> Nil
        | Cons(hd, tl) -> Cons(f(hd), tl.map(f))

    def filter(p) = match this
        | Nil -> Nil
        | Cons(hd, tl) -> {
            if (p(hd)) {
                Cons(hd, tl.filter(p))
            } else {
                tl.filter(p)
            }
        }

    def fold(f, i) = match this
        | Nil -> i
        | Cons(hd, tl) -> f(tl.fold(f, i), hd)
}

def main() {
    let list = Cons(1, Cons(2, Cons(3, Nil)))
    println(intToString(list.fold(fun (a, b) -> a + b, 0)))
}