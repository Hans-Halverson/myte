package List

type list<a> =
| Cons(a, list<a>)
| Nil

def length(l) = match l
    | Nil -> 0
    | Cons(_, tl) -> 1 + length(tl)

def map(f, l) = match l
    | Nil -> Nil
    | Cons(hd, tl) -> Cons(f(hd), map(f, tl))

def filter(p, l) = match l
    | Nil -> Nil
    | Cons(hd, tl) -> {
        if (p(hd)) {
            Cons(hd, filter(p, tl))
        } else {
            filter(p, tl)
        }
    }

def fold(f, i, l) = match l
    | Nil -> i
    | Cons(hd, tl) -> f(fold(f, i, tl), hd)

def main() {
    let list = Cons(1, Cons(2, Cons(3, Nil)))
    println(intToString(fold(fun (a, b) -> a + b, 0, list)))
}