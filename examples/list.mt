type list<a> =
| Cons(a, list<a>)
| Nil

def length(l) {
    match l
    | Nil -> return 0
    | Cons(_, tl) -> return 1 + length(tl)
}

def map(f, l) {
    match l
    | Nil -> return Nil
    | Cons(hd, tl) -> return Cons(f(hd), map(f, tl))
}

def filter(p, l) {
    match l
    | Nil -> return Nil
    | Cons(hd, tl) -> {
        if p(hd) {
            return Cons(hd, filter(p, tl))
        } else {
            return filter(p, tl)
        }
    }
}

def fold(f, i, l) {
    match l
    | Nil -> return i
    | Cons(hd, tl) -> return f(fold(f, i, tl), hd)
}

def main(args) {
    let lst = Cons(1, Cons(2, Cons(3, Nil)))
    println(intToString(fold(fun (n1, n2) -> n1 + n2, 0, lst)))

    return 0
}