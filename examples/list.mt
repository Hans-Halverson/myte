type list<a> = {
    | Cons(a, list<a>)
    | Nil
}

def map(f, lst) {
    match lst {
        | Nil -> return Nil
        | Cons(h, tl) -> return Cons(f(hd), map(f, tl))
    }
}
