package std::list

import std::option::{Some, None}
import std::iterator::{Iterator, Iterable}

type List<a> =
| Cons(a, List<a>)
| Nil

implement List<a> {
    def isEmpty() = this == Nil

    def length() = match this
        | Nil -> 0
        | Cons(_, tl) -> 1 + tl.length()

    def add(x) = match this
        | Nil -> Cons(x, Nil)
        | Cons(hd, tl) -> Cons(hd, tl.add(x))

    def append(x) = match this
        | Nil -> x
        | Cons(hd, tl) -> Cons(hd, tl.append(x))

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

    def zip(other: List<b>) = match (this, other)
        | (Nil, _) -> Nil
        | (_, Nil) -> Nil
        | (Cons(hd1, tl1), Cons(hd2, tl2)) -> Cons((hd1, hd2), tl1.zip(tl2))
}

type ListIterator<a> = ListIterator{mut curr: List<a>}

implement ListIterator<a> extends Iterator<a> {
    def next() = match this.curr
        | Cons(hd, tl) -> {
            this.curr = tl
            Some(hd)
        }
        | Nil -> None
}

implement List<a> extends Iterable<a> {
    def iterator() = ListIterator{curr: this}
}
