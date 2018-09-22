package std::list

import std::option::{Option, Some, None}
import std::iterator::{Iterator, Iterable}

type List<a> =
| Cons(a, List<a>)
| Nil

implement List<a> {
    def isEmpty(): bool = this == Nil

    def length(): int = match this
        | Nil -> 0
        | Cons(_, tl) -> 1 + tl.length()

    def add(x: a): List<a> = match this
        | Nil -> Cons(x, Nil)
        | Cons(hd, tl) -> Cons(hd, tl.add(x))

    def append(x: List<a>): List<a> = match this
        | Nil -> x
        | Cons(hd, tl) -> Cons(hd, tl.append(x))

    def map(f: a -> b): List<b> = match this
        | Nil -> Nil
        | Cons(hd, tl) -> Cons(f(hd), tl.map(f))

    def filter(p: a -> bool): List<a> = match this
        | Nil -> Nil
        | Cons(hd, tl) -> {
            if (p(hd)) {
                Cons(hd, tl.filter(p))
            } else {
                tl.filter(p)
            }
        }

    def fold(f: b -> a -> b, i: b): b = match this
        | Nil -> i
        | Cons(hd, tl) -> f(tl.fold(f, i), hd)

    def zip(other: List<b>): List<(a, b)> = match (this, other)
        | (Nil, _) -> Nil
        | (_, Nil) -> Nil
        | (Cons(hd1, tl1), Cons(hd2, tl2)) -> Cons((hd1, hd2), tl1.zip(tl2))
}

type ListIterator<a> = ListIterator{mut curr: List<a>}

implement ListIterator<a> extends Iterator<a> {
    def next(): Option<a> = match this.curr
        | Cons(hd, tl) -> {
            this.curr = tl
            Some(hd)
        }
        | Nil -> None
}

implement List<a> extends Iterable<a> {
    def iterator(): ListIterator<a> = ListIterator{curr: this}
}
