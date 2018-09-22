package Scope

import std::fmt::println

def f(): string {
    return a
}

const a = "hello"


def g(x: int): int {
    if (x == 0) {
        return 0
    } else {
        return h(x - 1)
    }
}

def h(x: int): int {
    if (x == 0) {
        return 0
    } else {
        return g(x - 1)
    }
}

def shadowing(x: a): int {
    let x = 100
    println(x.toString())
    def moreShadowing(x: b): b {
        return x
    }

    let x = 200
    return moreShadowing(x)
}

def patterns(x: list<a>): list<a> {
    match x
    | Nil -> return Nil
    | Cons(hd, tl) -> return Cons(hd, tl)
}

type list<a> = Nil | Cons(a, list<a>)


def main(): int {
    println(f())
    println(g(100))
    println(h(200))
    println(shadowing(30))

    patterns(Nil)

    return 0
}