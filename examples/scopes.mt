package scopes

def f() {
    return a
}

const a = "hello"


def g(x) {
    if (x == 0) {
        return 0
    } else {
        return h(x - 1)
    }
}

def h(x) {
    if (x == 0) {
        return 0
    } else {
        return g(x - 1)
    }
}

def shadowing(x) {
    let x = 100
    println(intToString(x))
    def moreShadowing(x) {
        return x
    }

    let x = 200
    return moreShadowing(x)
}

def patterns(x) {
    match x
    | Nil -> return Nil
    | Cons(hd, tl) -> return Cons(hd, tl)
}

type list<a> = Nil | Cons(a, list<a>)


def main(args) {
    println(f())
    println(intToString(g(100)))
    println(intToString(h(200)))
    println(intToString(shadowing(30)))

    patterns(Nil)

    return 0
}