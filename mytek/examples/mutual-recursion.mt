package MutualRecursion

import std::fmt::println

def f(x) {
    if (x == 0) {
        return 0
    } else {
        return g(x - 1)
    }
}

def g(x) {
    if (x == 0) {
        return 0
    } else {
        return f(x - 1)
    }
}

def recursiveTypes(t: type1) {
    match t
    | Int(i) -> return i
    | Type2(Bool(_)) -> return 10
    | Type2(Type1(Type2(_))) -> return 100
    | Type2(Type1(Int(i))) -> return i
}

type type1 = Int(int) | Type2(type2)

type type2 = Bool(bool) | Type1(type1)

def main() {
    println(f(10).toString())
    println(g(10).toString())

    let t1 = Int(1)
    let t2 = Type2(Bool(true))
    let t3 = Type2(Type1(Int(2)))
    let t4 = Type2(Type1(Type2(Bool(false))))

    println(recursiveTypes(t1))
    println(recursiveTypes(t2))
    println(recursiveTypes(t3))
    println(recursiveTypes(t4))
}