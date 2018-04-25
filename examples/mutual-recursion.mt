package MutualRecursion

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
    println(intToString(f(10)))
    println(intToString(g(10)))

    let t1 = Int(1)
    let t2 = Type2(Bool(true))
    let t3 = Type2(Type1(Int(2)))
    let t4 = Type2(Type1(Type2(Bool(false))))

    println(intToString(recursiveTypes(t1)))
    println(intToString(recursiveTypes(t2)))
    println(intToString(recursiveTypes(t3)))
    println(intToString(recursiveTypes(t4)))
}