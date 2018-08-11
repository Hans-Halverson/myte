package std::fmt

def print(x: a) {
    __builtin("__print", x.toString())
}

def println(x: a) {
    __builtin("__println", x.toString())
}
