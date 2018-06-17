package std::fmt

def print(x) {
    __builtin("__print", x.toString())
}

def println(x) {
    __builtin("__println", x.toString())
}
