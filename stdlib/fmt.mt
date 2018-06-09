package std::fmt

def print(str: string) {
    __builtin("__print", str)
}

def println(str: string) {
    __builtin("__println", str)
}
