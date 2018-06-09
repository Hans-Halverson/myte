package std::float

implement float {
    def toInt(): int {
        return __builtin("float.toInt", this)
    }

    def toString(): string {
        return __builtin("float.toString", this)
    }
}
