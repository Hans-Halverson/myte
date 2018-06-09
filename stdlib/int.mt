package std::int

implement int {
    def toFloat(): float {
        return __builtin("int.toFloat", this)
    }

    def toString(): string {
        return __builtin("int.toString", this)
    }
}
