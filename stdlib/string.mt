package std::string

implement string {
    def size(): int {
        return __builtin("string.size", this)
    }

    def toString(): string = this
}
