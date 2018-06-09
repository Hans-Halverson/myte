package std::set

implement set<a> {
    def add(x: a) {
        __builtin("set.add", this, x)
    }

    def remove(x: a) {
        __builtin("set.remove", this, x)
    }

    def contains(x: a): bool {
        return __builtin("set.contains", this, x)
    }

    def size(): int {
        return __builtin("set.size", this)
    }

    def toString(): string {
        return __builtin("set.toString", this)
    }
}
