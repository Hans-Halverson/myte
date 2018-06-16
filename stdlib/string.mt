package std::string

import std::ops::Add

implement string extends Add<string> {
    def add(s) = __builtin("string.add", this, s)

    def size(): int = __builtin("string.size", this)

    def toString(): string = this
}
