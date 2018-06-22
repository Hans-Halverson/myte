package std::string

import std::compare::{Comparable, Equal}
import std::ops::Add

implement string extends Add<string>, Equal<string>, Comparable<string> {
    def add(s) = __builtin("string.add", this, s)

    def size(): int = __builtin("string.size", this)

    def compare(other) = __builtin("string.compare", this, other)

    def equals(other) = __builtin("string.equals", this, other)

    def toString(): string = this
}
