package std::string

import std::compare::{Comparable, Comparison, Equal}
import std::ops::Add

implement string extends Add<string>, Equal<string>, Comparable<string> {
    def add(s: string): string = __builtin("string.add", this, s)

    def size(): int = __builtin("string.size", this)

    def compare(other: string): Comparison = __builtin("string.compare", this, other)

    def equals(other: string): bool = __builtin("string.equals", this, other)

    def toString(): string = this
}
