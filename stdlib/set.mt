package std::set

import std::compare::Equal
import std::iterator::{Iterator, Iterable}
import std::option::Option
import std::vec::VecIterator

implement set<a> extends Equal<set<a>> {
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

    def toVec(): vec<a> {
        return __builtin("set.toVec", this)
    }

    def equals(other: set<a>): bool {
        if (this.size() != other.size()) {
            return false
        }

        return this.all(fun x -> other.contains(x))
    }

    def toString(): string {
        return __builtin("set.toString", this)
    }
}

type SetIterator<a> = SetIterator(VecIterator<a>)

implement SetIterator<a> extends Iterator<a> {
    def next(): Option<a> = match this
        | SetIterator(vecIterator) -> vecIterator.next()
}

implement set<a> extends Iterable<a> {
    def iterator(): SetIterator<a> = SetIterator(this.toVec().iterator())
}
