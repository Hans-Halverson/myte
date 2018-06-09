package std::vec

import std::iterator::{Iterator, Iterable}
import std::ops::{Indexed, IndexedAssignment}
import std::option::{Option, Some, None}

implement vec<a> {
    def add(x: a) {
        __builtin("vec.add", this, x)
    }

    def remove(i: int) {
        __builtin("vec.remove", this, i)
    }

    def size(): int {
        return __builtin("vec.size", this)
    }

    def toString(): string {
        return __builtin("vec.toString", this)
    }
}

/*implement vec<a> extends Indexed<a>, IndexedAssignment<a> {
    def __index(i: int) {
        __builtin("vec.__index", this, i)
    }

    def __indexedAssignment(i: int, x: a) {
        return __builtin("vec.__indexedAssignment", this, i, a)
    }
}*/

type VecIterator<a> = VecIterator{vec: vec<a>, mut curr: int}

implement VecIterator<a> extends Iterator<a> {
    def next() {
        if (this.vec.size() == this.curr) {
            return None
        } else {
            const currElement = this.vec[this.curr]
            this.curr = this.curr + 1
            return Some(currElement)
        }
    }
}

implement vec<a> extends Iterable<a> {
    def iterator() = VecIterator{vec: this, curr: 0}
}
