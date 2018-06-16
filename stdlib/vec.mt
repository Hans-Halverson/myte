package std::vec

import std::iterator::{Iterator, Iterable}
import std::ops::{Index, IndexAssign}
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

implement vec<a> extends Index<int, a>, IndexAssign<int, a> {
    def index(i: int): a {
        return __builtin("vec.index", this, i)
    }

    def indexAssign(i: int, v: a): a {
        return __builtin("vec.indexAssign", this, i, v)
    }
}

type VecIterator<a> = VecIterator { vec: vec<a>, mut curr: int }

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
    def iterator() = VecIterator { vec: this, curr: 0 }
}
