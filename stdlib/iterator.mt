package std::iterator

import std::option::{Option, Some, None}

trait Iterator<a> {
    sig next(): Option<a>
}

trait Iterable<a> {
    sig iterator(): Iterator<a>

    def contains(x) {
        const iter = this.iterator()
        let curr = iter.next()
        while (true) {
            match curr
            | Some(y) when y == x -> return true
            | Some(_) -> curr = iter.next()
            | None -> break
        }

        return false
    }

    def forEach(f) {
        const iter = this.iterator()
        for (let curr = iter.next(), curr != None, curr = iter.next()) {
            curr.map(f)
        }
    }
}

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
