package std::iterator

import std::option::{Option, Some, None}

trait Iterator<a> {
    sig next(): Option<a>
}

trait Iterable<a> {
    sig iterator(): Iterator<a>

    def contains(x) {
        forEach (item in this) {
            if (item == x) {
                return true
            }
        }

        return false
    }
}
