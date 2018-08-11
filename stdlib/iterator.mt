package std::iterator

import std::option::{Option, Some, None}

trait Iterator<a> {
    sig next(): Option<a>
}

trait Iterable<a> {
    sig iterator(): Iterator<a>

    def contains(x: a): bool {
        forEach (item in this) {
            if (item == x) {
                return true
            }
        }

        return false
    }

    def all(p: a -> bool): bool {
        forEach (item in this) {
            if (!p(item)) {
                return false
            }
        }

        return true
    }

    def any(p: a -> bool): bool {
        forEach (item in this) {
            if (p(item)) {
                return true
            }
        }

        return false
    }
}
