package std::map

import std::compare::Equal
import std::iterator::{Iterator, Iterable}
import std::ops::{Index, IndexAssign}
import std::option::{Option, Some}
import std::vec::VecIterator

implement map<k, v> extends Equal<map<k, v>> {
    def remove(key: k) {
        __builtin("map.remove", this, key)
    }

    def size(): int {
        return __builtin("map.size", this)
    }

    def keys(): set<k> {
        return __builtin("map.keys", this)
    }

    def values(): vec<v> {
        return __builtin("map.values", this)
    }

    def containsKey(key: k): bool {
        return __builtin("map.containsKey", this, key)
    }

    def containsValue(value: v): bool {
        return __builtin("map.containsValue", this, value)
    }

    def toVec(): vec<(k, v)> {
        return __builtin("map.toVec", this)
    }

    def equals(other: map<k, v>): bool {
        if (this.size() != other.size()) {
            return false
        }

        return this.all(fun kvPair -> {
            let (key, value) = kvPair
            other[key] == Some(value)
        })
    }

    def toString(): string {
        return __builtin("map.toString", this)
    }
}

implement map<k, v> extends Index<k, Option<v>>, IndexAssign<k, v> {
    def index(key: k): Option<v> {
        return __builtin("map.index", this, key)
    }

    def indexAssign(key: k, value: v): v {
        return __builtin("map.indexAssign", this, key, value)
    }
}

type MapIterator<k, v> = MapIterator(VecIterator<(k, v)>)

implement MapIterator<k, v> extends Iterator<(k, v)> {
    def next(): Option<(k, v)> = match this
        | MapIterator(vecIterator) -> vecIterator.next()
}

implement map<k, v> extends Iterable<(k, v)> {
    def iterator(): MapIterator<k, v> = MapIterator(this.toVec().iterator())
}
