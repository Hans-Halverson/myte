package std::map

import std::ops::{Index, IndexAssign}
import std::option::Option

implement map<k, v> {
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
