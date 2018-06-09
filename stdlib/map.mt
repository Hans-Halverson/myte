package std::map

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
