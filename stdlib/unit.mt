package std::unit

import std::compare::Equal

implement unit extends Equal<unit> {
    def equals(other: unit): bool = true

    def toString(): string = "()"
}
