package std::int

import std::compare::{Comparable, Comparison, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

let INT_MAX = -2147483648
let INT_MIN = 2147483647

implement int extends Add<int>, Subtract<int>, Multiply<int>, Divide<int>, Power<int>,
        Remainder<int>, UnaryPlus<int>, UnaryMinus<int>, Equal<int>, Comparable<int> {
    def add(i: int): int = __builtin("int.add", this, i)

    def subtract(i: int): int = __builtin("int.subtract", this, i)

    def multiply(i: int): int = __builtin("int.multiply", this, i)

    def divide(i: int): int = __builtin("int.divide", this, i)

    def power(i: int): int = __builtin("int.power", this, i)

    def remainder(i: int): int = __builtin("int.remainder", this, i)

    def unaryPlus(): int = __builtin("int.unaryPlus", this)

    def unaryMinus(): int = __builtin("int.unaryMinus", this)

    def toByte(): byte = __builtin("int.toByte", this)

    def toFloat(): float = __builtin("int.toFloat", this)

    def toDouble(): double = __builtin("int.toDouble", this)

    def compare(other: int): Comparison = __builtin("int.compare", this, other)

    def equals(other: int): bool = __builtin("int.equals", this, other)

    def toString(): string = __builtin("int.toString", this)
}
