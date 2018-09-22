package std::float

import std::compare::{Comparable, Comparison, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

implement float extends Add<float>, Subtract<float>, Multiply<float>, Divide<float>, Power<float>,
        Remainder<float>, UnaryPlus<float>, UnaryMinus<float>, Equal<float>, Comparable<float> {
    def add(f: float): float = __builtin("float.add", this, f)

    def subtract(f: float): float = __builtin("float.subtract", this, f)

    def multiply(f: float): float = __builtin("float.multiply", this, f)

    def divide(f: float): float = __builtin("float.divide", this, f)

    def power(f: float): float = __builtin("float.power", this, f)

    def remainder(f: float): float = __builtin("float.remainder", this, f)

    def unaryPlus(): float = __builtin("float.unaryPlus", this)

    def unaryMinus(): float = __builtin("float.unaryMinus", this)

    def toByte(): byte = __builtin("float.toByte", this)

    def toInt(): int = __builtin("float.toInt", this)

    def toDouble(): double = __builtin("float.toDouble", this)

    def compare(other: float): Comparison = __builtin("float.compare", this, other)

    def equals(other: float): bool = __builtin("float.equals", this, other)

    def toString(): string = __builtin("float.toString", this)
}
