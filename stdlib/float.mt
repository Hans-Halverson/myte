package std::float

import std::compare::{Comparable, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

implement float extends Add<float>, Subtract<float>, Multiply<float>, Divide<float>, Power<float>,
        Remainder<float>, UnaryPlus<float>, UnaryMinus<float>, Equal<float>, Comparable<float> {
    def add(f) = __builtin("float.add", this, f)

    def subtract(f) = __builtin("float.subtract", this, f)

    def multiply(f) = __builtin("float.multiply", this, f)

    def divide(f) = __builtin("float.divide", this, f)

    def power(f) = __builtin("float.power", this, f)

    def remainder(f) = __builtin("float.remainder", this, f)

    def unaryPlus() = __builtin("float.unaryPlus", this)

    def unaryMinus() = __builtin("float.unaryMinus", this)

    def toByte(): byte = __builtin("float.toByte", this)

    def toInt(): int = __builtin("float.toInt", this)

    def toDouble(): double = __builtin("float.toDouble", this)

    def compare(other) = __builtin("float.compare", this, other)

    def equals(other) = __builtin("float.equals", this, other)

    def toString(): string = __builtin("float.toString", this)
}
