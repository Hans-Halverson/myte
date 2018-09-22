package std::double

import std::compare::{Comparable, Comparison, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

implement double extends Add<double>, Subtract<double>, Multiply<double>, Divide<double>,
        Power<double>, Remainder<double>, UnaryPlus<double>, UnaryMinus<double>, Equal<double>,
        Comparable<double> {
    def add(d: double): double = __builtin("double.add", this, d)

    def subtract(d: double): double = __builtin("double.subtract", this, d)

    def multiply(d: double): double = __builtin("double.multiply", this, d)

    def divide(d: double): double = __builtin("double.divide", this, d)

    def power(d: double): double = __builtin("double.power", this, d)

    def remainder(d: double): double = __builtin("double.remainder", this, d)

    def unaryPlus(): double = __builtin("double.unaryPlus", this)

    def unaryMinus(): double = __builtin("double.unaryMinus", this)

    def toByte(): byte = __builtin("double.toByte", this)

    def toInt(): int = __builtin("double.toInt", this)

    def toFloat(): float = __builtin("double.toFloat", this)

    def compare(other: double): Comparison = __builtin("double.compare", this, other)

    def equals(other: double): bool = __builtin("double.equals", this, other)

    def toString(): string = __builtin("double.toString", this)
}
