package std::double

import std::compare::{Comparable, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

implement double extends Add<double>, Subtract<double>, Multiply<double>, Divide<double>,
        Power<double>, Remainder<double>, UnaryPlus<double>, UnaryMinus<double>, Equal<double>,
        Comparable<double> {
    def add(f) = __builtin("double.add", this, f)

    def subtract(f) = __builtin("double.subtract", this, f)

    def multiply(f) = __builtin("double.multiply", this, f)

    def divide(f) = __builtin("double.divide", this, f)

    def power(f) = __builtin("double.power", this, f)

    def remainder(f) = __builtin("double.remainder", this, f)

    def unaryPlus() = __builtin("double.unaryPlus", this)

    def unaryMinus() = __builtin("double.unaryMinus", this)

    def toByte(): byte = __builtin("double.toByte", this)

    def toInt(): int = __builtin("double.toInt", this)

    def toFloat(): float = __builtin("double.toFloat", this)

    def compare(other) = __builtin("double.compare", this, other)

    def equals(other) = __builtin("double.equals", this, other)

    def toString(): string = __builtin("double.toString", this)
}
