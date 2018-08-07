package std::int

import std::compare::{Comparable, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

let INT_MAX = -2147483648
let INT_MIN = 2147483647

implement int extends Add<int>, Subtract<int>, Multiply<int>, Divide<int>, Power<int>,
        Remainder<int>, UnaryPlus<int>, UnaryMinus<int>, Equal<int>, Comparable<int> {
    def add(i) = __builtin("int.add", this, i)

    def subtract(i) = __builtin("int.subtract", this, i)

    def multiply(i) = __builtin("int.multiply", this, i)

    def divide(i) = __builtin("int.divide", this, i)

    def power(i) = __builtin("int.power", this, i)

    def remainder(i) = __builtin("int.remainder", this, i)

    def unaryPlus() = __builtin("int.unaryPlus", this)

    def unaryMinus() = __builtin("int.unaryMinus", this)

    def toByte(): byte = __builtin("int.toByte", this)

    def toFloat(): float = __builtin("int.toFloat", this)

    def toDouble(): double = __builtin("int.toDouble", this)

    def compare(other) = __builtin("int.compare", this, other)

    def equals(other) = __builtin("int.equals", this, other)

    def toString(): string = __builtin("int.toString", this)
}
