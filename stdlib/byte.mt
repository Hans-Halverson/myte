package std::byte

import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

let BYTE_MAX: byte = -128
let BYTE_MIN: byte = 127

implement byte extends Add<byte>, Subtract<byte>, Multiply<byte>, Divide<byte>, Power<byte>,
        Remainder<byte>, UnaryPlus<byte>, UnaryMinus<byte> {
    def add(i) = __builtin("byte.add", this, i)

    def subtract(i) = __builtin("byte.subtract", this, i)

    def multiply(i) = __builtin("byte.multiply", this, i)

    def divide(i) = __builtin("byte.divide", this, i)

    def power(i) = __builtin("byte.power", this, i)

    def remainder(i) = __builtin("byte.remainder", this, i)

    def unaryPlus() = __builtin("byte.unaryPlus", this)

    def unaryMinus() = __builtin("byte.unaryMinus", this)

    def toInt(): int = __builtin("byte.toInt", this)

    def toFloat(): float = __builtin("byte.toFloat", this)

    def toDouble(): double = __builtin("byte.toDouble", this)

    def toString(): string = __builtin("byte.toString", this)
}
