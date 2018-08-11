package std::byte

import std::compare::{Comparable, Comparison, Equal}
import std::ops::{Add, Subtract, Multiply, Divide, Power, Remainder, UnaryPlus, UnaryMinus}

let BYTE_MAX: byte = -128.toByte()
let BYTE_MIN: byte = 127.toByte()

implement byte extends Add<byte>, Subtract<byte>, Multiply<byte>, Divide<byte>, Power<byte>,
        Remainder<byte>, UnaryPlus<byte>, UnaryMinus<byte>, Equal<byte>, Comparable<byte> {
    def add(i: byte): byte = __builtin("byte.add", this, i)

    def subtract(i: byte): byte = __builtin("byte.subtract", this, i)

    def multiply(i: byte): byte = __builtin("byte.multiply", this, i)

    def divide(i: byte): byte = __builtin("byte.divide", this, i)

    def power(i: byte): byte = __builtin("byte.power", this, i)

    def remainder(i: byte): byte = __builtin("byte.remainder", this, i)

    def unaryPlus(): byte = __builtin("byte.unaryPlus", this)

    def unaryMinus(): byte = __builtin("byte.unaryMinus", this)

    def toInt(): int = __builtin("byte.toInt", this)

    def toFloat(): float = __builtin("byte.toFloat", this)

    def toDouble(): double = __builtin("byte.toDouble", this)

    def compare(other: byte): Comparison = __builtin("byte.compare", this, other)

    def equals(other: byte): bool = __builtin("byte.equals", this, other)

    def toString(): string = __builtin("byte.toString", this)
}
