package myte.ir.nodes

import myte.shared.*

sealed class NumberLiteralNode(type: Type) : IRNode(type)

class IntLiteralNode(val num: Int) : NumberLiteralNode(IntType)

class FloatLiteralNode(val num: Double) : NumberLiteralNode(FloatType)
