package myte.ir.nodes

import myte.shared.*

sealed class NumberLiteralNode(evalTypeExpr: TypeExpression) : IRNode(evalTypeExpr)

class IntLiteralNode(val num: Int) : NumberLiteralNode(IntTypeExpression)

class FloatLiteralNode(val num: Double) : NumberLiteralNode(FloatTypeExpression)
