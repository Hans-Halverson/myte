package myte.parser.ast

data class KeyedAccessExpression(val container: Expression, val key: Expression): Expression()
