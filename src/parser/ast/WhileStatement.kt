package myte.parser.ast

data class WhileStatement(val cond: Expression, val stmt: Statement) : Statement()
