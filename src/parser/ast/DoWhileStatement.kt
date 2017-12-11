package myte.parser.ast

data class DoWhileStatement(val cond: Expression, val stmt: Statement) : Statement()
