package myte.parser.ast

data class WhileStatement(val cond: Expression, val body: Statement) : Statement()
