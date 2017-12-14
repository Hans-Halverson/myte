package myte.parser.ast

data class DoWhileStatement(val cond: Expression, val body: Statement) : Statement()
