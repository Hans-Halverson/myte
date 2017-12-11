package myte.parser.ast

data class IfStatement(val cond: Expression, val conseq: Statement, val altern: Statement?) : Statement()
