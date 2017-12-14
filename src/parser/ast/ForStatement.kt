package myte.parser.ast

data class ForStatement(val init: Statement?, val cond: Expression?, val update: Statement?, val body: Statement) : Statement()
