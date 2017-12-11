package myte.parser.ast

data class BlockStatement(val stmts: List<Statement>) : Statement()
