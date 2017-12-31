package myte.parser.ast

data class MatchStatement(
    val expr: Expression,
    val cases: List<Pair<Expression, Statement>>
) : Statement()
