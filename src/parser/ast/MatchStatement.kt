package myte.parser.ast

import myte.shared.*

class MatchStatement(
    val expr: Expression,
    val cases: List<Pair<Expression, Statement>>,
    startContext: Context
) : Statement(startContext)
