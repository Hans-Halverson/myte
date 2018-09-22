package myte.parser.ast

import myte.shared.*

class MatchStatement(
    val expr: Expression,
    val cases: List<Triple<Expression, Expression?, Statement>>,
    startLocation: Location
) : Expression(startLocation)
