package myte.parser.ast

import myte.shared.*

class TupleLiteralExpression(
    val elements: List<Expression>,
    startLocation: Location
): Expression(startLocation)
