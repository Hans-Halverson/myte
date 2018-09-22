package myte.parser.ast

import myte.shared.*

class SetLiteralExpression(
    val elements: List<Expression>,
    startLocation: Location
): Expression(startLocation)
