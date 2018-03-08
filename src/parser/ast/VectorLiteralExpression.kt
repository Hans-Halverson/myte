package myte.parser.ast

import myte.shared.*

class VectorLiteralExpression(
    val elements: List<Expression>,
    startLocation: Location
): Expression(startLocation)
