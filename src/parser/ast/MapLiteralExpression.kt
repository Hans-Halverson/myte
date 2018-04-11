package myte.parser.ast

import myte.shared.*

class MapLiteralExpression(
    val keys: List<Expression>,
    val values: List<Expression>,
    startLocation: Location
): Expression(startLocation)
