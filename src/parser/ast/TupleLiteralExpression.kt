package myte.parser.ast

import myte.shared.*

class TupleLiteralExpression(
    val elements: List<Expression>,
    startContext: Context
): Expression(startContext)
