package myte.parser.ast

import myte.shared.*

class VectorLiteralExpression(
    val elements: List<Expression>,
    startContext: Context
): Expression(startContext)
