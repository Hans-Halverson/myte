package myte.parser.ast

import myte.shared.*

data class AccessExpression(
    val left: Expression,
    val right: Expression,
    val dotContext: Context
): Expression(left.startContext)
