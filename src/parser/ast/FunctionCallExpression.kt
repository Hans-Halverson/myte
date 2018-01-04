package myte.parser.ast

import myte.shared.*

data class FunctionCallExpression(
    val func: Identifier,
    val actualArgs: List<Expression>,
    val identContext: Context
) : Expression(identContext)
