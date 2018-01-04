package myte.parser.ast

import myte.shared.*

data class ReturnStatement(
    val expr: Expression?,
    val returnContext: Context
) : Statement(returnContext)
