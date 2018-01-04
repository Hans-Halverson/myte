package myte.parser.ast

import myte.shared.*

data class VariableAssignmentExpression(
    val lValue: Identifier,
    val rValue: Expression,
    val identContext: Context
) : Expression(identContext)
