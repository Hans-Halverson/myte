package myte.parser.ast

import myte.shared.*

data class KeyedAssignmentExpression(
    val lValue: KeyedAccessExpression,
    val rValue: Expression
) : Expression()
