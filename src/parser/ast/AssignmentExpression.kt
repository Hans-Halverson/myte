package myte.parser.ast

import myte.shared.*

data class AssignmentExpression(val ident: Identifier, val expr: Expression) : Expression()
