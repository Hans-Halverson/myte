package myte.parser.ast

import myte.shared.*

data class CallExpression(val func: Identifier, val actualArgs: List<Expression>) : Expression()
