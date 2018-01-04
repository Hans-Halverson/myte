package myte.parser.ast

import myte.shared.*

class LogicalNotExpression(val expr: Expression, startContext: Context) : Expression(startContext)
