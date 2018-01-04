package myte.parser.ast

import myte.shared.*

class BoolLiteralExpression(val bool: Boolean, startContext: Context): Expression(startContext)
