package myte.parser.ast

import myte.shared.*

class StringLiteralExpression(val str: String, startContext: Context): Expression(startContext)
