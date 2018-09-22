package myte.parser.ast

import myte.shared.*

class StringLiteralExpression(val str: String, startLocation: Location): Expression(startLocation)
