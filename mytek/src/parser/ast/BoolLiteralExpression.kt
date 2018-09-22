package myte.parser.ast

import myte.shared.*

class BoolLiteralExpression(val bool: Boolean, startLocation: Location): Expression(startLocation)
