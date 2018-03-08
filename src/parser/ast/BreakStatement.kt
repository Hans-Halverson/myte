package myte.parser.ast

import myte.shared.*

data class BreakStatement(val breakLocation: Location) : Statement(breakLocation)
