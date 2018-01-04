package myte.parser.ast

import myte.shared.*

data class BreakStatement(val breakContext: Context) : Statement(breakContext)
