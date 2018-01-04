package myte.parser.ast

import myte.shared.*

data class ContinueStatement(val continueContext: Context) : Statement(continueContext)
