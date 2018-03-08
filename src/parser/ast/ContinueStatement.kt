package myte.parser.ast

import myte.shared.*

data class ContinueStatement(val continueLocation: Location) : Statement(continueLocation)
