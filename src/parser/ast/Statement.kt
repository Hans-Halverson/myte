package myte.parser.ast

import myte.shared.*

abstract class TopLevelStatement()

abstract class Statement(val startLocation: Location) : TopLevelStatement()
