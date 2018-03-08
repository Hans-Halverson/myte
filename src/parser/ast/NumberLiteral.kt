package myte.parser.ast

import myte.shared.*

sealed class NumberLiteral(startLocation: Location) : Expression(startLocation)

class IntLiteral(val num: Int, startLocation: Location): NumberLiteral(startLocation)

class FloatLiteral(val num: Double, startLocation: Location) : NumberLiteral(startLocation)
