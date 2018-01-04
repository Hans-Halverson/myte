package myte.parser.ast

import myte.shared.*

sealed class NumberLiteral(startContext: Context) : Expression(startContext)

class IntLiteral(val num: Int, startContext: Context): NumberLiteral(startContext)

class FloatLiteral(val num: Double, startContext: Context) : NumberLiteral(startContext)
