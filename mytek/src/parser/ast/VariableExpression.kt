package myte.parser.ast

import myte.shared.*

class VariableExpression(
    val ident: ResolvableSymbol,
    val identLocation: Location
): Expression(identLocation)
