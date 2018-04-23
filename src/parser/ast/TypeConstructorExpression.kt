package myte.parser.ast

import myte.shared.*

class TypeConstructorExpression(
    val ident: ResolvableSymbol,
    val actualArgs: List<Expression>,
    identLocation: Location
) : Expression(identLocation)
