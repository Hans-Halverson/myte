package myte.parser.ast

import myte.shared.*

class LambdaExpression(
    val formalArgs: List<Identifier>,
    val body: Statement,
    startLocation: Location
) : Expression(startLocation)
