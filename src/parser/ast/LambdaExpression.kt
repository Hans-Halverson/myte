package myte.parser.ast

import myte.shared.*

class LambdaExpression(
    val formalArgs: List<Pair<Identifier, TypeExpression?>>,
    val body: Statement,
    startLocation: Location
) : Expression(startLocation)
