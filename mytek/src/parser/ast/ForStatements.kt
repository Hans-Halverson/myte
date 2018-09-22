package myte.parser.ast

import myte.shared.*

class ForStatement(
    val init: Statement?,
    val cond: Expression?,
    val update: Statement?,
    val body: Statement,
    startLocation: Location
) : Statement(startLocation)

class ForEachStatement(
    val lValue: Statement,
    val typeAnnotation: TypeExpression?,
    val iterable: Expression,
    val body: Statement,
    startLocation: Location
) : Statement(startLocation)
