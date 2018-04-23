package myte.parser.ast

import myte.shared.*

class VariableDefinitionStatement(
    val ident: Identifier,
    val expr: Expression,
    val typeAnnotation: TypeExpression?,
    val identLocation: Location,
    startLocation: Location
) : Statement(startLocation)
