package myte.parser.ast

import myte.shared.*

class AbstractFunctionDefinitionStatement(
    val ident: Identifier,
    val formalArgs: List<Pair<Identifier, TypeExpression>>,
    val returnTypeAnnotation: TypeExpression,
    val identLocation: Location,
    startLocation: Location
) : Statement(startLocation)
