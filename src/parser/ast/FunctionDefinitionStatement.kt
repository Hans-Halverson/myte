package myte.parser.ast

import myte.shared.*

class FunctionDefinitionStatement(
    val ident: Identifier,
    val formalArgs: List<Pair<Identifier, TypeExpression>>,
    val returnTypeAnnotation: TypeExpression?,
    val body: Statement,
    val identLocation: Location,
    startLocation: Location
) : Statement(startLocation)
