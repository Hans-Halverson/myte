package myte.parser.ast

import myte.shared.*

class FunctionSignatureDefinitionStatement(
    val ident: Identifier,
    val argTypes: List<TypeExpression>,
    val returnTypeAnnotation: TypeExpression,
    val identLocation: Location,
    startLocation: Location
) : Statement(startLocation)
