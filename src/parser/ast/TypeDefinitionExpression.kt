package myte.parser.ast

import myte.shared.*

class TypeDefinitionExpression(
    val typeIdent: Identifier,
    val typeParamIdents: List<Identifier>,
    val variants: List<Pair<Identifier, List<TypeExpression>>>
) : TopLevelStatement()
