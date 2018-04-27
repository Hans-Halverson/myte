package myte.parser.ast

import myte.shared.*

class TypeDefinitionExpression(
    val typeIdent: Identifier,
    val typeParamIdents: List<Identifier>,
    val tupleVariants: List<Pair<Identifier, List<TypeExpression>>>,
    val recordVariants: List<Pair<Identifier, Map<String, TypeExpression>>>
) : TopLevelStatement()
