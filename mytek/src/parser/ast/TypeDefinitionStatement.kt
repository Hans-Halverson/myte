package myte.parser.ast

import myte.shared.*

class TypeDefinitionStatement(
    val typeIdent: Identifier,
    val typeParamIdents: List<Identifier>,
    val tupleVariants: List<Pair<Identifier, List<TypeExpression>>>,
    val recordVariants: List<Pair<Identifier, Map<String, Pair<TypeExpression, Boolean>>>>
) : TopLevelStatement()
