package myte.parser.ast

import myte.shared.*

class TypeImplementationStatement(
    val typeIdent: ResolvableSymbol?,
    val builtinType: String?,
    val typeParamIdents: List<Identifier>,
    val thisIdent: Identifier,
    val methods: List<Pair<FunctionDefinitionStatement, Boolean>>,
    val extendedTraits: List<Pair<ResolvableSymbol, List<TypeExpression>>>,
    val typeLocation: Location
) : TopLevelStatement()
