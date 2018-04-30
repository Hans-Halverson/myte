package myte.parser.ast

import myte.shared.*

class TypeImplementationStatement(
    val typeIdent: ResolvableSymbol,
    val typeParamIdents: List<Identifier>,
    val thisIdent: Identifier,
    val methods: List<FunctionDefinitionStatement>
) : TopLevelStatement()
