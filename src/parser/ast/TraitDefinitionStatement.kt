package myte.parser.ast

import myte.shared.*

class TraitDefinitionStatement(
    val traitIdent: Identifier,
    val typeParamIdents: List<Identifier>,
    val thisIdent: Identifier,
    val abstractMethods: List<AbstractFunctionDefinitionStatement>,
    val concreteMethods: List<FunctionDefinitionStatement>
) : TopLevelStatement()
