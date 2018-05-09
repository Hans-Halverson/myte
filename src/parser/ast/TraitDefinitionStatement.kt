package myte.parser.ast

import myte.shared.*

class TraitDefinitionStatement(
    val traitIdent: Identifier,
    val typeParamIdents: List<Identifier>,
    val thisIdent: Identifier,
    val methodSignatures: List<Pair<FunctionSignatureDefinitionStatement, Boolean>>,
    val concreteMethods: List<Pair<FunctionDefinitionStatement, Boolean>>
) : TopLevelStatement()
