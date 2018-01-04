package myte.parser.ast

import myte.shared.*

class FunctionDefinitionStatement(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: Statement,
    val identContext: Context,
    startContext: Context
) : Statement(startContext)
