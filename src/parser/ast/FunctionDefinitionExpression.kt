package myte.parser.ast

import myte.shared.*

class FunctionDefinitionExpression(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: Expression,
    val identContext: Context,
    startContext: Context
) : Statement(startContext)
