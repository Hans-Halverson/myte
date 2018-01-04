package myte.parser.ast

import myte.shared.*

class ForStatement(
    val init: Statement?,
    val cond: Expression?,
    val update: Statement?,
    val body: Statement,
    startContext: Context
) : Statement(startContext)
