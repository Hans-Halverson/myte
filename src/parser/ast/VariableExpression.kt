package myte.parser.ast

import myte.shared.*

class VariableExpression(val ident: Identifier, val identContext: Context): Expression(identContext)
