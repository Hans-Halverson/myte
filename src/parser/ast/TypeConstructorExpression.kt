package myte.parser.ast

import myte.shared.*

class TypeConstructorExpression(
    val adtVariant: AlgebraicDataTypeVariant,
    val actualArgs: List<Expression>,
    val identContext: Context
) : Expression(identContext)
