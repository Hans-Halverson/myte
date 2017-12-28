package myte.parser.ast

import myte.shared.*

data class TypeConstructorExpression(
    val adtVariant: AlgebraicDataTypeVariant,
    val actualArgs: List<Expression>
) : Expression()
