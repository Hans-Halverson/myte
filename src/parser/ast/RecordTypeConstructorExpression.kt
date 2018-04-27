package myte.parser.ast

import myte.shared.*

class RecordTypeConstructorExpression(
    val typeConstructor: Expression,
    val fields: Map<String, Expression>,
    val isPattern: Boolean,
    val callLocation: Location
) : Expression(typeConstructor.startLocation)
