package myte.parser.ast

import java.math.BigDecimal
import java.math.BigInteger

import myte.shared.*

open class NumberLiteralExpression(startLocation: Location) : Expression(startLocation)

class IntegralLiteralExpression(
    val num: BigInteger,
    startLocation: Location
): NumberLiteralExpression(startLocation)

class DecimalLiteralExpression(
    val num: BigDecimal,
    startLocation: Location
) : NumberLiteralExpression(startLocation)
