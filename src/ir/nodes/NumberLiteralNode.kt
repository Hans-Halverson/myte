package myte.ir.nodes

import java.math.BigDecimal
import java.math.BigInteger

import myte.shared.*

/**
 * A node that represents a number literal.
 */
sealed class NumberLiteralNode(startLocation: Location) : IRNode(startLocation)

/**
 * A node that represents an exact integral literal before its type has been determined.
 *
 * @property num the integral literal value
 */
class IntegralLiteralNode(
    val num: BigInteger,
    startLocation: Location
) : NumberLiteralNode(startLocation)

/**
 * A node that represents an exact decimal literal before its type has been determined.
 *
 * @property num the decimal literal value
 */
class DecimalLiteralNode(
    val num: BigDecimal,
    startLocation: Location
) : NumberLiteralNode(startLocation)

/**
 * A node that represents a byte literal.
 *
 * @property num the byte literal value
 */
class ByteLiteralNode(val num: Byte, startLocation: Location) : NumberLiteralNode(startLocation)

/**
 * A node that represents an int literal.
 *
 * @property num the int literal value
 */
class IntLiteralNode(val num: Int, startLocation: Location) : NumberLiteralNode(startLocation)

/**
 * A node that represents a float literal.
 *
 * @property num the float literal value
 */
class FloatLiteralNode(val num: Float, startLocation: Location) : NumberLiteralNode(startLocation)

/**
 * A node that represents a double literal.
 *
 * @property num the double literal value
 */
class DoubleLiteralNode(val num: Double, startLocation: Location) : NumberLiteralNode(startLocation)
