package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a number literal.
 */
sealed class NumberLiteralNode(type: Type) : IRNode(type)

/**
 * A node that represents an int literal.
 *
 * @property num the int literal value
 */
class IntLiteralNode(val num: Int) : NumberLiteralNode(IntType)

/**
 * A node that represents a float literal.
 *
 * @property num the float literal value
 */
class FloatLiteralNode(val num: Double) : NumberLiteralNode(FloatType)
