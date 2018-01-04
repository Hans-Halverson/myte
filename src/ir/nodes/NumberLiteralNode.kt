package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a number literal.
 */
sealed class NumberLiteralNode(startContext: Context) : IRNode(startContext)

/**
 * A node that represents an int literal.
 *
 * @property num the int literal value
 */
class IntLiteralNode(val num: Int, startContext: Context) : NumberLiteralNode(startContext)

/**
 * A node that represents a float literal.
 *
 * @property num the float literal value
 */
class FloatLiteralNode(val num: Double, startContext: Context) : NumberLiteralNode(startContext)
