package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a string literal.
 *
 * @property str the string literal value
 */
data class StringLiteralNode(val str: String) : IRNode(StringTypeExpression)
