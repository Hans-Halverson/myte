package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a bool literal.
 *
 * @property bool the bool literal value
 */
data class BoolLiteralNode(val bool: Boolean) : IRNode(BoolType)
