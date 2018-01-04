package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a bool literal.
 *
 * @property bool the bool literal value
 */
class BoolLiteralNode(val bool: Boolean, startContext: Context) : IRNode(startContext)
