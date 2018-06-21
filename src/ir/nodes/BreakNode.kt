package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a break statement.
 *
 * @property breakLocation the location for the break keyword
 */
data class BreakNode(val breakLocation: Location) : IRNode(breakLocation)
