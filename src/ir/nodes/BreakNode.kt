package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a break statement.
 * @property breakContext the context for the break keyword
 */
data class BreakNode(val breakContext: Context) : IRNode(breakContext)
