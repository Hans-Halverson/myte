package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a continue statement.
 * @property continueContext the context for the continue keyword
 */
data class ContinueNode(val continueContext: Context) : IRNode(continueContext)
