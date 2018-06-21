package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a continue statement.
 *
 * @property continueLocation the location for the continue keyword
 */
data class ContinueNode(val continueLocation: Location) : IRNode(continueLocation)
