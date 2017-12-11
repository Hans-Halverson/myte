package myte.ir.nodes

import myte.shared.*

data class NumericAssignmentNode(val ident: Identifier, val expr: IRNumericNode) : IRNumericNode()
