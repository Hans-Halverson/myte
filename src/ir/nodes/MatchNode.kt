package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a match statement.
 *
 * @property expr the expression whose value will be matched on
 * @property cases a list of node pairs representing the match cases, where the first node is a
 *           pattern, and the second node is the statement to be executed if the pattern is matched
 */
data class MatchNode(
    val expr: IRNode,
    val cases: List<Pair<IRNode, IRNode>>
) : IRNode() {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr.map(func)
        cases.map { (pattern, statement) ->
            pattern.map(func)
            statement.map(func)
        }
    }
}
