package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a match statement.
 *
 * @property expr the expression whose value will be matched on
 * @property cases a list of node triples representing the match cases, where the first node is a
 *           pattern, the second node is an optional guard (or null if none exists), and the
 *           third is the statement to be executed if the pattern is matched.
 * @property isExpression whether this match should be treated like an expression
 */
class MatchNode(
    val expr: IRNode,
    val cases: List<Triple<IRNode, IRNode?, IRNode>>,
    val isExpression: Boolean,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr.map(func)
        cases.map { (pattern, guard, statement) ->
            pattern.map(func)
            guard?.map(func)
            statement.map(func)
        }
    }
}
