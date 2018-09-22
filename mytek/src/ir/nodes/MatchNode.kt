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
    var expr: IRNode,
    var cases: List<Triple<IRNode, IRNode?, IRNode>>,
    val isExpression: Boolean,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        expr.forEach(func)
        cases.forEach { (pattern, guard, statement) ->
            pattern.forEach(func)
            guard?.forEach(func)
            statement.forEach(func)
        }
    }

    override fun map(func: (IRNode) -> IRNode) {
        expr = func(expr)
        cases = cases.map { (pattern, guard, statement) ->
            Triple(func(pattern), guard?.let { func(it) }, func(statement))
        }

        expr.map(func)
        cases.forEach { (pattern, guard, statement) ->
            pattern.map(func)
            guard?.map(func)
            statement.map(func)
        }
    }
}
