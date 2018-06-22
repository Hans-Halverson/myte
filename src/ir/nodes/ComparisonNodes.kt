package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a comparison between two numbers.
 *
 * @property left the left hand side of the comparison
 * @property right the right hand side of the comparison
 */
sealed class ComparisonNode(var left: IRNode, var right: IRNode) : IRNode(left.startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        left.forEach(func)
        right.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        left = func(left)
        right = func(right)

        left.map(func)
        right.map(func)
    }
}

class LessThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right)

class LessThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right)

class GreaterThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right)

class GreaterThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right)
