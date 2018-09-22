package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an infix arithmetic operator applied to two numbers.
 *
 * @property left the left hand side of the arithmetic expression
 * @property right the right hand side of the arithmetic expression
 */
sealed class BinaryMathOperatorNode(
    var left: IRNode,
    var right: IRNode
) : IRNode(left.startLocation) {
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

class AddNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right)

class SubtractNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right)

class MultiplyNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right)

class DivideNode(numer: IRNode, denom: IRNode) : BinaryMathOperatorNode(numer, denom)

class ExponentNode(base: IRNode, exponent: IRNode) : BinaryMathOperatorNode(base, exponent)

class RemainderNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right)
