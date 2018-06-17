package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a for loop.
 *
 * @property init the (optional) initial statement that is evaluated before the for loop begins
 * @property cond the (optional) condition of the for loop
 * @property update the (optional) statement that is evaluated at the end of every loop
 * @property body the body of the for loop
 */
class ForNode(
    val init: IRNode?,
    val cond: IRNode?,
    val update: IRNode?,
    val body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        init?.map(func)
        cond?.map(func)
        update?.map(func)
        body.map(func)
    }
}

/**
 * A node that represents a for each loop.
 *
 * @property lValue the lValue that is initialized during the for loop
 * @property typeAnnotation the (optional) type annotation for the lValue
 * @property iterable the expression whose value is looped over
 * @property body the body of the for loop
 */
class ForEachNode(
    val lValue: IRNode,
    val typeAnnotation: Type?,
    val iterable: IRNode,
    val body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        lValue.map(func)
        iterable.map(func)
        body.map(func)
    }
}
