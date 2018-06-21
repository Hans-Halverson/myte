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
    var init: IRNode?,
    var cond: IRNode?,
    var update: IRNode?,
    var body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        init?.forEach(func)
        cond?.forEach(func)
        update?.forEach(func)
        body.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        init = init?.let { func(it) }
        cond = cond?.let { func(it) }
        update = update?.let { func(it) }
        body = func(body)

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
    var lValue: IRNode,
    val typeAnnotation: Type?,
    var iterable: IRNode,
    var body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        lValue.forEach(func)
        iterable.forEach(func)
        body.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        lValue = func(lValue)
        iterable = func(iterable)
        body = func(body)

        lValue.map(func)
        iterable.map(func)
        body.map(func)
    }
}
