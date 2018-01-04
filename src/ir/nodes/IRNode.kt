package myte.ir.nodes

import myte.shared.*

/**
 * A node in the internal representation of the program.
 *
 * @property type the type that this node evaluates to. Holds the most specific type known
 *           for this node when intially assigned, and later is reassigned with inferred node
 *           type after type inference has taken place.
 * @property startContext the context for the start of the content contained at this node
 */
abstract class IRNode(val startContext: Context) {
    lateinit var type: Type

    /*
     * Apply a function over the entire IR tree rooted at this node in preorder.
     * The function will be applied to each node before its child nodes.
     *
     * @param func the function that will be applied to every node in the IR tree
     */
    open fun <T> map(func: (IRNode) -> T) {
        func(this)
    }
}
