package myte.ir.nodes

import myte.shared.*

/**
 * A node in the internal representation of the program.
 *
 * @property evalTypeExpr the most specific type expression of the type that this node evaluates to
 *           that is known when the node is created
 */
abstract class IRNode(val evalTypeExpr: TypeExpression) {
    private var typeIfInferred: Type? = null

    /**
     * @property type the inferred type that this node evaluates to. This is initially null,
     *           so it is not safe to access until after type inference has finished.
     */
    var type: Type
        get() = typeIfInferred!!
        set(newType: Type) {
            typeIfInferred = newType
        }

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
