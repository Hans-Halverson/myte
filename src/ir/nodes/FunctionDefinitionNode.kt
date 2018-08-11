package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function definition.
 *
 * @property ident the identifier of the function that is being defined
 * @property formalArgs a list of identifiers for the formal arguments to the function
 * @property body the body of the function
 * @property signatures an optional list of type signatures that this function must implement
 * @property identLocation the location for the identifier of the function
 */
open class FunctionDefinitionNode(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    var body: IRNode,
    val signatures: List<Type>?,
    val identLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        body.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        body = func(body)
        body.map(func)
    }
}

class MethodDefinitionNode(
    ident: Identifier,
    formalArgs: List<Identifier>,
    body: IRNode,
    val thisIdent: Identifier,
    signatures: List<Type>?,
    identLocation: Location,
    startLocation: Location
) : FunctionDefinitionNode(
        ident,
        formalArgs,
        body,
        signatures,
        identLocation,
        startLocation
)
