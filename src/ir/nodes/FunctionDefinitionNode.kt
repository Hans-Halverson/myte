package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function definition.
 *
 * @property ident the identifier of the function that is being defined
 * @property formalArgs a list of identifiers for the formal arguments to the function
 * @property body the body of the function
 * @property returnTypeAnnotation an optional return type annotation for this function
 * @property signatures an optional list of type signatures that this function must implement
 * @property identLocation the location for the identifier of the function
 */
open class FunctionDefinitionNode(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: IRNode,
    val returnTypeAnnotation: Type?,
    val signatures: List<Type>?,
    val identLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        body.map(func)
    }
}

class MethodDefinitionNode(
    ident: Identifier,
    formalArgs: List<Identifier>,
    body: IRNode,
    returnTypeAnnotation: Type?,
    val thisIdent: Identifier,
    signatures: List<Type>?,
    identLocation: Location,
    startLocation: Location
) : FunctionDefinitionNode(
        ident,
        formalArgs,
        body,
        returnTypeAnnotation,
        signatures,
        identLocation,
        startLocation
)
