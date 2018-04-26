package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable definition through pattern matching deconstruction.
 *
 * @property pattern the pattern containing variables to be defined
 * @property expr the expression whose value is assigned to the deconstructed pattern
 * @property typeAnnotation the type that the pattern was annotated with
 * @property patternLocation the location of the beginning of the pattern
 */
class PatternDefinitionNode(
    val pattern: IRNode,
    val expr: IRNode,
    val typeAnnotation: Type,
    val patternLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        pattern.map(func)
        expr.map(func)
    }

    override fun equals(other: Any?): Boolean {
        if (other !is PatternDefinitionNode) {
            return false
        }

        return (pattern == other.pattern && expr == other.expr &&
                typeAnnotation == other.typeAnnotation)
    }
}

