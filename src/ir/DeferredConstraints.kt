package myte.ir

import myte.ir.nodes.*
import myte.shared.*

sealed class DeferredConstraint {
    /**
     * Attempt to apply this constraint.
     *
     * @param trigger the new type to apply this constraint to
     * @return whether the constraint was successfully applied, or whether it should be
     *         deferred even further.
     */
    abstract fun apply(trigger: Type): Boolean
}

/**
 * A contstraint that waits until a container in a keyed access has been resolved, and then
 * applies the correct constraints if it is a vector, map, or tuple.
 */
class KeyedAccessConstraint(
    val node: KeyedAccessNode,
    val boundVars: MutableSet<TypeVariable>,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        // The trigger type is the type of the node's container
        val containerType = trigger

        if (containerType is VectorType) {
            // Constrain eval type of node to be the element type of vector
            if (!typeChecker.unify(containerType.elementType, node.type)) {
                val types = typeChecker.typesToString(containerType.elementType, node.type,
                        boundVars)
                throw IRConversionException("Expected type of vector elements to be ${types[0]}, " +
                        "but found ${types[1]}", node.accessLocation)
            }

            // Constrain key to be an integer
            if (!typeChecker.unify(node.key.type, IntType)) {
                val typeStr = typeChecker.typeToString(node.key.type, boundVars)
                throw IRConversionException("Vector expected to have keys of type int, " +
                        "found ${typeStr}", node.key.startLocation)
            }

            return true
        } else if (containerType is MapType) {
            // Constrain eval type of node to be the value type of map
            if (!typeChecker.unify(containerType.valType, node.type)) {
                val types = typeChecker.typesToString(containerType.valType, node.type, boundVars)
                throw IRConversionException("Map expected to have values of type ${types[0]}, " +
                        "but found ${types[1]}", node.startLocation)
            }

            // Constrain key type of key node to be key type of map
            if (!typeChecker.unify(containerType.keyType, node.key.type)) {
                val types = typeChecker.typesToString(containerType.keyType, node.key.type,
                        boundVars)
                throw IRConversionException("Map expected to have keys of type ${types[0]}, " +
                        "but found ${types[1]}", node.startLocation) 
            }

            return true
        } else if (containerType is TupleType) {
            // Can only index using int literals for tuples, since we must know the exact index
            if (node.key !is IntLiteralNode) {
                throw IRConversionException("Can only use int literal keys for tuple",
                        node.key.startLocation)
            }

            typeChecker.unify(node.key.type, IntType)

            // Check that int literal is within bounds
            val num = node.key.num
            val tupleSize = containerType.elementTypes.size
            if (num < 0 || num >= tupleSize) {
                throw IRConversionException("Tuple of size ${tupleSize} cannot be indexed by " +
                        "key ${num}", node.key.startLocation)
            }

            // Constrain eval type of node to be the type of the corresponding tuple element
            val elementType = containerType.elementTypes[num]
            if (!typeChecker.unify(elementType, node.type)) {
                val types = typeChecker.typesToString(elementType, node.type, boundVars)
                throw IRConversionException("Tuple expected to have element of type ${types[0]} " +
                        "at index ${num}, but found ${types[1]}", node.startLocation)
            }

            return true
        } else if (containerType is TypeVariable) {
            return false
        } else {
            val typeStr = typeChecker.typeToString(containerType, boundVars)
            throw IRConversionException("Attempted to perform keyed access on type ${typeStr}, " +
                    "which does not support keyed access", node.startLocation)
        }
    }
}

/**
 * A contstraint that waits until a container in a keyed assignment has been resolved, and then
 * applies the correct constraints if it is a vector or map.
 */
class KeyedAssignmentConstraint(
    val node: KeyedAssignmentNode,
    val boundVars: MutableSet<TypeVariable>,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        // The trigger is the type of the node's container
        val containerType = trigger

        if (containerType is VectorType) {
            // Constrain eval type of node to be the element type of vector
            if (!typeChecker.unify(containerType.elementType, node.type)) {
                val types = typeChecker.typesToString(containerType.elementType, node.type,
                        boundVars)
                throw IRConversionException("Expected type of vector elements to be ${types[0]}, " +
                        "but found ${types[1]}", node.accessLocation)
            }

            // Constrain key to be an integer
            if (!typeChecker.unify(node.key.type, IntType)) {
                val type = typeChecker.typeToString(node.key.type, boundVars)
                throw IRConversionException("Vector expected to have keys of type int, " +
                        "found ${type}", node.key.startLocation)
            }

            // Constrain element type of vector to be type of rValue assigned to it
            if (!typeChecker.unify(node.rValue.type, node.type)) {
                val types = typeChecker.typesToString(node.type, node.rValue.type, boundVars)
                throw IRConversionException("Vector has elements of type ${types[0]}, " +
                        "but assigned ${types[1]}", node.rValue.startLocation)
            }

            return true
        } else if (containerType is MapType) {
            // Constrain eval type of node to be the value type of map
            if (!typeChecker.unify(containerType.valType, node.type)) {
                val types = typeChecker.typesToString(containerType.valType, node.type, boundVars)
                throw IRConversionException("Map expected to have values of type ${types[0]}, " +
                        "but found ${types[1]}", node.startLocation)
            }

            // Constrain key type of key node to be key type of map
            if (!typeChecker.unify(containerType.keyType, node.key.type)) {
                val types = typeChecker.typesToString(containerType.keyType, node.key.type,
                        boundVars)
                throw IRConversionException("Map expected to have keys of type ${types[0]}, " +
                        "but found ${types[1]}", node.startLocation) 
            }

            // Constrain value type of map to the type or rValue assigned to it
            if (!typeChecker.unify(containerType.valType, node.rValue.type)) {
                val types = typeChecker.typesToString(containerType.valType, node.rValue.type,
                        boundVars)
                throw IRConversionException("Map has values of type ${types[0]}, " +
                        "but assigned ${types[1]}", node.rValue.startLocation)
            }

            return true
        } else if (containerType is TypeVariable) {
            return false
        } else {
            val typeStr = typeChecker.typeToString(containerType, boundVars)
            throw IRConversionException("Attempted to perform keyed assignment on type " +
                    "${typeStr}, which does not support keyed access", node.startLocation)
        }
    }
}

