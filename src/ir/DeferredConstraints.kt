package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.shared.*

sealed class DeferredConstraint {
    /**
     * Attempt to apply this constraint.
     *
     * @param trigger the new type to apply this constraint to
     * @return a list of deferred constraints (along with thier triggers) to add if this
     *         constraint has been satisfied, or null if this constraint has not been satisfied
     */
    abstract fun apply(trigger: Type): Boolean

    /**
     * Assert that this constraint is unresolved at the end of type inference. This should
     * throw an exception with an appropriate error message.
     */
    abstract fun assertUnresolved()

    /**
     * Attempt to infer types if the fixed point iteration has stalled.
     * 
     * @return whether a type could be inferred or not
     */
    open fun inferOnFixedPointStall(trigger: Type): Boolean = false
}

/**
 * A constraint that waits until the type for a unary math operator is resolved, and then checks
 * that it is either an int or float.
 */
class UnaryMathOperatorConstraint(
    val node: UnaryMathOperatorNode,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val nodeType = trigger
        if (nodeType is IntType || nodeType is FloatType) {
            return true
        } else if (nodeType is TypeVariable) {
            return false
        } else {
            val typeStr = typeChecker.typeToString(nodeType)
            throw IRConversionException("Unary math operator expects arguments of type int or " +
                    "float, but found ${typeStr}", node.startLocation)
        }
    }

    override fun assertUnresolved() {
        throw IRConversionException("Could not infer type between int and float, consider adding " +
                "more type annotations", node.startLocation)
    }
}

/**
 * A constraint that waits until the type for a binary math operator is resolved, and then checks
 * that it is either an int or float.
 */
class BinaryMathOperatorConstraint(
    val node: BinaryMathOperatorNode,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val nodeType = trigger
        if (nodeType is IntType || nodeType is FloatType) {
            return true
        } else if (node is AddNode && nodeType is StringType) {
            return true
        } else if (nodeType is TypeVariable) {
            return false
        } else {
            val typeStr = typeChecker.typeToString(nodeType)
            throw IRConversionException("Binary math operator expects arguments of type int or " +
                    "float, but found ${typeStr}", node.startLocation)
        }
    }

    override fun assertUnresolved() {
        throw IRConversionException("Could not infer type between int and float, consider adding " +
                "more type annotations", node.startLocation)
    }
}

class AccessConstraint(
    val node: AccessNode,
    val boundVars: MutableSet<TypeVariable>,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val exprType = trigger

        // The toString method is already defined on all types
        if (node.field == TO_STRING_METHOD) {
            // Type of node is type of toString
            if (!typeChecker.unify(node.type, TO_STRING_TYPE)) {
                val types = typeChecker.typesToString(node.type, TO_STRING_TYPE)
                throw IRConversionException("Method inferred to have type " +
                        "${types[0]}, but expected ${types[1]}", node.accessLocation)
            }

            return true
        }

        if (exprType is TypeVariable) {
            return false
        }

        // First check whether there is a method with the given name in a trait this type implements
        for (extTrait in exprType.sig.traits) {
            val concreteIdent = extTrait.traitSig.methods[node.field]
            if (concreteIdent != null) {
                // First perform rep substitution from trait sig params to trait type params
                val concreteType = typeChecker.symbolTable.getInfo(concreteIdent)?.type!!
                val trtParamsMap = extTrait.traitSig.typeParams.zip(extTrait.typeParams).toMap()
                val traitSubstType = typeChecker.findRepSubstitution(concreteType, trtParamsMap)

                // Then perform rep substitution from type sig params to type params
                val adtParamsMap = exprType.sig.typeParams.zip(exprType.listTypeParams()).toMap()
                val adtSubstType = typeChecker.findRepSubstitution(traitSubstType, adtParamsMap)

                // Finally refresh all unbound variables
                val freshMethodType = typeChecker.findRepType(adtSubstType, boundVars)

                // Type of node is type of reparameterized concrete function defined in trait
                if (!typeChecker.unify(node.type, freshMethodType)) {
                    val types = typeChecker.typesToString(node.type, freshMethodType)
                    throw IRConversionException("Method inferred to have type " +
                            "${types[0]}, but expected ${types[1]}", node.accessLocation)
                }

                return true
            }
        }

        // Next check whether a method with the given name defined on this type
        val methodIdent = exprType.sig.methods[node.field]
        if (methodIdent != null) {
            val methodType = typeChecker.symbolTable.getInfo(methodIdent)?.type!!
            val paramsMap = exprType.sig.typeParams.zip(exprType.listTypeParams()).toMap()
            val methodTypeWithParams = typeChecker.findRepSubstitution(methodType, paramsMap)
            val accessType = typeChecker.findRepType(methodTypeWithParams, boundVars)

            // Type of node is type of corresponding method
            if (!typeChecker.unify(node.type, accessType)) {
                val types = typeChecker.typesToString(node.type, accessType)
                throw IRConversionException("Method inferred to have type ${types[0]}, " +
                        "but expected ${types[1]}", node.accessLocation)
            }

            return true
        }

        // If an algebriac data type, must still check simple record fields
        if (exprType is AlgebraicDataType) {
            // Error if expr is not resolved to a simple record type
            val recordVariant = exprType.adtSig.variants[0]
            if (exprType.adtSig.variants.size != 1 || recordVariant !is RecordVariant) {
                val typeStr = typeChecker.typeToString(exprType)
                throw IRConversionException("No field or method with name ${node.field} for " +
                        "type ${typeStr}", node.accessLocation)
            }

            // Type of node is type of corresponding field, if one exists
            val fieldType = recordVariant.getFieldsWithParams(exprType.typeParams)[node.field]
            if (fieldType != null) {
                if (!typeChecker.unify(node.type, fieldType)) {
                    val types = typeChecker.typesToString(node.type, fieldType)
                    throw IRConversionException("Field inferred to have type ${types[0]}, " +
                            "but expected ${types[1]}", node.accessLocation)
                }

                return true
            }
        // If a trait, must still check method signatures
        } else if (exprType is TraitType) {
            // Find method signature defined on trait
            val methodSignatureIdent = exprType.traitSig.methodSignatures[node.field]

            if (methodSignatureIdent != null) {
                val identType = typeChecker.symbolTable.getInfo(methodSignatureIdent)?.type!!
                val paramsMap = exprType.traitSig.typeParams.zip(exprType.typeParams).toMap()
                val methodType = typeChecker.findRepSubstitution(identType, paramsMap)

                val freshMethodType = typeChecker.findRepType(methodType, boundVars)

                // Type of node is type of reparameterized method defined in trait
                if (!typeChecker.unify(node.type, freshMethodType)) {
                    val types = typeChecker.typesToString(node.type, freshMethodType)
                    throw IRConversionException("Method inferred to have type " +
                            "${types[0]}, but expected ${types[1]}", node.accessLocation)
                }

                return true
            }
        }

        val typeStr = typeChecker.typeToString(exprType)
        throw IRConversionException("No method with name ${node.field} defined on ${typeStr}",
                node.accessLocation)
    }

    override fun assertUnresolved() {
        throw IRConversionException("Type could not be inferred, consider adding more type " +
                "annotations", node.expr.startLocation)
    }
}

class FieldAssignmentConstraint(
    val node: FieldAssignmentNode,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val exprType = trigger
        if (exprType is AlgebraicDataType) {
            // Error if expr is not resolved to a simple record type
            val recordVariant = exprType.adtSig.variants[0]
            if (exprType.adtSig.variants.size != 1 || recordVariant !is RecordVariant) {
                val typeStr = typeChecker.typeToString(exprType)
                throw IRConversionException("No field or method with name ${node.field} for " +
                        "type ${typeStr}", node.accessLocation)
            }

            // Error if no field with the given name is defined on the record type
            val fieldType = recordVariant.getFieldsWithParams(exprType.typeParams)[node.field]
            if (fieldType == null) {
                val typeStr = typeChecker.typeToString(exprType)
                throw IRConversionException("No field or method with name ${node.field} for " +
                        "type ${typeStr}", node.accessLocation)
            }

            // Make sure that field is mutable
            if (!recordVariant.fields[node.field]?.second!!) {
                val typeStr = typeChecker.typeToString(exprType)
                throw IRConversionException("Field ${node.field} on type ${typeStr} is mutable " +
                        "and can not be reassigned", node.accessLocation)
            }

            // Type of node is type of corresponding field or method
            if (!typeChecker.unify(node.type, fieldType)) {
                val types = typeChecker.typesToString(node.type, fieldType)
                throw IRConversionException("Field or method inferred to have type ${types[0]}, " +
                        "but expected ${types[1]}", node.accessLocation)
            }

            // Type of rValue must match type of field
            if (!typeChecker.unify(fieldType, node.rValue.type)) {
                val types = typeChecker.typesToString(fieldType, node.rValue.type)
                throw IRConversionException("Field has type ${types[0]} but assigned ${types[1]}",
                        node.accessLocation)
            }

            return true
        } else if (exprType is TypeVariable) {
            return false
        } else {
            val type = typeChecker.typeToString(exprType)
            throw IRConversionException("Can only assign field on simple record type, found $type",
                    node.accessLocation)
        }
    }

    override fun assertUnresolved() {
        throw IRConversionException("Type could not be inferred, consider adding more type " +
                "annotations", node.expr.startLocation)
    }
}

class SubtypeConstraint(
    val subType: Type,
    val except: () -> Unit,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val superType = trigger
        val repSubType = if (subType is TypeVariable) {
            typeChecker.findRepNode(subType).resolvedType
        } else {
            subType
        }

        // If the supertype is still a type variable, keep this constraint
        if (superType is TypeVariable) {
            return false
        // If the subtype is still a type variable, remove this constraint since the
        // super/subtype relation will be enforced by the supertype constraint on the subtype.
        } else if (repSubType is TypeVariable) {
            return true
        } else {
            if (!typeChecker.subtype(subType, superType, except)) {
                except()
            }

            return true
        }
    }

    override fun assertUnresolved() {
        throw ExceptionWithoutLocation("Supertype could not be inferred, consider adding more " +
                "type annotations")
    }

    override fun inferOnFixedPointStall(trigger: Type): Boolean {
        if (!typeChecker.unify(trigger, subType)) {
            except()
        }

        return true
    }
}

class SupertypeConstraint(
    val superType: Type,
    val except: () -> Unit,
    val typeChecker: TypeChecker
) : DeferredConstraint() {
    override fun apply(trigger: Type): Boolean {
        val subType = trigger
        val repSuperType = if (superType is TypeVariable) {
            typeChecker.findRepNode(superType).resolvedType
        } else {
            superType
        }

        // If the subtype is still a type variable, keep this constraint
        if (subType is TypeVariable) {
            return false
        // If the supertype is still a type variable, remove this constraint since the
        // super/subtype relation will be enforced by the subtype constraint on the supertype.
        } else if (repSuperType is TypeVariable) {
            return true
        } else {
            if (!typeChecker.subtype(subType, superType, except)) {
                except()
            }

            return true
        }
    }

    override fun assertUnresolved() {
        throw ExceptionWithoutLocation("Subtype could not be inferred, consider adding more " +
                "type annotations")
    }

    override fun inferOnFixedPointStall(trigger: Type): Boolean {
        if (!typeChecker.unify(trigger, superType)) {
            except()
        }

        return true
    }
}
