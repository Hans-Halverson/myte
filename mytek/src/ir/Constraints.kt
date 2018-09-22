package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.shared.*

sealed class Constraint() {
    /**
     * Resolve this constraint, enforcing it among all types and type variables in the program.
     * Fail with an exception if the constraint could not be enforced.
     */
    abstract fun resolve()

    /**
     * Assert that this constraint is unresolved at the end of type inference. This should
     * throw an exception with an appropriate error message.
     */
    abstract fun assertUnresolved(): Nothing
}

class AccessConstraint(
    val node: AccessNode,
    val boundVars: MutableSet<TypeVariable>,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        val exprType = typeChecker.currentRepType(node.expr.type)

        // The toString method is already defined on all types
        if (node.field == TO_STRING_METHOD) {
            // Type of node is type of toString
            if (!typeChecker.unify(node.type, TO_STRING_TYPE)) {
                val types = typeChecker.typesToString(node.type, TO_STRING_TYPE)
                throw IRConversionException("Method inferred to have type " +
                        "${types[0]}, but expected ${types[1]}", node.accessLocation)
            }

            return
        }

        if (exprType is OpenTypeVariable) {
            assertUnresolved()
        }

        // First check whether there is a method with the given name in a trait this type implements
        for (extTrait in exprType.sig.traits) {
            val concreteIdent = extTrait.traitSig.methods[node.field]
            if (concreteIdent != null) {
                // First perform rep substitution from trait sig params to trait type params
                val concreteType = typeChecker.symbolTable.getInfo(concreteIdent)?.type!!
                val trtParamsMap = (extTrait.traitSig.typeParams as List<TypeVariable>)
                        .zip(extTrait.typeParams).toMap()
                val traitSubstType = typeChecker.findRepSubstitution(concreteType, trtParamsMap)

                // Then perform rep substitution from type sig params to type params
                val adtParamsMap = (exprType.sig.typeParams as List<TypeVariable>)
                        .zip(exprType.listTypeParams()).toMap()
                val adtSubstType = typeChecker.findRepSubstitution(traitSubstType, adtParamsMap)

                // Finally refresh all unbound variables
                val freshMethodType = typeChecker.findRepType(adtSubstType, boundVars)

                // Type of node is type of reparameterized concrete function defined in trait
                if (!typeChecker.unify(node.type, freshMethodType)) {
                    val types = typeChecker.typesToString(node.type, freshMethodType)
                    throw IRConversionException("Method inferred to have type " +
                            "${types[0]}, but expected ${types[1]}", node.accessLocation)
                }

                return
            }
        }

        // Next check whether a method with the given name defined on this type
        val methodIdent = exprType.sig.methods[node.field]
        if (methodIdent != null) {
            val methodType = typeChecker.symbolTable.getInfo(methodIdent)?.type!!
            val paramsMap = (exprType.sig.typeParams as List<TypeVariable>)
                    .zip(exprType.listTypeParams()).toMap()
            val methodTypeWithParams = typeChecker.findRepSubstitution(methodType, paramsMap)
            val accessType = typeChecker.findRepType(methodTypeWithParams, boundVars)

            // Type of node is type of corresponding method
            if (!typeChecker.unify(node.type, accessType)) {
                val types = typeChecker.typesToString(node.type, accessType)
                throw IRConversionException("Method inferred to have type ${types[0]}, " +
                        "but expected ${types[1]}", node.accessLocation)
            }

            return
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

                return
            }
        // If a trait, must still check method signatures
        } else if (exprType is TraitType) {
            // Find method signature defined on trait
            val methodSignatureIdent = exprType.traitSig.methodSignatures[node.field]

            if (methodSignatureIdent != null) {
                val identType = typeChecker.symbolTable.getInfo(methodSignatureIdent)?.type!!
                val paramsMap = (exprType.traitSig.typeParams as List<TypeVariable>)
                        .zip(exprType.typeParams).toMap()
                val methodType = typeChecker.findRepSubstitution(identType, paramsMap)

                val freshMethodType = typeChecker.findRepType(methodType, boundVars)

                // Type of node is type of reparameterized method defined in trait
                if (!typeChecker.unify(node.type, freshMethodType)) {
                    val types = typeChecker.typesToString(node.type, freshMethodType)
                    throw IRConversionException("Method inferred to have type " +
                            "${types[0]}, but expected ${types[1]}", node.accessLocation)
                }

                return
            }
        }

        val typeStr = typeChecker.typeToString(exprType)
        throw IRConversionException("No method with name ${node.field} defined on ${typeStr}",
                node.accessLocation)
    }

    override fun assertUnresolved(): Nothing {
        throw IRConversionException("Type could not be inferred, consider adding more type " +
                "annotations", node.expr.startLocation)
    }
}

class FieldAssignmentConstraint(
    val node: FieldAssignmentNode,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        val exprType = typeChecker.currentRepType(node.expr.type)
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

            return
        } else if (exprType is OpenTypeVariable) {
            assertUnresolved()
        } else {
            val type = typeChecker.typeToString(exprType)
            throw IRConversionException("Can only assign field on simple record type, found $type",
                    node.accessLocation)
        }
    }

    override fun assertUnresolved(): Nothing {
        throw IRConversionException("Type could not be inferred, consider adding more type " +
                "annotations", node.expr.startLocation)
    }
}

class VectorLiteralConstraint(
    val node: VectorLiteralNode,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        // Find the lowest common subtype of all elements in the vector literal
        var elementType = node.elements.first().type
        for (elementNode in node.elements.drop(1)) {
            val lowestType = typeChecker.lowestCommonSupertype(elementType, elementNode.type)
            if (lowestType == null) {
                val types = typeChecker.typesToString(elementType, elementNode.type)
                throw IRConversionException("Could not infer element type for vector literal, " +
                        "${types[0]} and ${types[1]} have no common supertype", node.startLocation)
            }

            elementType = lowestType
        }

        if (!typeChecker.unify(node.type, VectorType(elementType))) {
            val types = typeChecker.typesToString(node.type, VectorType(elementType))
            throw IRConversionException("Vector literal inferred to have type ${types[0]}, but " +
                    "used as if it had type ${types[1]}", node.startLocation)
        }
    }

    override fun assertUnresolved(): Nothing {
        throw IRConversionException("Could not infer element type of vector literal, consider " +
                "adding more type annotations", node.startLocation)
    }
}

class SetLiteralConstraint(
    val node: SetLiteralNode,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        // Find the lowest common subtype of all elements in the set literal
        var elementType = node.elements.first().type
        for (elementNode in node.elements.drop(1)) {
            val lowestType = typeChecker.lowestCommonSupertype(elementType, elementNode.type)
            if (lowestType == null) {
                val types = typeChecker.typesToString(elementType, elementNode.type)
                throw IRConversionException("Could not infer element type for set literal, " +
                        "${types[0]} and ${types[1]} have no common supertype", node.startLocation)
            }

            elementType = lowestType
        }

        if (!typeChecker.unify(node.type, SetType(elementType))) {
            val types = typeChecker.typesToString(node.type, SetType(elementType))
            throw IRConversionException("Set literal inferred to have type ${types[0]}, but " +
                    "used as if it had type ${types[1]}", node.startLocation)
        }
    }

    override fun assertUnresolved(): Nothing {
        throw IRConversionException("Could not infer element type of set literal, consider " +
                "adding more type annotations", node.startLocation)
    }
}

class MapLiteralConstraint(
    val node: MapLiteralNode,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        // Find the lowest common subtype of all keys in the map literal
        var keyType = node.keys.first().type
        for (keyNode in node.keys.drop(1)) {
            val lowestType = typeChecker.lowestCommonSupertype(keyType, keyNode.type)
            if (lowestType == null) {
                val types = typeChecker.typesToString(keyType, keyNode.type)
                throw IRConversionException("Could not infer key type for map literal, " +
                        "${types[0]} and ${types[1]} have no common supertype", node.startLocation)
            }

            keyType = lowestType
        }

        // Find the lowest common subtype of all values in the map literal
        var valueType = node.values.first().type
        for (valueNode in node.values.drop(1)) {
            val lowestType = typeChecker.lowestCommonSupertype(valueType, valueNode.type)
            if (lowestType == null) {
                val types = typeChecker.typesToString(valueType, valueNode.type)
                throw IRConversionException("Could not infer value type for map literal, " +
                        "${types[0]} and ${types[1]} have no common supertype", node.startLocation)
            }

            valueType = lowestType
        }

        if (!typeChecker.unify(node.type, MapType(keyType, valueType))) {
            val types = typeChecker.typesToString(node.type, MapType(keyType, valueType))
            throw IRConversionException("Map literal inferred to have type ${types[0]}, but " +
                    "used as if it had type ${types[1]}", node.startLocation)
        }
    }

    override fun assertUnresolved(): Nothing {
        throw IRConversionException("Could not infer key and value types of map literal, " +
                "consider adding more type annotations", node.startLocation)
    }
}

class SubtypeConstraint(
    val subType: Type,
    val superType: OpenTypeVariable,
    val except: () -> Unit,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        val superType = typeChecker.currentRepType(superType)

        // If the supertype is still a type variable, keep this constraint
        if (superType is OpenTypeVariable) {
            assertUnresolved()
        } else {
            if (!typeChecker.subtype(subType, superType, except)) {
                except()
            }

            return
        }
    }

    override fun assertUnresolved(): Nothing {
        throw ExceptionWithoutLocation("Supertype could not be inferred, consider adding more " +
                "type annotations")
    }
}

class SupertypeConstraint(
    val subType: OpenTypeVariable,
    val superType: Type,
    val except: () -> Unit,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        val subType = typeChecker.currentRepType(subType)

        // If the subtype is still a type variable, keep this constraint
        if (subType is OpenTypeVariable) {
            assertUnresolved()
        } else {
            if (!typeChecker.subtype(subType, superType, except)) {
                except()
            }

            return
        }
    }

    override fun assertUnresolved(): Nothing {
        throw ExceptionWithoutLocation("Subtype could not be inferred, consider adding more " +
                "type annotations")
    }
}

class BidirectionalSubtypingConstraint(
    val subType: OpenTypeVariable,
    val superType: OpenTypeVariable,
    val except: () -> Unit,
    val typeChecker: TypeChecker
) : Constraint() {
    override fun resolve() {
        val repSubType = typeChecker.findRepNode(subType).resolvedType
        val repSuperType = typeChecker.findRepNode(superType).resolvedType

        if (repSubType is OpenTypeVariable && repSuperType is OpenTypeVariable) {
            assertUnresolved()
        } else {
            if (!typeChecker.subtype(repSubType, repSuperType, except)) {
                except()
            }

            return
        }
    }

    override fun assertUnresolved(): Nothing {
        throw ExceptionWithoutLocation("Subtype could not be inferred, consider adding more " +
                "type annotations")
    }
}
