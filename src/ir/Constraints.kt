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
        // TODO: Infer the least common supertype of all element types
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
        // TODO: Infer the least common supertype of all element types
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
        // TODO: Infer the least common supertype of all key and value types
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
