package myte.ir

import myte.ir.nodes.*
import myte.shared.*

/**
 * A node in the forest of type equivalence classes.
 *
 * @property resolvedType the type contained at this node
 * @property parent the (optional) parent node in the forest of type equivalence classes. This type
 *           is in the same equivalence class as its parent, and if the parent is null, this type
 *           is the representative for its equivalence class.
 * @property rank an upper bound on the longest path from this node to a leaf
 */
class TypeEquivalenceNode(
    var resolvedType: Type,
    var parent: TypeEquivalenceNode? = null,
    var rank: Int = 0
) {
    // An equivalence node is a root only when it does not have a parent
    val isRoot: Boolean
        get() = parent == null

    override fun equals(other: Any?): Boolean {
        if (other !is TypeEquivalenceNode) {
            return false
        }

        return (resolvedType == other.resolvedType)
    }
}

class TypeChecker(var symbolTable: SymbolTable) {
    private val typeVarToNode: MutableMap<TypeVariable, TypeEquivalenceNode> = mutableMapOf()
    private val deferredConstraints: MutableMap<TypeVariable, MutableList<DeferredConstraint>> =
            mutableMapOf()

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
    }

    /**
     * Add a type variable to the set of equivalence classes, if it does not already exist, and
     * return the equivalence class node for this type variable.
     */
    private fun addTypeVar(typeVar: TypeVariable): TypeEquivalenceNode {
        val typeEquivNode = typeVarToNode[typeVar]
        if (typeEquivNode == null) {
            val newEquivNode = TypeEquivalenceNode(typeVar)
            typeVarToNode[typeVar] = newEquivNode

            return newEquivNode
        }

        return typeEquivNode
    }

    /**
     * Find the root equivalence node for a given equivalence node, and compress the path
     * to the root along the way.
     */
    private fun findRoot(node: TypeEquivalenceNode): TypeEquivalenceNode {
        val parent = node.parent
        // If this is not a root, find the root and set the parent to point directly to the root
        if (parent != null) {
            val root = findRoot(parent)
            node.parent = root
            return root
        } else {
            // Otherwise return this node since it is its own root
            return node
        }
    }

    /**
     * Find the representative equivalence class node for the given type variable.
     */
    fun findRepNode(typeVar: TypeVariable): TypeEquivalenceNode {
        // The representative node of a type is found by finding the root of its equivalence node
        var node = addTypeVar(typeVar)
        return findRoot(node)
    }

    /**
     * Set the representative node of the given type variable to be resolved to the given type.
     * Returns true if successful, false if the type variable has already been resolved to an
     * an incompatible type.
     */
    private fun resolveType(typeVar: TypeVariable, resolvedType: Type): Boolean {
        val equivNode = findRepNode(typeVar)
        if (equivNode.resolvedType !is TypeVariable && equivNode.resolvedType != resolvedType) {
            return false
        }

        // Set resolved type
        equivNode.resolvedType = resolvedType

        // Apply all deferred constraints to newly resolved type, erroring if they cannot be applied
        val constraints = deferredConstraints.remove(typeVar)
        if (constraints != null) {
            for (constraint in constraints) {
                if (!constraint.apply(resolvedType)) {
                    constraint.assertUnresolved()
                }
            }
        }

        return true
    }

    /**
     * Add a deferred constraint to the given type variable.
     */
    fun addDeferredConstraint(type: Type, constraint: DeferredConstraint) {
        val repType = if (type is TypeVariable) findRepNode(type).resolvedType else type
        if (repType is TypeVariable) {
            // If a type variable, add deferred constraint to list of constraints for the variable
            val constraints = deferredConstraints[repType]
            if (constraints == null) {
                deferredConstraints[repType] = mutableListOf(constraint)
            } else {
                constraints.add(constraint)
            }
        } else {
            // Attempt to apply this constraint to concrete type, fail if not possible since there
            // will never be a point in the future in which it could be resolved.
            if (!constraint.apply(repType)) {
                constraint.assertUnresolved()
            }
        }
    }

    /**
     * Infer a final type for the given input type.
     */
    private fun inferType(type: Type): Type {
        return currentRepType(type)
    }

    /**
     * Attempt to resolve all outstanding constraints, possibly reaching stalls and notifying
     * constraints if there was a stall.
     */
    fun findFixedPoint() {
        fixedPointLoop@ while (true) {
            var foundFixedPoint = false
            while (!foundFixedPoint) {
                // Iterate through all constraints until no progress can be made
                foundFixedPoint = true

                val currentConstraints = deferredConstraints.toMap()
                for ((type, constraints) in currentConstraints) {
                    for (constraint in constraints.toList()) {
                        // Attempt to apply each constraint, remembering if one succeeds
                        if (constraint.apply(type)) {
                            foundFixedPoint = false

                            // Remove successfully applied constraints
                            deferredConstraints[type]?.remove(constraint)
                            if (deferredConstraints[type]?.isEmpty() == true) {
                                deferredConstraints.remove(type)
                            }
                        }
                    }
                }
            }

            // If there are still deferred constraints but no progress can be made, notify all
            // constraints that there has been a stall and attempt to infer types.
            if (!deferredConstraints.isEmpty()) {
                // Iterate through all constraints, notifying each that there has been a stall
                var canBreakStall = false

                val currentConstraints = deferredConstraints.toMap()
                for ((type, constraints) in currentConstraints) {
                    for (constraint in constraints.toList()) {
                        // Notify each constraint that there has been a stall, attempting to infer
                        if (constraint.inferOnFixedPointStall(type)) {
                            canBreakStall = true

                            // Remove succesfully applied constraints
                            deferredConstraints[type]?.remove(constraint)
                            if (deferredConstraints[type]?.isEmpty() == true) {
                                deferredConstraints.remove(type)
                            }
                        }
                    }
                }

                // If some progress was made by inferring constraints about the stall, repeat loop
                if (canBreakStall) {
                    continue@fixedPointLoop
                } else {
                    // If the stall could not be broken, show an example unresolved constraint
                    for (constraints in currentConstraints.values) {
                        for (constraint in constraints.toList()) {
                            constraint.assertUnresolved()
                        }
                    }

                    return
                }
            } else {
                break@fixedPointLoop
            }
        }
    }

    /**
     * Given a key and value, fill in the representative type variable to representative type
     * mappings for all variable/type pairs in the key and value types.
     *
     * @param keyType the key type to fill in type variable mappings for
     * @param valType the value type to associate with a type variable
     * @param substMap the map from representative type variables to representative types
     */
    fun substituteReps(
        keyType: Type,
        valType: Type,
        substMap: MutableMap<TypeVariable, Type>
    ) {
        val keyRepType = currentRepType(keyType)
        val valRepType = currentRepType(valType)

        when {
            // For non type variables, simply recur into types
            keyRepType is VectorType && valRepType is VectorType ->
                substituteReps(keyRepType.elementType, valRepType.elementType, substMap)
            keyRepType is SetType && valRepType is SetType ->
                substituteReps(keyRepType.elementType, valRepType.elementType, substMap)
            keyRepType is MapType && valRepType is MapType -> {
                substituteReps(keyRepType.keyType, valRepType.keyType, substMap)
                substituteReps(keyRepType.valType, valRepType.valType, substMap)
            }
            keyRepType is TupleType && valRepType is TupleType -> keyRepType.elementTypes
                .zip(valRepType.elementTypes).forEach { (keyType, valType) ->
                    substituteReps(keyType, valType, substMap)
                }
            keyRepType is FunctionType && valRepType is FunctionType -> {
                keyRepType.argTypes.zip(valRepType.argTypes).forEach { (keyType, valType) ->
                    substituteReps(keyType, valType, substMap)
                }

                substituteReps(keyRepType.returnType, valRepType.returnType, substMap)
            }
            keyRepType is AlgebraicDataType && valRepType is AlgebraicDataType ->
                keyRepType.typeParams.zip(valRepType.typeParams).forEach { (keyType, valType) ->
                    substituteReps(keyType, valType, substMap)
                }
            keyRepType is TraitType && valRepType is TraitType ->
                keyRepType.typeParams.zip(valRepType.typeParams).forEach { (keyType, valType) ->
                    substituteReps(keyType, valType, substMap)
                }
            // Add type variables to representative substitution map
            keyRepType is TypeVariable -> {
                substMap[keyRepType] = valRepType
            }
        }
    }

    /**
     * Find the substitution given a map and a type, working on the representative types of both
     * the type and the substitution.
     */
    fun findRepSubstitution(
        type: Type,
        substMap: Map<TypeVariable, Type>
    ): Type {
        // Find the representative substitution of the given substitution
        val repSubstMap: MutableMap<TypeVariable, Type> = mutableMapOf()
        substMap.forEach { (keyType, valType) ->
            substituteReps(keyType, valType, repSubstMap)
        }

        // Use the representative substitutions on this type's representative type
        val repType = currentRepType(type)

        return repType.substitute(repSubstMap)
    }

    fun currentRepType(type: Type): Type {
        return findRepType(type, mutableSetOf(), mutableMapOf(), false)
    }

    /**
     * Recursively find a fresh representative type for a given type. This will return the
     * representative type for this type, and the representative types for all child types
     * if the given type is a type constructor. All free type variables in the representative
     * type will also be remapped to fresh type vars.
     *
     * @param type the type to find the representative type for
     * @param boundVars set of all bound type variables. If not supplied, defaults to the empty set.
     * @param mappedVars map of old type variables to fresh type variables for all unbound type
     *        variables encountered so far. If not supplied, defaults to the empty map.
     * @param refresh whether to create a new type variable for all unbound type variables in type
     */
    fun findRepType(
        type: Type,
        boundVars: MutableSet<TypeVariable>,
        mappedVars: MutableMap<TypeVariable, TypeVariable> = mutableMapOf(),
        refresh: Boolean = true
    ): Type {
        // Find the representative type if this is a type variable, otherwise use the type
        val repType = if (type is TypeVariable) findRepNode(type).resolvedType else type

        return when (repType) {
            // Find the rep type for vector element type and reconstruct vector type
            is VectorType -> {
                VectorType(findRepType(repType.elementType, boundVars, mappedVars, refresh))
            }
            // Find the rep type for set element type and reconstruct set type
            is SetType -> {
                SetType(findRepType(repType.elementType, boundVars, mappedVars, refresh))
            }
            // Find the rep type for map's key and value types and reconstruct map type
            is MapType -> {
                MapType(findRepType(repType.keyType, boundVars, mappedVars, refresh),
                        findRepType(repType.valType, boundVars, mappedVars, refresh))
            }
            // Find the rep type for each tuple element and reconstruct tuple type
            is TupleType -> {
                val elementTypes = repType.elementTypes.map { elementType ->
                    findRepType(elementType, boundVars, mappedVars, refresh)
                }
                
                return TupleType(elementTypes)
            }
            // Find the rep type for each arg and return type, and reconstruct function type
            is FunctionType -> {
                val argTypes = repType.argTypes.map { argType ->
                    findRepType(argType, boundVars, mappedVars, refresh)
                }
                val returnType = findRepType(repType.returnType, boundVars, mappedVars, refresh)

                return FunctionType(argTypes, returnType)
            }
            // Find the rep type for each type parameter and reconstruct adt with correct adt sig
            is AlgebraicDataType -> {
                val typeParams = repType.typeParams.map { typeParam ->
                    findRepType(typeParam, boundVars, mappedVars, refresh)
                }

                return AlgebraicDataType(repType.adtSig, typeParams)
            }
            // Find the rep type for each type paramter and reconstruct trait with correct trait sig
            is TraitType -> {
                val typeParams = repType.typeParams.map { typeParam ->
                    findRepType(typeParam, boundVars, mappedVars, refresh)
                }

                return TraitType(repType.traitSig, typeParams)
            }
            is TypeVariable -> {
                // If already bound (has same representative as bound var), then return
                // existing type variable
                val repBoundVars = boundVars.map { boundVar ->
                    findRepNode(boundVar).resolvedType
                }

                if (repBoundVars.contains(repType)) {
                    return repType
                }

                // If not yet bound, return mapped variable or add to map if not yet mapped
                val mappedVar = mappedVars[repType]
                if (mappedVar != null) {
                    return mappedVar
                // Generate new type variable if refresh flag is set, otherwise return rep type
                } else if (refresh) {
                    val newVar = TypeVariable()
                    mappedVars[repType] = newVar
                    return newVar
                } else {
                    return repType
                }
            }
            else -> repType
        }
    }

    /**
     * Merge two types into the same equivalence class, where at least one type is a type variable.
     *
     * @return whether the two types can be merged into the same equivalence class.
     *         This is not possible if one is a type variable that appears in the other
     *         type.
     */
    private fun mergeTypes(type1: Type, type2: Type): Boolean {
        // If merging two type variables, set the first rep node to point to the second rep node
        if (type1 is TypeVariable && type2 is TypeVariable) {
            val rep1 = findRepNode(type1)
            val rep2 = findRepNode(type2)

            // Choose the type parameter to be the root node, or choose the lower ranked node
            val firstIsParent = if (type1 is TypeParameter) {
                true
            } else if (type2 is TypeParameter) {
                false
            } else {
                rep1.rank > rep2.rank
            }

            if (firstIsParent) {
                replaceRoot(rep2, rep1)
            } else {
                replaceRoot(rep1, rep2)

                // If ranks were equal, increment the rank of the newly non-root node
                if (rep1.rank == rep2.rank) {
                    rep2.rank++
                }
            }

            return true
        // If merging a type variable with a type, if occurs check passes resolve type variable
        } else if (type1 is TypeVariable && type2 !is TypeVariable) {
            if (occursIn(type1, type2)) {
                return false
            }

            return resolveType(type1, type2)
        // If merging a type with a type variable, if occurs check passes resolve type variable
        } else if (type1 !is TypeVariable && type2 is TypeVariable) {
            if (occursIn(type2, type1)) {
                return false
            }

            return resolveType(type2, type1)
        // Merge types should only be called if at least one type is a type variable
        } else {
            return false
        }
    }

    private fun replaceRoot(oldRoot: TypeEquivalenceNode, newRoot: TypeEquivalenceNode) {
        val oldType = oldRoot.resolvedType as TypeVariable
        val newType = newRoot.resolvedType as TypeVariable

        // Move all deferred constraints from the old root to the new root
        val constraints = deferredConstraints.remove(oldType)
        if (constraints != null) {
            val newRootConstraints = deferredConstraints[newType]
            if (newRootConstraints == null) {
                deferredConstraints[newType] = constraints
            } else {
                newRootConstraints.addAll(constraints)
            }
        }

        oldRoot.parent = newRoot
    }

    /**
     * Returns whether the given type variable occurs anywhere in a given type.
     */
    private fun occursIn(typeVar: TypeVariable, subst: Type): Boolean {
        return subst.getAllVariables().contains(typeVar)
    }

    /**
     * Unify two types by merging their representative types (and recursively merging their
     * child types) into the same equivalence class following the unification algorithm.
     * 
     * @return whether the two types can be unified or not
     */
    fun unify(t1: Type, t2: Type): Boolean {
        // If type variables, work with their representative types
        val type1 = if (t1 is TypeVariable) findRepNode(t1).resolvedType else t1
        val type2 = if (t2 is TypeVariable) findRepNode(t2).resolvedType else t2

        // If both types are already identical, they are already unified
        if (type1 == type2) {
            return true
        // If at least one type is a type variable, merge types together
        } else if (type1 is TypeVariable || type2 is TypeVariable) {
            // Cannot merge a type parameter with anything except a type variable
            if ((type1 is TypeParameter && type2 !is TypeVariable) ||
                    (type1 !is TypeVariable && type2 is TypeParameter)) {
                return false
            }

            return mergeTypes(type1, type2)
        // If both representatives have vector type, merge reps and unify their child types
        } else if (type1 is VectorType && type2 is VectorType) {
            return unify(type1.elementType, type2.elementType)
        // If both representatives have set type, merge reps and unify their child types
        } else if (type1 is SetType && type2 is SetType) {
            return unify(type1.elementType, type2.elementType)
        } else if (type1 is MapType && type2 is MapType) {
            return unify(type1.keyType, type2.keyType) &&
                    unify(type1.valType, type2.valType)
        // If both representatives have tuple types, merge reps of all element types
        } else if (type1 is TupleType && type2 is TupleType) {
            return type1.elementTypes.size == type2.elementTypes.size &&
                    type1.elementTypes.zip(type2.elementTypes)
                         .map({ (e1, e2) -> unify(e1, e2) })
                         .all({ x -> x })
        // If both representatives have function type, merge reps and unify their child types
        } else if (type1 is FunctionType && type2 is FunctionType) {
            return type1.argTypes.size == type2.argTypes.size &&
                    unify(type1.returnType, type2.returnType) &&
                    type1.argTypes.zip(type2.argTypes)
                         .map({ (a1, a2) -> unify(a1, a2) })
                         .all({ x -> x })
        // If both representatives are the same adt, merge reps and unify parameter types
        } else if (type1 is AlgebraicDataType && type2 is AlgebraicDataType) {
            return type1.adtSig == type2.adtSig &&
                    type1.typeParams.size == type2.typeParams.size &&
                    type1.typeParams.zip(type2.typeParams)
                         .map({ (p1, p2) -> unify(p1, p2) })
                         .all({ x -> x })
        // If both representatives are the same trait, merge reps and unify parameter types
        } else if (type1 is TraitType && type2 is TraitType) {
            return type1.traitSig == type2.traitSig &&
                    type1.typeParams.size == type2.typeParams.size &&
                    type1.typeParams.zip(type2.typeParams)
                         .map({ (p1, p2) -> unify(p1, p2) })
                         .all({ x -> x })
        } else {
            return false
        }
    }

    /**
     * Constrain two types to have a subtype/supertype relation.
     *
     * @return whether the two types can satisfy the the subtype/supertype relation
     */
    fun subtype(sbType: Type, spType: Type, except: () -> Unit): Boolean {
        // If type variables, work with their representative types
        val subType = if (sbType is TypeVariable) findRepNode(sbType).resolvedType else sbType
        val superType = if (spType is TypeVariable) findRepNode(spType).resolvedType else spType

        // The subtype relation is reflexive
        if (subType == superType) {
            return true
        // If both types are type variables, add supertype and subtype bounds to type variables
        } else if (subType is TypeVariable && superType is TypeVariable) {
            val subtypeConstraint = SubtypeConstraint(subType, except, this)
            val supertypeConstraint = SupertypeConstraint(superType, except, this)

            addDeferredConstraint(superType, subtypeConstraint)
            addDeferredConstraint(subType, supertypeConstraint)

            return true
        // If the subtype only is a type variable, add supertype bound to type variable
        } else if (subType is TypeVariable && superType !is TypeVariable) {
            val supertypeConstraint = SupertypeConstraint(superType, except, this)
            addDeferredConstraint(subType, supertypeConstraint)
            return true
        // If the supertype only is a type variable, add subtype bound to type variable
        } else if (subType !is TypeVariable && superType is TypeVariable) {
            val subtypeConstraint = SubtypeConstraint(subType, except, this)
            addDeferredConstraint(superType, subtypeConstraint)
            return true
        // Tuples types are covariant in element types, since they are immutable
        } else if (subType is TupleType && superType is TupleType) {
            return subType.elementTypes.size == superType.elementTypes.size &&
                    subType.elementTypes.zip(superType.elementTypes)
                        .all({ (e1, e2) -> subtype(e1, e2, except) })
        // Function types are contravariant in their element types, and covariant in return type
        } else if (subType is FunctionType && superType is FunctionType) {
            return subType.argTypes.size == superType.argTypes.size &&
                    subtype(subType.returnType, superType.returnType, except) &&
                    subType.argTypes.zip(superType.argTypes)
                        .all({ (a1, a2) -> subtype(a2, a1, except) })
        // Algebraic data types are invariant in their type parameters, since they are mutable
        } else if (subType is AlgebraicDataType && superType is AlgebraicDataType) {
            return subType.typeParams.size == superType.typeParams.size &&
                    subType.typeParams.zip(superType.typeParams)
                        .all({ (p1, p2) -> unify(p1, p2) })
        // Trait types are invariant in their type parameters, since they can only be implemented by
        // mutable algebraic data types, and contain no state of thier own.
        } else if (subType is TraitType && superType is TraitType) {
            return subType.typeParams.size == superType.typeParams.size &&
                    subType.typeParams.zip(superType.typeParams)
                        .all({ (p1, p2) -> unify(p1, p2) })
        // For an ADT to be a subtype of a trait, it must extend that trait
        } else if (subType is AlgebraicDataType && superType is TraitType) {
            val extendedTrait = subType.adtSig.traits.find { extTrait ->
                extTrait.traitSig == superType.traitSig
            }

            if (extendedTrait == null) {
                return false
            }

            // Find parameterized type of trait by performing rep substitution from trait sig params
            // to actual trait params in extended trait params.
            val paramsMap = subType.adtSig.typeParams.zip(subType.typeParams).toMap()
            val substTraitParams = extendedTrait.typeParams.map { typeParam ->
                findRepSubstitution(typeParam, paramsMap)
            }

            // Subtyping a trait is covariant in all type parameters
            return substTraitParams.zip(superType.typeParams)
                    .map({ (p1, p2) -> subtype(p1, p2, except) })
                    .all({ x -> x })
        // Vector, set, and map types are all invariant in type parameters since they are mutable.
        } else if ((subType is VectorType && superType is VectorType) ||
                (subType is SetType && superType is SetType) ||
                (subType is MapType && superType is MapType)) {
            return unify(subType, superType)
        } else {
            return false
        }
    }

    /**
     * Type check and perform unification for an IR tree rooted at a given node.
     *
     * @param node the ir node to type check
     * @param boundVars a set of all type variables that have been bound in the current type scope
     *        for this node
     * @param rebind whether or not to refresh unbound type variables for all type from identifiers
     *        that are encountered in this node or its children
     */
    fun typeCheck(node: IRNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        when (node) {
            // Literals
            is BoolLiteralNode -> typeCheckBoolLiteral(node)
            is StringLiteralNode -> typeCheckStringLiteral(node)
            is IntLiteralNode -> typeCheckIntLiteral(node)
            is FloatLiteralNode -> typeCheckFloatLiteral(node)
            is UnitLiteralNode -> typeCheckUnitLiteral(node)
            is VectorLiteralNode -> typeCheckVectorLiteral(node, boundVars, refresh)
            is SetLiteralNode -> typeCheckSetLiteral(node, boundVars, refresh)
            is MapLiteralNode -> typeCheckMapLiteral(node, boundVars, refresh)
            is TupleLiteralNode -> typeCheckTupleLiteral(node, boundVars, refresh)
            is LambdaNode -> typeCheckLambda(node, boundVars, refresh)
            is TupleTypeConstructorNode -> typeCheckTupleTypeConstructor(node, boundVars, refresh)
            is RecordTypeConstructorNode -> typeCheckRecordTypeConstructor(node, boundVars, refresh)
            // Variables and functions
            is VariableNode -> typeCheckVariable(node, boundVars, refresh)
            is FunctionCallNode -> typeCheckFunctionCall(node, boundVars, refresh)
            is AccessNode -> typeCheckAccess(node, boundVars, refresh)
            is FieldAssignmentNode -> typeCheckFieldAssignment(node, boundVars, refresh)
            is KeyedAccessNode -> typeCheckKeyedAccess(node, boundVars, refresh)
            is KeyedAssignmentNode -> typeCheckKeyedAssignment(node, boundVars, refresh)
            is VariableAssignmentNode -> typeCheckVariableAssignment(node, boundVars, refresh)
            is PatternAssignmentNode -> typeCheckPatternAssignment(node, boundVars, refresh)
            is VariableDefinitionNode -> typeCheckVariableDefinition(node, boundVars, refresh)
            is PatternDefinitionNode -> typeCheckPatternDefinition(node, boundVars, refresh)
            is FunctionDefinitionNode -> typeCheckFunctionDefinition(node, boundVars, refresh)
            // Math expressions
            is UnaryMathOperatorNode -> typeCheckUnaryMathOperator(node, boundVars, refresh)
            is BinaryMathOperatorNode -> typeCheckBinaryMathOperator(node, boundVars, refresh)
            // Logical operators
            is LogicalAndNode -> typeCheckLogicalAnd(node, boundVars, refresh)
            is LogicalOrNode -> typeCheckLogicalOr(node, boundVars, refresh)
            is LogicalNotNode -> typeCheckLogicalNot(node, boundVars, refresh)
            // Comparisons
            is EqualityNode -> typeCheckEquality(node, boundVars, refresh)
            is ComparisonNode -> typeCheckComparison(node, boundVars, refresh)
            // Control flow and structure
            is BlockNode -> typeCheckBlock(node, boundVars, refresh)
            is IfNode -> typeCheckIf(node, boundVars, refresh)
            is WhileNode -> typeCheckWhile(node, boundVars, refresh)
            is DoWhileNode -> typeCheckDoWhile(node, boundVars, refresh)
            is ForNode -> typeCheckFor(node, boundVars, refresh)
            is MatchNode -> typeCheckMatch(node, boundVars, refresh)
            is ReturnNode -> typeCheckReturn(node, boundVars, refresh)
            is BreakNode -> typeCheckBreak(node)
            is ContinueNode -> typeCheckContinue(node)
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unification functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckBoolLiteral(node: BoolLiteralNode) {
        // Type of bool literal must be a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Bool literal could not be inferred to have bool " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckStringLiteral(node: StringLiteralNode) {
        // Type of string literal must be a string
        if (!unify(node.type, StringType)) {
            val type = typeToString(node.type)
            throw IRConversionException("String literal could not be inferred to have string " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckIntLiteral(node: IntLiteralNode) {
        // Type of int literal must be an int
        if (!unify(node.type, IntType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Int literal could not be inferred to have int " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckFloatLiteral(node: FloatLiteralNode) {
        // Type of float literal must be a float
        if (!unify(node.type, FloatType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Float literal could not be inferred to have float " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckUnitLiteral(node: UnitLiteralNode) {
        // Type of unit literal must be unit
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Unit literal could not be inferred to have unit " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckVectorLiteral(
        node: VectorLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Vector type is initially unknown, so set as vector type with new type variable param
        val elementType = TypeVariable()
        val vectorType = VectorType(elementType)

        // Type of this vector literal must be a vector of the element type
        if (!unify(node.type, vectorType)) {
            val types = typesToString(node.type, vectorType)
            throw IRConversionException("Vector literal could not be inferred to have vector " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Every element in the vector's type must be a subtype of the vector's element type
        node.elements.forEach({ element ->
            val except = { ->
                val types = typesToString(element.type, elementType)
                throw IRConversionException("Vector must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, elementType, except)) {
                except()
            }
        })
    }

    fun typeCheckSetLiteral(
        node: SetLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Set type is initially unknown, so set as set type with new type variable param
        val elementType = TypeVariable()
        val setType = SetType(elementType)

        // Type of this set literal must be a set of the element type
        if (!unify(node.type, setType)) {
            val types = typesToString(node.type, setType)
            throw IRConversionException("Set literal could not be inferred to have set " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each element in the set's type must be a suptype of the sets's element type
        node.elements.forEach({ element ->
            val except = { ->
                val types = typesToString(element.type, elementType)
                throw IRConversionException("Set must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, elementType, except)) {
                except()
            }
        })
    }

    fun typeCheckMapLiteral(
        node: MapLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.keys.forEach { key -> typeCheck(key, boundVars, refresh) }
        node.values.forEach { value -> typeCheck(value, boundVars, refresh) }

        // Map type is initially unknown, so set as map type with new key and value params
        val keyType = TypeVariable()
        val valType = TypeVariable()
        val mapType = MapType(keyType, valType)

        // Type of this map literal must be the new map type
        if (!unify(node.type, mapType)) {
            val types = typesToString(node.type, mapType)
            throw IRConversionException("Map literal could not be inferred to have map type, " +
                "found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each key in the map's type must be a subtype of the map's key type
        node.keys.forEach({ key -> 
            val except = { ->
                val types = typesToString(key.type, keyType)
                throw IRConversionException("Map must have keys of same type, found " +
                        "${types[0]} and ${types[1]}", key.startLocation)
            }

            if (!subtype(key.type, keyType, except)) {
                except()
            }
        })

        // Each value in the map's type must be a subtype of the map's value type
        node.values.forEach({ value ->
            val except = { ->
                val types = typesToString(value.type, valType)
                throw IRConversionException("Map must have values of same type, found " +
                            "${types[0]} and ${types[1]}", value.startLocation)
            }

            if (!subtype(value.type, valType, except)) {
                except()
            }
        })
    }

    fun typeCheckTupleLiteral(
        node: TupleLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Tuple type is initially unknown, so set as tuple type with type variables for fields
        val nodeType = TupleType(node.elements.map { TypeVariable() })

        // Type of this tuple literal must be the given tuple type
        if (!unify(node.type, nodeType)) {
            val types = typesToString(node.type, nodeType)
            throw IRConversionException("Tuple literal could not be inferred to have tuple " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each element's type in the tuple must be a subtype of the corresponding element type
        node.elements.zip(nodeType.elementTypes).forEach { (element, expectedElementType) ->
            val except = { ->
                val types = typesToString(element.type, expectedElementType)
                throw IRConversionException("Cannot infer type for tuple element, expected " +
                        "${types[0]} but found ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, expectedElementType, except)) {
                except()
            }
        }
    }

    fun typeCheckVariable(
        node: VariableNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val info = symbolTable.getInfo(node.ident)
        if (info == null) {
            throw IRConversionException("Unknown identifier ${node.ident.name}", node.startLocation)
        }

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        // Since a type for an identifier is being found, a fresh version of that type must be
        // found (if applicable).
        val currentRepType = currentRepType(info.type)
        val expectedType = if (refresh && currentRepType is FunctionType) {
            findRepType(currentRepType, boundVars)
        } else {
            currentRepType
        }

        if (!unify(node.type, expectedType)) {
            val types = typesToString(node.type, expectedType)
            throw IRConversionException("Could not infer type for ${node.ident.name}, found " +
                    "${types[0]} but expected ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckUnaryMathOperator(
        node: UnaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Unify this node's type with its child's type
        if (!unify(node.node.type, node.type)) {
            val type = typeToString(node.node.type)
            throw IRConversionException("Unary math operator expects a number, found ${type}",
                    node.node.startLocation)
        }

        // Add deferred constraint to make sure node is int or float
        val unaryMathOperatorConstraint = UnaryMathOperatorConstraint(node, this)
        addDeferredConstraint(node.type, unaryMathOperatorConstraint)
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Unify this node's type with both its children's types
        if (!unify(node.left.type, node.type) ||
                !unify(node.right.type, node.type)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${types[0]} and ${types[1]}", node.right.startLocation)
        }

        // Add deferred constraint to make sure node is int or float
        val binaryMathOperatorConstraint = BinaryMathOperatorConstraint(node, this)
        addDeferredConstraint(node.type, binaryMathOperatorConstraint)
    }

    fun typeCheckLogicalAnd(
        node: LogicalAndNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Logical and evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical and, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical and must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical and expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical and expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckLogicalOr(
        node: LogicalOrNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Logical or evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical or, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical or must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical or expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical or expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckLogicalNot(
        node: LogicalNotNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Logical not evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical not, " +
                    "found ${type}", node.startLocation)
        }

        if (!unify(node.node.type, BoolType)) {
            val type = typeToString(node.node.type)
            throw IRConversionException("Logical not expects a bool, found ${type}",
                    node.startLocation)
        }
    }

    fun typeCheckEquality(
        node: EqualityNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Equality nodes evaluate to a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of comparison, " +
                    "found ${type}", node.startLocation)
        }

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Cannot check equality between different types, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckComparison(
        node: ComparisonNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Comparison nodes evaluate to a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of comparison, " +
                    "found ${type}", node.startLocation)
        }

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckAccess(node: AccessNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)
        
        val accessConstraint = AccessConstraint(node, boundVars, this)
        addDeferredConstraint(node.expr.type, accessConstraint)
    }

    fun typeCheckFieldAssignment(
        node: FieldAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.expr, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val fieldAssigmentConstraint = FieldAssignmentConstraint(node, this)
        addDeferredConstraint(node.expr.type, fieldAssigmentConstraint)
    }

    fun typeCheckKeyedAccess(
        node: KeyedAccessNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)

        val keyedAccessConstraint = KeyedAccessConstraint(node, this)
        addDeferredConstraint(node.container.type, keyedAccessConstraint)
    }

    fun typeCheckKeyedAssignment(
        node: KeyedAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val keyedAssignmentConstraint = KeyedAssignmentConstraint(node, this)
        addDeferredConstraint(node.container.type, keyedAssignmentConstraint)
    }

    fun typeCheckFunctionCall(
        node: FunctionCallNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.func, boundVars, refresh)
        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        val funcType = node.func.type

        // Each actual argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.actualArgs.map { arg ->
            val expectedArgType = TypeVariable()

            val except = { ->
                val types = typesToString(expectedArgType, arg.type)
                throw IRConversionException("Function expected argument of type " +
                        "${types[0]}, but found ${types[1]}", arg.startLocation)
            }

            if (!subtype(arg.type, expectedArgType, except)) {
                except()
            }

            expectedArgType
        }

        // Function call must evaluate to exact s
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, funcType)) {
            val types = typesToString(funcType, expectedFuncType)
            throw IRConversionException("Function inferred to have type " +
                    "${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckTupleTypeConstructor(
        node: TupleTypeConstructorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        // Create a fresh adt type for this type variant, since type params must be new
        val nodeType = node.adtVariant.adtSig.getFreshAdt()
        if (!unify(node.type, nodeType)) {
            val types = typesToString(nodeType, node.type)
            throw IRConversionException("Could not infer return type for type constructor, " +
                    "expected ${types[0]} but found ${types[1]}", node.startLocation)
        }

        // Find constructor arg types given the current adt params and adt variant
        val expectedArgTypes = node.adtVariant.getTypeConstructorWithParams(nodeType.typeParams)
        val actualArgTypes = node.actualArgs.map { arg -> arg.type }

        // If expected or actual args are empty, print special message if both aren't empty
        if (actualArgTypes.size == 0) {
            if (expectedArgTypes.size != 0) {
                throw IRConversionException("${node.adtVariant.name} expects arguments of " +
                        "type ${formatTypes(expectedArgTypes)}, but received no arguments",
                        node.startLocation)
            // If no args exist or were expected, there is nothing to type check since adt type
            // must have no params to infer.
            } else {
                return
            }
        } else if (expectedArgTypes.size == 0) {
            throw IRConversionException("${node.adtVariant.name} expects no arguments, but " +
                    "found arguments of type ${formatTypes(actualArgTypes)}",
                    node.startLocation)
        }

        // Each actual arg type must be a subtype of the corresponding expected arg type
        val except = { ->
            throw IRConversionException("${node.adtVariant.name} expected arguments of " +
                    "type ${formatTypes(expectedArgTypes)}, but found " +
                    "${formatTypes(actualArgTypes)}", node.startLocation)
        }

        val correctArgTypes = expectedArgTypes.size == actualArgTypes.size &&
                expectedArgTypes.zip(actualArgTypes)
                    .map({(expected, actual) -> subtype(actual, expected, except)})
                    .all({ x -> x })

        if (!correctArgTypes) {
            except()
        }
    }

    fun typeCheckRecordTypeConstructor(
        node: RecordTypeConstructorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.fields.forEach { (_, field) -> typeCheck(field, boundVars, refresh) }

        // Create a fresh adt type for this type variant, since type params must be new
        val nodeType = node.adtVariant.adtSig.getFreshAdt()
        if (!unify(node.type, nodeType)) {
            val types = typesToString(nodeType, node.type)
            throw IRConversionException("Could not infer return type for type constructor, " +
                    "expected ${types[0]} but found ${types[1]}", node.startLocation)
        }

        // Find constructor fields types given the current adt params and adt variant
        val expectedFieldTypes = node.adtVariant.getFieldsWithParams(nodeType.typeParams)

        // Make sure that every field's type is a subtype of the corresponding expected field type has the correct type
        node.fields.forEach { (fieldName, field) ->
            val expectedFieldType = expectedFieldTypes[fieldName]!!

            val except = { ->
                val types = typesToString(expectedFieldType, field.type)
                throw IRConversionException("${node.adtVariant.name} expected field ${fieldName} " +
                        "to have type ${types[0]}, but found ${types[1]}", field.startLocation)
            }

            if (!subtype(field.type, expectedFieldType, except)) {
                except()
            }
        }
    }

    fun typeCheckVariableAssignment(
        node: VariableAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val type = symbolTable.getInfo(node.lValue)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.lValue.name}", node.identLocation)
        }

        typeCheck(node.rValue, boundVars, refresh)

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        if (!unify(node.type, type)) {
            val types = typesToString(type, node.type)
            throw IRConversionException("Variable assignment should evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }

        // The value assigned to the variable should be a subtype of the variable's type
        val except = { ->
            val types = typesToString(node.type, node.rValue.type)
            throw IRConversionException("Type of ${node.lValue.name} is " +
                    "${types[0]}, but assigned ${types[1]}", node.rValue.startLocation)
        }

        if (!subtype(node.rValue.type, node.type, except)) {
            except()
        }
    }

    fun typeCheckPatternAssignment(
        node: PatternAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.pattern, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        // The type of the rValue should be a subtype of the pattern
        val except = { ->
            val types = typesToString(node.pattern.type, node.rValue.type)
            throw IRConversionException("Pattern has type ${types[0]}, " +
                    "but assigned ${types[1]}", node.rValue.startLocation)
        }

        if (!subtype(node.rValue.type, node.pattern.type, except)) {
            except()
        }

        // The evaluation type of the assignment should be the same as the pattern
        if (!unify(node.pattern.type, node.type)) {
            val types = typesToString(node.rValue.type, node.type)
            throw IRConversionException("Pattern assignment should evaluate to " +
                    "${types[0]}, but found ${types[1]}", node.rValue.startLocation)
        }
    }

    fun typeCheckVariableDefinition(
        node: VariableDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val varType = symbolTable.getInfo(node.ident)?.type
        if (varType == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}", node.identLocation)
        }

        typeCheck(node.expr, boundVars, refresh)

        // Variable definition node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Variable definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        if (node.typeAnnotation != null) {
            // Identifier's type variable must be equal to the type annotation
            if (!unify(node.typeAnnotation, varType)) {
                val types = typesToString(node.typeAnnotation, varType)
                throw IRConversionException("Variable expected to have type ${types[0]}, but " +
                        "found ${types[1]}", node.identLocation)
            }

            // If a type annotation is supplied, the expression must be a subtype of the annotation
            val except = { ->
                val types = typesToString(node.typeAnnotation, node.expr.type)
                throw IRConversionException("Variable has type ${types[0]}, but assigned " +
                        "${types[1]}", node.expr.startLocation)
            }

            if (!subtype(node.expr.type, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation is supplied, inferred type is the expression's inferred type
            if (!unify(node.expr.type, varType)) {
                val types = typesToString(varType, node.expr.type)
                throw IRConversionException("Variable has type ${types[0]}, but assigned " +
                        "${types[1]}", node.expr.startLocation)
            }
        }
    }

    fun typeCheckPatternDefinition(
        node: PatternDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.pattern, boundVars, refresh)
        typeCheck(node.expr, boundVars, refresh)

        // Pattern definition nodes evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Pattern definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        if (node.typeAnnotation != null) {
            // Type of pattern must be equal to type annotation
            if (!unify(node.pattern.type, node.typeAnnotation)) {
                val types = typesToString(node.typeAnnotation, node.pattern.type)
                throw IRConversionException("Pattern expected to have type ${types[0]}, but found " +
                        "${types[1]}", node.patternLocation)
            }

            // If a type annotation is supplied, the expression must be a subtype of the annotation
            val except = { ->
                val types = typesToString(node.typeAnnotation, node.expr.type)
                throw IRConversionException("Pattern has type ${types[0]}, " +
                            "but assigned ${types[1]}", node.expr.startLocation)
            }

            if (!subtype(node.expr.type, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation, expression type must be exactly equal to pattern's type
            if (!unify(node.expr.type, node.pattern.type)) {
                val types = typesToString(node.pattern.type, node.expr.type)
                throw IRConversionException("Pattern has type ${types[0]}, " +
                    "but assigned ${types[1]}", node.expr.startLocation)
            }
        }
    }

    fun typeCheckFunctionDefinition(
        node: FunctionDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val funcType = symbolTable.getInfo(node.ident)?.type

        if (funcType !is FunctionType) {
            throw IRConversionException("Unknown function ${node.ident.name}", node.identLocation)
        }

        val newBoundVars = boundVars.toHashSet()

        // Add all type variables in function type to the set of bound type variables
        funcType.getAllVariables().forEach { typeVar ->
            newBoundVars.add(typeVar)
        }

        // If this is a method definition, add all type implementation parameters to bound vars
        if (node is MethodDefinitionNode) {
            val type = symbolTable.getInfo(node.thisIdent)?.type!!
            type.getAllVariables().forEach { typeVar ->
                newBoundVars.add(typeVar)
            }
        }

        typeCheck(node.body, newBoundVars, refresh)

        // Function definition node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Function definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        // All returned types should be a subtype of the return type
        var foundReturn = false
        mapOverReturns(node.body, { retNode ->
            foundReturn = true
            val retType = retNode.expr?.type ?: UnitType

            val except = { ->
                val types = typesToString(funcType.returnType, retType)
                val location = retNode.expr?.startLocation ?: retNode.startLocation
                throw IRConversionException("${node.ident.name} must return ${types[0]} " +
                            "but found ${types[1]}", location)
            }

            // If a type annotation does not exist, returned types must all be exactly equal to
            // function's return type. If annotation does exist, returned types must only be
            // subtypes of this annotated type.
            val typesCheck = if (node.returnTypeAnnotation == null) {
                unify(retType, funcType.returnType)
            } else {
                subtype(retType, funcType.returnType, except)
            }

            if (!typesCheck) {
                except()
            }
        })

        // If no return statements were found, this function returns unit
        if (!foundReturn) {
            if (!unify(funcType.returnType, UnitType)) {
                val type = typeToString(funcType.returnType)
                throw IRConversionException("${node.ident.name} inferred to return unit, but " +
                        "expected ${type}", node.startLocation)
            }
        }

        // Main must always have return type int or unit, and a single optional argument
        // with type vec<string>.
        if (node.ident.name == "main") {
            // If there is a return, main must return an int
            if (foundReturn) {
                if (!unify(funcType.returnType, IntType)) {
                    val type = typeToString(funcType.returnType)
                    throw IRConversionException("Main function must return int, found ${type}",
                            node.startLocation)
                }
            }

            // Main must have single argument of type vec<string>, if any arguments exist
            val argType = VectorType(StringType)
            if ((funcType.argTypes.size >= 1 && !unify(funcType.argTypes[0], argType))
                    || funcType.argTypes.size >= 2) {
                throw IRConversionException("Main function must have a single argument of type " +
                        "${argType}", node.startLocation)
            }            
        }

        // If this function implements method signatures, this function type is a subtype
        // of each method signature type.
        if (node.signatures != null) {
            for (signature in node.signatures) {
                val except = { ->
                    val types = typesToString(signature, funcType)
                    throw IRConversionException("Method signature ${node.ident.name} has type " +
                            "${types[0]}, but implementation has type ${types[1]}",
                            node.startLocation)
                }

                if (!subtype(funcType, signature, except)) {
                    except()
                }
            }
        }
    }

    fun typeCheckLambda(node: LambdaNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        // Bind all types in the argument list
        val newBoundVars = boundVars.toHashSet()

        val argTypes = node.formalArgs.map { formalArg -> symbolTable.getInfo(formalArg)?.type!! }
        argTypes.forEach { type -> newBoundVars.addAll(type.getAllVariables()) }

        typeCheck(node.body, newBoundVars, refresh)

        // Create expected function type from argument list and new return type variable
        val returnType = TypeVariable()
        val funcType = FunctionType(argTypes, returnType)

        // Lambda expression node evaluates to this function type
        if (!unify(node.type, funcType)) {
            val types = typesToString(funcType, node.type)
            throw IRConversionException("Expected lambda expression to have type ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }

        // All returned types should have the same type, the return type of the lambda expression.
        // Note that implicitly returning traits in a lambda is not yet supported.
        mapOverReturns(node.body, { retNode ->
            val retType = retNode.expr?.type ?: UnitType
            if (!unify(retType, returnType)) {
                val types = typesToString(returnType, retType)
                val location = retNode.expr?.startLocation ?: retNode.startLocation
                throw IRConversionException("Lambda expression expected to return ${types[0]} " +
                        "but found ${types[1]}", location)
            }
        })
    }

    /**
     * Apply a function to every return rooted at a subtree, without recurring into nested
     * function definitions.
     */
    fun mapOverReturns(node: IRNode, func: (ReturnNode) -> Unit) {
        when (node) {
            is BlockNode -> node.nodes.map { n -> mapOverReturns(n, func) }
            is IfNode -> {
                mapOverReturns(node.conseq, func)
                if (node.altern != null) {
                    mapOverReturns(node.altern, func)
                }
            }
            is WhileNode -> mapOverReturns(node.body, func)
            is DoWhileNode -> mapOverReturns(node.body, func)
            is ForNode -> {
                if (node.init != null) {
                    mapOverReturns(node.init, func)
                }
                if (node.update != null) {
                    mapOverReturns(node.update, func)
                }
                mapOverReturns(node.body, func)
            }
            is MatchNode -> node.cases.forEach { (_, _, stmt) -> mapOverReturns(stmt, func) }
            is ReturnNode -> func(node)
        }
    }

    fun typeCheckBlock(node: BlockNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.nodes.forEach { child -> typeCheck(child, boundVars, refresh) }

        // If block is a statement or an expression with no cases, it must evaluate to unit
        if (!node.isExpression || node.nodes.isEmpty()) {
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("Block statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        // If block is a nonempty expression, it's type is a supertype of the last statement's type
        } else {
            val lastNode = node.nodes[node.nodes.size - 1]
            val except = { ->
                val types = typesToString(node.type, lastNode.type)
                throw IRConversionException("Block expression expected to evaluate to " + 
                        "${types[0]}, but found ${types[1]}", node.startLocation)
            }

            if (!subtype(lastNode.type, node.type, except)) {
                except()
            }
        }
    }

    fun typeCheckIf(node: IfNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of if must be a bool, but found ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.conseq, boundVars, refresh)

        if (node.altern != null) {
            typeCheck(node.altern, boundVars, refresh)
        }

        if (node.isExpression) {
            // If an expression, the if node must have an else case
            if (node.altern == null) {
                throw IRConversionException("If expression must have else case", node.startLocation)
            }

            // True case of if expression must evaluate to subtype of if expression's return type
            if (!unify(node.conseq.type, node.type)) {
                val types = typesToString(node.conseq.type, node.altern.type)
                throw IRConversionException("Both true and false cases of if expression must " +
                        "have the same type, found ${types[0]} and ${types[1]}", node.startLocation)
            }

            // False case of if expression must evaluate to subtype of if expression's return type
            if (!unify(node.altern.type, node.type)) {
                val types = typesToString(node.type, node.altern.type)
                throw IRConversionException("If expression expected to have type ${node.type}, " +
                        "but found ${types[1]}", node.startLocation)
            }
        } else {
            // If a statement, if cases do not have to have same type and if evaluates to unit
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("If statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        }
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // While node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("While loop should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of do while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // Do while node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Do while loop should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckFor(node: ForNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        if (node.init != null) {
            typeCheck(node.init, boundVars, refresh)
        }

        if (node.cond != null) {
            typeCheck(node.cond, boundVars, refresh)
            if (!unify(node.cond.type, BoolType)) {
                val type = typeToString(node.cond.type)
                throw IRConversionException("Condition of for must be a bool, but given ${type}",
                        node.cond.startLocation)
            }
        }

        if (node.update != null) {
            typeCheck(node.update, boundVars, refresh)
        }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("For loop should evaluate to the unit type, but found " +
                    "${type}", node.startLocation)
        }
    }

    fun typeCheckMatch(node: MatchNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)

        node.cases.forEach { (pattern, guard, statement) ->
            typeCheck(pattern, boundVars, false)

            // Bind all variables that are found in the pattern, including in child nodes
            val newBoundVars = boundVars.toHashSet()
            pattern.map { patNode ->
                patNode.type.getAllVariables().forEach { typeVar -> newBoundVars.add(typeVar) }
            }

            // Type check guard if it exists
            if (guard != null) {
                typeCheck(guard, newBoundVars, refresh)
            }

            typeCheck(statement, newBoundVars, refresh)
        }

        node.cases.forEach { (pat, guard, _) ->
            // All patterns must have same type as the matched expression
            if (!unify(pat.type, node.expr.type)) {
                val types = typesToString(node.expr.type, pat.type)
                throw IRConversionException("Patterns in match statement expected to have type " +
                        "${types[0]}, but found ${types[1]}", pat.startLocation)
            }

            // Guard statements, if they exist, must evaluate to bools
            if (guard != null) {
                if (!unify(guard.type, BoolType)) {
                    val type = typeToString(guard.type)
                    throw IRConversionException("Pattern matching guards must have type bool, " +
                            "but found ${type}", guard.startLocation)
                }
            }
        }

        if (node.isExpression) {
            // Evaluation type of match expression is evaluation type of cases
            val matchType = TypeVariable()
            if (!unify(node.type, matchType)) {
                val types = typesToString(node.type, matchType)
                throw IRConversionException("Match expression expected to have type " +
                        "${types[0]}, but found ${types[1]}", node.startLocation)
            }

            // If an expression, every case type must be a subtype of the match expression type
            node.cases.forEach { (_, _, case) ->
                val except = { ->
                    val types = typesToString(case.type, matchType)
                    throw IRConversionException("Cases in match expression must all have the " +
                            "same type, found ${types[0]} and ${types[1]}", case.startLocation)
                }

                if (!subtype(case.type, matchType, except)) {
                    except()
                }
            }
        } else {
            // If a statement, cases do not have to evaluate to same type and the entire match
            // node evaluates to the unit value.
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("Match statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        }
    }

    fun typeCheckReturn(node: ReturnNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        if (node.expr != null) {
            typeCheck(node.expr, boundVars, refresh)
        }

        // Return node evalutes to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckBreak(node: BreakNode) {
        // Break node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Break statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckContinue(node: ContinueNode) {
        // Continue node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    /**
     * Infer types for every identifier of the specified class in the symbol table. Be sure to
     * only infer types after all type checking and unification has taken place.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferSymbolTypes(idClass: IdentifierClass, freezeSymbols: Boolean) {
        // Infer types for every identifier of the specified class
        for ((_, identInfo) in symbolTable.identifiers) {
            if (identInfo.typeShouldBeInferred && !identInfo.typeIsInferred &&
                    identInfo.idClass == idClass) {
                identInfo.type = inferType(identInfo.type)

                if (freezeSymbols) {
                    identInfo.typeIsInferred = true
                }
            }
        }
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferFunctionTypes(freezeSymbols: Boolean) {
        inferSymbolTypes(IdentifierClass.FUNCTION, freezeSymbols)
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place, and after function types have been inferred.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferVariableTypes(freezeSymbols: Boolean) {
        inferSymbolTypes(IdentifierClass.VARIABLE, freezeSymbols)
    }

    /**
     * Infer types for every IR tree rooted at a list of nodes. Be sure to only infer types after
     * all type checking and unification has taken place, and after function and variable types
     * have been inferred.
     */
    fun inferIRTypes(root: IRNode) {
        // Infer return types for each IRNode
        root.map { node ->
            node.type = inferType(node.type)
        }
    }

    /**
     * Convert a single type into its string representation given a set of bound variables.
     */
    fun typeToString(type: Type): String {
        return formatType(currentRepType(type))
    }

    /**
     * Convert two types into their string representations given the same set of bound variables.
     */
    fun typesToString(type1: Type, type2: Type): List<String> {
        return formatTypes(listOf(currentRepType(type1), currentRepType(type2)))
    }
}
