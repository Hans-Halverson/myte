package myte.ir

import myte.eval.builtins.*
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
    private val typeVarToNode: MutableMap<OpenTypeVariable, TypeEquivalenceNode> = mutableMapOf()
    private var constraintGraph: ConstraintGraph = ConstraintGraph()

    /**
     * Reset for a new line from the REPL.
     */
    fun resetForReplLine(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
        constraintGraph = ConstraintGraph()
    }

    /**
     * Add a type variable to the set of equivalence classes, if it does not already exist, and
     * return the equivalence class node for this type variable.
     */
    private fun addTypeVar(typeVar: OpenTypeVariable): TypeEquivalenceNode {
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
    fun findRepNode(typeVar: OpenTypeVariable): TypeEquivalenceNode {
        // The representative node of a type is found by finding the root of its equivalence node
        var node = addTypeVar(typeVar)
        return findRoot(node)
    }

    /**
     * Set the representative node of the given type variable to be resolved to the given type.
     * Returns true if successful, false if the type variable has already been resolved to an
     * an incompatible type.
     */
    private fun resolveType(typeVar: OpenTypeVariable, resolvedType: Type): Boolean {
        val equivNode = findRepNode(typeVar)
        if (equivNode.resolvedType !is OpenTypeVariable && equivNode.resolvedType != resolvedType) {
            return false
        }

        // Set resolved type
        equivNode.resolvedType = resolvedType

        constraintGraph.resolveVariable(typeVar)

        return true
    }

    /**
     * Add a constraint to the given type variable.
     */
    fun addConstraint(
        constraint: Constraint,
        dependencies: Set<Type>
    ) {
        // Filter down to only dependencies whose rep types are still open type variables
        val openTypeVars = dependencies.mapNotNull({
            if (it is OpenTypeVariable) {
                val repType = findRepNode(it).resolvedType
                if (repType is OpenTypeVariable) {
                    repType
                } else {
                    null
                }
            } else {
                null
            }
        }).toSet()

        // Apply this constraint if there are no open type variables, otherwise add it to graph
        if (openTypeVars.isEmpty()) {
            constraint.resolve()
        } else {
            constraintGraph.addConstraint(constraint, openTypeVars)
        }
    }

    /**
     * Infer a final type for the given input type.
     */
    private fun inferType(type: Type): Type {
        return currentRepType(type)
    }

    /**
     * Assert that all outstanding constraints are resolved.
     */
    fun assertConstraintsSolved() {
        if (!constraintGraph.constraintDepends.isEmpty()) {
            for ((cons, _) in constraintGraph.constraintDepends) {
                cons.assertUnresolved()
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
        val repType = if (type is OpenTypeVariable) findRepNode(type).resolvedType else type

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
            // Find the rep type for each type parameter and reconstruct trait with correct sig
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
                    if (boundVar is OpenTypeVariable) {
                        findRepNode(boundVar).resolvedType
                    } else {
                        boundVar 
                    }
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
                    val newVar = OpenTypeVariable()
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
        if (type1 is OpenTypeVariable && type2 is OpenTypeVariable) {
            val rep1 = findRepNode(type1)
            val rep2 = findRepNode(type2)

            // Choose the lower ranked node to be the root node
            if (rep1.rank > rep2.rank) {
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
        } else if (type1 is OpenTypeVariable && type2 !is OpenTypeVariable) {
            if (occursIn(type1, type2)) {
                return false
            }

            return resolveType(type1, type2)
        // If merging a type with a type variable, if occurs check passes resolve type variable
        } else if (type1 !is OpenTypeVariable && type2 is OpenTypeVariable) {
            if (occursIn(type2, type1)) {
                return false
            }

            return resolveType(type2, type1)
        // Merge types should only be called if at least one type is a type variable
        } else {
            return false
        }
    }

    /**
     * Replace the root of an unresolved type variable to a new unresolved type variable.
     */
    private fun replaceRoot(oldRoot: TypeEquivalenceNode, newRoot: TypeEquivalenceNode) {
        val oldType = oldRoot.resolvedType as OpenTypeVariable
        val newType = newRoot.resolvedType as OpenTypeVariable

        oldRoot.parent = newRoot

        // Move all constraints from old root to the new root
        constraintGraph.moveConstraints(oldType, newType)
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
        val type1 = if (t1 is OpenTypeVariable) findRepNode(t1).resolvedType else t1
        val type2 = if (t2 is OpenTypeVariable) findRepNode(t2).resolvedType else t2

        // If both types are already identical, they are already unified
        if (type1 == type2) {
            return true
        // If at least one type is a type variable, merge types together
        } else if (type1 is OpenTypeVariable || type2 is OpenTypeVariable) {
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

    fun subtype(sbType: Type, spType: Type, except: () -> Unit, allowVars: Boolean = true): Boolean {
        // If type variables, work with their representative types
        val subType = if (sbType is OpenTypeVariable) findRepNode(sbType).resolvedType else sbType
        val superType = if (spType is OpenTypeVariable) findRepNode(spType).resolvedType else spType

        // The subtype relation is reflexive
        if (subType == superType) {
            return true
        // If not allowing variables, fail if either type is a type variable (when unequal)
        } else if (!allowVars && (subType is OpenTypeVariable || superType is OpenTypeVariable)) {
            return false
        // If both types are type variables, add supertype and subtype bounds to type variables
        } else if (subType is OpenTypeVariable && superType is OpenTypeVariable) {
            val subtypingConstraint =
                    BidirectionalSubtypingConstraint(subType, superType, except, this)
            val subtypingConstraint2 =
                    BidirectionalSubtypingConstraint(subType, superType, except, this)
            addConstraint(subtypingConstraint, setOf(subType))
            addConstraint(subtypingConstraint2, setOf(superType))

            return true
        // If the supertype only is a type variable, add subtype bound to type variable
        } else if (subType !is OpenTypeVariable && superType is OpenTypeVariable) {
            val subtypeConstraint = SubtypeConstraint(subType, superType, except, this)
            addConstraint(subtypeConstraint, setOf(superType))
            return true
        // If the subtype only is a type variable, we can apply constraints unless super is trait
        } else if (subType is OpenTypeVariable) {
            return when (superType) {
                // If supertype is a basic, unparameterized type or a parameterized type with
                // invariant type parameters, simply unify both types.
                is UnitType -> unify(subType, superType)
                is BoolType -> unify(subType, superType)
                is ByteType -> unify(subType, superType)
                is IntType -> unify(subType, superType)
                is FloatType -> unify(subType, superType)
                is DoubleType -> unify(subType, superType)
                is StringType -> unify(subType, superType)
                is AlgebraicDataType -> unify(subType, superType)
                is VectorType -> unify(subType, superType)
                is SetType -> unify(subType, superType)
                is MapType -> unify(subType, superType)
                is TypeParameter -> unify(subType, superType)
                // If supertype is a tuple, subtype must also be a tuple with covariant elements
                is TupleType -> {
                    val tupleSubType = TupleType(superType.elementTypes.map { OpenTypeVariable() })
                    return unify(subType, tupleSubType) &&
                            subtype(tupleSubType, superType, except, allowVars)
                }
                // If supertype is a function, subtype must also be a func with correct variance
                is FunctionType -> {
                    val funcSubType = FunctionType(superType.argTypes.map { OpenTypeVariable() },
                            OpenTypeVariable())
                    return unify(subType, funcSubType) &&
                            subtype(funcSubType, superType, except, allowVars)
                }
                // If supertype is a trait, add supertype bound to type variable
                is TraitType -> {
                    val supertypeConstraint = SupertypeConstraint(subType, superType, except, this)
                    addConstraint(supertypeConstraint, setOf(subType))
                    return true
                }
                // This should never be reached, as the both types being variables is handled above
                is OpenTypeVariable -> throw Exception("This case should be unreachable")
            }
        // Tuples types are covariant in element types, since they are immutable
        } else if (subType is TupleType && superType is TupleType) {
            return subType.elementTypes.size == superType.elementTypes.size &&
                    subType.elementTypes.zip(superType.elementTypes)
                        .all({ (e1, e2) -> subtype(e1, e2, except, allowVars) })
        // Function types are contravariant in their element types, and covariant in return type
        } else if (subType is FunctionType && superType is FunctionType) {
            return subType.argTypes.size == superType.argTypes.size &&
                    subtype(subType.returnType, superType.returnType, except, allowVars) &&
                    subType.argTypes.zip(superType.argTypes)
                        .all({ (a1, a2) -> subtype(a2, a1, except, allowVars) })
        // Algebraic data types are invariant in their type parameters, since they are mutable
        } else if (subType is AlgebraicDataType && superType is AlgebraicDataType) {
            return subType.typeParams.size == superType.typeParams.size &&
                    subType.typeParams.zip(superType.typeParams)
                        .all({ (p1, p2) -> unify(p1, p2) })
        // Trait types are invariant in their type parameters, since they can only be implemented by
        // mutable algebraic data types, and contain no state of their own.
        } else if (subType is TraitType && superType is TraitType) {
            return subType.typeParams.size == superType.typeParams.size &&
                    subType.typeParams.zip(superType.typeParams)
                        .all({ (p1, p2) -> unify(p1, p2) })
        // For an ADT to be a subtype of a trait, it must extend that trait
        } else if (superType is TraitType) {
            val extendedTrait = subType.sig.traits.find { extTrait ->
                extTrait.traitSig == superType.traitSig
            }

            if (extendedTrait == null) {
                return false
            }

            // Find parameterized type of trait by performing rep substitution from trait sig params
            // to actual trait params in extended trait params.
            val paramsMap = (subType.sig.typeParams as List<TypeVariable>)
                    .zip(subType.listTypeParams()).toMap()
            val substTraitParams = extendedTrait.typeParams.map { typeParam ->
                findRepSubstitution(typeParam, paramsMap)
            }

            // Subtyping a trait is invariant in all type parameters
            return substTraitParams.zip(superType.typeParams)
                    .map({ (p1, p2) -> unify(p1, p2) })
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
     * Return the lowest shared supertype of the two given types.
     */
    fun lowestCommonSupertype(t1: Type, t2: Type): Type? {
        val type1 = currentRepType(t1)
        val type2 = currentRepType(t2)

        return when {
            // Unparameterized types must be equal
            type1 is UnitType && type2 is UnitType ||
            type1 is BoolType && type2 is BoolType ||
            type1 is StringType && type2 is StringType ||
            type1 is ByteType && type2 is ByteType ||
            type1 is IntType && type2 is IntType ||
            type1 is FloatType && type2 is FloatType ||
            type1 is DoubleType && type2 is DoubleType -> type1
            // Invariant types must be equal to have a shared type
            type1 is VectorType && type2 is VectorType ||
            type1 is SetType && type2 is SetType ||
            type1 is MapType && type2 is MapType ||
            type1 is AlgebraicDataType && type2 is AlgebraicDataType ||
            type1 is TraitType && type2 is TraitType ||
            type1 is TypeVariable && type2 is TypeVariable -> if (type1 == type2) type1 else null
            // Tuples are covariant in element types, so the lowest common supertype is a tuple
            // of the lowest common supertypes of the element pairs.
            type1 is TupleType && type2 is TupleType -> {
                val elementTypes = type1.elementTypes.zip(type2.elementTypes)
                        .map({ (e1, e2) -> lowestCommonSupertype(e1, e2) })

                if (elementTypes.any({ it == null })) {
                    null
                } else {
                    TupleType(elementTypes.filterNotNull())
                }
            }
            // Functions are covariant in arg types and contravariant in return type, so find the
            // lowest common supertype of arg types and highest common subtype of return types.
            type1 is FunctionType && type2 is FunctionType -> {
                val argTypes = type1.argTypes.zip(type2.argTypes)
                        .map({ (a1, a2) -> highestCommonSubtype(a1, a2) })
                val returnType = lowestCommonSupertype(type1.returnType, type2.returnType)

                if (argTypes.any({ it == null }) || returnType == null) {
                    null
                } else {
                    FunctionType(argTypes.filterNotNull(), returnType)
                }
            }
            else -> {
                // Construct set of all trait types implemented by type1
                val type1ParamsMap = (type1.sig.typeParams as List<TypeVariable>)
                        .zip(type1.listTypeParams()).toMap()
                val type1Traits = if (type1 is TraitType) {
                    setOf(type1)
                } else {
                    type1.sig.traits.map({ findRepSubstitution(it, type1ParamsMap) }).toSet()
                }

                // Construct set of all trait types implemented by type2
                val type2ParamsMap = (type2.sig.typeParams as List<TypeVariable>)
                        .zip(type2.listTypeParams()).toMap()
                val type2Traits = if (type2 is TraitType) {
                    setOf(type2)
                } else {
                    type2.sig.traits.map({ findRepSubstitution(it, type2ParamsMap) }).toSet()
                }

                // If there is exactly one shared trait it is the common supertype, otherwise fail
                val sharedTraits = type1Traits.intersect(type2Traits)
                if (sharedTraits.size == 1) {
                    sharedTraits.first()
                } else {
                    null
                }
            }
        }
    }

    /**
     * Return the highest shared subtype of the two given types.
     */
    fun highestCommonSubtype(t1: Type, t2: Type): Type? {
        val type1 = currentRepType(t1)
        val type2 = currentRepType(t2)

        return when {
            // Unparameterized types must be equal
            type1 is UnitType && type2 is UnitType ||
            type1 is BoolType && type2 is BoolType ||
            type1 is StringType && type2 is StringType ||
            type1 is ByteType && type2 is ByteType ||
            type1 is IntType && type2 is IntType ||
            type1 is FloatType && type2 is FloatType ||
            type1 is DoubleType && type2 is DoubleType -> type1
            // Invariant types must be equal to have a shared type
            type1 is VectorType && type2 is VectorType ||
            type1 is SetType && type2 is SetType ||
            type1 is MapType && type2 is MapType ||
            type1 is AlgebraicDataType && type2 is AlgebraicDataType ||
            type1 is TraitType && type2 is TraitType ||
            type1 is TypeVariable && type2 is TypeVariable -> if (type1 == type2) type1 else null
            // Tuples are covariant in element types, so the highest common subtype is a tuple
            // of the highest common subtypes of the element pairs.
            type1 is TupleType && type2 is TupleType -> {
                val elements = type1.elementTypes.zip(type2.elementTypes)
                        .map({ (e1, e2) -> highestCommonSubtype(e1, e2) })

                if (elements.any({ it == null })) {
                    null
                } else {
                    TupleType(elements.filterNotNull())
                }
            }
            // Functions are covariant in arg types and contravariant in return type, so find the
            // highest common subtype of arg types and lowest common supertype of return types.
            type1 is FunctionType && type2 is FunctionType -> {
                val argTypes = type1.argTypes.zip(type2.argTypes)
                        .map({ (a1, a2) -> lowestCommonSupertype(a1, a2) })
                val returnType = highestCommonSubtype(type1.returnType, type2.returnType)

                if (argTypes.any({ it == null }) || returnType == null) {
                    null
                } else {
                    FunctionType(argTypes.filterNotNull(), returnType)
                }
            }
            type1 is TraitType && type2 !is TraitType -> {
                // Construct set of all trait types implemented by type2
                val type2ParamsMap = (type2.sig.typeParams as List<TypeVariable>)
                        .zip(type2.listTypeParams()).toMap()
                val type2Traits = 
                    type2.sig.traits.map({ findRepSubstitution(it, type2ParamsMap) }).toSet()

                // Return type2 if type1 is a trait implemented by type2, otherwise fail
                if (type2Traits.contains(type1)) {
                    type2
                } else {
                    null
                }
            }
            type1 !is TraitType && type2 is TraitType -> {
                // Construct set of all trait types implemented by type1
                val type1ParamsMap = (type1.sig.typeParams as List<TypeVariable>)
                        .zip(type1.listTypeParams()).toMap()
                val type1Traits = 
                    type1.sig.traits.map({ findRepSubstitution(it, type1ParamsMap) }).toSet()

                // Return type1 if type2 is a trait implemented by type1, otherwise fail
                if (type1Traits.contains(type2)) {
                    type1
                } else {
                    null
                }
            }
            else -> null
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
            is IntegralLiteralNode -> typeCheckIntegralLiteral(node)
            is DecimalLiteralNode -> typeCheckDecimalLiteral(node)
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
            is BuiltinNode -> typeCheckBuiltin(node, boundVars, refresh)
            is BuiltinMethodNode -> typeCheckBuiltinMethod(node, boundVars, refresh)
            is AccessNode -> typeCheckAccess(node, boundVars, refresh)
            is FieldAssignmentNode -> typeCheckFieldAssignment(node, boundVars, refresh)
            is IndexNode -> typeCheckIndex(node, boundVars, refresh)
            is IndexAssignNode -> typeCheckIndexAssign(node, boundVars, refresh)
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
            is ForEachNode -> typeCheckForEach(node, boundVars, refresh)
            is MatchNode -> typeCheckMatch(node, boundVars, refresh)
            is ReturnNode -> typeCheckReturn(node, boundVars, refresh)
            is BreakNode -> typeCheckBreak(node)
            is ContinueNode -> typeCheckContinue(node)
            // Wrapper nodes simply pass type checking to their children
            is WrapperNode -> typeCheck(node.node, boundVars, refresh)
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

    fun typeCheckIntegralLiteral(node: IntegralLiteralNode) {
        if (!unify(node.type, IntType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Int literal coud not be inferred to have type int, " +
                    "found ${type}", node.startLocation)
        }
    }

    fun typeCheckDecimalLiteral(node: DecimalLiteralNode) {
        if (!unify(node.type, DoubleType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Double literal coud not be inferred to have type " +
                    "double, found ${type}", node.startLocation)
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
        val vectorType = VectorType(OpenTypeVariable())

        // Type of this vector literal must be a vector of the element type
        if (!unify(node.type, vectorType)) {
            val types = typesToString(node.type, vectorType)
            throw IRConversionException("Vector literal could not be inferred to have vector " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Add a constraint which will determine element type once all elements are resolved
        if (!node.elements.isEmpty()) {
            val vectorLiteralConstraint = VectorLiteralConstraint(node, this)
            addConstraint(vectorLiteralConstraint, node.elements.map({ it.type }).toSet())
        }
    }

    fun typeCheckSetLiteral(
        node: SetLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Set type is initially unknown, so set as set type with new type variable param
        val setType = SetType(OpenTypeVariable())

        // Type of this set literal must be a set of the element type
        if (!unify(node.type, setType)) {
            val types = typesToString(node.type, setType)
            throw IRConversionException("Set literal could not be inferred to have set " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Add a constraint which will determine element type once all elements are resolved
        if (!node.elements.isEmpty()) {
            val setLiteralConstraint = SetLiteralConstraint(node, this)
            addConstraint(setLiteralConstraint, node.elements.map({ it.type }).toSet())
        }
    }

    fun typeCheckMapLiteral(
        node: MapLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.keys.forEach { key -> typeCheck(key, boundVars, refresh) }
        node.values.forEach { value -> typeCheck(value, boundVars, refresh) }

        // Map type is initially unknown, so set as map type with new key and value params
        val mapType = MapType(OpenTypeVariable(), OpenTypeVariable())

        // Type of this map literal must be the new map type
        if (!unify(node.type, mapType)) {
            val types = typesToString(node.type, mapType)
            throw IRConversionException("Map literal could not be inferred to have map type, " +
                "found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Add a constraint which will determine key and value types once all elements are resolved
        if (!node.keys.isEmpty()) {
            val mapLiteralConstraint = MapLiteralConstraint(node, this)
            addConstraint(mapLiteralConstraint,
                    node.keys.map({ it.type }).toSet() + node.values.map({ it.type }).toSet())
        }
    }

    fun typeCheckTupleLiteral(
        node: TupleLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Type of tuple node is constructed from types of elements
        val tupleType = TupleType(node.elements.map { it.type })
        if (!unify(node.type, tupleType)) {
            val types = typesToString(node.type, tupleType)
            throw IRConversionException("Tuple literal found to have type ${types[0]}, but " +
                    "expected ${types[1]}", node.startLocation)
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

        // Find the parameterized trait type for this operator based on the expression type
        val traitType = when (node) {
            is IdentityNode -> UNARY_PLUS_TRAIT_SIG.createTypeWithParams(listOf(node.node.type))
            is NegateNode -> UNARY_MINUS_TRAIT_SIG.createTypeWithParams(listOf(node.node.type))
        }

        // The left hand side must implement the correct operator trait
        val except = { ->
            val types = typesToString(node.node.type, traitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.startLocation)
        }

        if (!subtype(node.node.type, traitType, except)) {
            except()
        }

        // Unify this node's type with its child's type
        if (!unify(node.node.type, node.type)) {
            val types = typesToString(node.type, node.node.type)
            throw IRConversionException("Unary operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Find the parameterized trait type for this operator based on the right hand side
        val traitType = when (node) {
            is AddNode -> ADD_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is SubtractNode -> SUBTRACT_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is MultiplyNode -> MULTIPLY_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is DivideNode -> DIVIDE_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is ExponentNode -> POWER_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is RemainderNode -> REMAINDER_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
        }

        // The left hand side must implement the correct operator trait
        val except = { ->
            val types = typesToString(node.left.type, traitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.left.startLocation)
        }

        if (!subtype(node.left.type, traitType, except)) {
            except()
        }

        // The return type of this trait is the type of the right hand side
        if (!unify(node.type, node.right.type)) {
            val types = typesToString(node.type, node.right.type)
            throw IRConversionException("Binary operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }
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

        // Find the correctly parameterized comparable trait type (parameterized by right hand side)
        val comparable = COMPARABLE_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))

        // The left hand side must implement the comparable trait
        val except = { ->
            val types = typesToString(node.left.type, comparable)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.left.startLocation)
        }

        if (!subtype(node.left.type, comparable, except)) {
            except()
        }

        // The return type of a comparison is always a boolean
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Binary operation expected to evaluate to bool, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckAccess(node: AccessNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)
        
        val accessConstraint = AccessConstraint(node, boundVars, this)
        addConstraint(accessConstraint, setOf(node.expr.type))
    }

    fun typeCheckFieldAssignment(
        node: FieldAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.expr, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val fieldAssigmentConstraint = FieldAssignmentConstraint(node, this)
        addConstraint(fieldAssigmentConstraint, setOf(node.expr.type))
    }

    fun typeCheckIndex(
        node: IndexNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)

        // The result type of the index is unknown so far, so create type variable to hold it
        val indexResultType = OpenTypeVariable()
        val indexTraitType = INDEX_TRAIT_SIG
                .createTypeWithParams(listOf(node.key.type, indexResultType))

        // The index operation must evaluate to the result type of the index
        if (!unify(indexResultType, node.type)) {
            val types = typesToString(indexResultType, node.type)
            throw IRConversionException("Index operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.indexLocation)
        }

        // The container must be a subtype of the Index trait with the correct key and value types
        val except = { ->
            val types = typesToString(node.container.type, indexTraitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.indexLocation)
        }

        if (!subtype(node.container.type, indexTraitType, except)) {
            except()
        }
    }

    fun typeCheckIndexAssign(
        node: IndexAssignNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val indexAssignTraitType = INDEX_ASSIGN_TRAIT_SIG
                .createTypeWithParams(listOf(node.key.type, node.rValue.type))

        // The index assign operation must evaluate to the result type of the index
        if (!unify(node.rValue.type, node.type)) {
            val types = typesToString(node.rValue.type, node.type)
            throw IRConversionException("Index assign operation expected to evaluate to " +
                    "${types[0]}, but found ${types[1]}", node.indexLocation)
        }

        // The container must be a subtype of the IndexAssign trait with the correct key and value
        val except = { ->
            val types = typesToString(node.container.type, indexAssignTraitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.indexLocation)
        }

        if (!subtype(node.container.type, indexAssignTraitType, except)) {
            except()
        }
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
            val expectedArgType = OpenTypeVariable()

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

        // Function call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, funcType)) {
            val types = typesToString(funcType, expectedFuncType)
            throw IRConversionException("Function inferred to have type " +
                    "${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBuiltin(node: BuiltinNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.args.forEach { arg -> typeCheck(arg, boundVars, refresh) }

        // Refresh type variables in builtin type
        val funcType = findRepType(node.builtin.type, boundVars)

        // Each argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.args.map { arg ->
            val expectedArgType = OpenTypeVariable()

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

        // Function call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, funcType)) {
            val types = typesToString(funcType, expectedFuncType)
            throw IRConversionException("Builtin function ${node.builtin.name} inferred to have " +
                    "type ${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBuiltinMethod(
        node: BuiltinMethodNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.recv, boundVars, refresh)
        node.args.forEach { arg -> typeCheck(arg, boundVars, refresh) }

        val (recvType, methodType) = if (node.builtin.receiverType is VectorType) {
            val recvType = VectorType(OpenTypeVariable())
            val paramsMap = mapOf(node.builtin.receiverType.elementType as TypeVariable to
                    recvType.elementType)
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else if (node.builtin.receiverType is SetType) {
            val recvType = SetType(OpenTypeVariable())
            val paramsMap = mapOf(node.builtin.receiverType.elementType as TypeVariable to
                    recvType.elementType)
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else if (node.builtin.receiverType is MapType) {
            val recvType = MapType(OpenTypeVariable(), OpenTypeVariable())
            val paramsMap = mapOf(
                    node.builtin.receiverType.keyType as TypeVariable to recvType.keyType,
                    node.builtin.receiverType.valType as TypeVariable to recvType.valType
            )
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else {
            Pair(node.builtin.receiverType, node.builtin.type)
        }

        if (!unify(recvType, node.recv.type)) {
            val types = typesToString(recvType, node.recv.type)
            throw IRConversionException("Receiver for builtin method ${node.builtin.name} " +
                    "expected to have type ${types[0]}, but found ${types[1]}",
                    node.recv.startLocation)
        }

        // Each argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.args.map { arg ->
            val expectedArgType = OpenTypeVariable()

            val except = { ->
                val types = typesToString(expectedArgType, arg.type)
                throw IRConversionException("Builtin method ${node.builtin.name} expected " +
                        "argument of type ${types[0]}, but found ${types[1]}", arg.startLocation)
            }

            if (!subtype(arg.type, expectedArgType, except)) {
                except()
            }

            expectedArgType
        }

        // Method call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, methodType)) {
            val types = typesToString(methodType, expectedFuncType)
            throw IRConversionException("Builtin method ${node.builtin.name} inferred to have " +
                    "type ${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
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

            // All returned types must be subtypes of the annotated type.
            if (!subtype(retType, funcType.returnType, except)) {
                except()
            }
        })

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
        val returnType = OpenTypeVariable()
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
                node.altern?.let { mapOverReturns(it, func) }
            }
            is WhileNode -> mapOverReturns(node.body, func)
            is DoWhileNode -> mapOverReturns(node.body, func)
            is ForNode -> {
                node.init?.let { mapOverReturns(it, func) }
                node.update?.let { mapOverReturns(it, func) }
                mapOverReturns(node.body, func)
            }
            is ForEachNode -> {
                mapOverReturns(node.iterable, func)
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

        node.altern?.let { typeCheck(it, boundVars, refresh) }

        if (node.isExpression) {
            // If an expression, the if node must have an else case
            val altern = node.altern
            if (altern == null) {
                throw IRConversionException("If expression must have else case", node.startLocation)
            }

            // True case of if expression must evaluate to subtype of if expression's return type
            if (!unify(node.conseq.type, node.type)) {
                val types = typesToString(node.conseq.type, altern.type)
                throw IRConversionException("Both true and false cases of if expression must " +
                        "have the same type, found ${types[0]} and ${types[1]}", node.startLocation)
            }

            // False case of if expression must evaluate to subtype of if expression's return type
            if (!unify(altern.type, node.type)) {
                val types = typesToString(node.type, altern.type)
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
        node.init?.let { typeCheck(it, boundVars, refresh) }

        node.cond?.let { cond ->
            typeCheck(cond, boundVars, refresh)
            if (!unify(cond.type, BoolType)) {
                val type = typeToString(cond.type)
                throw IRConversionException("Condition of for must be a bool, but given ${type}",
                        cond.startLocation)
            }
        }

        node.update?.let { typeCheck(it, boundVars, refresh) }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("For loop should evaluate to the unit type, but found " +
                    "${type}", node.startLocation)
        }
    }

    fun typeCheckForEach(node: ForEachNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.lValue, boundVars, refresh)
        typeCheck(node.iterable, boundVars, refresh)

        // Iterable expression must implement iterable trait
        val forEachPatternType = OpenTypeVariable()
        val iterableTraitType = ITERABLE_TRAIT_SIG.createTypeWithParams(listOf(forEachPatternType))

        var except = { ->
            val types = typesToString(node.iterable.type, iterableTraitType)
            throw IRConversionException("Iterable in for each loop inferred to have type " +
                    "${types[0]}, but must implement ${types[1]}", node.iterable.startLocation)
        }

        if (!subtype(node.iterable.type, iterableTraitType, except)) {
            except()
        }

        if (node.typeAnnotation != null) {
            // Patterns's type variable must be equal to the type annotation
            if (!unify(node.typeAnnotation, node.lValue.type)) {
                val types = typesToString(node.typeAnnotation, node.lValue.type)
                throw IRConversionException("Pattern expected to have type ${types[0]}, but " +
                        "found ${types[1]}", node.lValue.startLocation)
            }

            // If a type annotation is supplied, the iterable must be a subtype of the annotation
            except = { ->
                val types = typesToString(node.typeAnnotation, forEachPatternType)
                throw IRConversionException("Pattern has type ${types[0]}, but assigned " +
                        "${types[1]}", node.lValue.startLocation)
            }

            if (!subtype(forEachPatternType, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation is supplied, inferred type is the iterable's inferred type
            if (!unify(forEachPatternType, node.lValue.type)) {
                val types = typesToString(node.lValue.type, forEachPatternType)
                throw IRConversionException("Pattern has type ${types[0]}, but assigned " +
                        "${types[1]}", node.lValue.startLocation)
            }
        }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("For each loop should evaluate to the unit type, but " +
                    "found ${type}", node.startLocation)
        }
    }

    fun typeCheckMatch(node: MatchNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)

        node.cases.forEach { (pattern, guard, statement) ->
            typeCheck(pattern, boundVars, false)

            // Bind all variables that are found in the pattern, including in child nodes
            val newBoundVars = boundVars.toHashSet()
            pattern.forEach { patNode ->
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
            val matchType = OpenTypeVariable()
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
        node.expr?.let { typeCheck(it, boundVars, refresh) }

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
        root.forEach { node ->
            // Wrapper nodes should be ignored
            if (node !is WrapperNode) {
                //println("$node has tvar ${node.type}")
                node.type = inferType(node.type)
            }
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
