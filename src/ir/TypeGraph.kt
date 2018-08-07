package myte.ir

import myte.shared.*

/**
 * A node in the forest of type equivalence classes.
 *
 * @property type the type contained at this node
 * @property parent the (optional) parent node in the forest of type equivalence classes. This type
 *           is in the same equivalence class as its parent, and if the parent is null, this type
 *           is the representative for its equivalence class.
 * @property rank an upper bound on the longest path from this node to a leaf
 */
sealed class TypeEquivalenceNode(
    var parent: TypeEquivalenceNode? = null,
    var rank: Int = 0
) {
    // An equivalence node is a root only when it does not have a parent
    val isRoot: Boolean
        get() = parent == null
}

class ResolvedNode(val type: Type) : TypeEquivalenceNode() {
    override fun toString(): String = "ResolvedNode(type: $type)"
}

class UnresolvedNode(
    val typeVar: TypeVariable,
    val subTypes: MutableSet<Type> = mutableSetOf(),
    val superTypes: MutableSet<Type> = mutableSetOf(),
    val subTypeVars: MutableSet<TypeVariable> = mutableSetOf(),
    val superTypeVars: MutableSet<TypeVariable> = mutableSetOf()
) : TypeEquivalenceNode() {
    override fun toString(): String = "UnresolvedNode(typeVar: $typeVar, subTypes: $subTypes, " +
            "superTypes: $superTypes, subTypeVars: $subTypeVars, superTypeVars: $superTypeVars)"
}

class TypeGraph(val typeEnvironment: TypeEnvironment) {
    private val typeVarToNode: MutableMap<TypeVariable, TypeEquivalenceNode> = mutableMapOf()
    private val deferredConstraints: MutableMap<TypeVariable, MutableList<DeferredConstraint>> =
            mutableMapOf()
    private var unresolvedVars: MutableSet<TypeVariable> = mutableSetOf()

    private val refreshedVars: MutableMap<TypeVariable, MutableList<TypeVariable>> = mutableMapOf()

    /**
     * Add a type variable to the set of equivalence classes, if it does not already exist, and
     * return the equivalence class node for this type variable.
     */
    private fun addTypeVar(typeVar: TypeVariable): TypeEquivalenceNode {
        val typeEquivNode = typeVarToNode[typeVar]
        if (typeEquivNode == null) {
            val newEquivNode = UnresolvedNode(typeVar)
            typeVarToNode[typeVar] = newEquivNode
            unresolvedVars.add(typeVar)

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
     * Find a shallow representative of the given type. This representative does not recursively
     * find the representatives of its children.
     */
    fun findShallowRepType(type: Type): Type {
        return if (type is TypeVariable) {
            val repNode = findRepNode(type)
            when (repNode) {
                is UnresolvedNode -> repNode.typeVar
                is ResolvedNode -> repNode.type
            }
        } else {
            type
        }
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
        val repType = findShallowRepType(type)

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
                val repBoundVars = boundVars.map(this::findShallowRepType)

                if (repBoundVars.contains(repType)) {
                    return repType
                }

                // If not yet bound, return mapped variable or add to map if not yet mapped
                val mappedVar = mappedVars[repType]
                if (mappedVar != null) {
                    return mappedVar
                // Generate new type variable if refresh flag is set, otherwise return rep type
                } else if (refresh && repType !is TypeParameter) {
                    val newVar = TypeVariable()
                    mappedVars[repType] = newVar

                    // Save old type variable to refreshed type variable mapping
                    val refreshed = refreshedVars[repType]
                    if (refreshed == null) {
                        refreshedVars[repType] = mutableListOf(newVar)
                    } else {
                        refreshed.add(newVar)
                    }

                    println("linking refreshed type $repType to new type $newVar")

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
        if (type1 is TypeVariable && type2 is TypeVariable) {
            val node1 = findRepNode(type1)
            val node2 = findRepNode(type2)

            // Choose the type parameter to be the root node, or choose the higher ranked node
            val firstIsParent = if (type2 is TypeParameter) {
                false
            } else if (type1 is TypeParameter) {
                true
            } else {
                node1.rank > node2.rank
            }

            println("merging $type1 and $type2, making ${if (firstIsParent) type1 else type2} the root")

            if (firstIsParent) {
                if (!replaceRoot(type2, type1)) {
                    return false
                }
            } else {
                if (!replaceRoot(type1, type2)) {
                    return false
                }

                // If ranks were equal, increment the rank of the newly non-root node
                if (node1.rank == node2.rank) {
                    node2.rank++
                }
            }

            return true
        // If merging an unresolved type variable with a type, if occurs check passes resolve var
        } else if (type1 is TypeVariable && type2 !is TypeVariable) {
            if (occursIn(type1, type2)) {
                return false
            }

            return resolveType(type1, type2)
        // If merging a type with an unresolved type variable, if occurs check passes resolve var
        } else if (type1 !is TypeVariable && type2 is TypeVariable) {
            if (occursIn(type2, type1)) {
                return false
            }

            return resolveType(type2, type1)
        } else {
            throw ExceptionWithoutLocation("mergeTypes called on two resolved nodes")
        }
    }

    /**
     * Set the representative node of the given type variable to be resolved to the given type.
     * Returns true if successful, false if the type variable has already been resolved to an
     * an incompatible type.
     */
    private fun resolveType(typeVar: TypeVariable, resolvedType: Type): Boolean {
        val equivNode = findRepNode(typeVar)
        when (equivNode) {
            is ResolvedNode -> return unify(equivNode.type, resolvedType)
            is UnresolvedNode -> {
                // Create resolved parent that this type variable points to
                equivNode.parent = ResolvedNode(resolvedType)

                if (!connectResolvedSupertype(equivNode, resolvedType)) {
                    return false
                }

                if (!connectResolvedSubtype(resolvedType, equivNode)) {
                    return false
                }

                moveDeferredConstraintsToType(typeVar, resolvedType)
                moveRefreshedVarsToType(typeVar, resolvedType)

                return true
            }
        }
    }

    private fun replaceRoot(oldType: TypeVariable, newType: TypeVariable): Boolean {
        val node1 = findRepNode(oldType)
        val node2 = findRepNode(newType)

        if (node1 is UnresolvedNode && node2 is UnresolvedNode) {
            val oldIsSubtypeOfNew = node1.superTypeVars.contains(node2.typeVar)
            val newIsSubtypeOfOld = node2.superTypeVars.contains(node1.typeVar)

            if (!oldIsSubtypeOfNew && !subtypeUnresolvedNodes(node1, node2)) {
                return false
            }

            if (!newIsSubtypeOfOld && !subtypeUnresolvedNodes(node2, node1)) {
                return false
            }

            if (!oldIsSubtypeOfNew) {
                shareUnresolvedSupertypeBounds(node1, node2)
                shareUnresolvedSubtypeBounds(node1, node2)
            }

            if (!newIsSubtypeOfOld) {
                shareUnresolvedSupertypeBounds(node2, node1)
                shareUnresolvedSubtypeBounds(node2, node1)
            }

            // Set old root to point to new root
            node1.parent = node2
        } else if (node1 is UnresolvedNode && node2 is ResolvedNode) {
            if (!connectResolvedSupertype(node1, node2.type) || !connectResolvedSubtype(node2.type, node1)) {
                return false
            }

            // Set old root to point to new root
            node1.parent = node2
        } else if (node1 is ResolvedNode && node2 is UnresolvedNode) {
            if (!connectResolvedSupertype(node2, node1.type) || !connectResolvedSubtype(node1.type, node2)) {
                return false
            }

            // Set old root to point to new root
            node1.parent = node2
        } else {
            // Set old root to point to new root
            node1.parent = node2

            if (!unify((node1 as ResolvedNode).type, (node2 as ResolvedNode).type)) {
                return false
            }
        }

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

        // Move all refreshed variables in map from the old root to the new root

        val oldRefreshed = refreshedVars.remove(oldType)

        if (oldRefreshed != null) {
            if (newType is TypeParameter) {
                for (refreshedVar in oldRefreshed) {
                    if (!unify(refreshedVar, newType)) {
                        throw Exception("TODO: Get of rid of this, can unification with type fail in replaceRoot??? between ${refreshedVar}, ${newType} with reps ${currentRepType(refreshedVar)}, ${currentRepType(newType)} ${refreshedVar is TypeParameter}")
                    }
                }
            } else {
                val newRefreshed = refreshedVars[newType]
                if (newRefreshed == null) {
                    refreshedVars[newType] = oldRefreshed
                } else {
                    newRefreshed.addAll(oldRefreshed)
                }
            }
        }

        return true
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
        val type1 = findShallowRepType(t1)
        val type2 = findShallowRepType(t2)

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

    fun subtype(sbType: Type, spType: Type, except: () -> Unit, allowVars: Boolean = true): Boolean {
        // If type variables, work with their representative types
        val subType = findShallowRepType(sbType)
        val superType = findShallowRepType(spType)

        // The subtype relation is reflexive
        if (subType == superType) {
            return true
        // If not allowing variables, fail if either type is a type variable (when unequal)
        } else if (!allowVars && (subType is TypeVariable || superType is TypeVariable)) {
            return false
        // If both types are type variables, add supertype and subtype bounds to type variables
        } else if (subType is TypeVariable && superType is TypeVariable) {
            val node1 = findRepNode(subType)
            val node2 = findRepNode(superType)

            if (node1 is UnresolvedNode && node2 is UnresolvedNode &&
                    !node1.superTypeVars.contains(node2.typeVar)) {
                shareUnresolvedSupertypeBounds(node1, node2)
                shareUnresolvedSubtypeBounds(node1, node2)

                return subtypeUnresolvedNodes(node1, node2)
            } else if (node1 is UnresolvedNode && node2 is ResolvedNode) {
                return connectResolvedSupertype(node1, node2.type)
            } else if (node1 is ResolvedNode && node2 is UnresolvedNode) {
                return connectResolvedSubtype(node1.type, node2)
            } else if (node1 is ResolvedNode && node2 is ResolvedNode) {
                return subtype(node1.type, node2.type, except)
            } else {
                return true
            }
        // If the supertype only is a type variable, add subtype bound to type variable
        } else if (subType !is TypeVariable && superType is TypeVariable) {
            val node = findRepNode(superType)

            return when (node) {
                is ResolvedNode -> subtype(subType, node.type, except)
                is UnresolvedNode -> connectResolvedSubtype(subType, node)
            }
        } else if (subType is TypeVariable && superType !is TypeVariable) {
            val node = findRepNode(subType)

            return when (node) {
                is ResolvedNode -> subtype(node.type, superType, except)
                is UnresolvedNode -> connectResolvedSupertype(node, superType)
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
        // mutable algebraic data types, and contain no state of thier own.
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
            val paramsMap = subType.sig.typeParams.zip(subType.listTypeParams()).toMap()
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

    // add_upper
    private fun addSupertype(typeNode: UnresolvedNode, superType: Type) {
        typeNode.superTypes.add(superType)

        val except = { -> throw Exception("addSupertype") }
        when (superType) {
            is TypeVariable, is TraitType -> {}
            is UnitType,
            is BoolType,
            is StringType,
            is ByteType,
            is IntType,
            is FloatType,
            is DoubleType,
            is AlgebraicDataType,
            is VectorType,
            is SetType,
            is MapType -> if (!unify(typeNode.typeVar, superType)) {
                except()
            }
            is TupleType -> {}
            //    val tuple = TupleType(superType.elementTypes.map { TypeVariable() })
            //    if (!unify(typeNode.typeVar, tuple) && subtype(tuple, superType, except, true)) {
            //        except()
            //    }
            //}
            is FunctionType -> {}
            //    val func = FunctionType(superType.argTypes.map { TypeVariable() },
            //            TypeVariable())
            //    if (!unify(typeNode.typeVar, func) && subtype(func, superType, except, true)) {
            //        except()
            //    }
            //}
        }
    }

    // edges_to_t
    /**
     * Add a supertype bound to an unresolved node. This supertype will be added as both a supertype
     * of this node, and as a supertype of all unresolved nodes that are subtypes of this node.
     */
    private fun addSupertypeToNode(typeNode: UnresolvedNode, superType: Type) {
        addSupertype(typeNode, superType)

        // Add given supertype to all unresolved subtype nodes
        for (subTypeVar in typeNode.subTypeVars.toList()) {
            val subTypeNode = findRepNode(subTypeVar)

            if (subTypeNode is UnresolvedNode && subTypeNode.typeVar != typeNode.typeVar) {
                addSupertype(subTypeNode, superType)
            }
        }
    }

    // edges_from_t
    /**
     * Add a subtype bound to an unresolved node. This subtype will be added as both a subtype of
     * this node, and as a subtype of all unresolved nodes that are supertypes of this node.
     */
    private fun addSubtypeToNode(typeNode: UnresolvedNode, subType: Type) {
        typeNode.subTypes.add(subType)

        // Add given subtype to all unresolved supertype nodes
        for (superTypeVar in typeNode.superTypeVars.toList()) {
            val superTypeNode = findRepNode(superTypeVar)

            if (superTypeNode is UnresolvedNode && superTypeNode.typeVar != typeNode.typeVar) {
                superTypeNode.subTypes.add(subType)
            }
        }
    }

    // edges_to_tvar
    /**
     * Add a supertype variable bound to an unresolved node. This supertype variable will be added
     * as both a supertype variable of this node, and as a supertype variable of all unresolved
     * nodes that are subtypes of this node.
     */
    private fun addSupertypeVarToNode(typeNode: UnresolvedNode, superTypeVar: TypeVariable) {
        typeNode.superTypeVars.add(superTypeVar)

        // Add given supertype var to all unresolved subtype nodes
        for (subTypeVar in typeNode.subTypeVars.toList()) {
            val subTypeNode = findRepNode(subTypeVar)

            // Filter out all resolved nodes, and all type vars unified with this node's type var
            if (subTypeNode is UnresolvedNode && subTypeNode.typeVar != typeNode.typeVar) {
                subTypeNode.superTypeVars.add(superTypeVar)
            }
        }
    }

    // edges_from_tvar
    /**
     * Add a subtype variable bound to an unresolved node. This subtype variable will be added as
     * both a subtype variable of this node, and as a subtype variable of all unresolved nodes that
     * are supertypes of this node.
     */
    private fun addSubtypeVarToNode(typeNode: UnresolvedNode, subTypeVar: TypeVariable) {
        typeNode.subTypeVars.add(subTypeVar)

        // Add given subtype var to all unresolved supertype nodes
        for (superTypeVar in typeNode.superTypeVars.toList()) {
            val superTypeNode = findRepNode(superTypeVar)

            if (superTypeNode is UnresolvedNode && superTypeNode.typeVar != typeNode.typeVar) {
                superTypeNode.subTypeVars.add(subTypeVar)
            }
        }
    }

    // add_upper_edges
    private fun shareUnresolvedSupertypeBounds(subTypeNode: UnresolvedNode, superTypeNode: UnresolvedNode) {
        // Add all supertype bounds from the supertype node to the subtype node
        for (superType in superTypeNode.superTypes.toList()) {
            addSupertypeToNode(subTypeNode, currentRepType(superType))
        }

        // Add the supertype node's type variable as a supertype var bound on the subtype node
        addSupertypeVarToNode(subTypeNode, superTypeNode.typeVar)

        // Add all supertype var bounds from the supertype node to the subtype node
        for (superTypeVar in superTypeNode.superTypeVars.toList()) {
            val superTypeVarNode = findRepNode(superTypeVar)

            if (superTypeVarNode is UnresolvedNode &&
                    superTypeVarNode.typeVar != superTypeNode.typeVar) {
                addSupertypeVarToNode(subTypeNode, superTypeVarNode.typeVar)
            }
        }
    }

    // add_lower_edges
    private fun shareUnresolvedSubtypeBounds(subTypeNode: UnresolvedNode, superTypeNode: UnresolvedNode) {
        // Add all subtype bounds from the subtype node to the supertype node
        for (subType in subTypeNode.subTypes.toList()) {
            addSubtypeToNode(superTypeNode, currentRepType(subType))
        }

        // Add the subtype node's type variable as a subtype var bound on the supertype node
        addSubtypeVarToNode(superTypeNode, subTypeNode.typeVar)


        // Add all subtype var bounds from the subtype node to the supertype node
        for (subTypeVar in subTypeNode.subTypeVars.toList()) {
            val subTypeVarNode = findRepNode(subTypeVar)

            if (subTypeVarNode is UnresolvedNode && subTypeVarNode.typeVar != subTypeNode.typeVar) {
                addSubtypeVarToNode(superTypeNode, subTypeVarNode.typeVar)
            }
        }
    }

    // edges_and_flows_to_t
    private fun connectResolvedSupertype(subTypeNode: UnresolvedNode, superType: Type): Boolean {
        if (!subTypeNode.superTypes.contains(superType)) {
            addSupertypeToNode(subTypeNode, superType)

            for (subType in subTypeNode.subTypes.toList()) {
                if (!subtype(subType, superType, { -> throw Exception("connectResolvedSupertype") })) {
                    return false
                }
            }
        }

        return true
    }

    // edges_and_flows_from_t
    private fun connectResolvedSubtype(subType: Type, superTypeNode: UnresolvedNode): Boolean {
        if (!superTypeNode.subTypes.contains(subType)) {
            addSubtypeToNode(superTypeNode, subType)

            for (superType in superTypeNode.superTypes.toList()) {
                if (!subtype(subType, superType, { -> throw Exception("connectResolvedSubtype") })) {
                    return false
                }
            }
        }

        return true
    }

    // flows_across
    private fun subtypeUnresolvedNodes(subTypeNode: UnresolvedNode, superTypeNode: UnresolvedNode): Boolean {
        for (subType in subTypeNode.subTypes.toList()) {
            for (superType in superTypeNode.superTypes.toList()) {
                if (!subtype(subType, superType, { -> throw Exception("subtypeUnresolvedNodes") })) {
                    return false
                }
            }
        }

        return true
    }

    fun resolveNode(node: UnresolvedNode) {
        if (!node.subTypes.isEmpty()) {
            // The resolved type of a node should be the union of the subtypes
            val unionOfSubtypes = node.subTypes.toList().reduce { type1, type2 ->
                lowestCommonSupertype(currentRepType(type1), currentRepType(type2))
            }

            node.parent = ResolvedNode(unionOfSubtypes)
            moveDeferredConstraintsToType(node.typeVar, unionOfSubtypes)
            moveRefreshedVarsToType(node.typeVar, unionOfSubtypes)
        } else if (node.superTypes.any({ it is TraitType })) {
            val traitTypes = node.superTypes.filter({ it is TraitType })
                    .map(this::currentRepType)
                    .toSet().toList()
            if (traitTypes.size != 1) {
                val types = formatTypes(traitTypes)
                throw ExceptionWithoutLocation("Could not resolve type variable, found multiple " +
                        "trait supertypes $types")
            }

            val traitType = traitTypes[0]

            node.parent = ResolvedNode(traitType)
            moveDeferredConstraintsToType(node.typeVar, traitType)
            moveRefreshedVarsToType(node.typeVar, traitType)
        } else {
            // If there are no subtypes or trait supertypes, and only subtype variables, then unify
            // all equivalence classes of subtypes and supertypes (since any non-variable subtypes
            // would appear in the non-variable bounds)
            node.subTypeVars.toList().forEach { subTypeVar ->
                if (!unify(node.typeVar, subTypeVar)) {
                    val types = typesToString(node.typeVar, subTypeVar)
                    throw ExceptionWithoutLocation("Could not unify ${types[0]} and ${types[1]} " +
                            "when resolving node")
                }
            }

            node.superTypeVars.toList().forEach { superTypeVar ->
                if (!unify(node.typeVar, superTypeVar)) {
                    val types = typesToString(node.typeVar, superTypeVar)
                    throw ExceptionWithoutLocation("Could not unify ${types[0]} and ${types[1]} " +
                            "when resolving node")
                }
            }
        }
    }

    fun lowestCommonSupertype(type1: Type, type2: Type): Type {
        if (isSubtype(type1, type2)) {
            return type2
        } else if (isSubtype(type2, type1)) {
            return type1
        }

        if (!unify(type1, type2)) {
            val types = typesToString(type1, type2)
            throw ExceptionWithoutLocation("No lowest common supertype for ${types[0]} and " +
                    "${types[1]}")
        }

        return currentRepType(type1)
    }

    fun isSubtype(subType: Type, superType: Type): Boolean {
        if (subType is TraitType && superType is TraitType) {
            return subType == superType
        } else if (subType is TraitType && superType !is TraitType) {
            return superType.sig.traits.contains(subType)
        } else if (subType !is TraitType && superType is TraitType) {
            return subType.sig.traits.contains(superType)
        } else if (subType is TupleType && superType is TupleType) {
            return subType.elementTypes.size == superType.elementTypes.size &&
                    subType.elementTypes.zip(superType.elementTypes)
                        .all({ (elem1, elem2) -> isSubtype(elem1, elem2) })
        } else if (subType is FunctionType && superType is FunctionType) {
            return subType.argTypes.size == superType.argTypes.size &&
                    subType.argTypes.zip(superType.argTypes)
                        .all({ (arg1, arg2) -> isSubtype(arg2, arg1)}) &&
                    isSubtype(subType.returnType, superType.returnType)
        } else {
            return subType == superType
        }
    }

    fun resolveAllVariables() {
        var currentUnresolvedVars = unresolvedVars
        while (!currentUnresolvedVars.isEmpty()) {
            unresolvedVars = mutableSetOf()

            for (unresolvedVar in currentUnresolvedVars) {
                val node = findRepNode(unresolvedVar)
                if (node is UnresolvedNode) {
                    resolveNode(node)
                }
            }

            currentUnresolvedVars = unresolvedVars
        }

        val currentConstraints = deferredConstraints.toMap()
        for ((type, constraints) in currentConstraints) {
            for (constraint in constraints.toList()) {
                // Remove successfully applied constraints
                if (constraint.tryApply(type)) {
                    deferredConstraints[type]?.remove(constraint)
                    if (deferredConstraints[type]?.isEmpty() == true) {
                        deferredConstraints.remove(type)
                    }
                } else {
                    constraint.assertUnresolved()
                }
            }
        }

        // TODO: Delete this
        /*for (i in 0..95) {
            val tvar = TypeVariable(i.toLong())
            val repType = currentRepType(tvar)

            println("At end, $tvar has type $repType (${repType is TypeParameter})")
        }*/
    }

    /**
     * Add a deferred constraint to the given type variable.
     */
    fun addDeferredConstraint(type: Type, constraint: DeferredConstraint) {
        val repType = findShallowRepType(type)
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
            constraint.forceApply(repType)
        }
    }

    /** 
     * Apply all deferred constraints to newly resolved type, erroring if they cannot be applied.
     */
    fun moveDeferredConstraintsToType(oldVar: TypeVariable, newType: Type) {
        val constraints = deferredConstraints.remove(oldVar)
        if (constraints != null) {
            for (constraint in constraints) {
                constraint.forceApply(newType)
            }
        }
    }

    fun moveRefreshedVarsToType(oldVar: TypeVariable, newType: Type) {
        println("moveRefreshedVarsToType $oldVar to $newType started")
        val refreshed = refreshedVars[oldVar]
        if (refreshed == null) {
            return
        }

        if (newType is TypeVariable && newType !is TypeParameter) {
            val newRefreshedVars = refreshedVars.remove(newType)
            if (newRefreshedVars == null) {
                refreshedVars[newType] = refreshed
            } else {
                newRefreshedVars.addAll(refreshed)
            }
        } else {
            for (refreshedVar in refreshed) {
                if (!unify(refreshedVar, newType)) {
                    val p1s = currentRepType(refreshedVar).getAllVariables().map({ it ->
                        "${it}: ${it is TypeParameter}"
                    }).joinToString()
                    val p2s = currentRepType(newType).getAllVariables().map({ it ->
                        "${it}: ${it is TypeParameter}"
                    }).joinToString()
                    throw Exception("TODO: Get of rid of this, can unification with type fail in moveRefreshedVarsToType???  between ${refreshedVar}, ${newType} with reps ${currentRepType(refreshedVar)}, ${currentRepType(newType)} ${p1s} ${p2s}")
                }
            }

            refreshedVars.remove(oldVar)
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