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
private class TypeEquivalenceNode(
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
    private fun findRepNode(typeVar: TypeVariable): TypeEquivalenceNode {
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
        if (equivNode.resolvedType == resolvedType || equivNode.resolvedType is TypeVariable) {
            equivNode.resolvedType = resolvedType
            return true
        } else {
            return false
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
     */
    private fun findRepType(
        type: Type,
        boundVars: MutableSet<TypeVariable>,
        mappedVars: MutableMap<TypeVariable, TypeVariable> = mutableMapOf()
    ): Type {
        // Find the representative type if this is a type variable, otherwise use the type
        val repType = if (type is TypeVariable) findRepNode(type).resolvedType else type

        return when (repType) {
            // Find the rep type for vector element type and reconstruct vector type
            is VectorType -> {
                VectorType(findRepType(repType.elementType, boundVars, mappedVars))
            }
            // Find the rep type for set element type and reconstruct set type
            is SetType -> {
                SetType(findRepType(repType.elementType, boundVars, mappedVars))
            }
            // Find the rep type for map's key and value types and reconstruct map type
            is MapType -> {
                MapType(findRepType(repType.keyType, boundVars, mappedVars),
                        findRepType(repType.valType, boundVars, mappedVars))
            }
            // Find the rep type for each tuple element and reconstruct tuple type
            is TupleType -> {
                val elementTypes = repType.elementTypes.map { elementType ->
                    findRepType(elementType, boundVars, mappedVars)
                }
                
                return TupleType(elementTypes)
            }
            // Find the rep type for each arg and return type, and reconstruct function type
            is FunctionType -> {
                val argTypes = repType.argTypes.map { argType ->
                    findRepType(argType, boundVars, mappedVars)
                }
                val returnType = findRepType(repType.returnType, boundVars, mappedVars)

                return FunctionType(argTypes, returnType)
            }
            // Find the rep type for each type parameter and reconstruct adt with correct adt sig
            is AlgebraicDataType -> {
                val typeParams = repType.typeParams.map { typeParam ->
                    findRepType(typeParam, boundVars, mappedVars)
                }

                return AlgebraicDataType(repType.adtSig, typeParams)
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
                } else {
                    val newVar = TypeVariable()
                    mappedVars[repType] = newVar
                    return newVar
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

            // Choose the lower ranked node to be the root
            if (rep1.rank > rep2.rank) {
                rep2.parent = rep1
            } else {
                rep1.parent = rep2
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
    private fun unify(t1: Type, t2: Type): Boolean {
        // If type variables, work with their representative types
        val type1 = if (t1 is TypeVariable) findRepNode(t1).resolvedType else t1
        val type2 = if (t2 is TypeVariable) findRepNode(t2).resolvedType else t2

        // If both types are already identical, they are already unified
        if (type1 == type2) {
            return true
        // If at least one type is a type variable, merge types together
        } else if (type1 is TypeVariable || type2 is TypeVariable) {
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
            is BoolLiteralNode -> typeCheckBoolLiteral(node, boundVars)
            is StringLiteralNode -> typeCheckStringLiteral(node, boundVars)
            is IntLiteralNode -> typeCheckIntLiteral(node, boundVars)
            is FloatLiteralNode -> typeCheckFloatLiteral(node, boundVars)
            is VectorLiteralNode -> typeCheckVectorLiteral(node, boundVars, refresh)
            is SetLiteralNode -> typeCheckSetLiteral(node, boundVars, refresh)
            is MapLiteralNode -> typeCheckMapLiteral(node, boundVars, refresh)
            is TupleLiteralNode -> typeCheckTupleLiteral(node, boundVars, refresh)
            // Variables and functions
            is VariableNode -> typeCheckVariable(node, boundVars, refresh)
            is FunctionCallNode -> typeCheckFunctionCall(node, boundVars, refresh)
            is TypeConstructorNode -> typeCheckTypeConstructor(node, boundVars, refresh)
            is KeyedAccessNode -> typeCheckKeyedAccess(node, boundVars, refresh)
            is KeyedAssignmentNode -> typeCheckKeyedAssignment(node, boundVars, refresh)
            is VariableAssignmentNode -> typeCheckVariableAssignment(node, boundVars, refresh)
            is VariableDefinitionNode -> typeCheckVariableDefinition(node, boundVars, refresh)
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
            is BreakNode -> typeCheckBreak(node, boundVars)
            is ContinueNode -> typeCheckContinue(node, boundVars)
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unification functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckBoolLiteral(
        node: BoolLiteralNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        // Type of bool literal must be a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Bool literal could not be inferred to have bool " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckStringLiteral(
        node: StringLiteralNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        // Type of string literal must be a string
        if (!unify(node.type, StringType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("String literal could not be inferred to have string " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckIntLiteral(
        node: IntLiteralNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        // Type of int literal must be an int
        if (!unify(node.type, IntType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Int literal could not be inferred to have int " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckFloatLiteral(
        node: FloatLiteralNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        // Type of float literal must be a float
        if (!unify(node.type, FloatType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Float literal could not be inferred to have float " +
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
            val types = typesToString(node.type, vectorType, boundVars)
            throw IRConversionException("Vector literal could not be inferred to have vector " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Attempt to unify the types of each vector element with the type variable param
        node.elements.forEach({ element ->
            if (!unify(element.type, elementType)) {
                val types = typesToString(element.type, elementType, boundVars)
                throw IRConversionException("Vector must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
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
            val types = typesToString(node.type, setType, boundVars)
            throw IRConversionException("Set literal could not be inferred to have set " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Attempt to unify the types of each set element with the type variable param
        node.elements.forEach({ element ->
            if (!unify(element.type, elementType)) {
                val types = typesToString(element.type, elementType, boundVars)
                throw IRConversionException("Set must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
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
            val types = typesToString(node.type, mapType, boundVars)
            throw IRConversionException("Map literal could not be inferred to have map type, " +
                "found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Attempt to unify the types of each key with the new key param
        node.keys.forEach({ key -> 
            if (!unify(key.type, keyType)) {
                val types = typesToString(key.type, keyType, boundVars)
                throw IRConversionException("Map must have keys of same type, found " +
                        "${types[0]} and ${types[1]}", key.startLocation)
            }
        })

        // Attempt to unify the types of each value with the new value param
        node.values.forEach({ value -> 
            if (!unify(value.type, valType)) {
                val types = typesToString(value.type, valType, boundVars)
                throw IRConversionException("Map must have values of same type, found " +
                        "${types[0]} and ${types[1]}", value.startLocation)
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
            val types = typesToString(node.type, nodeType, boundVars)
            throw IRConversionException("Tuple literal could not be inferred to have tuple " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Unify the types of each tuple element with its respective element type variable
        node.elements.zip(nodeType.elementTypes).forEach { (element, expectedElementType) ->
            if (!unify(element.type, expectedElementType)) {
                val types = typesToString(element.type, expectedElementType, boundVars)
                throw IRConversionException("Cannot infer type for tuple element, expected " +
                        "${types[0]} but found ${types[1]}", element.startLocation)
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
            throw IRConversionException("Unknown variable ${node.ident.name}", node.startLocation)
        }

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        // Since a type for an identifier is being found, a fresh version of that type must be
        // found (if applicable).
        val expectedType = if (refresh) findRepType(info.type, boundVars) else info.type

        if (!unify(node.type, expectedType)) {
            val types = typesToString(node.type, expectedType, boundVars)
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
            val type = typeToString(node.node.type, boundVars)
            throw IRConversionException("Unary math operator expects a number, found ${type}",
                    node.node.startLocation)
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Unify this node's type with both it's children's types
        if (!unify(node.left.type, node.type) ||
                !unify(node.right.type, node.type)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${types[0]} and ${types[1]}", node.right.startLocation)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Could not infer bool type for result of logical and, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical and must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
            throw IRConversionException("Logical and expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Could not infer bool type for result of logical or, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical or must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
            throw IRConversionException("Logical or expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Could not infer bool type for result of logical not, " +
                    "found ${type}", node.startLocation)
        }

        if (!unify(node.node.type, BoolType)) {
            val type = typeToString(node.node.type, boundVars)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Could not infer bool type for result of comparison, " +
                    "found ${type}", node.startLocation)
        }

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Could not infer bool type for result of comparison, " +
                    "found ${type}", node.startLocation)
        }

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val types = typesToString(node.left.type, node.right.type, boundVars)
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckKeyedAccess(
        node: KeyedAccessNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)

        // Constrain eval type of node to be the element type of vector
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            val type = typeToString(node.container.type, boundVars)
            throw IRConversionException("Can only perform keyed access on a vector, found ${type}",
                    node.accessLocation)
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            val type = typeToString(node.key.type, boundVars)
            throw IRConversionException("Key in keyed access must be an int, found ${type}",
                    node.key.startLocation)
        }
    }

    fun typeCheckKeyedAssignment(
        node: KeyedAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        // Constrain eval type of container to be the element type of vector, while simultaneously
        // constraining eval type of assignment to be the element type of vector.
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            val type = typeToString(node.container.type, boundVars)
            throw IRConversionException("Can only perform keyed access on a vector, found ${type}",
                    node.accessLocation)
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            val type = typeToString(node.key.type, boundVars)
            throw IRConversionException("Key in keyed access must be an int, found ${type}",
                    node.key.startLocation)
        }

        // Constrain element type of vector to be type of rValue assigned to it
        if (!unify(node.rValue.type, node.type)) {
            val types = typesToString(node.type, node.rValue.type, boundVars)
            throw IRConversionException("Expected type for assignment is " +
                    "${types[0]}, but assigned ${types[1]}", node.rValue.startLocation)
        }
    }

    fun typeCheckFunctionCall(
        node: FunctionCallNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        // Function node must be a variable node
        val func = if (node.func is VariableNode) {
            node.func.ident
        } else {
            throw IRConversionException("Can only call functions", node.func.startLocation)
        }

        val funcInfo = symbolTable.getInfo(func)
        val funcType = funcInfo?.type
        if (funcType == null) {
            throw IRConversionException("Unknown function ${func.name}", node.startLocation)
        }

        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        // Since a type for an identifier is being found, a fresh version of that type must be used.
        val funcRepType = findRepType(funcType, boundVars)

        // Return type is a float if numeric
        if (funcInfo.props.contains(IdentifierProperty.NUMERIC)) {
            if (!unify(node.type, FloatType)) {
                val type = typeToString(node.type, boundVars)
                throw IRConversionException("Return type for numeric function must be float, but " +
                        "found ${type}", node.startLocation)
            }
        }

        // Unify the arguments to the function with the expected argument types stored for the
        // function in the symbol table
        val argTypes = node.actualArgs.map { arg -> arg.type }
        val expectedFuncType = FunctionType(argTypes, node.type)

        if (!unify(expectedFuncType, funcRepType)) {
            // If type of identifier is a known function, provide more useful error message
            if (funcRepType is FunctionType) {
                val argRepTypes = argTypes.map { argType -> findRepType(argType, boundVars) }
                throw IRConversionException("${func.name} expected arguments of type " +
                        "${formatTypes(funcRepType.argTypes)}, but found " +
                        "${formatTypes(argRepTypes)}", node.startLocation)
            } else {
                val types = formatTypes(listOf(findRepType(expectedFuncType, boundVars),
                        funcRepType))
                throw IRConversionException("${func.name} expected to have type " +
                        "${types[0]}, but found ${types[1]}", node.startLocation)
            }
        }
    }

    fun typeCheckTypeConstructor(
        node: TypeConstructorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        // Create a fresh adt type for this type variant, since exact type params aren't known yet
        val nodeType = node.adtVariant.adtSig.getFreshAdt()
        if (!unify(node.type, nodeType)) {
            val types = typesToString(nodeType, node.type, boundVars)
            throw IRConversionException("Could not infer return type for type constructor, " +
                    "expected ${types[0]} but found ${types[1]}", node.startLocation)
        }

        // Find constructor arg types given the current adt params and adt variant
        val expectedArgTypes = node.adtVariant.getTypeConstructorWithParams(nodeType.typeParams)
        val actualArgTypes = node.actualArgs.map { arg -> arg.type }

        // If either expected or actual args are empty, print special message if both aren't empty
        if (actualArgTypes.size == 0) {
            if (expectedArgTypes.size != 0) {
                throw IRConversionException("${node.adtVariant.name} expects arguments of type " +
                        "${formatTypes(expectedArgTypes)}, but received no arguments",
                        node.startLocation)
            // If no args exist or were expected, there is nothing to type check since adt type
            // must have no params to infer.
            } else {
                return
            }
        } else if (expectedArgTypes.size == 0) {
            throw IRConversionException("${node.adtVariant.name} expects no arguments, but found " +
                        "arguments of type ${formatTypes(actualArgTypes)}", node.startLocation)
        }

        // Unify each arg type with its expected type
        val canUnify = expectedArgTypes.size == actualArgTypes.size &&
                expectedArgTypes.zip(actualArgTypes)
                    .map({(expected, actual) -> unify(expected, actual)})
                    .all({ x -> x })

        if (!canUnify) {
            throw IRConversionException("${node.adtVariant.name} expected arguments of type " +
                    "${formatTypes(expectedArgTypes)}, but found ${formatTypes(actualArgTypes)}",
                    node.startLocation)
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
        // Since a type for an identifier is being found, a fresh version of that type must be used.
        val freshType = findRepType(type, boundVars)
        if (!unify(node.type, freshType)) {
            val types = typesToString(freshType, node.type, boundVars)
            throw IRConversionException("Variable assignment should evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }

        // The value assigned to the variable should have the same type as the variable
        if (!unify(node.rValue.type, node.type)) {
            val types = typesToString(node.type, node.rValue.type, boundVars)
            throw IRConversionException("Type of ${node.lValue.name} is " +
                    "${types[0]}, but assigned ${types[1]}", node.rValue.startLocation)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Variable definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        // The value assigned to the variable should have the same type as the variable
        if (!unify(node.expr.type, varType)) {
            val types = typesToString(varType, node.expr.type, boundVars)
            throw IRConversionException("Type of ${node.ident.name} is " +
                    "${types[0]}, but assigned ${types[1]}", node.expr.startLocation)
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

        typeCheck(node.body, newBoundVars, refresh)

        // Function definition node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Function definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        // Unify all returned types with the return type of this function
        mapOverReturns(node.body, { retNode ->
            val retType = retNode.expr?.type ?: UnitType
            if (!unify(retType, funcType.returnType)) {
                val types = typesToString(funcType.returnType, retType, newBoundVars)
                val location = retNode.expr?.startLocation ?: retNode.startLocation
                throw IRConversionException("${node.ident.name} must return ${types[0]} " +
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
            is MatchNode -> node.cases.forEach { (_, stmt) -> mapOverReturns(stmt, func) }
            is ReturnNode -> func(node)
        }
    }

    fun typeCheckBlock(node: BlockNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.nodes.forEach { child -> typeCheck(child, boundVars, refresh) }

        // Block node evaluates to a unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Block should evaluate to the unit type, but found ${type}",
                    node.startLocation)
        }
    }

    fun typeCheckIf(node: IfNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type, boundVars)
            throw IRConversionException("Condition of if must be a bool, but found ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.conseq, boundVars, refresh)

        if (node.altern != null) {
            typeCheck(node.altern, boundVars, refresh)
        }

        // If node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("If statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type, boundVars)
            throw IRConversionException("Condition of while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // While node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("While loop should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type, boundVars)
            throw IRConversionException("Condition of do while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // Do while node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
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
                val type = typeToString(node.cond.type, boundVars)
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
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("For loop should evaluate to the unit type, but found " +
                    "${type}", node.startLocation)
        }
    }

    fun typeCheckMatch(node: MatchNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)

        node.cases.forEach { (pattern, statement) ->
            typeCheck(pattern, boundVars, false)

            // Bind all variables that are found in the pattern, including in child nodes
            val newBoundVars = boundVars.toHashSet()
            pattern.map { patNode ->
                patNode.type.getAllVariables().forEach { typeVar -> newBoundVars.add(typeVar) }
            }

            typeCheck(statement, newBoundVars, refresh)
        }

        // Match node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Match statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        // All patterns must have same type as the matched expression
        node.cases.forEach { (pat, _) ->
            if (!unify(pat.type, node.expr.type)) {
                val types = typesToString(node.expr.type, pat.type, boundVars)
                throw IRConversionException("Patterns in match statement expected to have type " +
                        "${types[0]}, but found ${types[1]}", pat.startLocation)
            }
        }
    }

    fun typeCheckReturn(node: ReturnNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        if (node.expr != null) {
            typeCheck(node.expr, boundVars, refresh)
        }

        // Return node evalutes to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckBreak(node: BreakNode, boundVars: MutableSet<TypeVariable>) {
        // Break node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Break statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckContinue(node: ContinueNode, boundVars: MutableSet<TypeVariable>) {
        // Continue node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type, boundVars)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    /**
     * Infer types for every identifier of the specified class in the symbol table. Be sure to
     * only infer types after all type checking and unification has taken place.
     */
    fun inferSymbolTypes(idClass: IdentifierClass) {
        // Infer types for every identifier of the specified class
        for ((_, identInfo) in symbolTable.identifiers) {
            if (identInfo.idClass == idClass) {
                identInfo.type = findRepType(identInfo.type, mutableSetOf())
            }
        }
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place.
     */
    fun inferFunctionTypes() {
        inferSymbolTypes(IdentifierClass.FUNCTION)
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place, and after function types have been inferred.
     */
    fun inferVariableTypes() {
        inferSymbolTypes(IdentifierClass.VARIABLE)
    }

    /**
     * Infer types for every IR tree rooted at a list of nodes. Be sure to only infer types after
     * all type checking and unification has taken place, and after function and variable types
     * have been inferred.
     */
    fun inferIRTypes(root: IRNode) {
        // Infer return types for each IRNode
        root.map { node ->
            node.type = findRepType(node.type, mutableSetOf())
        }
    }

    /**
     * Convert a single type into its string representation given a set of bound variables.
     */
    fun typeToString(type: Type, boundVars: MutableSet<TypeVariable>): String {
        return formatType(findRepType(type, boundVars))
    }

    /**
     * Convert two types into their string representations given the same set of bound variables.
     */
    fun typesToString(type1: Type, type2: Type, boundVars: MutableSet<TypeVariable>): List<String> {
        return formatTypes(listOf(findRepType(type1, boundVars), findRepType(type2, boundVars)))
    }

}
