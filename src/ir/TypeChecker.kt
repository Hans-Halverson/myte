package myte.ir

import myte.ir.nodes.*
import myte.shared.*

/**
 * A node in the forest of type equivalence classes.
 *
 * @property type the type contained at this node
 * @property parent the (optional) parent node in the forest of type equivalence classes. This type
 *           is in the same equivalence class as its parent, and if the parent is null, this type
 *           is the representative for its equivalence class.
 */
private class TypeEquivalenceNode(
    val type: Type,
    var parent: TypeEquivalenceNode? = null
) {
    override fun equals(other: Any?): Boolean {
        if (other !is TypeEquivalenceNode) {
            return false
        }

        return (type == other.type)
    }
}

class TypeChecker(var symbolTable: SymbolTable) {
    private val typeToNode: MutableMap<Type, TypeEquivalenceNode> = mutableMapOf()

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
    }

    /**
     * Add a type to the set of equivalence classes, if it does not already exist, and return the
     * equivalence class node for this type.
     */
    private fun addType(type: Type): TypeEquivalenceNode {
        val typeEquivNode = typeToNode[type]
        if (typeEquivNode == null) {
            val newEquivNode = TypeEquivalenceNode(type)
            typeToNode[type] = newEquivNode

            return newEquivNode
        }

        return typeEquivNode
    }

    /**
     * Find the representative equivalence class node for the given type.
     */
    private fun findRepresentativeNode(type: Type): TypeEquivalenceNode {
        // The representative of a node is found by following parent pointers until the last node
        var currentNode: TypeEquivalenceNode = addType(type)
        var parent = currentNode.parent
        while (parent != null) {
            currentNode = parent
            parent = currentNode.parent
        }

        return currentNode
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
     * @param freshVars whether or not to create and map new type variables for every unbound type
     *        variables encountered in the representative type
     */
    private fun findRepType(
        type: Type,
        boundVars: MutableSet<TypeVariable>,
        mappedVars: MutableMap<TypeVariable, TypeVariable> = mutableMapOf(),
        freshVars: Boolean = true
    ): Type {
        val repType = findRepresentativeNode(type).type
        return when (repType) {
            // Find the rep type for vector element type and reconstruct vector type
            is VectorType -> {
                VectorType(findRepType(repType.elementType, boundVars,
                        mappedVars, freshVars))
            }
            // Find the rep type for each tuple element and reconstruct tuple type
            is TupleType -> {
                val elementTypes = repType.elementTypes.map { elementType ->
                    findRepType(elementType, boundVars, mappedVars, freshVars)
                }
                
                return TupleType(elementTypes)
            }
            // Find the rep type for each arg and return type, and reconstruct function type
            is FunctionType -> {
                val argTypes = repType.argTypes.map { argType ->
                    findRepType(argType, boundVars, mappedVars, freshVars)
                }
                val returnType = findRepType(repType.returnType, boundVars, mappedVars, freshVars)

                return FunctionType(argTypes, returnType)
            }
            // Find the rep type for each type parameter and reconstruct adt with correct adt sig
            is AlgebraicDataType -> {
                val typeParams = repType.typeParams.map { typeParam ->
                    findRepType(typeParam, boundVars, mappedVars, freshVars)
                }

                return AlgebraicDataType(repType.adtSig, typeParams)
            }
            is TypeVariable -> {
                // If already bound (has same representative as bound var), then return
                // existing type variable
                if (freshVars) {
                    val repBoundVars = boundVars.map { boundVar ->
                        findRepresentativeNode(boundVar).type
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
                } else {
                    return repType
                }
            }
            else -> repType
        }
    }

    /**
     * Merge two representative nodes into the same equivalence class.
     *
     * @return whether the two representative nodes can be merged into the same equivalence class.
     *         This is not possible if one is a type variable that appears in the other
     *         type.
     */
    private fun mergeReps(rep1: TypeEquivalenceNode, rep2: TypeEquivalenceNode): Boolean {
        if (rep1 == rep2) {
            return true
        }

        // If a rep is not a type variable, set it as the merged representative.
        // If a type variable is encountered, be sure to perform the occurs check.
        if (rep1.type is TypeVariable) {
            if (occursIn(rep1.type, rep2.type)) {
                return false
            }

            rep1.parent = rep2
        } else {
            if (rep2.type is TypeVariable && occursIn(rep2.type, rep1.type)) {
                return false
            }

            rep2.parent = rep1
        }

        return true
    }

    /**
     * Returns whether the given type variable occurs anywhere in a given type.
     */
    private fun occursIn(typeVar: TypeVariable, subst: Type): Boolean {
        if (typeVar == subst) {
            return true
        }

        return when (subst) {
            is VectorType -> occursIn(typeVar, subst.elementType)
            is TupleType -> {
                subst.elementTypes.map({ elementType -> occursIn(typeVar, elementType) })
                                  .any({ x -> x})
            }
            is FunctionType -> occursIn(typeVar, subst.returnType) ||
                    subst.argTypes.map({ argType -> occursIn(typeVar, argType) })
                                  .any({ x -> x })
            is AlgebraicDataType -> {
                subst.typeParams.map({ typeParam -> occursIn(typeVar, typeParam) })
                                .any({ x -> x })
            }

            else -> false
        }
    }

    /**
     * Unify two types by merging them (and recursively merging their child types) into the same
     * equivalence class following the unification algorithm.
     * 
     * @return whether the two types can be unified or not
     */
    private fun unify(t1: Type, t2: Type): Boolean {
        val rep1 = findRepresentativeNode(t1)
        val rep2 = findRepresentativeNode(t2)

        val type1 = rep1.type
        val type2 = rep2.type

        // If representatives are the same, they can be unified
        if (rep1 == rep2) {
            return true
        // If both representatives have vector type, merge reps and unify their child types
        } else if (type1 is VectorType && type2 is VectorType) {
            val canUnify = unify(type1.elementType, type2.elementType)
            if (canUnify) {
                mergeReps(rep1, rep2)
            }

            return canUnify
        // If both representatives have tuple types, merge reps of all element types
        } else if (type1 is TupleType && type2 is TupleType) {
            val canUnify = type1.elementTypes.size == type2.elementTypes.size &&
                    type1.elementTypes.zip(type2.elementTypes)
                         .map({ (e1, e2) -> unify(e1, e2) })
                         .all({ x -> x })

            if (canUnify) {
                mergeReps(rep1, rep2)
            }

            return canUnify
        // If both representatives have function type, merge reps and unify their child types
        } else if (type1 is FunctionType && type2 is FunctionType) {
            val canUnify = type1.argTypes.size == type2.argTypes.size &&
                    unify(type1.returnType, type2.returnType) &&
                    type1.argTypes.zip(type2.argTypes)
                         .map({ (a1, a2) -> unify(a1, a2) })
                         .all({ x -> x })

            if (canUnify) {
                mergeReps(rep1, rep2)
            }

            return canUnify
        // If both representatives are the same adt, merge reps and unify parameter types
        } else if (type1 is AlgebraicDataType && type2 is AlgebraicDataType) {
            val canUnify = type1.adtSig == type2.adtSig &&
                    type1.typeParams.size == type2.typeParams.size &&
                    type1.typeParams.zip(type2.typeParams)
                         .map({ (p1, p2) -> unify(p1, p2) })
                         .all({ x -> x })

            if (canUnify) {
                mergeReps(rep1, rep2)
            }

            return canUnify
        // If a representative is type variable, merge reps
        } else if (type1 is TypeVariable || type2 is TypeVariable) {
            return mergeReps(rep1, rep2)
        // Otherwise the types cannot be unified
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
            is BoolLiteralNode -> node.type = BoolType
            is StringLiteralNode -> node.type = StringType
            is IntLiteralNode -> node.type = IntType
            is FloatLiteralNode -> node.type = FloatType
            is VectorLiteralNode -> typeCheckVectorLiteral(node, boundVars, refresh)
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
            is BreakNode -> node.type = UnitType
            is ContinueNode -> node.type = UnitType
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unification functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckVectorLiteral(
        node: VectorLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Vector type is initially unknown, so set as vector type with new type variable param
        val elementType = TypeVariable()
        node.type = VectorType(elementType)

        // Attempt to unify the types of each vector element with the type variable param
        node.elements.forEach({ element ->
            if (!unify(element.type, elementType)) {
                val typeStrings = formatTypes(listOf(findRepType(element.type, boundVars),
                        findRepType(elementType, boundVars)))
                throw IRConversionException("Vector must have elements of same type, found " +
                        "${typeStrings[0]} and ${typeStrings[1]}", element.startContext)
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
        node.type = nodeType

        // Unify the types of each tuple element with its respective element type variable
        node.elements.zip(nodeType.elementTypes).forEach { (element, expectedElementType) ->
            if (!unify(element.type, expectedElementType)) {
                val typeStrings = formatTypes(listOf(findRepType(element.type, boundVars),
                        findRepType(expectedElementType, boundVars)))
                throw IRConversionException("Cannot infer type for tuple element, expected " +
                        "${typeStrings[0]} but found ${typeStrings[1]}", element.startContext)
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
            throw IRConversionException("Unknown variable ${node.ident.name}", node.identContext)
        }

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        // Since a type for an identifier is being found, a fresh version of that type must be
        // found (if applicable).
        if (refresh) {
            node.type = findRepType(info.type, boundVars)
        } else {
            node.type = info.type
        }
    }

    fun typeCheckUnaryMathOperator(
        node: UnaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Set the node type to be a new type variable, as type is not yet known
        node.type = TypeVariable()

        // Unify this node's type with its child's type
        if (!unify(node.node.type, node.type)) {
            throw IRConversionException("Unary math operator expects a number, found " +
                    "${formatType(findRepType(node.node.type, boundVars))}", node.node.startContext)
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Set the node type to be a new type variable, as type is not yet known
        node.type = TypeVariable()

        // Unify this node's type with both it's children's types
        if (!unify(node.left.type, node.type) ||
                !unify(node.right.type, node.type)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${typeStrings[0]} and ${typeStrings[1]}", node.right.startContext)
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
        node.type = BoolType

        // Both sides of logical and must have type bool
        if (!unify(node.left.type, BoolType)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Logical and expects two bools, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.left.startContext)
        }

        if (!unify(node.right.type, BoolType)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Logical and expects two bools, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.right.startContext)
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
        node.type = BoolType

        // Both sides of logical or must have type bool
        if (!unify(node.left.type, BoolType)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Logical or expects two bools, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.left.startContext)
        }

        if (!unify(node.right.type, BoolType)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Logical or expects two bools, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.right.startContext)
        }
    }

    fun typeCheckLogicalNot(
        node: LogicalNotNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Logical not evaluates to bool
        node.type = BoolType

        if (!unify(node.node.type, BoolType)) {
            throw IRConversionException("Logical not expects a bool, found " +
                    "${formatType(findRepType(node.node.type, boundVars))}", node.startContext)
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
        node.type = BoolType

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Cannot check equality between different types, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.right.startContext)
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
        node.type = BoolType

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val typeStrings = formatTypes(listOf(findRepType(node.left.type, boundVars),
                    findRepType(node.right.type, boundVars)))
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${typeStrings[0]} and ${typeStrings[1]}", node.right.startContext)
        }
    }

    fun typeCheckKeyedAccess(
        node: KeyedAccessNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)

        // Type of keyed access node is new type variable, as type is not yet known
        node.type = TypeVariable()

        // Constrain eval type of node to be the element type of vector
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            throw IRConversionException("Can only perform keyed access on a vector, found " +
                    "${formatType(findRepType(node.container.type, boundVars))}",
                    node.accessContext)
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            throw IRConversionException("Key in keyed access must be an int, found " +
                    "${formatType(findRepType(node.key.type, boundVars))}", node.key.startContext)
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

        // Type of keyed assignment node is new type variable, as type is not yet known
        node.type = TypeVariable()

        // Constrain eval type of container to be the element type of vector, while simultaneously
        // constraining eval type of assignment to be the element type of vector.
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            throw IRConversionException("Can only perform keyed access on a vector, found " +
                    "${formatType(findRepType(node.container.type, boundVars))}",
                    node.accessContext)
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            throw IRConversionException("Key in keyed access must be an int, found " +
                    "${formatType(findRepType(node.key.type, boundVars))}", node.key.startContext)
        }

        // Constrain element type of vector to be type of rValue assigned to it
        if (!unify(node.rValue.type, node.type)) {
            val typeStrings = formatTypes(listOf(findRepType(node.type, boundVars),
                    findRepType(node.rValue.type, boundVars)))
            throw IRConversionException("Expected type for assignment is " +
                    "${typeStrings[0]}, but assigned ${typeStrings[1]}", node.rValue.startContext)
        }
    }

    fun typeCheckFunctionCall(
        node: FunctionCallNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val funcInfo = symbolTable.getInfo(node.func)
        val funcType = funcInfo?.type
        if (funcType == null) {
            throw IRConversionException("Unknown function ${node.func.name}", node.identContext)
        }

        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        // Since a type for an identifier is being found, a fresh version of that type must be used.
        val funcRepType = findRepType(funcType, boundVars)

        // Return type is a float if numeric, otherwise it is unknown so use new type variable
        node.type = if (funcInfo.props.contains(IdentifierProperty.NUMERIC)) {
            FloatType
        } else {
            TypeVariable()
        }

        // Unify the arguments to the function with the expected argument types stored for the
        // function in the symbol table
        val argTypes = node.actualArgs.map { arg -> arg.type }
        val expectedFuncType = FunctionType(argTypes, node.type)

        if (!unify(expectedFuncType, funcRepType)) {
            // If type of identifier is a known function, provide more useful error message
            if (funcRepType is FunctionType) {
                val argRepTypes = argTypes.map { argType -> findRepType(argType, boundVars) }
                throw IRConversionException("${node.func.name} expected arguments of type " +
                        "${formatTypes(funcRepType.argTypes)}, but found " +
                        "${formatTypes(argRepTypes)}", node.identContext)
            } else {
                val typeStrings = formatTypes(listOf(findRepType(expectedFuncType, boundVars),
                        funcRepType))
                throw IRConversionException("${node.func.name} expected to have type " +
                        "${typeStrings[0]}, but found ${typeStrings[1]}", node.identContext)
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
        node.type = nodeType

        // Find constructor arg types given the current adt params and adt variant
        val expectedArgTypes = node.adtVariant.getTypeConstructorWithParams(nodeType.typeParams)
        val actualArgTypes = node.actualArgs.map { arg -> arg.type }

        // If either expected or actual args are empty, print special message if both aren't empty
        if (actualArgTypes.size == 0) {
            if (expectedArgTypes.size != 0) {
                throw IRConversionException("${node.adtVariant.name} expects arguments of type " +
                        "${formatTypes(expectedArgTypes)}, but received no arguments",
                        node.identContext)
            // If no args exist or were expected, there is nothing to type check since adt type
            // must have no params to infer.
            } else {
                return
            }
        } else if (expectedArgTypes.size == 0) {
            throw IRConversionException("${node.adtVariant.name} expects no arguments, but found " +
                        "arguments of type ${formatTypes(actualArgTypes)}", node.identContext)
        }

        // Unify each arg type with its expected type
        val canUnify = expectedArgTypes.size == actualArgTypes.size &&
                expectedArgTypes.zip(actualArgTypes)
                    .map({(expected, actual) -> unify(expected, actual)})
                    .all({ x -> x })

        if (!canUnify) {
            throw IRConversionException("${node.adtVariant.name} expected arguments of type " +
                    "${formatTypes(expectedArgTypes)}, but found ${formatTypes(actualArgTypes)}",
                    node.identContext)
        }
    }

    fun typeCheckVariableAssignment(
        node: VariableAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val type = symbolTable.getInfo(node.lValue)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.lValue.name}", node.identContext)
        }

        typeCheck(node.rValue, boundVars, refresh)

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        // Since a type for an identifier is being found, a fresh version of that type must be used.
        node.type = findRepType(type, boundVars)

        if (!unify(node.rValue.type, node.type)) {
            val typeStrings = formatTypes(listOf(findRepType(node.type, boundVars),
                    findRepType(node.rValue.type, boundVars)))
            throw IRConversionException("Type of ${node.lValue.name} is " +
                    "${typeStrings[0]}, but assigned ${typeStrings[1]}", node.rValue.startContext)
        }
    }

    fun typeCheckVariableDefinition(
        node: VariableDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}", node.identContext)
        }

        typeCheck(node.expr, boundVars, refresh)

        // Variable definition node evaluates to unit value
        node.type = UnitType

        if (!unify(node.expr.type, type)) {
            val typeStrings = formatTypes(listOf(findRepType(type, boundVars),
                    findRepType(node.expr.type, boundVars)))
            throw IRConversionException("Type of ${node.ident.name} is " +
                    "${typeStrings[0]}, but assigned ${typeStrings[1]}", node.expr.startContext)
        }
    }

    fun typeCheckFunctionDefinition(
        node: FunctionDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type !is FunctionType) {
            throw IRConversionException("Unknown function ${node.ident.name}", node.identContext)
        }

        val newBoundVars = boundVars.toHashSet()

        // Add all type variables in function type to the set of bound type variables
        type.getAllVariables().forEach { typeVar ->
            newBoundVars.add(typeVar)
        }

        typeCheck(node.body, newBoundVars, refresh)

        // Function definition node evaluates to unit value
        node.type = UnitType

        // Unify all returned types with the return type of this function
        mapOverReturns(node.body, { retNode ->
            val retType = retNode.expr?.type ?: UnitType
            if (!unify(retType, type.returnType)) {
                val typeStrings = formatTypes(listOf(findRepType(type.returnType, newBoundVars),
                        findRepType(retType, newBoundVars)))
                val context = retNode.expr?.startContext ?: retNode.startContext
                throw IRConversionException("${node.ident.name} must return " +
                        "${typeStrings[0]} but found ${typeStrings[1]}", context)
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
        node.type = UnitType
    }

    fun typeCheckIf(node: IfNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of if must be a bool, but found " +
                    "${formatType(findRepType(node.cond.type, boundVars))}", node.cond.startContext)
        }

        typeCheck(node.conseq, boundVars, refresh)

        if (node.altern != null) {
            typeCheck(node.altern, boundVars, refresh)
        }

        // If node evaluates to unit value
        node.type = UnitType
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of while must be a bool, but given " +
                    "${formatType(findRepType(node.cond.type, boundVars))}", node.cond.startContext)
        }

        typeCheck(node.body, boundVars, refresh)

        // While node evaluates to unit value
        node.type = UnitType
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of do while must be a bool, but given " +
                    "${formatType(findRepType(node.cond.type, boundVars))}", node.cond.startContext)
        }

        typeCheck(node.body, boundVars, refresh)

        // Do while node evaluates to unit value
        node.type = UnitType
    }

    fun typeCheckFor(node: ForNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        if (node.init != null) {
            typeCheck(node.init, boundVars, refresh)
        }

        if (node.cond != null) {
            typeCheck(node.cond, boundVars, refresh)
            if (!unify(node.cond.type, BoolType)) {
                throw IRConversionException("Condition of for must be a bool, but given " +
                        "${formatType(findRepType(node.cond.type, boundVars))}",
                        node.cond.startContext)
            }
        }

        if (node.update != null) {
            typeCheck(node.update, boundVars, refresh)
        }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        node.type = UnitType
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
        node.type = UnitType

        // All patterns must have same type as the matched expression
        node.cases.forEach { (pat, _) ->
            if (!unify(pat.type, node.expr.type)) {
                val typeStrings = formatTypes(listOf(findRepType(node.expr.type, boundVars),
                        findRepType(pat.type, boundVars)))
                throw IRConversionException("Patterns in match statement expected to have type " +
                        "${typeStrings[0]}, but found ${typeStrings[1]}", pat.startContext)
            }
        }
    }

    fun typeCheckReturn(node: ReturnNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        if (node.expr != null) {
            typeCheck(node.expr, boundVars, refresh)
        }

        // Return node evalutes to unit value
        node.type = UnitType
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

}
