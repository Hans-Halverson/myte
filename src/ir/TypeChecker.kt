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
     * Add a type to the set of equivalence classes, if it does not already exist.
     */
    private fun addType(type: Type) {
        if (!typeToNode.contains(type)) {
            typeToNode[type] = TypeEquivalenceNode(type)
        }
    }

    /**
     * Find the representative equivalence class node for the given type.
     */
    private fun findRepresentativeNode(type: Type): TypeEquivalenceNode {
        addType(type)

        var node = typeToNode[type]
        if (node == null) {
            throw IRConversionException("Unknown type ${type}")
        }

        // The representative of a node is found by following parent pointers until the last node
        var currentNode: TypeEquivalenceNode = node
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
     */
    fun typeCheck(node: IRNode, boundVars: MutableSet<TypeVariable>) {
        when (node) {
            is VectorLiteralNode -> typeCheckVectorLiteral(node, boundVars)
            is TupleLiteralNode -> typeCheckTupleLiteral(node, boundVars)
            is UnaryMathOperatorNode -> typeCheckUnaryMathOperator(node, boundVars)
            is BinaryMathOperatorNode -> typeCheckBinaryMathOperator(node, boundVars)
            is LogicalAndNode -> typeCheckLogicalAnd(node, boundVars)
            is LogicalOrNode -> typeCheckLogicalOr(node, boundVars)
            is LogicalNotNode -> typeCheckLogicalNot(node, boundVars)
            is EqualityNode -> typeCheckEquality(node, boundVars)
            is ComparisonNode -> typeCheckComparison(node, boundVars)
            is KeyedAccessNode -> typeCheckKeyedAccess(node, boundVars)
            is KeyedAssignmentNode -> typeCheckKeyedAssignment(node, boundVars)
            is FunctionCallNode -> typeCheckFunctionCall(node, boundVars)
            is VariableAssignmentNode -> typeCheckVariableAssignment(node, boundVars)
            is VariableDefinitionNode -> typeCheckVariableDefinition(node, boundVars)
            is FunctionDefinitionNode -> typeCheckFunctionDefinition(node, boundVars)
            is BlockNode -> typeCheckBlock(node, boundVars)
            is IfNode -> typeCheckIf(node, boundVars)
            is WhileNode -> typeCheckWhile(node, boundVars)
            is DoWhileNode -> typeCheckDoWhile(node, boundVars)
            is ForNode -> typeCheckFor(node, boundVars)
            is ReturnNode -> typeCheckReturn(node, boundVars)
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unification functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckVectorLiteral(node: VectorLiteralNode, boundVars: MutableSet<TypeVariable>) {
        node.elements.forEach { element -> typeCheck(element, boundVars) }

        // Attempt to unify the types of each vector element with a new type variable
        val expectedElementType = TypeVariable()
        val expectedType = VectorType(expectedElementType)

        node.elements.forEach({ element ->
            if (!unify(findRepType(element.type, boundVars), expectedElementType)) {
                throw IRConversionException("Vector must have elements of same type, found " +
                        "${findRepType(element.type, boundVars)} and " +
                        "${findRepType(expectedElementType, boundVars)}")
            }
        })

        // Attempt to unify the vector eval type with the new vector type variable
        if (!unify(node.type, expectedType)) {
            throw IRConversionException("Cannot infer type for vector, expected " +
                    "${findRepType(expectedType, boundVars)} but found " +
                    "${findRepType(node.type, boundVars)}")
        }
    }

    fun typeCheckTupleLiteral(node: TupleLiteralNode, boundVars: MutableSet<TypeVariable>) {
        node.elements.forEach { element -> typeCheck(element, boundVars) }

        val nodetype = node.type
        if (nodetype !is TupleType) {
            throw IRConversionException("Expected tuple literal to have tuple type, but found " +
                    "${findRepType(nodetype, boundVars)}")
        }

        // Attempt to unify the types of each tuple element with its respective element type
        val canUnifyElements = node.elements.zip(nodetype.elementTypes)
                .map({ (element, expectedElementType) ->
                    unify(findRepType(element.type, boundVars), expectedElementType)
                }).all({ x -> x })

        val expectedType = TupleType(nodetype.elementTypes)

        // Attempt to unify the list eval type with the new list type variable
        if (!canUnifyElements || !unify(nodetype, expectedType)) {
            throw IRConversionException("Cannot infer type for tuple, expected " +
                    "${findRepType(expectedType, boundVars)} but found " +
                    "${findRepType(node.type, boundVars)}")
        }
    }

    fun typeCheckUnaryMathOperator(
        node: UnaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        typeCheck(node.node, boundVars)

        // Unify this node's type with its child's type, and verify it is a number type
        val canUnify = unify(node.node.type, node.type)
        val repType = findRepType(node.type, boundVars)

        if (!canUnify || repType !is NumberType) {
            throw IRConversionException("Unary math operator expects a number, found " +
                    "${findRepType(node.node.type, boundVars)}")
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        // Unify this node's type with both it's children's types
        if (!unify(node.left.type, node.type) ||
                !unify(node.right.type, node.type)) {
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${findRepType(node.left.type, boundVars)} and " +
                    "${findRepType(node.right.type, boundVars)}")
        }
    }

    fun typeCheckLogicalAnd(node: LogicalAndNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        if (!unify(node.left.type, BoolType) ||
                !unify(node.right.type, BoolType)) {
            throw IRConversionException("Logical and expects two bools, found " +
                    "${findRepType(node.left.type, boundVars)} and " +
                    "${findRepType(node.right.type, boundVars)}")
        }
    }

    fun typeCheckLogicalOr(node: LogicalOrNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        if (!unify(node.left.type, BoolType) ||
                !unify(node.right.type, BoolType)) {
            throw IRConversionException("Logical or expects two bools, found " +
                    "${findRepType(node.left.type, boundVars)} and " +
                    "${findRepType(node.right.type, boundVars)}")
        }
    }

    fun typeCheckLogicalNot(node: LogicalNotNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.node, boundVars)

        if (!unify(node.node.type, BoolType)) {
            throw IRConversionException("Logical not expects a bool, found " +
                    "${findRepType(node.node.type, boundVars)}")
        }
    }

    fun typeCheckEquality(node: EqualityNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        // Create a new type variable and unify both children with it, as both children must have
        // the same unkown type.
        val typeVar = TypeVariable()

        val leftRepType = findRepType(node.left.type, boundVars)
        val rightRepType = findRepType(node.right.type, boundVars)

        if (!unify(leftRepType, typeVar) || !unify(rightRepType, typeVar)) {
            throw IRConversionException("Cannot check equality between different types, found " +
                    "${findRepType(leftRepType, boundVars)} and " +
                    "${findRepType(rightRepType, boundVars)}")
        }
    }

    fun typeCheckComparison(node: ComparisonNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        // Create a new type variable and unify both children with it, as both children must have
        // the same type.
        val typeVar = TypeVariable()

        val leftRepType = findRepType(node.left.type, boundVars)
        val rightRepType = findRepType(node.right.type, boundVars)

        if (!unify(leftRepType, typeVar) || !unify(rightRepType, typeVar)) {
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${findRepType(leftRepType, boundVars)} and " +
                    "${findRepType(rightRepType, boundVars)}")
        }
    }

    fun typeCheckKeyedAccess(node: KeyedAccessNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.container, boundVars)
        typeCheck(node.key, boundVars)

        // Constrain eval type of node to be the element type of vector
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            throw IRConversionException("Can only perform keyed access on a vector, found " +
                    "${findRepType(node.container.type, boundVars)}")
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            throw IRConversionException("Key in keyed access must be an int, found " +
                    "${findRepType(node.key.type, boundVars)}")
        }
    }

    fun typeCheckKeyedAssignment(node: KeyedAssignmentNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.container, boundVars)
        typeCheck(node.key, boundVars)
        typeCheck(node.rValue, boundVars)

        // Constrain eval type of container to be the element type of vector, while simultaneously
        // constraining eval type of assignment to be the element type of vector.
        val expectedVectorType = VectorType(node.type)

        if (!unify(node.container.type, expectedVectorType)) {
            throw IRConversionException("Can only perform keyed access on a vector, found " +
                    "${findRepType(node.container.type, boundVars)}")
        }

        // Constrain key to be an integer
        if (!unify(node.key.type, IntType)) {
            throw IRConversionException("Key in keyed access must be an int, found " +
                    "${findRepType(node.key.type, boundVars)}")
        }

        // Constrain element type of vector to be type of rValue assigned to it
        val rValueRepType = findRepType(node.rValue.type, boundVars)
        if (!unify(rValueRepType, node.type)) {
            throw IRConversionException("Expected type for assignment is " +
                    "${findRepType(node.type, boundVars)}, but assigned " +
                    "${findRepType(rValueRepType, boundVars)}")
        }
    }

    fun typeCheckFunctionCall(node: FunctionCallNode, boundVars: MutableSet<TypeVariable>) {
        val funcType = symbolTable.getInfo(node.func)?.type
        if (funcType == null) {
            throw IRConversionException("Unknown function ${node.func.name}")
        }

        val funcRepType = findRepType(funcType, boundVars)

        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars) }

        // Unify the arguments to the function with the expected argument types stored for the
        // function in the symbol table
        val argTypes = node.actualArgs.map { arg -> findRepType(arg.type, boundVars) }
        val returnType = node.type
        val expectedFuncType = FunctionType(argTypes, returnType)

        if (!unify(expectedFuncType, funcRepType)) {
            // If type of identifier is a known function, provide more useful error message
            if (funcRepType is FunctionType) {
                val argRepTypes = argTypes.map { argType -> findRepType(argType, boundVars) }
                throw IRConversionException("${node.func.name} expected arguments of type " +
                        "${funcRepType.argTypes}, but found ${argRepTypes}")
            } else {
                throw IRConversionException("${node.func.name} expected to have type " +
                        "${findRepType(expectedFuncType, boundVars)}, but found ${funcRepType}")
            }
        }
    }

    fun typeCheckVariableAssignment(
        node: VariableAssignmentNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        val type = symbolTable.getInfo(node.lValue)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.lValue.name}")
        }

        typeCheck(node.rValue, boundVars)

        val nodeRepType = findRepType(node.rValue.type, boundVars)

        if (!unify(nodeRepType, type)) {
            throw IRConversionException("Type of ${node.lValue.name} is " +
                    "${findRepType(type, boundVars)}, but assigned " +
                    "${findRepType(nodeRepType, boundVars)}")
        }
    }

    fun typeCheckVariableDefinition(
        node: VariableDefinitionNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}")
        }

        typeCheck(node.expr, boundVars)

        val freshNodeType = findRepType(node.expr.type, boundVars)

        if (!unify(freshNodeType, type)) {
            throw IRConversionException("Type of ${node.ident.name} is " +
                    "${findRepType(type, boundVars)}, but assigned " +
                    "${findRepType(freshNodeType, boundVars)}")
        }
    }

    fun typeCheckFunctionDefinition(
        node: FunctionDefinitionNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type !is FunctionType) {
            throw IRConversionException("Unknown function ${node.ident.name}")
        }

        val newBoundVars = boundVars.toHashSet()

        // Add all type variables in function type to the set of bound type variables
        type.getAllVariables().forEach { typeVar ->
            newBoundVars.add(typeVar)
        }

        typeCheck(node.body, newBoundVars)

        // Unify all returned types with the return type of this function
        mapOverReturns(node.body, { retNode ->
            val retType = retNode.expr?.type ?: UnitType
            if (!unify(retType, type.returnType)) {
                throw IRConversionException("${node.ident.name} must return " +
                        "${findRepType(type.returnType, newBoundVars)} but found " +
                        "${findRepType(retType, newBoundVars)}")
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
            is ReturnNode -> func(node)
        }
    }

    fun typeCheckBlock(node: BlockNode, boundVars: MutableSet<TypeVariable>) {
        node.nodes.forEach { child -> typeCheck(child, boundVars) }
    }

    fun typeCheckIf(node: IfNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.cond, boundVars)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of if must be a bool, but found " +
                    "${findRepType(node.cond.type, boundVars)}")
        }

        typeCheck(node.conseq, boundVars)

        if (node.altern != null) {
            typeCheck(node.altern, boundVars)
        }
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.cond, boundVars)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of while must be a bool, but given " +
                    "${findRepType(node.cond.type, boundVars)}")
        }

        typeCheck(node.body, boundVars)
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.cond, boundVars)

        if (!unify(node.cond.type, BoolType)) {
            throw IRConversionException("Condition of do while must be a bool, but given " +
                    "${findRepType(node.cond.type, boundVars)}")
        }

        typeCheck(node.body, boundVars)
    }

    fun typeCheckFor(node: ForNode, boundVars: MutableSet<TypeVariable>) {
        if (node.init != null) {
            typeCheck(node.init, boundVars)
        }

        if (node.cond != null) {
            typeCheck(node.cond, boundVars)
            if (!unify(node.cond.type, BoolType)) {
                throw IRConversionException("Condition of for must be a bool, but given " +
                        "${findRepType(node.cond.type, boundVars)}")
            }
        }

        if (node.update != null) {
            typeCheck(node.update, boundVars)
        }

        typeCheck(node.body, boundVars)
    }

    fun typeCheckReturn(node: ReturnNode, boundVars: MutableSet<TypeVariable>) {
        if (node.expr != null) {
            typeCheck(node.expr, boundVars)
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

}
