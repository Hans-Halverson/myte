package myte.ir

import myte.ir.nodes.*
import myte.shared.*

/**
 * A node in the forest of type equivalence classes.
 *
 * @property type the type expression contiained at this node
 * @property parent the (optional) parent node in the forest of type equivalence classes. This type
 *           is in the same equivalence class as its parent, and if the parent is null, this type
 *           is the representative for its equivalence class.
 */
private class TypeEquivalenceNode(
    val type: TypeExpression,
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
    private val typeToNode: MutableMap<TypeExpression, TypeEquivalenceNode> = mutableMapOf()
    private val boundTypeVars: MutableMap<TypeVariable, TypeParameter> = mutableMapOf()

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
    }

    /**
     * Add a type expression to the set of equivalence classes, if it does not already exist.
     */
    private fun addTypeExpression(type: TypeExpression) {
        if (!typeToNode.contains(type)) {
            typeToNode[type] = TypeEquivalenceNode(type)
        }
    }

    /**
     * Find the representative equivalence class node for the given type expression.
     */
    private fun findRepresentativeNode(type: TypeExpression): TypeEquivalenceNode {
        addTypeExpression(type)

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
     * Recursively find a fresh representative type expression for a given type expression. This
     * will return the representative type expression for this type, and the representative type
     * type expressions for all child types if the given type is a type constructor. All free type
     * variables in the representative type expression will also be remapped to fresh type vars.
     *
     * @param type the type expression to find the representative type for
     * @param boundVars set of all bound type variables. If not supplied, defaults to the empty set.
     * @param mappedVars map of old type variables to fresh type variables for all non-bound type
     *        variables encountered so far. If not supplied, defaults to the empty map.
     */
    private fun findRepType(
        type: TypeExpression,
        boundVars: MutableSet<TypeVariable>,
        mappedVars: MutableMap<TypeVariable, TypeVariable> = mutableMapOf()
    ): TypeExpression {
        val repType = findRepresentativeNode(type).type
        return when (repType) {
            is ListTypeExpression -> {
                ListTypeExpression(findRepType(repType.elementType, boundVars, mappedVars))
            }
            is FunctionTypeExpression -> {
                val argTypes = repType.argTypes.map { argType ->
                    findRepType(argType, boundVars, mappedVars)
                }
                val returnType = findRepType(repType.returnType, boundVars, mappedVars)

                return FunctionTypeExpression(argTypes, returnType)
            }
            is TypeVariable -> {
                // If already bound (has same representative as bound var), then return
                // existing type variable
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
                    val newVar = newTypeVariable()
                    mappedVars[repType] = newVar
                    return newVar
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
     *         type expression.
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
     * Returns whether the given type variable occurs anywhere in a given type expression.
     */
    private fun occursIn(typeVar: TypeVariable, subst: TypeExpression): Boolean {
        if (typeVar == subst) {
            return true
        }

        return when (subst) {
            is ListTypeExpression -> occursIn(typeVar, subst.elementType)
            is FunctionTypeExpression -> occursIn(typeVar, subst.returnType) ||
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
    private fun unify(t1: TypeExpression, t2: TypeExpression): Boolean {
        val rep1 = findRepresentativeNode(t1)
        val rep2 = findRepresentativeNode(t2)

        val type1 = rep1.type
        val type2 = rep2.type

        // If representatives are the same, they can be unified
        if (rep1 == rep2) {
            return true
        // If both representatives have list type, merge reps and unify their child types
        } else if (type1 is ListTypeExpression && type2 is ListTypeExpression) {
            val canUnify = unify(type1.elementType, type2.elementType)
            if (canUnify) {
                mergeReps(rep1, rep2)
            }

            return canUnify
        // If both representatives have function type, merge reps and unify their child types
        } else if (type1 is FunctionTypeExpression && type2 is FunctionTypeExpression) {
            val canUnify = type1.argTypes.size == type2.argTypes.size &&
                    unify(type1.returnType, type2.returnType) &&
                    type1.argTypes.zip(type2.argTypes)
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
     */
    fun typeCheck(node: IRNode, boundVars: MutableSet<TypeVariable>) {
        when (node) {
            is ListLiteralNode -> typeCheckListLiteral(node, boundVars)
            is UnaryMathOperatorNode -> typeCheckUnaryMathOperator(node, boundVars)
            is BinaryMathOperatorNode -> typeCheckBinaryMathOperator(node, boundVars)
            is LogicalAndNode -> typeCheckLogicalAnd(node, boundVars)
            is LogicalOrNode -> typeCheckLogicalOr(node, boundVars)
            is LogicalNotNode -> typeCheckLogicalNot(node, boundVars)
            is EqualityNode -> typeCheckEquality(node, boundVars)
            is ComparisonNode -> typeCheckComparison(node, boundVars)
            is FunctionCallNode -> typeCheckFunctionCall(node, boundVars)
            is AssignmentNode -> typeCheckAssignment(node, boundVars)
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

    fun typeCheckListLiteral(node: ListLiteralNode, boundVars: MutableSet<TypeVariable>) {
        node.elements.forEach { element -> typeCheck(element, boundVars) }

        // Attempt to unify the types of each list element with a new type variable
        val expectedElementType = newTypeVariable()
        val expectedType = ListTypeExpression(expectedElementType)

        node.elements.forEach({ element ->
            if (!unify(findRepType(element.evalTypeExpr, boundVars), expectedElementType)) {
                throw IRConversionException("List must have elements of same type, found " +
                        "${findRepType(element.evalTypeExpr, boundVars)} and " +
                        "${findRepType(expectedElementType, boundVars)}")
            }
        })

        // Attempt to unify the list eval type with the new list type variable
        if (!unify(node.evalTypeExpr, expectedType)) {
            throw IRConversionException("Cannot infer type for list, expected " +
                    "${findRepType(expectedType, boundVars)} but found " +
                    "${findRepType(node.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckUnaryMathOperator(
        node: UnaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        typeCheck(node.node, boundVars)

        // Unify this node's type with its child's type, and verify it is a number type
        val canUnify = unify(node.node.evalTypeExpr, node.evalTypeExpr)
        val repType = findRepType(node.evalTypeExpr, boundVars)

        if (!canUnify || repType !is NumberTypeExpression) {
            throw IRConversionException("Unary math operator expects a number, found " +
                    "${findRepType(node.node.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        // Unify this node's type with both it's children's types
        if (!unify(node.left.evalTypeExpr, node.evalTypeExpr) ||
                !unify(node.right.evalTypeExpr, node.evalTypeExpr)) {
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${findRepType(node.left.evalTypeExpr, boundVars)} and " +
                    "${findRepType(node.right.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckLogicalAnd(node: LogicalAndNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        if (!unify(node.left.evalTypeExpr, BoolTypeExpression) ||
                !unify(node.right.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical and expects two bools, found " +
                    "${findRepType(node.left.evalTypeExpr, boundVars)} and " +
                    "${findRepType(node.right.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckLogicalOr(node: LogicalOrNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        if (!unify(node.left.evalTypeExpr, BoolTypeExpression) ||
                !unify(node.right.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical or expects two bools, found " +
                    "${findRepType(node.left.evalTypeExpr, boundVars)} and " +
                    "${findRepType(node.right.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckLogicalNot(node: LogicalNotNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.node, boundVars)

        if (!unify(node.node.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical not expects a bool, found " +
                    "${findRepType(node.node.evalTypeExpr, boundVars)}")
        }
    }

    fun typeCheckEquality(node: EqualityNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.left, boundVars)
        typeCheck(node.right, boundVars)

        // Create a new type variable and unify both children with it, as both children must have
        // the same unkown type.
        val typeVar = newTypeVariable()

        val leftRepType = findRepType(node.left.evalTypeExpr, boundVars)
        val rightRepType = findRepType(node.right.evalTypeExpr, boundVars)

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
        val typeVar = newTypeVariable()

        val leftRepType = findRepType(node.left.evalTypeExpr, boundVars)
        val rightRepType = findRepType(node.right.evalTypeExpr, boundVars)

        if (!unify(leftRepType, typeVar) || !unify(rightRepType, typeVar)) {
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${findRepType(leftRepType, boundVars)} and " +
                    "${findRepType(rightRepType, boundVars)}")
        }
    }

    fun typeCheckFunctionCall(node: FunctionCallNode, boundVars: MutableSet<TypeVariable>) {
        val funcTypeExpr = symbolTable.getInfo(node.func)?.typeExpr
        if (funcTypeExpr == null) {
            throw IRConversionException("Unknown function ${node.func.name}")
        }

        val funcRepType = findRepType(funcTypeExpr, boundVars)

        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars) }

        // Unify the arguments to the function with the expected argument types stored for the
        // function in the symbol table
        val argTypeExprs = node.actualArgs.map { arg -> findRepType(arg.evalTypeExpr, boundVars) }
        val returnTypeExpr = node.evalTypeExpr
        val expectedFuncExpr = FunctionTypeExpression(argTypeExprs, returnTypeExpr)

        if (!unify(expectedFuncExpr, funcRepType)) {
            // If type of identifier is a known function, provide more useful error message
            if (funcRepType is FunctionTypeExpression) {
                val argRepTypes = argTypeExprs.map { argType -> findRepType(argType, boundVars) }
                throw IRConversionException("${node.func.name} expected arguments of type " +
                        "${funcRepType.argTypes}, but found ${argRepTypes}")
            } else {
                throw IRConversionException("${node.func.name} expected to have type " +
                        "${findRepType(expectedFuncExpr, boundVars)}, but found ${funcRepType}")
            }
        }
    }

    fun typeCheckAssignment(node: AssignmentNode, boundVars: MutableSet<TypeVariable>) {
        val typeExpr = symbolTable.getInfo(node.ident)?.typeExpr
        if (typeExpr == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}")
        }

        typeCheck(node.expr, boundVars)

        val nodeRepType = findRepType(node.expr.evalTypeExpr, boundVars)

        if (!unify(nodeRepType, typeExpr)) {
            throw IRConversionException("Type of ${node.ident.name} is " +
                    "${findRepType(typeExpr, boundVars)}, but assigned " +
                    "${findRepType(nodeRepType, boundVars)}")
        }
    }

    fun typeCheckVariableDefinition(
        node: VariableDefinitionNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        val typeExpr = symbolTable.getInfo(node.ident)?.typeExpr
        if (typeExpr == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}")
        }

        typeCheck(node.expr, boundVars)

        val freshNodeType = findRepType(node.expr.evalTypeExpr, boundVars)

        if (!unify(freshNodeType, typeExpr)) {
            throw IRConversionException("Type of ${node.ident.name} is " +
                    "${findRepType(typeExpr, boundVars)}, but assigned " +
                    "${findRepType(freshNodeType, boundVars)}")
        }
    }

    fun typeCheckFunctionDefinition(
        node: FunctionDefinitionNode,
        boundVars: MutableSet<TypeVariable>
    ) {
        val typeExpr = symbolTable.getInfo(node.ident)?.typeExpr
        if (typeExpr !is FunctionTypeExpression) {
            throw IRConversionException("Unknown function ${node.ident.name}")
        }

        val newBoundVars = boundVars.toHashSet()

        // Add all type variables in function type to the set of bound type variables
        typeExpr.getAllVariables().forEach { typeVar ->
            newBoundVars.add(typeVar)
        }

        typeCheck(node.body, newBoundVars)

        // Unify all returned expression types with the return type of this function
        mapOverReturns(node.body, { retNode ->
            val retTypeExpr = retNode.expr?.evalTypeExpr ?: UnitTypeExpression
            if (!unify(retTypeExpr, typeExpr.returnType)) {
                throw IRConversionException("${node.ident.name} must return " +
                        "${findRepType(typeExpr.returnType, newBoundVars)} but found " +
                        "${findRepType(retTypeExpr, newBoundVars)}")
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

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of if must be a bool, but found " +
                    "${findRepType(node.cond.evalTypeExpr, boundVars)}")
        }

        typeCheck(node.conseq, boundVars)

        if (node.altern != null) {
            typeCheck(node.altern, boundVars)
        }
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.cond, boundVars)

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of while must be a bool, but given " +
                    "${findRepType(node.cond.evalTypeExpr, boundVars)}")
        }

        typeCheck(node.body, boundVars)
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>) {
        typeCheck(node.cond, boundVars)

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of do while must be a bool, but given " +
                    "${findRepType(node.cond.evalTypeExpr, boundVars)}")
        }

        typeCheck(node.body, boundVars)
    }

    fun typeCheckFor(node: ForNode, boundVars: MutableSet<TypeVariable>) {
        if (node.init != null) {
            typeCheck(node.init, boundVars)
        }

        if (node.cond != null) {
            typeCheck(node.cond, boundVars)
            if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
                throw IRConversionException("Condition of for must be a bool, but given " +
                        "${findRepType(node.cond.evalTypeExpr, boundVars)}")
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
     * Find the inferred type for a type expression after all unification has taken place.
     * If the representative type of any child type is still a type variable and has not been
     * unified to a concrete type, then return null.
     */
    fun inferredTypeForExpression(typeExpr: TypeExpression): Type {
        val repType = findRepresentativeNode(typeExpr).type
        return when (repType) {
            is TypeVariable -> {
                // This this type variable has been bound to a param, return the param
                val boundTypeParam = boundTypeVars[repType]
                if (boundTypeParam != null) {
                    return boundTypeParam
                // Otherwise, create new type parameter and bind this type variable to it
                } else {
                    val newTypeParam = newTypeParameter()
                    boundTypeVars[repType] = newTypeParam
                    newTypeParam
                }
            }
            is UnitTypeExpression -> UnitType
            is BoolTypeExpression -> BoolType
            is StringTypeExpression -> StringType
            is IntTypeExpression -> IntType
            is FloatTypeExpression -> FloatType
            is ListTypeExpression -> {
                // Infer type for element type and reconstruct list type
                val elementRepType = findRepresentativeNode(repType.elementType).type
                val elementType = inferredTypeForExpression(elementRepType)
                return ListType(elementType)
            }
            is FunctionTypeExpression -> {
                // Infer types for arguments and return type, then reconstruct type
                val argTypes = repType.argTypes
                        .map { argType -> findRepresentativeNode(argType).type }
                        .map { argRepType -> inferredTypeForExpression(argRepType) }

                val returnRepType = findRepresentativeNode(repType.returnType).type
                val returnType = inferredTypeForExpression(returnRepType)

                return FunctionType(argTypes, returnType)
            }
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
                identInfo.type = inferredTypeForExpression(identInfo.typeExpr)
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
            node.type = inferredTypeForExpression(node.evalTypeExpr)
        }
    }

}
