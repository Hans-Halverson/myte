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

class TypeChecker(val symbolTable: SymbolTable) {
    private val typeToNode: MutableMap<TypeExpression, TypeEquivalenceNode> = mutableMapOf()

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
     * Recursively find the representative type expression for a given type expression. This will
     * return the representative type expression for this type, and the representative type
     * type expressions for all child types if the given type is a type constructor.
     */
    private fun findRepType(type: TypeExpression): TypeExpression {
        val repType = findRepresentativeNode(type).type
        return when (repType) {
            is ListTypeExpression -> ListTypeExpression(findRepType(repType.elementType))
            is FunctionTypeExpression -> {
                FunctionTypeExpression(repType.argTypes.map(this::findRepType), 
                        findRepType(repType.returnType))
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
    fun typeCheck(node: IRNode) {
        when (node) {
            is ListLiteralNode -> typeCheckListLiteral(node)
            is UnaryMathOperatorNode -> typeCheckUnaryMathOperator(node)
            is BinaryMathOperatorNode -> typeCheckBinaryMathOperator(node)
            is LogicalAndNode -> typeCheckLogicalAnd(node)
            is LogicalOrNode -> typeCheckLogicalOr(node)
            is LogicalNotNode -> typeCheckLogicalNot(node)
            is EqualityNode -> typeCheckEquality(node)
            is ComparisonNode -> typeCheckComparison(node)
            is FunctionCallNode -> typeCheckFunctionCall(node)
            is AssignmentNode -> typeCheckAssignment(node)
            is VariableDefinitionNode -> typeCheckVariableDefinition(node)
            is FunctionDefinitionNode -> typeCheckFunctionDefinition(node)
            is BlockNode -> typeCheckBlock(node)
            is IfNode -> typeCheckIf(node)
            is WhileNode -> typeCheckWhile(node)
            is DoWhileNode -> typeCheckDoWhile(node)
            is ForNode -> typeCheckFor(node)
            is ReturnNode -> typeCheckReturn(node)
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unficiation functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckListLiteral(node: ListLiteralNode) {
        node.elements.forEach(this::typeCheck)

        // Attempt to unify the types of each list element with a new type variable
        val expectedElementType = newTypeVariable()
        val expectedType = ListTypeExpression(expectedElementType)

        node.elements.forEach({ element ->
            if (!unify(element.evalTypeExpr, expectedElementType)) {
                throw IRConversionException("List must have elements of same type, found " +
                        "${findRepType(element.evalTypeExpr)} and " +
                        "${findRepType(expectedElementType)}")
            }
        })

        // Attempt to unify the list eval type with the new list type variable
        if (!unify(node.evalTypeExpr, expectedType)) {
            throw IRConversionException("Cannot infer type for list, expected " +
                    "${findRepType(expectedType)} but found ${findRepType(node.evalTypeExpr)}")
        }
    }

    fun typeCheckUnaryMathOperator(node: UnaryMathOperatorNode) {
        typeCheck(node.node)

        // Unify this node's type with its child's type, and verify it is a number type
        val canUnify = unify(node.node.evalTypeExpr, node.evalTypeExpr)
        val repType = findRepType(node.evalTypeExpr)

        if (!canUnify || repType !is NumberTypeExpression) {
            throw IRConversionException("Unary math operator expects a number, found " +
                    "${findRepType(node.node.evalTypeExpr)}")
        }
    }

    fun typeCheckBinaryMathOperator(node: BinaryMathOperatorNode) {
        typeCheck(node.left)
        typeCheck(node.right)

        // Unify this node's type with both it's children's types, and verify that both
        // children have the same numeric type
        val canUnify = unify(node.left.evalTypeExpr, node.evalTypeExpr) &&
            unify(node.right.evalTypeExpr, node.evalTypeExpr)
        val repType = findRepType(node.evalTypeExpr)

        if (!canUnify || repType !is NumberTypeExpression) {
            throw IRConversionException("Binary math operator expects two numbers of same type, " +
                    "found ${findRepType(node.left.evalTypeExpr)} and " +
                    "${findRepType(node.right.evalTypeExpr)}")
        }
    }

    fun typeCheckLogicalAnd(node: LogicalAndNode) {
        typeCheck(node.left)
        typeCheck(node.right)

        if (!unify(node.left.evalTypeExpr, BoolTypeExpression) ||
                !unify(node.right.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical and expects two bools, found " +
                    "${findRepType(node.left.evalTypeExpr)} and " +
                    "${findRepType(node.right.evalTypeExpr)}")
        }
    }

    fun typeCheckLogicalOr(node: LogicalOrNode) {
        typeCheck(node.left)
        typeCheck(node.right)

        if (!unify(node.left.evalTypeExpr, BoolTypeExpression) ||
                !unify(node.right.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical or expects two bools, found " +
                    "${findRepType(node.left.evalTypeExpr)} and " +
                    "${findRepType(node.right.evalTypeExpr)}")
        }
    }

    fun typeCheckLogicalNot(node: LogicalNotNode) {
        typeCheck(node.node)

        if (!unify(node.node.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Logical not expects a bool, found " +
                    "${findRepType(node.node.evalTypeExpr)}")
        }
    }

    fun typeCheckEquality(node: EqualityNode) {
        typeCheck(node.left)
        typeCheck(node.right)

        // Create a new type variable and unify both children with it, as both children must have
        // the same unkown type.
        val typeVar = newTypeVariable()

        if (!unify(node.left.evalTypeExpr, typeVar) || !unify(node.right.evalTypeExpr, typeVar)) {
            throw IRConversionException("Cannot check equality between different types, found " +
                    "${findRepType(node.left.evalTypeExpr)} and " +
                    "${findRepType(node.right.evalTypeExpr)}")
        }
    }

    fun typeCheckComparison(node: ComparisonNode) {
        typeCheck(node.left)
        typeCheck(node.right)

        // Create a new type variable and unify both children with it, as both children must have
        // the same numeric type.
        val typeVar = newTypeVariable()
        val canUnify = unify(node.left.evalTypeExpr, typeVar) &&
            unify(node.right.evalTypeExpr, typeVar)
        val repType = findRepType(typeVar)

        if (!canUnify || repType !is NumberTypeExpression) {
            throw IRConversionException("Comparison expects two numbers of same type, found " +
                    "${findRepType(node.left.evalTypeExpr)} and " +
                    "${findRepType(node.right.evalTypeExpr)}")
        }
    }

    fun typeCheckFunctionCall(node: FunctionCallNode) {
        val funcTypeExpr = symbolTable.getInfo(node.func)?.typeExpr
        if (funcTypeExpr == null) {
            throw IRConversionException("Unknown function ${node.func.name}")
        }

        val funcRepType = findRepType(funcTypeExpr)

        node.actualArgs.map(this::typeCheck)

        // Unify the arguments to the function with the expected argument types stored for the
        // function in the symbol table
        val argTypeExprs = node.actualArgs.map { arg -> arg.evalTypeExpr }
        val expectedFuncExpr = FunctionTypeExpression(argTypeExprs, node.evalTypeExpr)

        if (!unify(expectedFuncExpr, funcRepType)) {
            // If type of identifier is a known function, provide more useful error message
            if (funcRepType is FunctionTypeExpression) {
                throw IRConversionException("${node.func.name} expected arguments of type " +
                        "${funcRepType.argTypes}, but found ${argTypeExprs.map(this::findRepType)}")
            } else {
                throw IRConversionException("${node.func.name} expected to have type " +
                        "${findRepType(expectedFuncExpr)}, but found ${funcRepType}")
            }
        }
    }

    fun typeCheckAssignment(node: AssignmentNode) {
        val typeExpr = symbolTable.getInfo(node.ident)?.typeExpr
        if (typeExpr == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}")
        }

        typeCheck(node.expr)

        if (!unify(node.expr.evalTypeExpr, typeExpr)) {
            throw IRConversionException("Type of ${node.ident.name} is ${findRepType(typeExpr)}, " +
                    "but assigned ${findRepType(node.expr.evalTypeExpr)}")
        }
    }

    fun typeCheckVariableDefinition(node: VariableDefinitionNode) {
        val typeExpr = symbolTable.getInfo(node.ident)?.typeExpr
        if (typeExpr == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}")
        }

        typeCheck(node.expr)

        if (!unify(node.expr.evalTypeExpr, typeExpr)) {
            throw IRConversionException("Type of ${node.ident.name} is ${findRepType(typeExpr)}, " +
                    "but assigned ${findRepType(node.expr.evalTypeExpr)}")
        }
    }

    fun typeCheckFunctionDefinition(node: FunctionDefinitionNode) {
        typeCheck(node.body)
    }

    fun typeCheckBlock(node: BlockNode) {
        node.nodes.map(this::typeCheck)
    }

    fun typeCheckIf(node: IfNode) {
        typeCheck(node.cond)

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of if must be a bool, but found " +
                    "${findRepType(node.cond.evalTypeExpr)}")
        }

        typeCheck(node.conseq)
        if (node.altern != null) {
            typeCheck(node.altern)
        }
    }

    fun typeCheckWhile(node: WhileNode) {
        typeCheck(node.cond)

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of while must be a bool, but given " +
                    "${findRepType(node.cond.evalTypeExpr)}")
        }

        typeCheck(node.body)
    }

    fun typeCheckDoWhile(node: DoWhileNode) {
        typeCheck(node.cond)

        if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
            throw IRConversionException("Condition of do while must be a bool, but given " +
                    "${findRepType(node.cond.evalTypeExpr)}")
        }

        typeCheck(node.body)
    }

    fun typeCheckFor(node: ForNode) {
        if (node.init != null) {
            typeCheck(node.init)
        }

        if (node.cond != null) {
            typeCheck(node.cond)
            if (!unify(node.cond.evalTypeExpr, BoolTypeExpression)) {
                throw IRConversionException("Condition of for must be a bool, but given " +
                        "${findRepType(node.cond.evalTypeExpr)}")
            }
        }

        if (node.update != null) {
            typeCheck(node.update)
        }

        typeCheck(node.body)
    }

    fun typeCheckReturn(node: ReturnNode) {
        if (node.expr != null) {
            typeCheck(node.expr)
        }
    }

    /**
     * Find the inferred type for a type expression after all unification has taken place.
     * If the representative type of any child type is still a type variable and has not been
     * unified to a concrete type, then return null.
     */
    fun inferredTypeForExpression(typeExpr: TypeExpression): Type? {
        val repType = findRepresentativeNode(typeExpr).type
        return when (repType) {
            is TypeVariable -> null
            is UnitTypeExpression -> UnitType
            is BoolTypeExpression -> BoolType
            is StringTypeExpression -> StringType
            is IntTypeExpression -> IntType
            is FloatTypeExpression -> FloatType
            is ListTypeExpression -> {
                // Infer type for element type, if null then return null
                val elementRepType = findRepresentativeNode(repType.elementType).type
                val elementType = inferredTypeForExpression(elementRepType)
                if (elementType == null) {
                    return null
                } else {
                    return ListType(elementType)
                }
            }
            is FunctionTypeExpression -> {
                // Infer types for arguments and return type, if any are null then return null
                val argTypes = repType.argTypes
                        .map { argType -> findRepresentativeNode(argType).type }
                        .map { argRepType -> inferredTypeForExpression(argRepType) }

                val returnRepType = findRepresentativeNode(repType.returnType).type
                val returnType = inferredTypeForExpression(returnRepType)

                if (returnType == null || argTypes.any { argType -> argType == null} ) {
                    return null
                } else {
                    return FunctionType(argTypes.map { x -> x!! } , returnType)
                }
            }
        }
    }

    /**
     * Infer types for every identifier in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place.
     */
    fun inferSymbolTypes() {
        // Infer types for every identifier
        for ((ident, identInfo) in symbolTable.identifiers) {
            val inferredType = inferredTypeForExpression(identInfo.typeExpr)
            if (inferredType == null) {
                throw IRConversionException("Could not infer type for ${ident.name}, found " +
                        "${findRepType(identInfo.typeExpr)}")
            }

            identInfo.type = inferredType
        }
    }

    /**
     * Infer types for every IR tree rooted at a list of nodes. Be sure to only infer types after
     * all type checking and unification has taken place.
     */
    fun inferIRTypes(root: IRNode) {
        // Infer return types for each IRNode
        root.map { node ->
            val inferredType = inferredTypeForExpression(node.evalTypeExpr)
            if (inferredType == null) {
                throw IRConversionException("Could not infer type for " +
                        "${findRepType(node.evalTypeExpr)}")
            }

            node.type = inferredType
        }
    }

}
