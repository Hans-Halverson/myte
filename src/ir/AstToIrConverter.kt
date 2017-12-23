package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

class AstToIrConverter(val symbolTable: SymbolTable) {

    private val typeChecker = TypeChecker(symbolTable)

    /**
     * Convert a node in the ast into a node in the internal representation.
     *
     * @param stmt the ast node
     * @return the exivalent node in the internal representation
     */
    fun convert(stmt: Statement): IRNode {
        return when {
            // Literals and variables
            stmt is BoolLiteralExpression -> BoolLiteralNode(stmt.bool)
            stmt is StringLiteralExpression -> StringLiteralNode(stmt.str)
            stmt is IntLiteral -> IntLiteralNode(stmt.num)
            stmt is FloatLiteral -> FloatLiteralNode(stmt.num)
            stmt is ListLiteralExpression -> convertListLiteral(stmt)
            // Variables and functions
            stmt is IdentifierExpression -> convertVariable(stmt)
            stmt is CallExpression -> convertFunctionCall(stmt)
            stmt is AssignmentExpression -> convertAssignment(stmt)
            stmt is VariableDefinitionStatement -> convertVariableDefinition(stmt)
            stmt is FunctionDefinitionStatement -> convertFunctionDefinition(stmt)
            stmt is FunctionDefinitionExpression -> convertFunctionDefinitionExpression(stmt)
            // Math expressions
            stmt is UnaryPlusExpression -> convertUnaryPlus(stmt)
            stmt is UnaryMinusExpression -> convertUnaryMinus(stmt)
            stmt is BinaryMathOperatorExpression -> convertBinaryMathOperator(stmt)
            // Logical operators
            stmt is LogicalAndExpression -> convertLogicalAnd(stmt)
            stmt is LogicalOrExpression -> convertLogicalOr(stmt)
            stmt is LogicalNotExpression -> convertLogicalNot(stmt)
            // Comparisons
            stmt is EqualityExpression -> convertEquality(stmt)
            stmt is ComparisonExpression -> convertComparison(stmt)
            // Control flow and structure
            stmt is GroupExpression -> convert(stmt.expr)
            stmt is BlockStatement -> BlockNode(stmt.stmts.map(this::convert))
            stmt is IfStatement -> convertIf(stmt)
            stmt is WhileStatement -> convertWhile(stmt)
            stmt is DoWhileStatement -> convertDoWhile(stmt)
            stmt is ForStatement -> convertFor(stmt)
            stmt is ReturnStatement -> convertReturn(stmt)
            stmt is BreakStatement -> BreakNode
            stmt is ContinueStatement -> ContinueNode
            else -> throw IRConversionException("Unexpected statement ${stmt}")
        }
    }

    /**
     * Returns whether an identifier has the given identifier property.
     */
    fun identHasProp(ident: Identifier, prop: IdentifierProperty): Boolean {
        return symbolTable.getInfo(ident)?.props?.contains(prop) ?: false
    }

    /**
     * Returns whether an identifier is numeric.
     */
    fun isNumeric(ident: Identifier): Boolean {
        return identHasProp(ident, IdentifierProperty.NUMERIC)
    }

    /**
     * Returns whether an identifier is immutable.
     */
    fun isImmutable(ident: Identifier): Boolean {
        return identHasProp(ident, IdentifierProperty.IMMUTABLE)
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Conversion functions for each AST node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun convertFunctionDefinition(stmt: FunctionDefinitionStatement): FunctionDefinitionNode {
        val body = convert(stmt.body)
        return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, body)
    }

    fun convertFunctionDefinitionExpression(
            stmt: FunctionDefinitionExpression
    ): FunctionDefinitionNode {
        val body = convert(stmt.body)
        // Wrap the body of a function expression in a return node
        return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, ReturnNode(body)) 
    }

    fun convertVariableDefinition(stmt: VariableDefinitionStatement): VariableDefinitionNode {
        return VariableDefinitionNode(stmt.ident, convert(stmt.expr))
    }

    fun convertIf(stmt: IfStatement): IfNode {
        val cond = convert(stmt.cond)
        val conseq = convert(stmt.conseq)
        val altern = if (stmt.altern != null) convert(stmt.altern) else null

        return IfNode(cond, conseq, altern)
    }

    fun convertWhile(stmt: WhileStatement): WhileNode {
        return WhileNode(convert(stmt.cond), convert(stmt.body))
    }

    fun convertDoWhile(stmt: DoWhileStatement): DoWhileNode {
        return DoWhileNode(convert(stmt.cond), convert(stmt.body))
    }

    fun convertFor(stmt: ForStatement): ForNode {
        val init = if (stmt.init != null) convert(stmt.init) else null
        val cond = if (stmt.cond != null) convert(stmt.cond) else null
        val update = if (stmt.update != null) convert(stmt.update) else null

        return ForNode(init, cond, update, convert(stmt.body))
    }

    fun convertVariable(expr: IdentifierExpression): VariableNode {
        val info = symbolTable.getInfo(expr.ident)
        if (info == null) {
            throw IRConversionException("Unknown variable ${expr.ident.name}")
        }

        // The best known eval type of a variable is the type expression stored in the symbol table
        return VariableNode(expr.ident, info.typeExpr)
    }

    fun convertFunctionCall(expr: CallExpression): IRNode {
        val args = expr.actualArgs.map(this::convert)

        if (isNumeric(expr.func)) {
            return FunctionCallNode(expr.func, args, FloatTypeExpression)
        } else {
            val funcType = symbolTable.getInfo(expr.func)?.typeExpr

            // If type of function is known, use the return type expression as the eval type.
            // Otherwise generate a new type variable for the eval type.
            if (funcType is FunctionTypeExpression) {
                return FunctionCallNode(expr.func, args, funcType.returnType)
            } else {
                return FunctionCallNode(expr.func, args, newTypeVariable())
            }            
        }
    }

    fun convertAssignment(expr: AssignmentExpression): AssignmentNode {
        val info = symbolTable.getInfo(expr.ident)
        if (info == null) {
            throw IRConversionException("Unknown variable ${expr.ident.name}")
        }

        if (isImmutable(expr.ident)) {
            throw IRConversionException("Cannot reassign immutable variable ${expr.ident.name}")
        }

        // The best known eval type of a variable is the type expression stored in the symbol table
        return AssignmentNode(expr.ident, convert(expr.expr), info.typeExpr)
    }

    fun convertReturn(expr: ReturnStatement): ReturnNode {
        val returnVal = if (expr.expr != null) convert(expr.expr) else null 
        return ReturnNode(returnVal)
    }

    fun convertListLiteral(expr: ListLiteralExpression): ListLiteralNode {
        return ListLiteralNode(expr.elements.map(this::convert))
    }

    fun convertEquality(expr: EqualityExpression): EqualityNode {
        val left = convert(expr.left)
        val right = convert(expr.right)

        return when (expr) {
            is EqualsExpression -> EqualsNode(left, right)
            is NotEqualsExpression -> NotEqualsNode(left, right)
        }
    }

    fun convertComparison(expr: ComparisonExpression): ComparisonNode {
        val leftNode = convert(expr.left)
        val rightNode = convert(expr.right)

        return when (expr) {
            is LessThanExpression -> LessThanNode(leftNode, rightNode)
            is LessThanOrEqualExpression -> LessThanOrEqualNode(leftNode, rightNode)
            is GreaterThanExpression -> GreaterThanNode(leftNode, rightNode)
            is GreaterThanOrEqualExpression -> GreaterThanOrEqualNode(leftNode, rightNode)
        }
    }

    fun convertLogicalAnd(expr: LogicalAndExpression): LogicalAndNode {
        return LogicalAndNode(convert(expr.left), convert(expr.right))
    }

    fun convertLogicalOr(expr: LogicalOrExpression): LogicalOrNode {
        return LogicalOrNode(convert(expr.left), convert(expr.right))
    }

    fun convertLogicalNot(expr: LogicalNotExpression): LogicalNotNode {
        return LogicalNotNode(convert(expr.expr))
    }

    fun convertBinaryMathOperator(expr: BinaryMathOperatorExpression): BinaryMathOperatorNode {
        val leftNode = convert(expr.left)
        val rightNode = convert(expr.right)

        return when (expr) {
            is AddExpression -> AddNode(leftNode, rightNode)
            is SubtractExpression -> SubtractNode(leftNode, rightNode)
            is MultiplyExpression -> MultiplyNode(leftNode, rightNode)
            is DivideExpression -> DivideNode(leftNode, rightNode)
            is ExponentExpression -> ExponentNode(leftNode, rightNode)
        }
    }

    fun convertUnaryPlus(expr: UnaryPlusExpression): IdentityNode {
        return IdentityNode(convert(expr.expr))
    }

    fun convertUnaryMinus(expr: UnaryMinusExpression): NegateNode {
        return NegateNode(convert(expr.expr))
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type and structure checks after IR tree has been created
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Infer the types for the internal representation, and infer types for every symbol in
     * in the symbol table.
     * 
     * @param nodes a list of IRNodes corresponding to the complete internal representation
     * @throws IRConversionException if there are type clashes or if types can not be inferred
     *         for every node in the IR and for every symbol in the symbol table
     */
    fun inferTypes(nodes: List<IRNode>) {
        nodes.forEach(typeChecker::typeCheck)
        nodes.forEach(typeChecker::inferIRTypes)
        typeChecker.inferSymbolTypes()
    }

    /**
     * Assert that the IR has the correct physical structure (e.g. functions always return with
     * the correct type, control flow statements like break, continue, return only occur in 
     * valid locations).
     */
    fun assertIRStructure(node: IRNode) {
        functionsReturnCorrectType(node)
        jumpsInAllowedPlaces(node, false, false)
    }

    /**
     * Return whether all execution paths in the subtree rooted at an IR node have a return
     * statement.
     */
    fun allPathsHaveReturn(node: IRNode): Boolean {
        return when (node) {
            is ReturnNode -> true
            is IfNode -> allPathsHaveReturn(node.conseq) && 
                    node.altern != null &&
                    allPathsHaveReturn(node.altern)
            is BlockNode -> allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
            else -> false
        }
    }

    /**
     * Check whether all return statements in the subtree reeoted an an IR node return the correct
     * type.
     *
     * @throws IRConversionException if a return is found that returns the incorrect type
     */
    fun returnsHaveType(node: IRNode, type: Type) {
        when (node) {
            is ReturnNode -> {
                val returnType = if (node.expr == null) UnitType else node.expr.type
                if (returnType != type) {
                    throw IRConversionException("Return of ${type} expected, but instead found " +
                            "${returnType}")
                }
            }
            is BlockNode -> node.nodes.forEach { n -> returnsHaveType(n, type) }
            is IfNode -> {
                returnsHaveType(node.conseq, type)
                if (node.altern != null) {
                    returnsHaveType(node.altern, type)
                }
            }
            is WhileNode -> returnsHaveType(node.body, type)
            is DoWhileNode -> returnsHaveType(node.body, type)
            is ForNode -> returnsHaveType(node.body, type)
        }
    }

    /**
     * Check whether all functions in the subtree rooted at a node always return with the correct
     * type in all possible execution paths.
     *
     * @throws IRConversionException if a return is found that returns the incorrect type, or if
     *         an execution path does not end in a return
     */
    fun functionsReturnCorrectType(node: IRNode) {
        node.map { func ->
            // Find all function definitions, and save expected return type
            if (func is FunctionDefinitionNode) {
                val funcType = symbolTable.getInfo(func.ident)?.type
                if (funcType !is FunctionType) {
                    throw IRConversionException("Unknown function ${func.ident.name}")
                }

                // For each function definition, all child returns must return correct type
                returnsHaveType(func, funcType.returnType)

                // Check that all paths in the function return a value
                if (!allPathsHaveReturn(func.body)) {
                    throw IRConversionException("Every branch of ${func.ident.name} must return " +
                            "a value")
                }
            }
        }
    }

    /**
     * Check whether all break, continue, and return statements in the subtree rooted at a node
     * occur in valid locations.
     *
     * @param node the root of the IR tree to check
     * @param allowReturn whether to allow a return statement in this subtree
     * @param allowBreakOrContinue whether to allow a break or continue statement in this subtree
     * @throws IRConversionException if a break, return, or continue occurs at an invalid location
     */
    fun jumpsInAllowedPlaces(node: IRNode, allowReturn: Boolean, allowBreakOrContinue: Boolean) {
        when (node) {
            is BlockNode -> node.nodes.map { n -> 
                jumpsInAllowedPlaces(n, allowReturn, allowBreakOrContinue )
            }
            is IfNode -> {
                jumpsInAllowedPlaces(node.conseq, allowReturn, allowBreakOrContinue)
                if (node.altern != null) {
                    jumpsInAllowedPlaces(node.altern, allowReturn, allowBreakOrContinue)
                }
            }
            is WhileNode -> jumpsInAllowedPlaces(node.body, allowReturn, true)
            is DoWhileNode -> jumpsInAllowedPlaces(node.body, allowReturn, true)
            is ForNode -> {
                if (node.init != null) {
                    jumpsInAllowedPlaces(node.init, allowReturn, false)
                }
                jumpsInAllowedPlaces(node.body, allowReturn, true)
            }
            is FunctionDefinitionNode -> jumpsInAllowedPlaces(node.body, true, false)
            is ReturnNode -> if (!allowReturn) {
                throw IRConversionException("Return must appear in function body")
            }
            is BreakNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Break must appear in loop")
            }
            is ContinueNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Continue must appear in loop")
            }
            else -> {}
        }
    }
}
