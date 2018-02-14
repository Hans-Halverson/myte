package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

class AstToIrConverter(var symbolTable: SymbolTable) {

    private val typeChecker = TypeChecker(symbolTable)

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
        typeChecker.resetSymbolTable(newSymbolTable)
    }

    /**
     * Convert a node in the ast into a node in the internal representation.
     *
     * @param stmt the ast node
     * @return the exivalent node in the internal representation
     */
    fun convert(stmt: Statement): IRNode {
        return when {
            // Literals
            stmt is BoolLiteralExpression -> BoolLiteralNode(stmt.bool, stmt.startContext)
            stmt is StringLiteralExpression -> StringLiteralNode(stmt.str, stmt.startContext)
            stmt is IntLiteral -> IntLiteralNode(stmt.num, stmt.startContext)
            stmt is FloatLiteral -> FloatLiteralNode(stmt.num, stmt.startContext)
            stmt is VectorLiteralExpression -> convertVectorLiteral(stmt)
            stmt is TupleLiteralExpression -> convertTupleLiteral(stmt)
            // Variables and functions
            stmt is VariableExpression -> convertVariable(stmt)
            stmt is FunctionCallExpression -> convertFunctionCall(stmt)
            stmt is TypeConstructorExpression -> convertTypeConstructor(stmt)
            stmt is KeyedAccessExpression -> convertKeyedAccess(stmt)
            stmt is KeyedAssignmentExpression -> convertKeyedAssignment(stmt)
            stmt is VariableAssignmentExpression -> convertVariableAssignment(stmt)
            stmt is VariableDefinitionStatement -> convertVariableDefinition(stmt)
            stmt is FunctionDefinitionStatement -> convertFunctionDefinition(stmt)
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
            stmt is BlockStatement -> BlockNode(stmt.stmts.map(this::convert), stmt.startContext)
            stmt is IfStatement -> convertIf(stmt)
            stmt is WhileStatement -> convertWhile(stmt)
            stmt is DoWhileStatement -> convertDoWhile(stmt)
            stmt is ForStatement -> convertFor(stmt)
            stmt is MatchStatement -> convertMatch(stmt)
            stmt is ReturnStatement -> convertReturn(stmt)
            stmt is BreakStatement -> BreakNode(stmt.breakContext)
            stmt is ContinueStatement -> ContinueNode(stmt.continueContext)
            else -> throw IRConversionException("Unknown statement ${stmt}", stmt.startContext)
        }
    }

    /**
     * Returns whether an identifier has the given identifier property.
     */
    fun identHasProp(ident: Identifier, prop: IdentifierProperty): Boolean {
        return symbolTable.getInfo(ident)?.props?.contains(prop) ?: false
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

        // Check that all paths in the function return a value
        if (!allPathsHaveReturn(body)) {
            throw IRConversionException("Every branch of ${stmt.ident.name} must return " +
                    "a value", stmt.identContext)
        }

        return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, body, stmt.identContext,
                stmt.startContext)
    }

    fun convertVariableDefinition(stmt: VariableDefinitionStatement): VariableDefinitionNode {
        return VariableDefinitionNode(stmt.ident, convert(stmt.expr), stmt.identContext,
                stmt.startContext)
    }

    fun convertIf(stmt: IfStatement): IfNode {
        val cond = convert(stmt.cond)
        val conseq = convert(stmt.conseq)
        val altern = if (stmt.altern != null) convert(stmt.altern) else null

        return IfNode(cond, conseq, altern, stmt.startContext)
    }

    fun convertWhile(stmt: WhileStatement): WhileNode {
        return WhileNode(convert(stmt.cond), convert(stmt.body), stmt.startContext)
    }

    fun convertDoWhile(stmt: DoWhileStatement): DoWhileNode {
        return DoWhileNode(convert(stmt.cond), convert(stmt.body), stmt.startContext)
    }

    fun convertFor(stmt: ForStatement): ForNode {
        val init = if (stmt.init != null) convert(stmt.init) else null
        val cond = if (stmt.cond != null) convert(stmt.cond) else null
        val update = if (stmt.update != null) convert(stmt.update) else null

        return ForNode(init, cond, update, convert(stmt.body), stmt.startContext)
    }

    fun convertMatch(stmt: MatchStatement): MatchNode {
        val expr = convert(stmt.expr)
        val cases = stmt.cases.map { (pattern, statement) ->
            Pair(convert(pattern), convert(statement))
        }

        return MatchNode(expr, cases, stmt.startContext)
    }

    fun convertVariable(expr: VariableExpression): IRNode {
        val info = symbolTable.getInfo(expr.ident)
        if (info == null) {
            throw IRConversionException("Unknown variable ${expr.ident.name}", expr.identContext)
        }

        return VariableNode(expr.ident, expr.identContext)
    }

    fun convertFunctionCall(expr: FunctionCallExpression): FunctionCallNode {
        val func = convert(expr.func)
        val args = expr.actualArgs.map(this::convert)

        return FunctionCallNode(func, args, expr.identContext)
    }

    fun convertTypeConstructor(expr: TypeConstructorExpression): TypeConstructorNode {
        val args = expr.actualArgs.map(this::convert)

        return TypeConstructorNode(expr.adtVariant, args, expr.identContext)
    }

    fun convertKeyedAccess(expr: KeyedAccessExpression): KeyedAccessNode {
        return KeyedAccessNode(convert(expr.container), convert(expr.key), expr.accessContext)
    }

    fun convertKeyedAssignment(expr: KeyedAssignmentExpression): KeyedAssignmentNode {
        // Convert the keyed access, and use its properties to construct the keyed assignment
        val keyedAccess = convertKeyedAccess(expr.lValue)
        return KeyedAssignmentNode(keyedAccess.container, keyedAccess.key,
                convert(expr.rValue), expr.accessContext)
    }

    fun convertVariableAssignment(expr: VariableAssignmentExpression): VariableAssignmentNode {
        val info = symbolTable.getInfo(expr.lValue)
        if (info == null) {
            throw IRConversionException("Unknown variable ${expr.lValue.name}", expr.identContext)
        }

        if (info.idClass != IdentifierClass.VARIABLE) {
            throw IRConversionException("Cannot reassign ${expr.lValue.name}, can only " +
                    "reassign variables", expr.identContext)
        }

        if (isImmutable(expr.lValue)) {
            throw IRConversionException("Cannot reassign immutable variable ${expr.lValue.name}",
                    expr.identContext)
        }

        return VariableAssignmentNode(expr.lValue, convert(expr.rValue), expr.identContext)
    }

    fun convertReturn(expr: ReturnStatement): ReturnNode {
        val returnVal = if (expr.expr != null) convert(expr.expr) else null 
        return ReturnNode(returnVal, expr.returnContext)
    }

    fun convertVectorLiteral(expr: VectorLiteralExpression): VectorLiteralNode {
        return VectorLiteralNode(expr.elements.map(this::convert), expr.startContext)
    }

    fun convertTupleLiteral(expr: TupleLiteralExpression): TupleLiteralNode {
        return TupleLiteralNode(expr.elements.map(this::convert), expr.startContext)
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
        return LogicalNotNode(convert(expr.expr), expr.startContext)
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
        return IdentityNode(convert(expr.expr), expr.startContext)
    }

    fun convertUnaryMinus(expr: UnaryMinusExpression): NegateNode {
        return NegateNode(convert(expr.expr), expr.startContext)
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
        val boundVars: MutableSet<TypeVariable> = hashSetOf()
        nodes.forEach { node -> typeChecker.typeCheck(node, boundVars, true) }

        typeChecker.inferFunctionTypes()
        typeChecker.inferVariableTypes()

        nodes.forEach(typeChecker::inferIRTypes)
    }

    /**
     * Assert that the IR has the correct physical structure (e.g. functions always return with
     * the correct type, control flow statements like break, continue, return only occur in 
     * valid locations).
     */
    fun assertIRStructure(node: IRNode) {
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
            is MatchNode -> node.cases.map({ (_, stmt) -> allPathsHaveReturn(stmt) })
                                      .all({ x -> x })
            is BlockNode -> allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
            else -> false
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
                if (node.update != null) {
                    jumpsInAllowedPlaces(node.update, allowReturn, false)
                }
                jumpsInAllowedPlaces(node.body, allowReturn, true)
            }
            is MatchNode -> {
                node.cases.forEach { (_, stmt) ->
                    jumpsInAllowedPlaces(stmt, allowReturn, allowBreakOrContinue)
                }
            }
            is FunctionDefinitionNode -> jumpsInAllowedPlaces(node.body, true, false)
            is ReturnNode -> if (!allowReturn) {
                throw IRConversionException("Return must appear in function body",
                        node.returnContext)
            }
            is BreakNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Break must appear in loop", node.breakContext)
            }
            is ContinueNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Continue must appear in loop", node.continueContext)
            }
        }
    }
}
