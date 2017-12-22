package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

val NEGATIVE_ONE_INT_NODE = IntLiteralNode(-1)
val NEGATIVE_ONE_FLOAT_NODE = FloatLiteralNode(-1.0)

class AstToIrConverter(val symbolTable: SymbolTable) {

	private val typeChecker = TypeChecker(symbolTable)

	fun convert(stmt: Statement): IRNode {
		return when {
			// General statements / expressions
			stmt is IdentifierExpression -> convertVariable(stmt)
			stmt is VariableDefinitionStatement -> convertVariableDefinition(stmt)
			stmt is FunctionDefinitionStatement -> convertFunctionDefinition(stmt)
			stmt is FunctionDefinitionExpression -> convertFunctionDefinitionExpression(stmt)
			stmt is BlockStatement -> BlockNode(stmt.stmts.map(this::convert))
			stmt is IfStatement -> convertIf(stmt)
			stmt is WhileStatement -> convertWhile(stmt)
			stmt is DoWhileStatement -> convertDoWhile(stmt)
			stmt is ForStatement -> convertFor(stmt)
			stmt is CallExpression -> convertFunctionCall(stmt)
			stmt is AssignmentExpression -> convertAssignment(stmt)
			stmt is GroupExpression -> convert(stmt.expr)
			stmt is ReturnStatement -> convertReturn(stmt)
			stmt is BreakStatement -> BreakNode
			stmt is ContinueStatement -> ContinueNode
			stmt is StringLiteralExpression -> StringLiteralNode(stmt.str)
			stmt is ListLiteralExpression -> convertListLiteral(stmt)

			// Boolean expressions
			stmt is BooleanLiteralExpression -> BooleanLiteralNode(stmt.bool)
			stmt is EqualityExpression -> convertEquality(stmt)
			stmt is ComparisonExpression -> convertComparison(stmt)
			stmt is LogicalAndExpression -> convertLogicalAnd(stmt)
			stmt is LogicalOrExpression -> convertLogicalOr(stmt)
			stmt is LogicalNotExpression -> convertLogicalNot(stmt)

			// Numeric expressions
			stmt is IntLiteral -> IntLiteralNode(stmt.num)
			stmt is FloatLiteral -> FloatLiteralNode(stmt.num)
			stmt is BinaryMathOperatorExpression -> convertBinaryMathOperator(stmt)
			stmt is UnaryPlusExpression -> convertUnaryPlus(stmt)
			stmt is UnaryMinusExpression -> convertUnaryMinus(stmt)
			else -> throw IRConversionException("Unexpected statement ${stmt}")
		}
	}

	fun identHasProp(ident: Identifier, prop: IdentifierProperty): Boolean {
		return symbolTable.getInfo(ident)?.props?.contains(prop) ?: false
	}

	fun isNumeric(ident: Identifier): Boolean {
		return identHasProp(ident, IdentifierProperty.NUMERIC)
	}

	fun isImmutable(ident: Identifier): Boolean {
		return identHasProp(ident, IdentifierProperty.IMMUTABLE)
	}

	fun convertFunctionDefinition(stmt: FunctionDefinitionStatement): FunctionDefinitionNode {
		val body = convert(stmt.body)

		if (!allPathsHaveReturn(body)) {
			throw IRConversionException("Every branch of ${stmt.ident.name} must return a value")
		}

		return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, body)
	}

	fun convertFunctionDefinitionExpression(stmt: FunctionDefinitionExpression): FunctionDefinitionNode {
		val body = convert(stmt.body)

		return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, ReturnNode(body)) 
	}

	fun convertVariableDefinition(stmt: VariableDefinitionStatement): VariableDefinitionNode {
		val info = symbolTable.getInfo(stmt.ident)
		if (info == null) {
			throw IRConversionException("Unknown variable ${stmt.ident.name}")
		}

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

		return VariableNode(expr.ident, info.typeExpr)
	}

	fun convertFunctionCall(expr: CallExpression): IRNode {
		val args = expr.actualArgs.map(this::convert)

		if (isNumeric(expr.func)) {
			return FunctionCallNode(expr.func, args, FloatTypeExpression)
		} else {
			val funcType = symbolTable.getInfo(expr.func)?.typeExpr
			val returnType = if (funcType is FunctionTypeExpression) funcType.returnParam else newTypeVariable()
			
			return FunctionCallNode(expr.func, args, returnType)
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

	fun inferTypes(nodes: List<IRNode>) {
		nodes.forEach(typeChecker::typeCheck)
		nodes.forEach(typeChecker::inferIRTypes)
		typeChecker.inferSymbolTypes()
	}

	fun assertIRStructure(node: IRNode) {
		functionsReturnCorrectType(node)
		jumpsInAllowedPlaces(node, false, false)
	}

	fun allPathsHaveReturn(node: IRNode): Boolean {
		return when (node) {
			is ReturnNode -> true
			is IfNode -> allPathsHaveReturn(node.conseq) && (node.altern != null && allPathsHaveReturn(node.altern))
			is BlockNode -> allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
			else -> false
		}
	}

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
			}
		}
	}

	fun returnsHaveType(node: IRNode, type: Type) {
		when (node) {
			is ReturnNode -> {
				val returnType = if (node.expr == null) UnitType else node.expr.type
				if (returnType != type) {
					throw IRConversionException("Return of ${type} expected, but instead found ${returnType}")
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

	fun jumpsInAllowedPlaces(node: IRNode, allowReturn: Boolean, allowBreakOrContinue: Boolean) {
		when (node) {
			is BlockNode -> node.nodes.map { n -> jumpsInAllowedPlaces(n, allowReturn, allowBreakOrContinue )}
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
			is ReturnNode -> if (!allowReturn) throw IRConversionException("Return must appear in function body")
			is BreakNode -> if (!allowBreakOrContinue) throw IRConversionException("Break must appear in loop")
			is ContinueNode -> if (!allowBreakOrContinue) throw IRConversionException("Continue must appear in loop")
			else -> {}
		}
	}
}
