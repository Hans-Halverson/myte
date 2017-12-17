package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

val NEGATIVE_ONE_INT_NODE = IntLiteralNode(-1)
val NEGATIVE_ONE_FLOAT_NODE = FloatLiteralNode(-1.0)

class AstToIrConverter(val symbolTable: SymbolTable) {

	fun convert(stmt: Statement): IRNode {
		return when {
			// General statements / expressions
			stmt is IdentifierExpression -> convertVariable(stmt)
			stmt is VariableDefinitionStatement -> convertVariableDefinition(stmt)
			stmt is FunctionDefinitionStatement -> convertFunctionDefinition(stmt)
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

	fun isNumericFunction(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.NUMERIC_FUNCTION
	}

	fun isFunction(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.FUNCTION
	}

	fun convertFunctionDefinition(stmt: FunctionDefinitionStatement): FunctionDefinitionNode {
		val type = symbolTable.getInfo(stmt.ident)?.type
		if (type !is FunctionType) {
			throw IRConversionException("Unknown function ${stmt.ident.name}")
		}

		val body = convert(stmt.body)

		if (!returnsHaveType(body, type.returnType)) {
			throw IRConversionException("${stmt.ident.name} must return ${type.returnType}")
		}

		if (!allPathsHaveReturn(body)) {
			throw IRConversionException("Every branch of ${stmt.ident.name} must return a value")
		}

		return FunctionDefinitionNode(stmt.ident, stmt.formalArgs, body)
	}

	fun convertVariableDefinition(stmt: VariableDefinitionStatement): VariableDefinitionNode {
		val info = symbolTable.getInfo(stmt.ident)
		if (info == null) {
			throw IRConversionException("Unknown variable ${stmt.ident.name}")
		}

		val body = convert(stmt.expr)

		if (info.type != body.type) {
			throw IRConversionException("${stmt.ident.name} has type ${info.type}, but was assigned ${body.type}")
		}

		return VariableDefinitionNode(stmt.ident, convert(stmt.expr))
	}

	fun convertIf(stmt: IfStatement): IfNode {
		val cond = convert(stmt.cond)
		if (cond.type !is BoolType) {
			throw IRConversionException("Condition of if must be a bool, but given ${cond.type}")
		}

		val conseq = convert(stmt.conseq)
		val altern = if (stmt.altern != null) convert(stmt.altern) else null

		return IfNode(cond, conseq, altern)
	}

	fun convertWhile(stmt: WhileStatement): WhileNode {
		val cond = convert(stmt.cond)
		if (cond.type !is BoolType) {
			throw IRConversionException("Condition of while must be a bool, but given ${cond.type}")
		}

		return WhileNode(cond, convert(stmt.body))
	}

	fun convertDoWhile(stmt: DoWhileStatement): DoWhileNode {
		val cond = convert(stmt.cond)
		if (cond.type !is BoolType) {
			throw IRConversionException("Condition of do while must be a bool, but given ${cond.type}")
		}

		return DoWhileNode(cond, convert(stmt.body))
	}

	fun convertFor(stmt: ForStatement): ForNode {
		val init = if (stmt.init != null) convert(stmt.init) else null

		val cond = if (stmt.cond != null) convert(stmt.cond) else null
		if (cond != null && cond.type !is BoolType) {
			throw IRConversionException("Condition of for must be a bool, but given ${cond.type}")
		}

		val update = if (stmt.update != null) convert(stmt.update) else null

		return ForNode(init, cond, update, convert(stmt.body))
	}

	fun convertVariable(expr: IdentifierExpression): VariableNode {
		val info = symbolTable.getInfo(expr.ident)
		if (info == null) {
			throw IRConversionException("Unknown variable ${expr.ident.name}")
		}

		return VariableNode(expr.ident, info.type)
	}

	fun convertFunctionCall(expr: CallExpression): IRNode {
		val funcType = symbolTable.getInfo(expr.func)?.type
		if (funcType !is FunctionType) {
			throw IRConversionException("Unknown function ${expr.func.name}")
		}

		val args = expr.actualArgs.map(this::convert)
		val actualArgTypes = args.map { arg -> arg.type }

		if (actualArgTypes != funcType.argTypes) {
			throw IRConversionException("${expr.func.name} expected arguments of type ${funcType.argTypes}, but found ${actualArgTypes}")
		}

		if (isNumericFunction(expr.func)) {
			return NumericCallNode(expr.func, args)
		} else {
			return FunctionCallNode(expr.func, args, funcType.returnType)
		}
	}

	fun convertAssignment(expr: AssignmentExpression): AssignmentNode {
		val info = symbolTable.getInfo(expr.ident)
		if (info == null) {
			throw IRConversionException("Unknown variable ${expr.ident.name}")
		}

		val body = convert(expr.expr)

		if (info.type != body.type) {
			throw IRConversionException("Type of ${expr.ident.name} is ${info.type}, but assigned ${body.type}")
		}

		return AssignmentNode(expr.ident, body, info.type)
	}

	fun convertReturn(expr: ReturnStatement): ReturnNode {
		val returnVal = if (expr.expr != null) convert(expr.expr) else null 
		return ReturnNode(returnVal)
	}

	fun convertEquality(expr: EqualityExpression): EqualityNode {
		val left = convert(expr.left)
		val right = convert(expr.right)

		if (left.type != right.type) {
			throw IRConversionException("Cannot check equality between different types, found ${left.type} and ${right.type}")
		}

		return when (expr) {
			is EqualsExpression -> EqualsNode(left, right)
			is NotEqualsExpression -> NotEqualsNode(left, right)
		}
	}

	fun convertComparison(expr: ComparisonExpression): ComparisonNode {
		val leftNode = convert(expr.left)
		val rightNode = convert(expr.right)

		if (leftNode.type !is NumberType || rightNode.type !is NumberType) {
			throw IRConversionException("Comparison expects two numbers, found ${leftNode.type} and ${rightNode.type}")
		}

		val (left, right, type) = coerceNumbers(leftNode, rightNode)

		return when (expr) {
			is LessThanExpression -> LessThanNode(left, right, type)
			is LessThanOrEqualExpression -> LessThanOrEqualNode(left, right, type)
			is GreaterThanExpression -> GreaterThanNode(left, right, type)
			is GreaterThanOrEqualExpression -> GreaterThanOrEqualNode(left, right, type)
		}
	}

	fun convertLogicalAnd(expr: LogicalAndExpression): LogicalAndNode {
		val left = convert(expr.left)
		val right = convert(expr.right)

		if (left.type != BoolType || right.type != BoolType) {
			throw IRConversionException("Logical and expects two bools, found ${left.type} and ${right.type}")
		}

		return LogicalAndNode(left, right)
	}

	fun convertLogicalOr(expr: LogicalOrExpression): LogicalOrNode {
		val left = convert(expr.left)
		val right = convert(expr.right)

		if (left.type != BoolType || right.type != BoolType) {
			throw IRConversionException("Logical or expects two bools, found ${left.type} and ${right.type}")
		}

		return LogicalOrNode(left, right)
	}

	fun convertLogicalNot(expr: LogicalNotExpression): LogicalNotNode {
		val body = convert(expr.expr)

		if (body.type != BoolType) {
			throw IRConversionException("Logical not expects a bool, found ${body.type}")
		}

		return LogicalNotNode(body)
	}

	fun convertBinaryMathOperator(expr: BinaryMathOperatorExpression): BinaryMathOperatorNode {
		val leftNode = convert(expr.left)
		val rightNode = convert(expr.right)

		if (leftNode.type !is NumberType || rightNode.type !is NumberType) {
			throw IRConversionException("Binary math operator expected two numbers, found ${leftNode.type} and ${rightNode.type}")
		}

		val (left, right, type) = coerceNumbers(leftNode, rightNode)

		return when (expr) {
			is AddExpression -> AddNode(left, right, type)
			is SubtractExpression -> SubtractNode(left, right, type)
			is MultiplyExpression -> MultiplyNode(left, right, type)
			is DivideExpression -> DivideNode(left, right, type)
			is ExponentExpression -> ExponentNode(left, right, type)
		}
	}

	fun coerceNumbers(left: IRNode, right: IRNode): Triple<IRNode, IRNode, NumberType> {
		if (left.type is IntType && right.type is IntType) {
			return Triple(left, right, IntType)
		} else if (left.type is IntType && right.type is FloatType) {
			val intToFloat = BUILTINS[INT_TO_FLOAT_BUILTIN]!!
			val floatLeft = FunctionCallNode(intToFloat.getIdent(), listOf(left), intToFloat.type.returnType)
	
			return Triple(floatLeft, right, FloatType)
		} else if (left.type is FloatType && right.type is IntType) {
			val intToFloat = BUILTINS[INT_TO_FLOAT_BUILTIN]!!
			val floatRight = FunctionCallNode(intToFloat.getIdent(), listOf(right), intToFloat.type.returnType)
			
			return Triple(left, floatRight, FloatType)
		} else if (left.type is FloatType && right.type is FloatType) {
			return Triple(left, right, FloatType)
		} else {
			throw IRConversionException("Cannot coerce ${left.type} and ${right.type} into common type")
		}
	}

	fun convertUnaryPlus(expr: UnaryPlusExpression): IRNode {
		val body = convert(expr.expr)
		if (body.type !is NumberType) {
			throw IRConversionException("Unary plus operator expected a number type, found ${body.type}")
		}

		return body
	}

	fun convertUnaryMinus(expr: UnaryMinusExpression): MultiplyNode {
		val body = convert(expr.expr)
		return when (body.type) {
			is IntType -> MultiplyNode(NEGATIVE_ONE_INT_NODE, body, IntType)
			is FloatType -> MultiplyNode(NEGATIVE_ONE_FLOAT_NODE, body, FloatType)
			else -> throw IRConversionException("Unary minus operator expected a number type, found ${body.type}")
		}
	}

	fun allPathsHaveReturn(node: IRNode): Boolean {
		return when (node) {
			is ReturnNode -> true
			is IfNode -> allPathsHaveReturn(node.conseq) && (node.altern != null && allPathsHaveReturn(node.altern))
			is BlockNode -> allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
			else -> false
		}
	}

	fun returnsHaveType(node: IRNode, type: Type): Boolean {
		return when (node) {
			is ReturnNode -> (node.expr?.type == type) || (node.expr == null && type == UnitType)
			is BlockNode -> node.nodes.all { n -> returnsHaveType(n, type) }
			is IfNode -> returnsHaveType(node.conseq, type) && (node.altern == null || returnsHaveType(node.altern, type))
			is WhileNode -> returnsHaveType(node.body, type)
			is DoWhileNode -> returnsHaveType(node.body, type)
			is ForNode -> returnsHaveType(node.body, type)
			else -> true
		}
	}

	fun assertIRStructure(node: IRNode) {
		jumpsInAllowedPlaces(node, false, false)
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
