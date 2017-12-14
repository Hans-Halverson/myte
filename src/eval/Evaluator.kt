package myte.eval

import myte.eval.values.*
import myte.ir.nodes.*
import myte.parser.*
import myte.shared.*

import java.util.Stack


class Evaluator(val symbolTable: SymbolTable) {

	private val environment = Environment()

	fun evaluate(node: IRNode, env: Environment = environment): Value {
		return when (node) {
			is ConstantNode -> NumberValue(node.num)
			is BooleanLiteralNode -> BooleanValue(node.bool)
			is VariableNode -> env.lookup(node.ident)
			is BinaryMathOperatorNode -> evalBinaryMathOperator(node, env)
			is LogicalNotNode -> BooleanValue(!evalBoolean(node.node, env).bool)
			is LogicalAndNode -> evalLogicalAnd(node, env)
			is LogicalOrNode -> evalLogicalOr(node, env)
			is EqualityNode -> evalEquality(node, env)
			is ComparisonNode -> evalComparison(node, env)
			is NumericCallNode -> evalNumericCall(node, env)
			is FunctionCallNode -> evalFunctionCall(node, env)
			is AssignmentNode -> evalAssignment(node, env)
			is VariableDefinitionNode -> {
				env.extend(node.ident, evaluate(node.expr))
				return UnitValue
			}
			is FunctionDefinitionNode -> {
				val type = symbolTable.getInfo(node.ident)?.type
				if (type !is FunctionType) {
					throw EvaluationException("Unknown function ${node.ident.name}")
				}
				env.extend(node.ident, Closure(node.ident, node.formalArgs, node.body, env.copy(), type))
				return UnitValue
			}
			is BlockNode -> evalBlock(node, env)
			is IfNode -> evalIf(node, env)
			is WhileNode -> evalWhile(node, env)
			is DoWhileNode -> evalDoWhile(node, env)
			is ForNode -> evalFor(node, env)
			is ReturnNode -> evalReturn(node, env)
			else -> throw EvaluationException("Unknown IR node ${node}")
		}
	}

	fun evalNumber(node: IRNode, env: Environment): NumberValue {
		val value = evaluate(node, env)
		if (value !is NumberValue) {
			throw EvaluationException("Expected ${value} to be a number")
		}

		return value
	}

	fun evalBoolean(node: IRNode, env: Environment): BooleanValue {
		val value = evaluate(node, env)
		if (value !is BooleanValue) {
			throw EvaluationException("Expected ${value} to be a boolean")
		}

		return value
	}

	fun evalBinaryMathOperator(node: BinaryMathOperatorNode, env: Environment): NumberValue {
		val left = evalNumber(node.left, env)
		val right = evalNumber(node.right, env)

		return NumberValue(node.compute(left.num, right.num))
	}

	fun evalLogicalAnd(node: LogicalAndNode, env: Environment): BooleanValue {
		val leftValue = evalBoolean(node.left, env)

		if (leftValue.bool) {
			return evalBoolean(node.right, env)
		} else {
			return leftValue;
		}
	}

	fun evalLogicalOr(node: LogicalOrNode, env: Environment): BooleanValue {
		val leftValue = evalBoolean(node.left, env)

		if (!leftValue.bool) {
			return evalBoolean(node.right, env)
		} else {
			return leftValue;
		}
	}

	fun evalEquality(node: EqualityNode, env: Environment): BooleanValue {
		val leftVal = evaluate(node.left, env)
		val rightVal = evaluate(node.right, env)

		return BooleanValue(node.compare(leftVal, rightVal))
	}

	fun evalComparison(node: ComparisonNode, env: Environment): BooleanValue {
		val leftVal = evalNumber(node.left, env)
		val rightVal = evalNumber(node.right, env)

		return BooleanValue(node.compare(leftVal.num, rightVal.num))
	}

	fun evalNumericCall(node: NumericCallNode, env: Environment): Value {
		val closure = env.lookup(node.func)
		if (closure !is Closure) {
			throw EvaluationException("Cannot call ${closure}, can only call functions")
		}

		if (node.actualArgs.size != closure.formalArgs.size) {
			throw EvaluationException("${node.func} expected ${closure.formalArgs.size} arguments, but received ${node.actualArgs.size}")
		}

		val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }

		val applicationEnv: Environment = closure.environment.copy()
		closure.formalArgs.zip(actualArgs).forEach { (ident, value) -> applicationEnv.extend(ident, value) }

		return evaluate(closure.body, applicationEnv)
	}

	fun evalFunctionCall(node: FunctionCallNode, env: Environment): Value {
		val closure = env.lookup(node.func)
		if (closure !is Closure) {
			throw EvaluationException("Cannot call ${closure}, can only call functions")
		}

		if (node.actualArgs.size != closure.formalArgs.size) {
			throw EvaluationException("${node.func} expected ${closure.formalArgs.size} arguments, but received ${node.actualArgs.size}")
		}

		val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }

		val applicationEnv: Environment = closure.environment.copy()
		applicationEnv.enterScope()

		closure.formalArgs.zip(actualArgs).forEach { (ident, value) -> applicationEnv.extend(ident, value) }

		try {
			evaluate(closure.body, applicationEnv)
		} catch (returnException: Return) {
			applicationEnv.exitScope()
			return returnException.returnValue
		}

		throw EvaluationException("No return value")
	}

	fun evalAssignment(node: AssignmentNode, env: Environment): Value {
		val value = evaluate(node.expr, env)
		env.reassign(node.ident, value)

		return value
	}

	fun evalBlock(node: BlockNode, env: Environment): UnitValue {
		env.enterScope()
		for (childNode in node.nodes) {
			evaluate(childNode, env)
		}

		env.exitScope()

		return UnitValue
	}

	fun evalIf(node: IfNode, env: Environment): UnitValue {
		val cond = evalBoolean(node.cond, env)

		if (cond.bool) {
			evaluate(node.conseq, env)
		} else if (node.altern != null) {
			evaluate(node.altern, env)
		}

		return UnitValue
	}

	fun evalWhile(node: WhileNode, env: Environment): UnitValue {
		var cond = evalBoolean(node.cond, env)

		while (cond.bool) {
			evaluate(node.body, env)
			cond = evalBoolean(node.cond, env)
		}

		return UnitValue
	}

	fun evalDoWhile(node: DoWhileNode, env: Environment): UnitValue {
		do {
			evaluate(node.body, env)
			val cond = evalBoolean(node.cond, env)
		} while (cond.bool)

		return UnitValue
	}

	fun evalFor(node: ForNode, env: Environment): UnitValue {
		env.enterScope()

		if (node.init != null) {
			evaluate(node.init, env)
		}

		var condition = true
		if (node.cond != null) {
			condition = evalBoolean(node.cond, env).bool
		}

		while (condition) {
			evaluate(node.body, env)

			if (node.update != null) {
				evaluate(node.update, env)
			}

			if (node.cond != null) {
				condition = evalBoolean(node.cond, env).bool
			}
		}

		env.exitScope()

		return UnitValue
	}

	fun evalReturn(node: ReturnNode, env: Environment): UnitValue {
		val returnVal = if (node.expr != null) evaluate(node.expr, env) else UnitValue
		throw Return(returnVal)
	}

}
