package myte.eval

import myte.eval.values.*
import myte.ir.nodes.*
import myte.shared.*

import java.util.Stack


class Evaluator(val printInternalValues: Boolean = true) {

	private val environment = Environment()

	private fun printIfEnabled(value: Value) {
		if (printInternalValues) {
			printValue(value)
		}
	}

	fun evaluate(node: IRNode, env: Environment = environment): Value {
		return when (node) {
			is ConstantNode -> NumberValue(node.num)
			is BooleanLiteralNode -> BooleanValue(node.bool)
			is NumericVariableNode -> env.lookup(node.ident)
			is VariableNode -> env.lookup(node.ident)
			is AddNode -> NumberValue(evalNumber(node.left, env).num + evalNumber(node.right, env).num)
			is SubtractNode -> NumberValue(evalNumber(node.left, env).num - evalNumber(node.right, env).num)
			is MultiplyNode -> NumberValue(evalNumber(node.left, env).num * evalNumber(node.right, env).num)
			is DivideNode -> NumberValue(evalNumber(node.numer, env).num / evalNumber(node.denom, env).num)
			is ExponentNode -> NumberValue(Math.pow(evalNumber(node.base, env).num, evalNumber(node.exponent, env).num))
			is LogicalNotNode -> BooleanValue(!evalBoolean(node.node, env).bool)
			is LogicalAndNode -> evalLogicalAnd(node, env)
			is LogicalOrNode -> evalLogicalOr(node, env)
			is EqualsNode -> BooleanValue(evaluate(node.left, env) == evaluate(node.right, env))
			is NotEqualsNode -> BooleanValue(evaluate(node.left, env) != evaluate(node.right, env))
			is ComparisonNode -> evalComparison(node, env)
			is NumericCallNode -> evalNumericCall(node, env)
			is FunctionCallNode -> evalFunctionCall(node, env)
			is NumericAssignmentNode -> evalNumericAssignment(node, env)
			is AssignmentNode -> evalAssignment(node, env)
			is DefineNumericVariable -> {
				env.extend(node.ident, evaluate(node.expr))
				return UnitValue()
			}
			is DefineNumericFunction -> {
				env.extend(node.ident, NumericClosure(node.formalArgs, node.expr, env.copy()))
				return UnitValue()
			}
			is VariableDefinitionNode -> {
				env.extend(node.ident, evaluate(node.expr))
				return UnitValue()
			}
			is FunctionDefinitionNode -> {
				env.extend(node.ident, Closure(node.formalArgs, node.stmt, env.copy()))
				return UnitValue()
			}
			is BlockNode -> evalBlock(node, env)
			is IfNode -> evalIf(node, env)
			is WhileNode -> evalWhile(node, env)
			is DoWhileNode -> evalDoWhile(node, env)
			is ForNode -> evalFor(node, env)
			else -> throw EvaluationException("Unknown IR node ${node}")
		}
	}

	fun evalNumber(node: IRNumericNode, env: Environment): NumberValue {
		val value = evaluate(node, env)
		if (value !is NumberValue) {
			throw EvaluationException("Expected ${value} to be a number")
		}

		return value
	}

	fun evalBoolean(node: IRBooleanNode, env: Environment): BooleanValue {
		val value = evaluate(node, env)
		if (value !is BooleanValue) {
			throw EvaluationException("Expected ${value} to be a boolean")
		}

		return value
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

	fun evalComparison(node: ComparisonNode, env: Environment): BooleanValue {
		val leftVal = evalNumber(node.left, env)
		val rightVal = evalNumber(node.right, env)

		return BooleanValue(node.compare(leftVal.num, rightVal.num))
	}

	fun evalNumericCall(node: NumericCallNode, env: Environment): Value {
		val closure = env.lookup(node.func)
		if (closure !is NumericClosure) {
			throw EvaluationException("Cannot call ${closure}, can only call functions")
		}

		if (node.actualArgs.size != closure.formalArgs.size) {
			throw EvaluationException("${node.func} expected ${closure.formalArgs.size} arguments, but received ${node.actualArgs.size}")
		}

		val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }

		val applicationEnv: Environment = closure.environment.copy()
		closure.formalArgs.zip(actualArgs).forEach { (ident, value) -> applicationEnv.extend(ident, value) }

		return evaluate(closure.expr, applicationEnv)
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
		closure.formalArgs.zip(actualArgs).forEach { (ident, value) -> applicationEnv.extend(ident, value) }

		return evaluate(closure.stmt, applicationEnv)
	}

	fun evalBlock(node: BlockNode, env: Environment): UnitValue {
		env.enterScope()
		for (childNode in node.nodes) {
			val value = evaluate(childNode, env)
			printIfEnabled(value)
		}

		env.exitScope()

		return UnitValue()
	}

	fun evalIf(node: IfNode, env: Environment): UnitValue {
		val cond = evalBoolean(node.cond, env)

		if (cond.bool) {
			val value = evaluate(node.conseq, env)
			printIfEnabled(value)
		} else if (node.altern != null) {
			val value = evaluate(node.altern, env)
			printIfEnabled(value)
		}

		return UnitValue()
	}

	fun evalWhile(node: WhileNode, env: Environment): UnitValue {
		var cond = evalBoolean(node.cond, env)

		while (cond.bool) {
			evaluate(node.stmt, env)
			cond = evalBoolean(node.cond, env)
		}

		return UnitValue()
	}

	fun evalDoWhile(node: DoWhileNode, env: Environment): UnitValue {
		do {
			evaluate(node.stmt, env)
			val cond = evalBoolean(node.cond, env)
		} while (cond.bool)

		return UnitValue()
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
			val value = evaluate(node.stmt, env)
			printIfEnabled(value)

			if (node.update != null) {
				evaluate(node.update, env)
			}

			if (node.cond != null) {
				condition = evalBoolean(node.cond, env).bool
			}
		}

		env.exitScope()

		return UnitValue()
	}

	fun evalNumericAssignment(node: NumericAssignmentNode, env: Environment): Value {
		val value = evaluate(node.expr, env)
		env.reassign(node.ident, value)

		return value
	}

	fun evalAssignment(node: AssignmentNode, env: Environment): Value {
		val value = evaluate(node.expr, env)
		env.reassign(node.ident, value)

		return value
	}

}
