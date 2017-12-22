package myte.eval

import myte.eval.values.*
import myte.ir.nodes.*
import myte.parser.*
import myte.shared.*

import java.util.Stack


class Evaluator(val symbolTable: SymbolTable, val environment: Environment) {

	fun evaluate(node: IRNode, env: Environment = environment): Value {
		return when (node) {
			is IntLiteralNode -> IntValue(node.num)
			is FloatLiteralNode -> FloatValue(node.num)
			is BooleanLiteralNode -> BoolValue(node.bool)
			is StringLiteralNode -> StringValue(node.str)
			is ListLiteralNode -> evalListLiteralNode(node, env)
			is VariableNode -> env.lookup(node.ident)
			is BinaryMathOperatorNode -> evalBinaryMathOperator(node, env)
			is NegateNode -> evalNegate(node, env)
			is LogicalNotNode -> BoolValue(!evalBool(node.node, env).bool)
			is LogicalAndNode -> evalLogicalAnd(node, env)
			is LogicalOrNode -> evalLogicalOr(node, env)
			is EqualityNode -> evalEquality(node, env)
			is ComparisonNode -> evalComparison(node, env)
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

				env.extend(node.ident, ClosureValue(node.ident, node.formalArgs, node.body, env.copy(), type))
				return UnitValue
			}
			is BlockNode -> evalBlock(node, env)
			is IfNode -> evalIf(node, env)
			is WhileNode -> evalWhile(node, env)
			is DoWhileNode -> evalDoWhile(node, env)
			is ForNode -> evalFor(node, env)
			is ReturnNode -> evalReturn(node, env)
			is BreakNode -> throw Break
			is ContinueNode -> throw Continue
			else -> throw EvaluationException("Unknown IR node ${node}")
		}
	}

	fun evalInt(node: IRNode, env: Environment): IntValue {
		val value = evaluate(node, env)
		if (value !is IntValue) {
			throw EvaluationException("Expected ${value} to be an int")
		}

		return value
	}

	fun evalFloat(node: IRNode, env: Environment): FloatValue {
		val value = evaluate(node, env)
		if (value !is FloatValue) {
			throw EvaluationException("Expected ${value} to be a float")
		}

		return value
	}

	fun evalBool(node: IRNode, env: Environment): BoolValue {
		val value = evaluate(node, env)
		if (value !is BoolValue) {
			throw EvaluationException("Expected ${value} to be a boolean")
		}

		return value
	}

	fun evalListLiteralNode(node: ListLiteralNode, env: Environment): ListValue {
		val type = node.type
		if (type !is ListType) {
			throw EvaluationException("Expected list literal to have list type, but found ${type}")
		}

		val list = node.elements.map({ element -> evaluate(element, env) }).toMutableList()

		return ListValue(list, type)
	}

	fun evalBinaryMathOperator(node: BinaryMathOperatorNode, env: Environment): NumberValue {
// TODO: Readd ints
//		return when (node.type) {
//			is IntType -> {
//				val left = evalInt(node.left, env)
//				val right = evalInt(node.right, env)
//
//				IntValue(node.computeInt(left.num, right.num))
//			}
//			is FloatType -> {
//				val left = evalFloat(node.left, env)
//				val right = evalFloat(node.right, env)
//
//				FloatValue(node.computeFloat(left.num, right.num))
//			}
//			else -> throw EvaluationException("Binary math operator must have a number type, given ${node.type}")
//		}

		val left = evalFloat(node.left, env)
		val right = evalFloat(node.right, env)

		return FloatValue(node.computeFloat(left.num, right.num))
	}

	fun evalNegate(node: NegateNode, env: Environment): NumberValue {
// TODO: Readd ints
//		return when (node.type) {
//			is IntType -> IntValue(evalInt(node.expr, env).num)
//			is FloatType -> FloatValue(evalFloat(node.expr, env).num)
//			else -> throw EvaluationException("Unary math operator must have a number type, given ${node.type}")
//		}
		return FloatValue(-evalFloat(node.expr, env).num)
	}

	fun evalLogicalAnd(node: LogicalAndNode, env: Environment): BoolValue {
		val leftValue = evalBool(node.left, env)

		if (leftValue.bool) {
			return evalBool(node.right, env)
		} else {
			return leftValue;
		}
	}

	fun evalLogicalOr(node: LogicalOrNode, env: Environment): BoolValue {
		val leftValue = evalBool(node.left, env)

		if (!leftValue.bool) {
			return evalBool(node.right, env)
		} else {
			return leftValue;
		}
	}

	fun evalEquality(node: EqualityNode, env: Environment): BoolValue {
		val leftVal = evaluate(node.left, env)
		val rightVal = evaluate(node.right, env)

		return BoolValue(node.compare(leftVal, rightVal))
	}

	fun evalComparison(node: ComparisonNode, env: Environment): BoolValue {
		return when (node.numType) {
			is IntType -> {
				val left = evalInt(node.left, env)
				val right = evalInt(node.right, env)

				BoolValue(node.compareInts(left.num, right.num))
			}
			is FloatType -> {
				val left = evalFloat(node.left, env)
				val right = evalFloat(node.right, env)

				BoolValue(node.compareFloats(left.num, right.num))
			}
		}
	}

	fun evalFunctionCall(node: FunctionCallNode, env: Environment): Value {
		val closureValue = env.lookup(node.func)
		if (closureValue is BuiltinValue) {
			val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }
			return closureValue.func(actualArgs)
		} else if (closureValue !is ClosureValue) {
			throw EvaluationException("Cannot call ${closureValue}, can only call functions")
		}

		if (node.actualArgs.size != closureValue.formalArgs.size) {
			throw EvaluationException("${node.func} expected ${closureValue.formalArgs.size} arguments, but received ${node.actualArgs.size}")
		}

		val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }

		val applicationEnv: Environment = closureValue.environment.copy()
		applicationEnv.enterScope()

		closureValue.formalArgs.zip(actualArgs).forEach { (ident, value) -> applicationEnv.extend(ident, value) }

		try {
			evaluate(closureValue.body, applicationEnv)
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
		val cond = evalBool(node.cond, env)

		if (cond.bool) {
			evaluate(node.conseq, env)
		} else if (node.altern != null) {
			evaluate(node.altern, env)
		}

		return UnitValue
	}

	fun evalWhile(node: WhileNode, env: Environment): UnitValue {
		var cond = evalBool(node.cond, env)

		try {
			while (cond.bool) {
				try {
					evaluate(node.body, env)
				} catch (e: Continue) {}

				cond = evalBool(node.cond, env)
			}
		} catch (e: Break) {}

		return UnitValue
	}

	fun evalDoWhile(node: DoWhileNode, env: Environment): UnitValue {
		try {
			do {
				try {
					evaluate(node.body, env)
				} catch (e: Continue) {}

				val cond = evalBool(node.cond, env)
			} while (cond.bool)
		} catch (e: Break) {}

		return UnitValue
	}

	fun evalFor(node: ForNode, env: Environment): UnitValue {
		env.enterScope()

		if (node.init != null) {
			evaluate(node.init, env)
		}

		var condition = true
		if (node.cond != null) {
			condition = evalBool(node.cond, env).bool
		}

		try {
			while (condition) {
				try {
					evaluate(node.body, env)
				} catch (e: Continue) {}

				if (node.update != null) {
					evaluate(node.update, env)
				}

				if (node.cond != null) {
					condition = evalBool(node.cond, env).bool
				}
			}
		} catch (e: Break) {}

		env.exitScope()

		return UnitValue
	}

	fun evalReturn(node: ReturnNode, env: Environment): UnitValue {
		val returnVal = if (node.expr != null) evaluate(node.expr, env) else UnitValue
		throw Return(returnVal)
	}

}
