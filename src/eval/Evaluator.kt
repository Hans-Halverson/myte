package myte.eval

import myte.eval.values.*
import myte.ir.nodes.*
import myte.parser.*
import myte.shared.*

class Evaluator(var symbolTable: SymbolTable, val environment: Environment) {

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
    }

    /**
     * Evaluate a node in the environment.
     * @param node the IR node to evaluate
     * @param environment the current environment to evaluate in
     * return the value that results from evaluating the node in the environment
     */
    fun evaluate(node: IRNode, env: Environment = environment): Value {
        return when (node) {
            // Literals and variables
            is BoolLiteralNode -> BoolValue(node.bool)
            is StringLiteralNode -> StringValue(node.str)
            is IntLiteralNode -> IntValue(node.num)
            is FloatLiteralNode -> FloatValue(node.num)
            is ListLiteralNode -> evalListLiteralNode(node, env)
            // Variables and functions
            is VariableNode -> env.lookup(node.ident)
            is FunctionCallNode -> evalFunctionCall(node, env)
            is AssignmentNode -> evalAssignment(node, env)
            is VariableDefinitionNode -> evalVariableDefinition(node, env)
            is FunctionDefinitionNode -> evalFunctionDefinition(node, env)
            // Math expressions
            is UnaryMathOperatorNode -> evalUnaryMathOperator(node, env)
            is BinaryMathOperatorNode -> evalBinaryMathOperator(node, env)
            // Logical operators
            is LogicalNotNode -> BoolValue(!evalBool(node.node, env).bool)
            is LogicalAndNode -> evalLogicalAnd(node, env)
            is LogicalOrNode -> evalLogicalOr(node, env)
            // Comparisons
            is EqualityNode -> evalEquality(node, env)
            is ComparisonNode -> evalComparison(node, env)            
            // Control flow and structure
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

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Evaluation functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Evaluate and assert that value has type int.
     */
    fun evalInt(node: IRNode, env: Environment): IntValue {
        val value = evaluate(node, env)
        if (value !is IntValue) {
            throw EvaluationException("Expected ${value} to be an int")
        }

        return value
    }

    /**
     * Evaluate and assert that value has type float.
     */
    fun evalFloat(node: IRNode, env: Environment): FloatValue {
        val value = evaluate(node, env)
        if (value !is FloatValue) {
            throw EvaluationException("Expected ${value} to be a float")
        }

        return value
    }

    /**
     * Evaluate and assert that value has type bool.
     */
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

        val list = node.elements.map({ element ->
            evaluate(element, env)
        }).toMutableList()

        return ListValue(list, type)
    }

    fun evalUnaryMathOperator(node: UnaryMathOperatorNode, env: Environment): NumberValue {
        return when (node.type) {
            is IntType -> {
                val expr = evalInt(node.node, env)
                IntValue(node.computeInt(expr.num))
            }
            is FloatType -> {
                val expr = evalFloat(node.node, env)
                FloatValue(node.computeFloat(expr.num))
            }
            else -> throw EvaluationException("Unary math operator must have a number type, " +
                    "given ${node.type}")
        }
    }

    fun evalBinaryMathOperator(node: BinaryMathOperatorNode, env: Environment): NumberValue {
        return when (node.type) {
            is IntType -> {
                val left = evalInt(node.left, env)
                val right = evalInt(node.right, env)

                IntValue(node.computeInt(left.num, right.num))
            }
            is FloatType -> {
                val left = evalFloat(node.left, env)
                val right = evalFloat(node.right, env)

                FloatValue(node.computeFloat(left.num, right.num))
            }
            else -> throw EvaluationException("Binary math operator must have a number type, " +
                    "given ${node.type}")
        }
    }

    fun evalLogicalAnd(node: LogicalAndNode, env: Environment): BoolValue {
        val leftValue = evalBool(node.left, env)

        // Use short circuiting: if the left evaluates to false, then do not evaluate right
        if (leftValue.bool) {
            return evalBool(node.right, env)
        } else {
            return leftValue;
        }
    }

    fun evalLogicalOr(node: LogicalOrNode, env: Environment): BoolValue {
        val leftValue = evalBool(node.left, env)

        // Use short circuiting: if the left evaluates to true, then do not evaluate right
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
        return when (node.left.type) {
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
            else -> throw EvaluationException("Comparison must be between two numbers, "
                    + "given ${node.type}")
        }
    }

    fun evalFunctionCall(node: FunctionCallNode, env: Environment): Value {
        val closureValue = env.lookup(node.func)

        // Check if bound function is a builtin. If so, use its stored evaluation function
        if (closureValue is BuiltinValue) {
            val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }
            return closureValue.func(actualArgs)
        } else if (closureValue !is ClosureValue) {
            throw EvaluationException("Cannot call ${closureValue}, can only call functions")
        }

        // Check that number of arguments is correct
        if (node.actualArgs.size != closureValue.formalArgs.size) {
            throw EvaluationException("${node.func} expected ${closureValue.formalArgs.size} " +
                    "arguments, but received ${node.actualArgs.size}")
        }

        val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }

        // Create a copy of the environment that is saved for this closure, and enter a scope which
        // has all the formal arguments identifiers bound to actual values.
        val applicationEnv: Environment = closureValue.environment.copy()
        applicationEnv.enterScope()

        closureValue.formalArgs.zip(actualArgs).forEach { (ident, value) ->
            applicationEnv.extend(ident, value)
        }

        // A closure can only be exited with with a return
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

    fun evalVariableDefinition(node: VariableDefinitionNode, env: Environment): UnitValue {
        env.extend(node.ident, evaluate(node.expr))
        return UnitValue
    }

    fun evalFunctionDefinition(node: FunctionDefinitionNode, env: Environment): UnitValue {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type !is FunctionType) {
            throw EvaluationException("Unknown function ${node.ident.name}")
        }

        env.extend(node.ident, ClosureValue(node.ident, node.formalArgs, node.body,
                env.copy(), type))
        return UnitValue
    }

    fun evalBlock(node: BlockNode, env: Environment): UnitValue {
        // Evaluate all the children of a block node in a new scope, which is removed after block
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

    /**
     * Evaluate a node until a continue is encountered.
     */
    fun evalUntilContinue(node: IRNode, env: Environment) {
        try {
            evaluate(node, env)
        } catch (e: Continue) {
        } // Do nothing
    }

    fun evalWhile(node: WhileNode, env: Environment): UnitValue {
        var cond = evalBool(node.cond, env)

        try {
            while (cond.bool) {
                // If a continue is encountered, continue to next condition check
                evalUntilContinue(node.body, env)

                cond = evalBool(node.cond, env)
            }
        // If a break is encountered, exit the loop
        } catch (e: Break) {
        } // Do nothing

        return UnitValue
    }

    fun evalDoWhile(node: DoWhileNode, env: Environment): UnitValue {
        try {
            do {
                // If a continue is encountered, continue to next condition check
                evalUntilContinue(node.body, env)

                val cond = evalBool(node.cond, env)
            } while (cond.bool)
        // If a break is encountered, exit the loop
        } catch (e: Break) {
        } // Do nothing

        return UnitValue
    }

    fun evalFor(node: ForNode, env: Environment): UnitValue {
        // Body of for loop must be in a new scope, since the init section must be local to just
        // the body of the for loop.
        env.enterScope()

        // Evaluate init statement and condition before for loop enters, if they exist
        if (node.init != null) {
            evaluate(node.init, env)
        }

        var condition = true
        if (node.cond != null) {
            condition = evalBool(node.cond, env).bool
        }

        try {
            while (condition) {
                // If a continue is encountered, continue to next condition check
                evalUntilContinue(node.body, env)

                if (node.update != null) {
                    evaluate(node.update, env)
                }

                if (node.cond != null) {
                    condition = evalBool(node.cond, env).bool
                }
            }
        // If a break is encountered, exit the loop
        } catch (e: Break) {
        } // Do nothing

        env.exitScope()

        return UnitValue
    }

    fun evalReturn(node: ReturnNode, env: Environment): UnitValue {
        // If no return value is specified, this return returns unit
        val returnVal = if (node.expr != null) evaluate(node.expr, env) else UnitValue
        throw Return(returnVal)
    }

}
