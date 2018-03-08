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
            is VectorLiteralNode -> evalVectorLiteralNode(node, env)
            is TupleLiteralNode -> evalTupleLiteralNode(node, env)
            // Variables and functions
            is VariableNode -> env.lookup(node.ident)
            is KeyedAccessNode -> evalKeyedAccess(node, env)
            is KeyedAssignmentNode -> evalKeyedAssignment(node, env)
            is FunctionCallNode -> evalFunctionCall(node, env)
            is TypeConstructorNode -> evalTypeConstructor(node, env)
            is VariableAssignmentNode -> evalVariableAssignment(node, env)
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
            is MatchNode -> evalMatch(node, env)
            is ReturnNode -> evalReturn(node, env)
            is BreakNode -> throw Break
            is ContinueNode -> throw Continue
            else -> throw EvaluationException("Unknown IR node ${node}", node.startLocation)
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
            throw EvaluationException("Expected ${value} to be an int", node.startLocation)
        }

        return value
    }

    /**
     * Evaluate and assert that value has type float.
     */
    fun evalFloat(node: IRNode, env: Environment): FloatValue {
        val value = evaluate(node, env)
        if (value !is FloatValue) {
            throw EvaluationException("Expected ${value} to be a float", node.startLocation)
        }

        return value
    }

    /**
     * Evaluate and assert that value has type bool.
     */
    fun evalBool(node: IRNode, env: Environment): BoolValue {
        val value = evaluate(node, env)
        if (value !is BoolValue) {
            throw EvaluationException("Expected ${value} to be a boolean", node.startLocation)
        }

        return value
    }

    fun evalVectorLiteralNode(node: VectorLiteralNode, env: Environment): VectorValue {
        val type = node.type
        if (type !is VectorType) {
            throw EvaluationException("Expected vector literal to have vector type, " +
                    "but found ${formatType(type)}", node.startLocation)
        }

        val vector = node.elements.map({ element ->
            evaluate(element, env)
        }).toMutableList()

        return VectorValue(vector, type)
    }

    fun evalTupleLiteralNode(node: TupleLiteralNode, env: Environment): TupleValue {
        val type = node.type
        if (type !is TupleType) {
            throw EvaluationException("Expected tuple literal to have tuple type, " +
                    "but found ${formatType(type)}", node.startLocation)
        }

        val tuple = node.elements.map({ element ->
            evaluate(element, env)
        }).toMutableList()

        return TupleValue(tuple, type)
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
                    "given ${formatType(node.type)}", node.startLocation)
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
                    "given ${formatType(node.type)}", node.startLocation)
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
                    + "given ${formatType(node.type)}", node.startLocation)
        }
    }

    fun evalKeyedAccess(node: KeyedAccessNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        if (container !is VectorValue) {
            throw EvaluationException("Can only perform keyed access on a vector, " +
                    "found ${formatType(container.type)}", node.startLocation)
        }

        val key = evalInt(node.key, env)
        if (key.num < 0 || key.num >= container.elements.size) {
            throw EvaluationException("Index ${key.num} is outside bounds of vector",
                    node.key.startLocation)
        }

        return container.elements[key.num]
    }

    fun evalKeyedAssignment(node: KeyedAssignmentNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        if (container !is VectorValue) {
            throw EvaluationException("Can only perform keyed access on a vector, " +
                    "found ${formatType(container.type)}", node.startLocation)
        }

        val key = evalInt(node.key, env)
        if (key.num < 0 || key.num >= container.elements.size) {
            throw EvaluationException("Index ${key.num} is outside bounds of vector",
                    node.key.startLocation)
        }

        val rValue = evaluate(node.rValue, env)
        container.elements[key.num] = rValue

        return rValue
    }

    fun evalFunctionCall(node: FunctionCallNode, env: Environment): Value {
        if (node.func !is VariableNode) {
            throw EvaluationException("Can only call function", node.func.startLocation)
        }
        
        val closureValue = env.lookup(node.func.ident)

        // Check if bound function is a builtin. If so, use its stored evaluation function
        if (closureValue is BuiltinValue) {
            val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }
            return closureValue.func(actualArgs)
        } else if (closureValue !is ClosureValue) {
            throw EvaluationException("Cannot call ${closureValue}, can only call functions",
                    node.startLocation)
        }

        // Check that number of arguments is correct
        if (node.actualArgs.size != closureValue.formalArgs.size) {
            throw EvaluationException("${node.func} expected ${closureValue.formalArgs.size} " +
                    "arguments, but received ${node.actualArgs.size}", node.startLocation)
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

            // Set type of return value to match type of input arguments
            val returnValue = returnException.returnValue
            returnValue.type = node.type
            
            return returnValue
        }

        throw EvaluationException("No return value", node.startLocation)
    }

    fun evalTypeConstructor(node: TypeConstructorNode, env: Environment): AlgebraicDataTypeValue {
        val actualArgs = node.actualArgs.map { expr -> evaluate(expr, env) }
        return AlgebraicDataTypeValue(node.adtVariant, actualArgs, node.type)
    }

    fun evalVariableAssignment(node: VariableAssignmentNode, env: Environment): Value {
        val value = evaluate(node.rValue, env)
        env.reassign(node.lValue, value)

        return value
    }

    fun evalVariableDefinition(node: VariableDefinitionNode, env: Environment): UnitValue {
        env.extend(node.ident, evaluate(node.expr, env))
        return UnitValue
    }

    fun evalFunctionDefinition(node: FunctionDefinitionNode, env: Environment): UnitValue {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type !is FunctionType) {
            throw EvaluationException("Unknown function ${node.ident.name}", node.identLocation)
        }

        env.extend(node.ident, ClosureValue(node.formalArgs, node.body, env.copy(), type))

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

    /**
     * Structurally match a value with a pattern. If the value has the same structure as the
     * pattern, true will be returned and all variables in the structure will be bound to the
     * appropriate value. If the value does not match the pattern, false will be returned, however
     * variables in the pattern may still be bound to values that were matched in the middle of the
     * matching process.
     *
     * @param value the value to match against the pattern
     * @param pattern the pattern to match against
     * @param env the current environment
     */
    fun matchPattern(value: Value, pattern: IRNode, env: Environment): Boolean {
        // If a variable is ever encountered, bind it the current value
        if (pattern is VariableNode) {
            env.extend(pattern.ident, value)
            return true
        }

        return when (value) {
            // Literals match if they contain the same wrapped value
            is BoolValue -> pattern is BoolLiteralNode && pattern.bool == value.bool
            is StringValue -> pattern is StringLiteralNode && pattern.str == value.str
            is IntValue -> pattern is IntLiteralNode && pattern.num == value.num
            is FloatValue -> pattern is FloatLiteralNode && pattern.num == value.num
            // Vectors match if they are the same size and contain the same elements
            is VectorValue -> pattern is VectorLiteralNode &&
                    value.elements.size == pattern.elements.size &&
                    value.elements.zip(pattern.elements)
                        .map({ (elem, pat) -> matchPattern(elem, pat, env) })
                        .all({ x -> x })
            // Tuples match if they contain the same elements, as they must be the same size
            is TupleValue -> pattern is TupleLiteralNode &&
                    value.tuple.zip(pattern.elements)
                        .map({ (elem, pat) -> matchPattern(elem, pat, env) })
                        .all({ x -> x })
            // ADTs match if they are the same variant and contain the same elements
            is AlgebraicDataTypeValue -> pattern is TypeConstructorNode &&
                    value.adtVariant == pattern.adtVariant &&
                    value.fields.zip(pattern.actualArgs)
                        .map({ (field, pat) -> matchPattern(field, pat, env) })
                        .all({ x -> x })
            // Nothing can match functions or unit
            is BuiltinValue -> false
            is ClosureValue -> false
            is UnitValue -> false
            else -> throw EvaluationException("Unknown value ${value}", pattern.startLocation)
        }
    }

    fun evalMatch(node: MatchNode, env: Environment): UnitValue {
        // Evaluate expression to match on in current environment
        val exprValue = evaluate(node.expr, env)

        // Find first pattern which matches value, and execute its corresponding statement
        for ((pattern, statement) in node.cases) {
            env.enterScope()

            if (matchPattern(exprValue, pattern, env)) {
                evaluate(statement, env)
                env.exitScope()
                break
            }

            env.exitScope()
        }

        return UnitValue
    }

    fun evalReturn(node: ReturnNode, env: Environment): UnitValue {
        // If no return value is specified, this return returns unit
        val returnVal = if (node.expr != null) evaluate(node.expr, env) else UnitValue
        throw Return(returnVal)
    }

}
