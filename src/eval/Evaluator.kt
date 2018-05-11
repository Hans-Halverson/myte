package myte.eval

import myte.eval.builtins.*
import myte.eval.values.*
import myte.ir.*
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

    fun evaluateFiles(filesResult: ConvertFilesResult, args: List<String>): Int {
        // Evaluate all nodes
        filesResult.nodes.forEach { node -> evaluate(node) }

        // Find main function and apply it to arguments, returning return value of main
        val mainClosure = environment.lookup(filesResult.main) as ClosureValue

        val argValues: MutableList<Value> = args.map({ str -> StringValue(str) }).toMutableList()
        val argVector = VectorValue(argValues, VectorType(StringType))
        val mainReturnValue = applyClosureToArgs(mainClosure, listOf(argVector))

        // Return success if main returns unit, otherwise the status int
        if (mainReturnValue is UnitValue) {
            return 0
        }

        return (mainReturnValue as IntValue).num
    }

    fun evaluatePackages(packagesResult: ConvertPackagesResult) {
        packagesResult.nodes.forEach { node -> evaluate(node) }
    }

    fun evaluateReplLine(replLineResult: ConvertReplLineResult, typeChecker: TypeChecker) {
        // First evaluate nodes to process, then evaluate nodes to evaluate and print values
        for (node in replLineResult.toProcess) {
            evaluate(node)
        }

        for (node in replLineResult.toEvaluate) {
            val value = evaluate(node)
            if (value !is UnitValue) {
                println("${value} : ${formatType(typeChecker.currentRepType(value.type))}")
            }
        }
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
            is UnitLiteralNode -> UnitValue
            is VectorLiteralNode -> evalVectorLiteralNode(node, env)
            is SetLiteralNode -> evalSetLiteralNode(node, env)
            is MapLiteralNode -> evalMapLiteralNode(node, env)
            is TupleLiteralNode -> evalTupleLiteralNode(node, env)
            is LambdaNode -> evalLambda(node, env)
            is TupleTypeConstructorNode -> evalTupleTypeConstructor(node, env)
            is RecordTypeConstructorNode -> evalRecordTypeConstructor(node, env)
            // Variables and functions
            is VariableNode -> env.lookup(node.ident)
            is AccessNode -> evalAccess(node, env)
            is FieldAssignmentNode -> evalFieldAssignment(node, env)
            is KeyedAccessNode -> evalKeyedAccess(node, env)
            is KeyedAssignmentNode -> evalKeyedAssignment(node, env)
            is FunctionCallNode -> evalFunctionCall(node, env)
            is PatternAssignmentNode -> evalPatternAssignment(node, env)
            is VariableAssignmentNode -> evalVariableAssignment(node, env)
            is PatternDefinitionNode -> evalPatternDefinition(node, env)
            is VariableDefinitionNode -> evalVariableDefinition(node, env)
            is MethodDefinitionNode -> evalMethodDefinition(node, env)
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

    fun evalSetLiteralNode(node: SetLiteralNode, env: Environment): SetValue {
        val type = node.type
        if (type !is SetType) {
            throw EvaluationException("Expected set literal to have set type, " +
                    "but found ${formatType(type)}", node.startLocation)
        }

        val setElements = node.elements.map({ element ->
            evaluate(element, env)
        }).toMutableSet()

        return SetValue(setElements, type)
    }

    fun evalMapLiteralNode(node: MapLiteralNode, env: Environment): MapValue {
        val type = node.type
        if (type !is MapType) {
            throw EvaluationException("Expected map literal to have map type, " +
                    "but found ${formatType(type)}", node.startLocation)
        }

        val keys = node.keys.map({ key -> evaluate(key, env) })
        val values = node.values.map({ value -> evaluate(value, env) })

        val mapElements = keys.zip(values).toMap().toMutableMap()

        return MapValue(mapElements, type)
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

    fun evalAccess(node: AccessNode, env: Environment): Value {
        val expr = evaluate(node.expr, env)
        val type = expr.type

        // If a trait, find concrete method with the given name
        if (type is TraitType) {
            val methodIdent = type.traitSig.concreteMethods[node.field]!!
            val methodValue = env.lookup(methodIdent) as MethodValue
            return methodValue.withReceiver(expr, node.type)
        // If an ADT, find method with the given name, or concrete method on extended trait
        } else if (type is AlgebraicDataType) {
            // Find and return method with given name if one exists
            val methodIdent = type.adtSig.methods[node.field]
            if (methodIdent != null) {
                val methodValue = env.lookup(methodIdent) as MethodValue
                return methodValue.withReceiver(expr, node.type)
            }

            // Find concrete method on extended trait with the given name
            for (extendedTrait in type.adtSig.traits) {
                val traitMethodIdent = extendedTrait.traitSig.concreteMethods[node.field]
                if (traitMethodIdent != null) {
                    val methodValue = env.lookup(traitMethodIdent) as MethodValue
                    return methodValue.withReceiver(expr, node.type)
                }
            }

            // If value is a record, find field with the given name
            if (expr is RecordVariantValue) {
                return expr.fields[node.field]!!
            } else {
                throw EvaluationException("No method with name ${node.field} for " +
                        "type ${expr.type}", node.accessLocation)
            }
        } else if (type is VectorType) {
            val builtin = VECTOR_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is SetType) {
            val builtin = SET_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is MapType) {
            val builtin = MAP_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is IntType) {
            val builtin = INT_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is FloatType) {
            val builtin = FLOAT_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is StringType) {
            val builtin = STRING_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is BoolType) {
            val builtin = BOOL_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is UnitType) {
            val builtin = UNIT_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else if (type is TupleType) {
            val builtin = TUPLE_BUILTIN_METHODS[node.field]!!
            return BuiltinMethodValue(builtin::eval, expr, node.type as FunctionType)
        } else {
            throw EvaluationException("No field or method with name ${node.field} for " +
                    "type ${expr.type}", node.accessLocation)
        }
    }

    fun evalFieldAssignment(node: FieldAssignmentNode, env: Environment): Value {
        val expr = evaluate(node.expr, env)
        if (expr !is RecordVariantValue) {
            throw EvaluationException("No method with name ${node.field} for type ${expr.type}",
                    node.accessLocation)
        }

        val rValue = evaluate(node.rValue, env)
        expr.fields[node.field] = rValue

        return rValue
    }

    fun evalKeyedAccess(node: KeyedAccessNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        if (container is VectorValue) {
            val key = evalInt(node.key, env)

            // Look up index in vector and return it, erroring if key is outside bounds of vector
            if (key.num < 0 || key.num >= container.elements.size) {
                throw EvaluationException("Index ${key.num} is outside bounds of vector",
                        node.key.startLocation)
            }

            return container.elements[key.num]
        } else if (container is MapValue) {
            val key = evaluate(node.key, env)
            val value = container.map[key]

            // Look up key in map and return it, erroring if no value is found
            if (value == null) {
                throw EvaluationException("No value for key ${key} in ${container}",
                        node.key.startLocation)
            }
            
            return value
        } else if (container is TupleValue) {
            val key = evalInt(node.key, env)

            // Look up index in tuple and return it, erroring if key is outside arity of tuple
            if (key.num < 0 || key.num >= container.tuple.size) {
                throw EvaluationException("Index ${key.num} is outside bounds of tuple",
                        node.key.startLocation)
            }

            return container.tuple[key.num]
        } else {
            throw EvaluationException("Can't perform keyed access on " +
                    "${formatType(container.type)}", node.startLocation)
        }
    }

    fun evalKeyedAssignment(node: KeyedAssignmentNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        if (container is VectorValue) {
            val key = evalInt(node.key, env)

            // Look up index in vector and assign to it, erroring if key is outside bounds of vector
            if (key.num < 0 || key.num >= container.elements.size) {
                throw EvaluationException("Index ${key.num} is outside bounds of vector",
                        node.key.startLocation)
            }

            val rValue = evaluate(node.rValue, env)
            container.elements[key.num] = rValue

            return rValue
        } else if (container is MapValue) {
            val key = evaluate(node.key, env)
            val value = evaluate(node.rValue, env)

            // Add key to map, overwriting any value for that key that already exists
            container.map[key] = value
            
            return value
        } else {
            throw EvaluationException("Can't perform keyed assignment on " +
                    "${formatType(container.type)}", node.startLocation)
        }
    }

    fun applyClosureToArgs(closure: ClosureValue, args: List<Value>): Value {
        // Create a copy of the environment that is saved for this closure, and enter a scope which
        // has all the formal arguments identifiers bound to actual values.
        val applicationEnv: Environment = closure.environment.copy()
        applicationEnv.enterScope()

        // Bind "this" if a method is being called
        if (closure is MethodValue) {
            if (closure.receiver == null) {
                throw ExceptionWithoutLocation("Cannot call method without receiver")
            }

            applicationEnv.extend(closure.thisIdent, closure.receiver)
        }

        closure.formalArgs.zip(args).forEach { (ident, value) ->
            applicationEnv.extend(ident, value)
        }

        // A closure may be exited with a return, and returns unit if no return is evaluated
        try {
            evaluate(closure.body, applicationEnv)
        } catch (returnException: Return) {
            applicationEnv.exitScope()
            return returnException.returnValue            
        }

        return UnitValue
    }

    fun evalFunctionCall(node: FunctionCallNode, env: Environment): Value {
        val closureValue = evaluate(node.func, env)

        // Check if bound function is a builtin. If so, use its stored evaluation function
        if (closureValue is BuiltinValue) {
            val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }
            return closureValue.func(actualArgs)
        // If bound function is builtin method, use stored evaluation function applied to receiver
        } else if (closureValue is BuiltinMethodValue) {
            val actualArgs: List<Value> = node.actualArgs.map { expr -> evaluate(expr, env) }
            return closureValue.func(actualArgs, closureValue.receiver)
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

        // Evaluate body of closure and retrieve return value
        val returnValue = applyClosureToArgs(closureValue, actualArgs)

        // Set type of return value to match type of input arguments
        returnValue.type = node.type

        return returnValue
    }

    fun evalTupleTypeConstructor(
        node: TupleTypeConstructorNode,
        env: Environment
    ): TupleVariantValue {
        val actualArgs = node.actualArgs.map { expr -> evaluate(expr, env) }
        return TupleVariantValue(node.adtVariant, actualArgs, node.type)
    }

    fun evalPatternAssignment(node: PatternAssignmentNode, env: Environment): Value {
        val value = evaluate(node.rValue, env)
        if (!matchPattern(value, node.pattern, env, true)) {
            throw EvaluationException("Could not match value to pattern", node.startLocation)
        }

        return value
    }

    fun evalRecordTypeConstructor(
        node: RecordTypeConstructorNode,
        env: Environment
    ): RecordVariantValue {
        val fields = node.fields.mapValues { (_, field) -> evaluate(field, env) }
        return RecordVariantValue(node.adtVariant, fields.toMutableMap(), node.type)
    }

    fun evalVariableAssignment(node: VariableAssignmentNode, env: Environment): Value {
        val value = evaluate(node.rValue, env)
        env.reassign(node.lValue, value)

        return value
    }

    fun evalPatternDefinition(node: PatternDefinitionNode, env: Environment): Value {
        val value = evaluate(node.expr, env)
        if (!matchPattern(value, node.pattern, env, true)) {
            throw EvaluationException("Could not match value to pattern", node.startLocation)
        }

        return UnitValue
    }

    fun evalVariableDefinition(node: VariableDefinitionNode, env: Environment): UnitValue {
        env.extend(node.ident, evaluate(node.expr, env))
        return UnitValue
    }

    fun evalMethodDefinition(node: MethodDefinitionNode, env: Environment): UnitValue {
        val type = symbolTable.getInfo(node.ident)?.type
        if (type !is FunctionType) {
            throw EvaluationException("Unknown function ${node.ident.name}", node.identLocation)
        }

        env.extendGlobal(node.ident, MethodValue(node.formalArgs, node.body, env.copy(),
                node.thisIdent, null, type))

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

    fun evalLambda(node: LambdaNode, env: Environment): ClosureValue {
        val funcType = node.type
        if (funcType !is FunctionType) {
            throw EvaluationException("Lambda expression must have function type, " +
                    "found ${formatType(funcType)}", node.startLocation)
        }

        return ClosureValue(node.formalArgs, node.body, env.copy(), funcType)
    }

    fun evalBlock(node: BlockNode, env: Environment): Value {
        // Evaluate all the children of a block node in a new scope, which is removed after block
        env.enterScope()

        var result: Value = UnitValue

        for (childNode in node.nodes) {
            result = evaluate(childNode, env)
        }

        env.exitScope()

        // If an expression, return result of evaluating last statement. Otherwise return unit.
        if (node.isExpression) {
            return result
        } else {
            return UnitValue
        }
    }

    fun evalIf(node: IfNode, env: Environment): Value {
        val cond = evalBool(node.cond, env)

        var result: Value = UnitValue

        if (cond.bool) {
            result = evaluate(node.conseq, env)
        } else if (node.altern != null) {
            result = evaluate(node.altern, env)
        }

        // If an expression, evaluate to evaluated case. If statement, always evaluate to unit.
        if (node.isExpression) {
            return result
        } else {
            return UnitValue
        }
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
     * @param bind whether to bind variables to matched values or not
     */
    fun matchPattern(value: Value, pattern: IRNode, env: Environment, bind: Boolean): Boolean {
        // If a variable is ever encountered, bind it the current value
        if (pattern is VariableNode) {
            if (bind) {
                env.extend(pattern.ident, value)
            }

            return true
        }

        return when (value) {
            // Literals match if they contain the same wrapped value
            is BoolValue -> pattern is BoolLiteralNode && pattern.bool == value.bool
            is StringValue -> pattern is StringLiteralNode && pattern.str == value.str
            is IntValue -> pattern is IntLiteralNode && pattern.num == value.num
            is FloatValue -> pattern is FloatLiteralNode && pattern.num == value.num
            is UnitValue -> pattern is UnitLiteralNode
            // Vectors match if they are the same size and contain the same elements
            is VectorValue -> pattern is VectorLiteralNode &&
                    value.elements.size == pattern.elements.size &&
                    value.elements.zip(pattern.elements)
                        .map({ (elem, pat) -> matchPattern(elem, pat, env, bind) })
                        .all({ x -> x })
            is SetValue -> matchSetPattern(value, pattern, env)
            is MapValue -> matchMapPattern(value, pattern, env)
            // Tuples match if they contain the same elements, as they must be the same size
            is TupleValue -> pattern is TupleLiteralNode &&
                    value.tuple.zip(pattern.elements)
                        .map({ (elem, pat) -> matchPattern(elem, pat, env, bind) })
                        .all({ x -> x })
            // ADTs match if they are the same variant and contain the same elements
            is TupleVariantValue -> pattern is TupleTypeConstructorNode &&
                    value.adtVariant == pattern.adtVariant &&
                    value.fields.zip(pattern.actualArgs)
                        .map({ (field, pat) -> matchPattern(field, pat, env, bind) })
                        .all({ x -> x })
            is RecordVariantValue -> pattern is RecordTypeConstructorNode &&
                    value.adtVariant == pattern.adtVariant &&
                    pattern.fields.map({ (name, field) ->
                        matchPattern(value.fields[name]!!, field, env, bind)
                    }).all({ x -> x })
            // Nothing can match functions or unit
            is BuiltinValue -> false
            is ClosureValue -> false
            else -> throw EvaluationException("Unknown value ${value}", pattern.startLocation)
        }
    }

    fun matchSetPattern(value: SetValue, pattern: IRNode, env: Environment): Boolean {
        // Error if a set is matched to non-set literal, or if set size differs
        if (pattern !is SetLiteralNode || value.elements.size != pattern.elements.size) {
            return false
        }

        // Find a mapping between the given values and patterns
        val valueList = value.elements.toList()
        val mapping = findMatches(valueList, pattern.elements) {
            v, p -> matchPattern(v, p, env, false)
        }

        // Error if the value and patterns can not be made to match
        if (mapping == null) {
            return false
        }

        // Bind each value and pattern that are matched together, and return sucess
        for ((i, j) in mapping.withIndex()) {
            matchPattern(valueList[i], pattern.elements[j], env, true)
        }
        
        return true
    }

    fun matchMapPattern(value: MapValue, pattern: IRNode, env: Environment): Boolean {
        // Error if a map is matched to non-map literal, or if map size differs
        if (pattern !is MapLiteralNode || value.map.size != pattern.keys.size) {
            return false
        }

        // Find a mapping between the given values and patterns
        val valueList = value.map.entries.toList()
        val patternList = pattern.keys.zip(pattern.values)

        val mapping = findMatches(valueList, patternList) {
            (valueKey, valueVal), (patternKey, patternVal) -> 
                matchPattern(valueKey, patternKey, env, false) &&
                matchPattern(valueVal, patternVal, env, false)
        }

        // Error if the value and patterns can not be made to match
        if (mapping == null) {
            return false
        }

        // Bind each value and pattern that are matched together, and return sucess
        for ((i, j) in mapping.withIndex()) {
            val (valueKey, valueVal) = valueList[i]
            val (patternKey, patternVal) = patternList[j]

            matchPattern(valueKey, patternKey, env, true)
            matchPattern(valueVal, patternVal, env, true)
        }

        return true
    }

    fun evalMatch(node: MatchNode, env: Environment): Value {
        // Evaluate expression to match on in current environment
        val exprValue = evaluate(node.expr, env)
        var result: Value = UnitValue

        // Find first pattern which matches value, and execute its corresponding statement
        for ((pattern, guard, statement) in node.cases) {
            env.enterScope()

            if (matchPattern(exprValue, pattern, env, true)) {
                // Do not evaluate this case if a guard exists and fails
                if (guard != null) {
                    val passesGuard = evalBool(guard, env)
                    if (!passesGuard.bool) {
                        env.exitScope()
                        continue
                    }
                }

                result = evaluate(statement, env)
                env.exitScope()
                break
            }

            env.exitScope()
        }

        // If an expression, return result of evaluating matched case. Otherwise always return unit.
        if (node.isExpression) {
            return result
        } else {
            return UnitValue
        }
    }

    fun evalReturn(node: ReturnNode, env: Environment): UnitValue {
        // If no return value is specified, this return returns unit
        val returnVal = if (node.expr != null) evaluate(node.expr, env) else UnitValue
        throw Return(returnVal)
    }

}
