package myte.eval

import myte.eval.builtins.*
import myte.eval.values.*
import myte.ir.*
import myte.ir.nodes.*
import myte.parser.*
import myte.shared.*

class Evaluator(var symbolTable: SymbolTable, val environment: Environment) {

    /**
     * Reset for a new line from the REPL.
     */
    fun resetForReplLine(newSymbolTable: SymbolTable) {
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
            if (value is ClosureValue || value is BuiltinValue || value is BuiltinMethodValue) {
                println("${value} : ${formatType(typeChecker.currentRepType(value.type))}")
            } else if (value !is UnitValue) {
                val valueStr = callToString(value, environment, this).str
                println("${valueStr} : ${formatType(typeChecker.currentRepType(value.type))}")
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
            is ByteLiteralNode -> ByteValue(node.num)
            is IntLiteralNode -> IntValue(node.num)
            is FloatLiteralNode -> FloatValue(node.num)
            is DoubleLiteralNode -> DoubleValue(node.num)
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
            is IndexNode -> evalIndex(node, env)
            is IndexAssignNode -> evalIndexAssign(node, env)
            is FunctionCallNode -> evalFunctionCall(node, env)
            is BuiltinNode -> evalBuiltin(node, env)
            is BuiltinMethodNode -> evalBuiltinMethod(node, env)
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
            is ForEachNode -> evalForEach(node, env)
            is MatchNode -> evalMatch(node, env)
            is ReturnNode -> evalReturn(node, env)
            is BreakNode -> throw Break
            is ContinueNode -> throw Continue
            // Wrapper nodes should simply delegate to the wrapped node
            is WrapperNode -> evaluate(node.node, env)
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

    fun evalUnaryMathOperator(node: UnaryMathOperatorNode, env: Environment): Value {
        val expr = evaluate(node.node, env)

        return when (node) {
            is IdentityNode -> callMethod("unaryPlus", expr, listOf(), env)
            is NegateNode -> callMethod("unaryMinus", expr, listOf(), env)
        }
    }

    fun evalBinaryMathOperator(node: BinaryMathOperatorNode, env: Environment): Value {
        val left = evaluate(node.left, env)
        val right = evaluate(node.right, env)

        return when (node) {
            is AddNode -> callMethod("add", left, listOf(right), env)
            is SubtractNode -> callMethod("subtract", left, listOf(right), env)
            is MultiplyNode -> callMethod("multiply", left, listOf(right), env)
            is DivideNode -> callMethod("divide", left, listOf(right), env)
            is ExponentNode -> callMethod("exponent", left, listOf(right), env)
            is RemainderNode -> callMethod("remainder", left, listOf(right), env)
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

    fun evalComparison(node: ComparisonNode, env: Environment): Value {
        val left = evaluate(node.left, env)
        val right = evaluate(node.right, env)

        return when (node) {
            is LessThanNode -> callMethod("lessThan", left, listOf(right), env)
            is LessThanOrEqualNode -> callMethod("lessThanOrEquals", left, listOf(right), env)
            is GreaterThanNode -> callMethod("greaterThan", left, listOf(right), env)
            is GreaterThanOrEqualNode -> callMethod("greaterThanOrEquals", left, listOf(right), env)
        }
    }

    fun evalAccess(node: AccessNode, env: Environment): Value {
        val expr = evaluate(node.expr, env)
        return accessValue(expr, node.field, node.type, node.accessLocation, env)
    }

    fun accessValue(
        value: Value,
        name: String,
        type: Type,
        location: Location,
        env: Environment
    ): Value {
        // First check for a method on an extended trait with the given name
        for (extendedTrait in value.type.sig.traits) {
            val traitMethodIdent = extendedTrait.traitSig.methods[name]
            if (traitMethodIdent != null) {
                val methodValue = env.lookup(traitMethodIdent) as MethodValue
                return methodValue.withReceiver(value, type)
            }
        }

        // Then check for methods defined on this value's type
        val methodIdent = value.type.sig.methods[name]
        if (methodIdent != null) {
            val methodValue = env.lookup(methodIdent) as MethodValue
            return methodValue.withReceiver(value, type)
        }

        // Finally, if value is a record, find field with the given name
        if (value is RecordVariantValue) {
            val field = value.fields[name]
            if (field != null) {
                return field
            }
        }

        // Use default toString implementation for ADTs if none is defined on the type
        if (name == TO_STRING_METHOD && value is AlgebraicDataTypeValue) {
            return BuiltinMethodValue(AlgebraicDataTypeToStringBuiltinMethod::eval,
                    value, TO_STRING_TYPE)
        }

        throw EvaluationException("In evaluation, no field or method with name ${name} for " +
                "type ${formatType(value.type)}", location)
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

    fun callMethod(
        op: String,
        value: Value,
        args: List<Value>,
        env: Environment
    ): Value {
        val opFunction = accessValue(value, op, FunctionType(listOf(), UnitType), NO_LOCATION, env)
        return applyFunction(opFunction, args, env, NO_LOCATION)
    }

    fun evalIndex(node: IndexNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        val key = evaluate(node.key, env)

        return callMethod("index", container, listOf(key), env)
    }

    fun evalIndexAssign(node: IndexAssignNode, env: Environment): Value {
        val container = evaluate(node.container, env)
        val key = evaluate(node.key, env)
        val rValue = evaluate(node.rValue, env)

        return callMethod("indexAssign", container, listOf(key, rValue), env)
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
        val actualArgs = node.actualArgs.map { expr -> evaluate(expr, env) }

        return applyFunction(closureValue, actualArgs, env, node.startLocation)
    }

    fun evalBuiltin(node: BuiltinNode, env: Environment): Value {
        val args = node.args.map { expr -> evaluate(expr, env) }
        return node.builtin.evalWrapper(args)
    }

    fun evalBuiltinMethod(node: BuiltinMethodNode, env: Environment): Value {
        val recv = evaluate(node.recv, env)
        val args = node.args.map { expr -> evaluate(expr, env) }

        return node.builtin.eval(args, recv, env, this)
    }

    fun applyFunction(
        closureValue: Value,
        args: List<Value>,
        env: Environment,
        location: Location
    ): Value {
        // Check if bound function is a builtin. If so, use its stored evaluation function
        if (closureValue is BuiltinValue) {
            return closureValue.func(args)
        // If bound function is builtin method, use stored evaluation function applied to receiver
        } else if (closureValue is BuiltinMethodValue) {
            return closureValue.func(args, closureValue.receiver, env, this)
        } else if (closureValue !is ClosureValue) {
            throw EvaluationException("Cannot call ${closureValue}, can only call functions",
                    location)
        }

        // Check that number of arguments is correct
        if (args.size != closureValue.formalArgs.size) {
            throw EvaluationException("Function expected ${closureValue.formalArgs.size} " +
                    "arguments, but received ${args.size}", location)
        }

        // Evaluate body of closure and retrieve return value
        return applyClosureToArgs(closureValue, args)
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
        val altern = node.altern

        var result: Value = UnitValue

        if (cond.bool) {
            result = evaluate(node.conseq, env)
        } else if (altern != null) {
            result = evaluate(altern, env)
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
        node.init?.let { evaluate(it, env) }

        var condition = true
        node.cond?.let { cond ->
            condition = evalBool(cond, env).bool
        }

        try {
            while (condition) {
                // If a continue is encountered, continue to next condition check
                evalUntilContinue(node.body, env)

                node.update?.let { evaluate(it, env) }

                node.cond?.let { cond ->
                    condition = evalBool(cond, env).bool
                }
            }
        // If a break is encountered, exit the loop
        } catch (e: Break) {
        } // Do nothing

        env.exitScope()

        return UnitValue
    }

    fun evalForEach(node: ForEachNode, env: Environment): UnitValue {
        // Body of for leach oop must be in a new scope, since the assigned pattern must be local
        // to just the body of the for loop.
        env.enterScope()

        // Find the current item from the iterable
        val iterable = evaluate(node.iterable, env)
        val iterator = callMethod("iterator", iterable, listOf(), env)
        var currentItem = callMethod("next", iterator, listOf(), env) as TupleVariantValue

        val noneVariant = getVariantForBuiltinType(OPTION_TYPE_SIG, OPTION_TYPE_NONE_VARIANT)

        try {
            // While there are still items in the iterable, evaluate body of for each loop
            while (currentItem.adtVariant != noneVariant) {
                // First match pattern to current item, binding all variables in pattern
                if (!matchPattern(currentItem.fields[0], node.lValue, env, true)) {
                    throw EvaluationException("Could not match value to pattern", node.startLocation)
                }

                evalUntilContinue(node.body, env)
                currentItem = callMethod("next", iterator, listOf(), env) as TupleVariantValue
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
        val expr = node.expr
        val returnVal = if (expr != null) evaluate(expr, env) else UnitValue
        throw Return(returnVal)
    }

}
