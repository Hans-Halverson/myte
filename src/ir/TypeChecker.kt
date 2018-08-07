package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.shared.*

class TypeChecker(var symbolTable: SymbolTable) {
    val typeEnvironment = TypeEnvironment()
    val typeGraph = TypeGraph(typeEnvironment)

    /**
     * Reset the type checker for another line from the REPL.
     */
    fun resetForReplLine(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
        typeEnvironment.returnToGlobalScope()
    }

    fun unify(type1: Type, type2: Type): Boolean = typeGraph.unify(type1, type2)

    fun subtype(
        subType: Type,
        superType: Type,
        except: () -> Unit,
        allowVars: Boolean = true
    ): Boolean {
        return typeGraph.subtype(subType, superType, except, allowVars)
    }

    fun currentRepType(type: Type): Type = typeGraph.currentRepType(type)

    fun findRepNode(type: TypeVariable): TypeEquivalenceNode = typeGraph.findRepNode(type)

    fun findRepType(
        type: Type,
        boundVars: MutableSet<TypeVariable>,
        mappedVars: MutableMap<TypeVariable, TypeVariable> = mutableMapOf(),
        refresh: Boolean = true
    ): Type {
        return typeGraph.findRepType(type, boundVars, mappedVars, refresh)
    }

    fun findRepSubstitution(type: Type, substMap: Map<TypeVariable, Type>): Type {
        return typeGraph.findRepSubstitution(type, substMap)
    }

    fun addDeferredConstraint(type: Type, constraint: DeferredConstraint) {
        typeGraph.addDeferredConstraint(type, constraint)
    }

    fun typeToString(type: Type): String = typeGraph.typeToString(type)

    fun typesToString(t1: Type, t2: Type): List<String> = typeGraph.typesToString(t1, t2)

    fun resolveAllVariables() = typeGraph.resolveAllVariables()

    /**
     * Type check and perform unification for an IR tree rooted at a given node.
     *
     * @param node the ir node to type check
     * @param boundVars a set of all type variables that have been bound in the current type scope
     *        for this node
     * @param rebind whether or not to refresh unbound type variables for all type from identifiers
     *        that are encountered in this node or its children
     */
    fun typeCheck(node: IRNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        when (node) {
            // Literals
            is BoolLiteralNode -> typeCheckBoolLiteral(node)
            is StringLiteralNode -> typeCheckStringLiteral(node)
            is IntegralLiteralNode -> typeCheckIntegralLiteral(node)
            is DecimalLiteralNode -> typeCheckDecimalLiteral(node)
            is UnitLiteralNode -> typeCheckUnitLiteral(node)
            is VectorLiteralNode -> typeCheckVectorLiteral(node, boundVars, refresh)
            is SetLiteralNode -> typeCheckSetLiteral(node, boundVars, refresh)
            is MapLiteralNode -> typeCheckMapLiteral(node, boundVars, refresh)
            is TupleLiteralNode -> typeCheckTupleLiteral(node, boundVars, refresh)
            is LambdaNode -> typeCheckLambda(node, boundVars, refresh)
            is TupleTypeConstructorNode -> typeCheckTupleTypeConstructor(node, boundVars, refresh)
            is RecordTypeConstructorNode -> typeCheckRecordTypeConstructor(node, boundVars, refresh)
            // Variables and functions
            is VariableNode -> typeCheckVariable(node, boundVars, refresh)
            is FunctionCallNode -> typeCheckFunctionCall(node, boundVars, refresh)
            is BuiltinNode -> typeCheckBuiltin(node, boundVars, refresh)
            is BuiltinMethodNode -> typeCheckBuiltinMethod(node, boundVars, refresh)
            is AccessNode -> typeCheckAccess(node, boundVars, refresh)
            is FieldAssignmentNode -> typeCheckFieldAssignment(node, boundVars, refresh)
            is IndexNode -> typeCheckIndex(node, boundVars, refresh)
            is IndexAssignNode -> typeCheckIndexAssign(node, boundVars, refresh)
            is VariableAssignmentNode -> typeCheckVariableAssignment(node, boundVars, refresh)
            is PatternAssignmentNode -> typeCheckPatternAssignment(node, boundVars, refresh)
            is VariableDefinitionNode -> typeCheckVariableDefinition(node, boundVars, refresh)
            is PatternDefinitionNode -> typeCheckPatternDefinition(node, boundVars, refresh)
            is FunctionDefinitionNode -> typeCheckFunctionDefinition(node, boundVars, refresh)
            // Math expressions
            is UnaryMathOperatorNode -> typeCheckUnaryMathOperator(node, boundVars, refresh)
            is BinaryMathOperatorNode -> typeCheckBinaryMathOperator(node, boundVars, refresh)
            // Logical operators
            is LogicalAndNode -> typeCheckLogicalAnd(node, boundVars, refresh)
            is LogicalOrNode -> typeCheckLogicalOr(node, boundVars, refresh)
            is LogicalNotNode -> typeCheckLogicalNot(node, boundVars, refresh)
            // Comparisons
            is EqualityNode -> typeCheckEquality(node, boundVars, refresh)
            is ComparisonNode -> typeCheckComparison(node, boundVars, refresh)
            // Control flow and structure
            is BlockNode -> typeCheckBlock(node, boundVars, refresh)
            is IfNode -> typeCheckIf(node, boundVars, refresh)
            is WhileNode -> typeCheckWhile(node, boundVars, refresh)
            is DoWhileNode -> typeCheckDoWhile(node, boundVars, refresh)
            is ForNode -> typeCheckFor(node, boundVars, refresh)
            is ForEachNode -> typeCheckForEach(node, boundVars, refresh)
            is MatchNode -> typeCheckMatch(node, boundVars, refresh)
            is ReturnNode -> typeCheckReturn(node, boundVars, refresh)
            is BreakNode -> typeCheckBreak(node)
            is ContinueNode -> typeCheckContinue(node)
            // Definition wrappers
            is TraitDefinitionNode -> typeCheckTraitDefinition(node, boundVars, refresh)
            // Wrapper nodes simply pass type checking to their children
            is WrapperNode -> typeCheck(node.node, boundVars, refresh)
            else -> return
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type checking and unification functions for each IR node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun typeCheckBoolLiteral(node: BoolLiteralNode) {
        // Type of bool literal must be a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Bool literal could not be inferred to have bool " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckStringLiteral(node: StringLiteralNode) {
        // Type of string literal must be a string
        if (!unify(node.type, StringType)) {
            val type = typeToString(node.type)
            throw IRConversionException("String literal could not be inferred to have string " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckIntegralLiteral(node: IntegralLiteralNode) {
        if (!unify(node.type, IntType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Int literal coud not be inferred to have type int, " +
                    "found ${type}", node.startLocation)
        }
    }

    fun typeCheckDecimalLiteral(node: DecimalLiteralNode) {
        if (!unify(node.type, DoubleType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Double literal coud not be inferred to have type " +
                    "double, found ${type}", node.startLocation)
        }
    }

    fun typeCheckUnitLiteral(node: UnitLiteralNode) {
        // Type of unit literal must be unit
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Unit literal could not be inferred to have unit " +
                    "type, found ${type}", node.startLocation)
        }
    }

    fun typeCheckVectorLiteral(
        node: VectorLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Vector type is initially unknown, so set as vector type with new type variable param
        val elementType = TypeVariable()
        val vectorType = VectorType(elementType)

        // Type of this vector literal must be a vector of the element type
        if (!unify(node.type, vectorType)) {
            val types = typesToString(node.type, vectorType)
            throw IRConversionException("Vector literal could not be inferred to have vector " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Every element in the vector's type must be a subtype of the vector's element type
        node.elements.forEach({ element ->
            val except = { ->
                val types = typesToString(element.type, elementType)
                throw IRConversionException("Vector must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, elementType, except)) {
                except()
            }
        })
    }

    fun typeCheckSetLiteral(
        node: SetLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Set type is initially unknown, so set as set type with new type variable param
        val elementType = TypeVariable()
        val setType = SetType(elementType)

        // Type of this set literal must be a set of the element type
        if (!unify(node.type, setType)) {
            val types = typesToString(node.type, setType)
            throw IRConversionException("Set literal could not be inferred to have set " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each element in the set's type must be a suptype of the sets's element type
        node.elements.forEach({ element ->
            val except = { ->
                val types = typesToString(element.type, elementType)
                throw IRConversionException("Set must have elements of same type, found " +
                        "${types[0]} and ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, elementType, except)) {
                except()
            }
        })
    }

    fun typeCheckMapLiteral(
        node: MapLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.keys.forEach { key -> typeCheck(key, boundVars, refresh) }
        node.values.forEach { value -> typeCheck(value, boundVars, refresh) }

        // Map type is initially unknown, so set as map type with new key and value params
        val keyType = TypeVariable()
        val valType = TypeVariable()
        val mapType = MapType(keyType, valType)

        // Type of this map literal must be the new map type
        if (!unify(node.type, mapType)) {
            val types = typesToString(node.type, mapType)
            throw IRConversionException("Map literal could not be inferred to have map type, " +
                "found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each key in the map's type must be a subtype of the map's key type
        node.keys.forEach({ key -> 
            val except = { ->
                val types = typesToString(key.type, keyType)
                throw IRConversionException("Map must have keys of same type, found " +
                        "${types[0]} and ${types[1]}", key.startLocation)
            }

            if (!subtype(key.type, keyType, except)) {
                except()
            }
        })

        // Each value in the map's type must be a subtype of the map's value type
        node.values.forEach({ value ->
            val except = { ->
                val types = typesToString(value.type, valType)
                throw IRConversionException("Map must have values of same type, found " +
                            "${types[0]} and ${types[1]}", value.startLocation)
            }

            if (!subtype(value.type, valType, except)) {
                except()
            }
        })
    }

    fun typeCheckTupleLiteral(
        node: TupleLiteralNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.elements.forEach { element -> typeCheck(element, boundVars, refresh) }

        // Tuple type is initially unknown, so set as tuple type with type variables for fields
        val nodeType = TupleType(node.elements.map { TypeVariable() })

        // Type of this tuple literal must be the given tuple type
        if (!unify(node.type, nodeType)) {
            val types = typesToString(node.type, nodeType)
            throw IRConversionException("Tuple literal could not be inferred to have tuple " +
                    "type, found ${types[0]} but expected ${types[1]}", node.startLocation)
        }

        // Each element's type in the tuple must be a subtype of the corresponding element type
        node.elements.zip(nodeType.elementTypes).forEach { (element, expectedElementType) ->
            val except = { ->
                val types = typesToString(element.type, expectedElementType)
                throw IRConversionException("Cannot infer type for tuple element, expected " +
                        "${types[0]} but found ${types[1]}", element.startLocation)
            }

            if (!subtype(element.type, expectedElementType, except)) {
                except()
            }
        }
    }

    fun typeCheckVariable(
        node: VariableNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val info = symbolTable.getInfo(node.ident)
        if (info == null) {
            throw IRConversionException("Unknown identifier ${node.ident.name}", node.startLocation)
        }

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        // Since a type for an identifier is being found, a fresh version of that type must be
        // found (if applicable).
        val currentRepType = currentRepType(info.type)
        val expectedType = if (refresh && currentRepType is FunctionType) {
            findRepType(currentRepType, boundVars)
        } else {
            currentRepType
        }

        if (!unify(node.type, expectedType)) {
            val types = typesToString(node.type, expectedType)
            throw IRConversionException("Could not infer type for ${node.ident.name}, found " +
                    "${types[0]} but expected ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckUnaryMathOperator(
        node: UnaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Find the parameterized trait type for this operator based on the expression type
        val traitType = when (node) {
            is IdentityNode -> UNARY_PLUS_TRAIT_SIG.createTypeWithParams(listOf(node.node.type))
            is NegateNode -> UNARY_MINUS_TRAIT_SIG.createTypeWithParams(listOf(node.node.type))
        }

        // The left hand side must implement the correct operator trait
        val except = { ->
            val types = typesToString(node.node.type, traitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.startLocation)
        }

        if (!subtype(node.node.type, traitType, except)) {
            except()
        }

        // Unify this node's type with its child's type
        if (!unify(node.node.type, node.type)) {
            val types = typesToString(node.type, node.node.type)
            throw IRConversionException("Unary operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBinaryMathOperator(
        node: BinaryMathOperatorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Find the parameterized trait type for this operator based on the right hand side
        val traitType = when (node) {
            is AddNode -> ADD_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is SubtractNode -> SUBTRACT_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is MultiplyNode -> MULTIPLY_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is DivideNode -> DIVIDE_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is ExponentNode -> POWER_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
            is RemainderNode -> REMAINDER_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))
        }

        // The left hand side must implement the correct operator trait
        val except = { ->
            val types = typesToString(node.left.type, traitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.left.startLocation)
        }

        if (!subtype(node.left.type, traitType, except)) {
            except()
        }

        // The return type of this trait is the type of the right hand side
        if (!unify(node.type, node.right.type)) {
            val types = typesToString(node.type, node.right.type)
            throw IRConversionException("Binary operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckLogicalAnd(
        node: LogicalAndNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Logical and evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical and, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical and must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical and expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical and expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckLogicalOr(
        node: LogicalOrNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Logical or evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical or, " +
                    "found ${type}", node.left.startLocation)
        }

        // Both sides of logical or must have type bool
        if (!unify(node.left.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical or expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.left.startLocation)
        }

        if (!unify(node.right.type, BoolType)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Logical or expects two bools, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckLogicalNot(
        node: LogicalNotNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.node, boundVars, refresh)

        // Logical not evaluates to bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of logical not, " +
                    "found ${type}", node.startLocation)
        }

        if (!unify(node.node.type, BoolType)) {
            val type = typeToString(node.node.type)
            throw IRConversionException("Logical not expects a bool, found ${type}",
                    node.startLocation)
        }
    }

    fun typeCheckEquality(
        node: EqualityNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Equality nodes evaluate to a bool
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Could not infer bool type for result of comparison, " +
                    "found ${type}", node.startLocation)
        }

        // Unify both child types together, as both children must have the same unknown type.
        if (!unify(node.left.type, node.right.type)) {
            val types = typesToString(node.left.type, node.right.type)
            throw IRConversionException("Cannot check equality between different types, found " +
                    "${types[0]} and ${types[1]}", node.right.startLocation)
        }
    }

    fun typeCheckComparison(
        node: ComparisonNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.left, boundVars, refresh)
        typeCheck(node.right, boundVars, refresh)

        // Find the correctly parameterized comparable trait type (parameterized by right hand side)
        val comparable = COMPARABLE_TRAIT_SIG.createTypeWithParams(listOf(node.right.type))

        // The left hand side must implement the comparable trait
        val except = { ->
            val types = typesToString(node.left.type, comparable)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.left.startLocation)
        }

        if (!subtype(node.left.type, comparable, except)) {
            except()
        }

        // The return type of a comparison is always a boolean
        if (!unify(node.type, BoolType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Binary operation expected to evaluate to bool, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckAccess(node: AccessNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)
        
        val accessConstraint = AccessConstraint(node, boundVars, this)
        addDeferredConstraint(node.expr.type, accessConstraint)
    }

    fun typeCheckFieldAssignment(
        node: FieldAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.expr, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val fieldAssigmentConstraint = FieldAssignmentConstraint(node, this)
        addDeferredConstraint(node.expr.type, fieldAssigmentConstraint)
    }

    fun typeCheckIndex(
        node: IndexNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)

        // The result type of the index is unknown so far, so create type variable to hold it
        val indexResultType = TypeVariable()
        val indexTraitType = INDEX_TRAIT_SIG
                .createTypeWithParams(listOf(node.key.type, indexResultType))

        // The index operation must evaluate to the result type of the index
        if (!unify(indexResultType, node.type)) {
            val types = typesToString(indexResultType, node.type)
            throw IRConversionException("Index operation expected to evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.indexLocation)
        }

        // The container must be a subtype of the Index trait with the correct key and value types
        val except = { ->
            val types = typesToString(node.container.type, indexTraitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.indexLocation)
        }

        if (!subtype(node.container.type, indexTraitType, except)) {
            except()
        }
    }

    fun typeCheckIndexAssign(
        node: IndexAssignNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.container, boundVars, refresh)
        typeCheck(node.key, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        val indexAssignTraitType = INDEX_ASSIGN_TRAIT_SIG
                .createTypeWithParams(listOf(node.key.type, node.rValue.type))

        // The index assign operation must evaluate to the result type of the index
        if (!unify(node.rValue.type, node.type)) {
            val types = typesToString(node.rValue.type, node.type)
            throw IRConversionException("Index assign operation expected to evaluate to " +
                    "${types[0]}, but found ${types[1]}", node.indexLocation)
        }

        // The container must be a subtype of the IndexAssign trait with the correct key and value
        val except = { ->
            val types = typesToString(node.container.type, indexAssignTraitType)
            throw IRConversionException("Inferred type ${types[0]} does not implement ${types[1]}",
                    node.indexLocation)
        }

        if (!subtype(node.container.type, indexAssignTraitType, except)) {
            except()
        }
    }

    fun typeCheckFunctionCall(
        node: FunctionCallNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.func, boundVars, refresh)
        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        val funcType = node.func.type

        // Each actual argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.actualArgs.map { arg ->
            val expectedArgType = TypeVariable()

            val except = { ->
                val types = typesToString(expectedArgType, arg.type)
                throw IRConversionException("Function expected argument of type " +
                        "${types[0]}, but found ${types[1]}", arg.startLocation)
            }

            if (!subtype(arg.type, expectedArgType, except)) {
                except()
            }

            expectedArgType
        }

        // Function call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, funcType)) {
            val types = typesToString(funcType, expectedFuncType)
            throw IRConversionException("Function inferred to have type " +
                    "${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBuiltin(node: BuiltinNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.args.forEach { arg -> typeCheck(arg, boundVars, refresh) }

        // Refresh type variables in builtin type
        val funcType = findRepType(node.builtin.type, boundVars)

        // Each argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.args.map { arg ->
            val expectedArgType = TypeVariable()

            val except = { ->
                val types = typesToString(expectedArgType, arg.type)
                throw IRConversionException("Function expected argument of type " +
                        "${types[0]}, but found ${types[1]}", arg.startLocation)
            }

            if (!subtype(arg.type, expectedArgType, except)) {
                except()
            }

            expectedArgType
        }

        // Function call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, funcType)) {
            val types = typesToString(funcType, expectedFuncType)
            throw IRConversionException("Builtin function ${node.builtin.name} inferred to have " +
                    "type ${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckBuiltinMethod(
        node: BuiltinMethodNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.recv, boundVars, refresh)
        node.args.forEach { arg -> typeCheck(arg, boundVars, refresh) }

        val (recvType, methodType) = if (node.builtin.receiverType is VectorType) {
            val recvType = VectorType(TypeVariable())
            val paramsMap = mapOf(node.builtin.receiverType.elementType as TypeVariable to
                    recvType.elementType)
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else if (node.builtin.receiverType is SetType) {
            val recvType = SetType(TypeVariable())
            val paramsMap = mapOf(node.builtin.receiverType.elementType as TypeVariable to
                    recvType.elementType)
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else if (node.builtin.receiverType is MapType) {
            val recvType = MapType(TypeVariable(), TypeVariable())
            val paramsMap = mapOf(
                    node.builtin.receiverType.keyType as TypeVariable to recvType.keyType,
                    node.builtin.receiverType.valType as TypeVariable to recvType.valType
            )
            val methodType = node.builtin.type.substitute(paramsMap)

            Pair(recvType, methodType)
        } else {
            Pair(node.builtin.receiverType, node.builtin.type)
        }

        if (!unify(recvType, node.recv.type)) {
            val types = typesToString(recvType, node.recv.type)
            throw IRConversionException("Receiver for builtin method ${node.builtin.name} " +
                    "expected to have type ${types[0]}, but found ${types[1]}",
                    node.recv.startLocation)
        }

        // Each argument's type must be a subtype of the expected argument type
        val expectedArgTypes = node.args.map { arg ->
            val expectedArgType = TypeVariable()

            val except = { ->
                val types = typesToString(expectedArgType, arg.type)
                throw IRConversionException("Builtin method ${node.builtin.name} expected " +
                        "argument of type ${types[0]}, but found ${types[1]}", arg.startLocation)
            }

            if (!subtype(arg.type, expectedArgType, except)) {
                except()
            }

            expectedArgType
        }

        // Method call must evaluate to exact return type
        val expectedFuncType = FunctionType(expectedArgTypes, node.type)
        if (!unify(expectedFuncType, methodType)) {
            val types = typesToString(methodType, expectedFuncType)
            throw IRConversionException("Builtin method ${node.builtin.name} inferred to have " +
                    "type ${types[0]}, but used as if it had type ${types[1]}", node.startLocation)
        }
    }

    fun typeCheckTupleTypeConstructor(
        node: TupleTypeConstructorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.actualArgs.forEach { actualArg -> typeCheck(actualArg, boundVars, refresh) }

        // Create a fresh adt type for this type variant, since type params must be new
        val nodeType = node.adtVariant.adtSig.getFreshAdt()
        if (!unify(node.type, nodeType)) {
            val types = typesToString(nodeType, node.type)
            throw IRConversionException("Could not infer return type for type constructor, " +
                    "expected ${types[0]} but found ${types[1]}", node.startLocation)
        }

        // Find constructor arg types given the current adt params and adt variant
        val expectedArgTypes = node.adtVariant.getTypeConstructorWithParams(nodeType.typeParams)
        val actualArgTypes = node.actualArgs.map { arg -> arg.type }

        // If expected or actual args are empty, print special message if both aren't empty
        if (actualArgTypes.size == 0) {
            if (expectedArgTypes.size != 0) {
                throw IRConversionException("${node.adtVariant.name} expects arguments of " +
                        "type ${formatTypes(expectedArgTypes)}, but received no arguments",
                        node.startLocation)
            // If no args exist or were expected, there is nothing to type check since adt type
            // must have no params to infer.
            } else {
                return
            }
        } else if (expectedArgTypes.size == 0) {
            throw IRConversionException("${node.adtVariant.name} expects no arguments, but " +
                    "found arguments of type ${formatTypes(actualArgTypes)}",
                    node.startLocation)
        }

        // Each actual arg type must be a subtype of the corresponding expected arg type
        val except = { ->
            throw IRConversionException("${node.adtVariant.name} expected arguments of " +
                    "type ${formatTypes(expectedArgTypes)}, but found " +
                    "${formatTypes(actualArgTypes)}", node.startLocation)
        }

        val correctArgTypes = expectedArgTypes.size == actualArgTypes.size &&
                expectedArgTypes.zip(actualArgTypes)
                    .map({(expected, actual) -> subtype(actual, expected, except)})
                    .all({ x -> x })

        if (!correctArgTypes) {
            except()
        }
    }

    fun typeCheckRecordTypeConstructor(
        node: RecordTypeConstructorNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.fields.forEach { (_, field) -> typeCheck(field, boundVars, refresh) }

        // Create a fresh adt type for this type variant, since type params must be new
        val nodeType = node.adtVariant.adtSig.getFreshAdt()
        if (!unify(node.type, nodeType)) {
            val types = typesToString(nodeType, node.type)
            throw IRConversionException("Could not infer return type for type constructor, " +
                    "expected ${types[0]} but found ${types[1]}", node.startLocation)
        }

        // Find constructor fields types given the current adt params and adt variant
        val expectedFieldTypes = node.adtVariant.getFieldsWithParams(nodeType.typeParams)

        // Make sure that every field's type is a subtype of the corresponding expected field type
        node.fields.forEach { (fieldName, field) ->
            val expectedFieldType = expectedFieldTypes[fieldName]!!

            val except = { ->
                val types = typesToString(expectedFieldType, field.type)
                throw IRConversionException("${node.adtVariant.name} expected field ${fieldName} " +
                        "to have type ${types[0]}, but found ${types[1]}", field.startLocation)
            }

            if (!subtype(field.type, expectedFieldType, except)) {
                except()
            }
        }
    }

    fun typeCheckVariableAssignment(
        node: VariableAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val type = symbolTable.getInfo(node.lValue)?.type
        if (type == null) {
            throw IRConversionException("Unknown variable ${node.lValue.name}", node.identLocation)
        }

        typeCheck(node.rValue, boundVars, refresh)

        // The evaluation type of this node is the type stored for the variable in the symbol table.
        if (!unify(node.type, type)) {
            val types = typesToString(type, node.type)
            throw IRConversionException("Variable assignment should evaluate to ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }

        // The value assigned to the variable should be a subtype of the variable's type
        val except = { ->
            val types = typesToString(node.type, node.rValue.type)
            throw IRConversionException("Type of ${node.lValue.name} is " +
                    "${types[0]}, but assigned ${types[1]}", node.rValue.startLocation)
        }

        if (!subtype(node.rValue.type, node.type, except)) {
            except()
        }
    }

    fun typeCheckPatternAssignment(
        node: PatternAssignmentNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.pattern, boundVars, refresh)
        typeCheck(node.rValue, boundVars, refresh)

        // The type of the rValue should be a subtype of the pattern
        val except = { ->
            val types = typesToString(node.pattern.type, node.rValue.type)
            throw IRConversionException("Pattern has type ${types[0]}, " +
                    "but assigned ${types[1]}", node.rValue.startLocation)
        }

        if (!subtype(node.rValue.type, node.pattern.type, except)) {
            except()
        }

        // The evaluation type of the assignment should be the same as the pattern
        if (!unify(node.pattern.type, node.type)) {
            val types = typesToString(node.rValue.type, node.type)
            throw IRConversionException("Pattern assignment should evaluate to " +
                    "${types[0]}, but found ${types[1]}", node.rValue.startLocation)
        }
    }

    fun typeCheckVariableDefinition(
        node: VariableDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val varType = symbolTable.getInfo(node.ident)?.type
        if (varType == null) {
            throw IRConversionException("Unknown variable ${node.ident.name}", node.identLocation)
        }

        typeCheck(node.expr, boundVars, refresh)

        // Variable definition node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Variable definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        if (node.typeAnnotation != null) {
            // Identifier's type variable must be equal to the type annotation
            if (!unify(node.typeAnnotation, varType)) {
                val types = typesToString(node.typeAnnotation, varType)
                throw IRConversionException("Variable expected to have type ${types[0]}, but " +
                        "found ${types[1]}", node.identLocation)
            }

            // If a type annotation is supplied, the expression must be a subtype of the annotation
            val except = { ->
                val types = typesToString(node.typeAnnotation, node.expr.type)
                throw IRConversionException("Variable has type ${types[0]}, but assigned " +
                        "${types[1]}", node.expr.startLocation)
            }

            if (!subtype(node.expr.type, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation is supplied, inferred type is the expression's inferred type
            if (!unify(node.expr.type, varType)) {
                val types = typesToString(varType, node.expr.type)
                throw IRConversionException("Variable has type ${types[0]}, but assigned " +
                        "${types[1]}", node.expr.startLocation)
            }
        }
    }

    fun typeCheckPatternDefinition(
        node: PatternDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        typeCheck(node.pattern, boundVars, refresh)
        typeCheck(node.expr, boundVars, refresh)

        // Pattern definition nodes evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Pattern definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        if (node.typeAnnotation != null) {
            // Type of pattern must be equal to type annotation
            if (!unify(node.pattern.type, node.typeAnnotation)) {
                val types = typesToString(node.typeAnnotation, node.pattern.type)
                throw IRConversionException("Pattern expected to have type ${types[0]}, but found " +
                        "${types[1]}", node.patternLocation)
            }

            // If a type annotation is supplied, the expression must be a subtype of the annotation
            val except = { ->
                val types = typesToString(node.typeAnnotation, node.expr.type)
                throw IRConversionException("Pattern has type ${types[0]}, " +
                            "but assigned ${types[1]}", node.expr.startLocation)
            }

            if (!subtype(node.expr.type, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation, expression type must be exactly equal to pattern's type
            if (!unify(node.expr.type, node.pattern.type)) {
                val types = typesToString(node.pattern.type, node.expr.type)
                throw IRConversionException("Pattern has type ${types[0]}, " +
                    "but assigned ${types[1]}", node.expr.startLocation)
            }
        }
    }

    fun typeCheckFunctionDefinition(
        node: FunctionDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        val funcType = symbolTable.getInfo(node.ident)?.type

        if (funcType !is FunctionType) {
            throw IRConversionException("Unknown function ${node.ident.name}", node.identLocation)
        }

        val newBoundVars = boundVars.toHashSet()

        // Add all type variables in function type to the set of bound type variables
        funcType.getAllVariables().forEach { typeVar ->
            newBoundVars.add(typeVar)
        }

        // If this is a method definition, add all type implementation parameters to bound vars
        if (node is MethodDefinitionNode) {
            val type = symbolTable.getInfo(node.thisIdent)?.type!!
            type.getAllVariables().forEach { typeVar ->
                newBoundVars.add(typeVar)
            }
        }

        typeCheck(node.body, newBoundVars, refresh)

        // Function definition node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Function definition should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }

        // All returned types should be a subtype of the return type
        var foundReturn = false
        mapOverReturns(node.body, { retNode ->
            foundReturn = true
            val retType = retNode.expr?.type ?: UnitType

            val except = { ->
                val types = typesToString(funcType.returnType, retType)
                val location = retNode.expr?.startLocation ?: retNode.startLocation
                throw IRConversionException("${node.ident.name} must return ${types[0]} " +
                            "but found ${types[1]}", location)
            }

            // If a type annotation does not exist, returned types must all be exactly equal to
            // function's return type. If annotation does exist, returned types must only be
            // subtypes of this annotated type.
            val typesCheck = if (node.returnTypeAnnotation == null) {
                unify(retType, funcType.returnType)
            } else {
                subtype(retType, funcType.returnType, except)
            }

            if (!typesCheck) {
                except()
            }
        })

        // If no return statements were found, this function returns unit
        if (!foundReturn) {
            if (!unify(funcType.returnType, UnitType)) {
                val type = typeToString(funcType.returnType)
                throw IRConversionException("${node.ident.name} inferred to return unit, but " +
                        "expected ${type}", node.startLocation)
            }
        }

        // Main must always have return type int or unit, and a single optional argument
        // with type vec<string>.
        if (node.ident.name == "main") {
            // If there is a return, main must return an int
            if (foundReturn) {
                if (!unify(funcType.returnType, IntType)) {
                    val type = typeToString(funcType.returnType)
                    throw IRConversionException("Main function must return int, found ${type}",
                            node.startLocation)
                }
            }

            // Main must have single argument of type vec<string>, if any arguments exist
            val argType = VectorType(StringType)
            if ((funcType.argTypes.size >= 1 && !unify(funcType.argTypes[0], argType))
                    || funcType.argTypes.size >= 2) {
                throw IRConversionException("Main function must have a single argument of type " +
                        "${argType}", node.startLocation)
            }            
        }

        // If this function implements method signatures, this function type is a subtype
        // of each method signature type.
        if (node.signatures != null) {
            for (signature in node.signatures) {
                val except = { ->
                    val types = typesToString(signature, funcType)
                    println("oh no it failed on ${signature} and ${funcType} reps: ${currentRepType(signature)} ${currentRepType(funcType)}")
                    throw IRConversionException("Method signature ${node.ident.name} has type " +
                            "${types[0]}, but implementation has type ${types[1]}",
                            node.startLocation)
                }

                if (!subtype(funcType, signature, except)) {
                    except()
                }
            }
        }
    }

    fun typeCheckLambda(node: LambdaNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        // Bind all types in the argument list
        val newBoundVars = boundVars.toHashSet()

        val argTypes = node.formalArgs.map { formalArg -> symbolTable.getInfo(formalArg)?.type!! }
        argTypes.forEach { argType ->
            val argTypeVars = argType.getAllVariables()
            newBoundVars.addAll(argTypeVars)
        }

        typeCheck(node.body, newBoundVars, refresh)

        // Create expected function type from argument list and new return type variable
        val returnType = TypeVariable()
        val funcType = FunctionType(argTypes, returnType)

        // Lambda expression node evaluates to this function type
        if (!unify(node.type, funcType)) {
            val types = typesToString(funcType, node.type)
            throw IRConversionException("Expected lambda expression to have type ${types[0]}, " +
                    "but found ${types[1]}", node.startLocation)
        }

        // All returned types should have the same type, the return type of the lambda expression.
        // Note that implicitly returning traits in a lambda is not yet supported.
        mapOverReturns(node.body, { retNode ->
            val retType = retNode.expr?.type ?: UnitType
            if (!unify(retType, returnType)) {
                val types = typesToString(returnType, retType)
                val location = retNode.expr?.startLocation ?: retNode.startLocation
                throw IRConversionException("Lambda expression expected to return ${types[0]} " +
                        "but found ${types[1]}", location)
            }
        })
    }

    /**
     * Apply a function to every return rooted at a subtree, without recurring into nested
     * function definitions.
     */
    fun mapOverReturns(node: IRNode, func: (ReturnNode) -> Unit) {
        when (node) {
            is BlockNode -> node.nodes.map { n -> mapOverReturns(n, func) }
            is IfNode -> {
                mapOverReturns(node.conseq, func)
                node.altern?.let { mapOverReturns(it, func) }
            }
            is WhileNode -> mapOverReturns(node.body, func)
            is DoWhileNode -> mapOverReturns(node.body, func)
            is ForNode -> {
                node.init?.let { mapOverReturns(it, func) }
                node.update?.let { mapOverReturns(it, func) }
                mapOverReturns(node.body, func)
            }
            is ForEachNode -> {
                mapOverReturns(node.iterable, func)
                mapOverReturns(node.body, func)
            }
            is MatchNode -> node.cases.forEach { (_, _, stmt) -> mapOverReturns(stmt, func) }
            is ReturnNode -> func(node)
        }
    }

    fun typeCheckBlock(node: BlockNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.nodes.forEach { child -> typeCheck(child, boundVars, refresh) }

        // If block is a statement or an expression with no cases, it must evaluate to unit
        if (!node.isExpression || node.nodes.isEmpty()) {
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("Block statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        // If block is a nonempty expression, it's type is a supertype of the last statement's type
        } else {
            val lastNode = node.nodes[node.nodes.size - 1]
            val except = { ->
                val types = typesToString(node.type, lastNode.type)
                throw IRConversionException("Block expression expected to evaluate to " + 
                        "${types[0]}, but found ${types[1]}", node.startLocation)
            }

            if (!subtype(lastNode.type, node.type, except)) {
                except()
            }
        }
    }

    fun typeCheckIf(node: IfNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of if must be a bool, but found ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.conseq, boundVars, refresh)

        node.altern?.let { typeCheck(it, boundVars, refresh) }

        if (node.isExpression) {
            // If an expression, the if node must have an else case
            val altern = node.altern
            if (altern == null) {
                throw IRConversionException("If expression must have else case", node.startLocation)
            }

            // True case of if expression must evaluate to subtype of if expression's return type
            if (!unify(node.conseq.type, node.type)) {
                val types = typesToString(node.conseq.type, altern.type)
                throw IRConversionException("Both true and false cases of if expression must " +
                        "have the same type, found ${types[0]} and ${types[1]}", node.startLocation)
            }

            // False case of if expression must evaluate to subtype of if expression's return type
            if (!unify(altern.type, node.type)) {
                val types = typesToString(node.type, altern.type)
                throw IRConversionException("If expression expected to have type ${node.type}, " +
                        "but found ${types[1]}", node.startLocation)
            }
        } else {
            // If a statement, if cases do not have to have same type and if evaluates to unit
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("If statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        }
    }

    fun typeCheckWhile(node: WhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // While node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("While loop should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckDoWhile(node: DoWhileNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.cond, boundVars, refresh)

        if (!unify(node.cond.type, BoolType)) {
            val type = typeToString(node.cond.type)
            throw IRConversionException("Condition of do while must be a bool, but given ${type}",
                    node.cond.startLocation)
        }

        typeCheck(node.body, boundVars, refresh)

        // Do while node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Do while loop should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckFor(node: ForNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.init?.let { typeCheck(it, boundVars, refresh) }

        node.cond?.let { cond ->
            typeCheck(cond, boundVars, refresh)
            if (!unify(cond.type, BoolType)) {
                val type = typeToString(cond.type)
                throw IRConversionException("Condition of for must be a bool, but given ${type}",
                        cond.startLocation)
            }
        }

        node.update?.let { typeCheck(it, boundVars, refresh) }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("For loop should evaluate to the unit type, but found " +
                    "${type}", node.startLocation)
        }
    }

    fun typeCheckForEach(node: ForEachNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.lValue, boundVars, refresh)
        typeCheck(node.iterable, boundVars, refresh)

        // Iterable expression must implement iterable trait
        val forEachPatternType = TypeVariable()
        val iterableTraitType = ITERABLE_TRAIT_SIG.createTypeWithParams(listOf(forEachPatternType))

        var except = { ->
            val types = typesToString(node.iterable.type, iterableTraitType)
            throw IRConversionException("Iterable in for each loop inferred to have type " +
                    "${types[0]}, but must implement ${types[1]}", node.iterable.startLocation)
        }

        if (!subtype(node.iterable.type, iterableTraitType, except)) {
            except()
        }

        if (node.typeAnnotation != null) {
            // Patterns's type variable must be equal to the type annotation
            if (!unify(node.typeAnnotation, node.lValue.type)) {
                val types = typesToString(node.typeAnnotation, node.lValue.type)
                throw IRConversionException("Pattern expected to have type ${types[0]}, but " +
                        "found ${types[1]}", node.lValue.startLocation)
            }

            // If a type annotation is supplied, the iterable must be a subtype of the annotation
            except = { ->
                val types = typesToString(node.typeAnnotation, forEachPatternType)
                throw IRConversionException("Pattern has type ${types[0]}, but assigned " +
                        "${types[1]}", node.lValue.startLocation)
            }

            if (!subtype(forEachPatternType, node.typeAnnotation, except)) {
                except()
            }
        } else {
            // If no type annotation is supplied, inferred type is the iterable's inferred type
            if (!unify(forEachPatternType, node.lValue.type)) {
                val types = typesToString(node.lValue.type, forEachPatternType)
                throw IRConversionException("Pattern has type ${types[0]}, but assigned " +
                        "${types[1]}", node.lValue.startLocation)
            }
        }

        typeCheck(node.body, boundVars, refresh)

        // For node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("For each loop should evaluate to the unit type, but " +
                    "found ${type}", node.startLocation)
        }
    }

    fun typeCheckMatch(node: MatchNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        typeCheck(node.expr, boundVars, refresh)

        node.cases.forEach { (pattern, guard, statement) ->
            typeCheck(pattern, boundVars, false)

            // Bind all variables that are found in the pattern, including in child nodes
            val newBoundVars = boundVars.toHashSet()
            pattern.forEach { patNode ->
                patNode.type.getAllVariables().forEach { typeVar -> newBoundVars.add(typeVar) }
            }

            // Type check guard if it exists
            if (guard != null) {
                typeCheck(guard, newBoundVars, refresh)
            }

            typeCheck(statement, newBoundVars, refresh)
        }

        node.cases.forEach { (pat, guard, _) ->
            // All patterns must have same type as the matched expression
            if (!unify(pat.type, node.expr.type)) {
                val types = typesToString(node.expr.type, pat.type)
                throw IRConversionException("Patterns in match statement expected to have type " +
                        "${types[0]}, but found ${types[1]}", pat.startLocation)
            }

            // Guard statements, if they exist, must evaluate to bools
            if (guard != null) {
                if (!unify(guard.type, BoolType)) {
                    val type = typeToString(guard.type)
                    throw IRConversionException("Pattern matching guards must have type bool, " +
                            "but found ${type}", guard.startLocation)
                }
            }
        }

        if (node.isExpression) {
            // Evaluation type of match expression is evaluation type of cases
            val matchType = TypeVariable()
            if (!unify(node.type, matchType)) {
                val types = typesToString(node.type, matchType)
                throw IRConversionException("Match expression expected to have type " +
                        "${types[0]}, but found ${types[1]}", node.startLocation)
            }

            // If an expression, every case type must be a subtype of the match expression type
            node.cases.forEach { (_, _, case) ->
                val except = { ->
                    val types = typesToString(case.type, matchType)
                    throw IRConversionException("Cases in match expression must all have the " +
                            "same type, found ${types[0]} and ${types[1]}", case.startLocation)
                }

                if (!subtype(case.type, matchType, except)) {
                    except()
                }
            }
        } else {
            // If a statement, cases do not have to evaluate to same type and the entire match
            // node evaluates to the unit value.
            if (!unify(node.type, UnitType)) {
                val type = typeToString(node.type)
                throw IRConversionException("Match statement should evaluate to the unit type, " +
                        "but found ${type}", node.startLocation)
            }
        }
    }

    fun typeCheckReturn(node: ReturnNode, boundVars: MutableSet<TypeVariable>, refresh: Boolean) {
        node.expr?.let { typeCheck(it, boundVars, refresh) }

        // Return node evalutes to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckBreak(node: BreakNode) {
        // Break node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Break statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckContinue(node: ContinueNode) {
        // Continue node evaluates to unit value
        if (!unify(node.type, UnitType)) {
            val type = typeToString(node.type)
            throw IRConversionException("Return statement should evaluate to the unit type, " +
                    "but found ${type}", node.startLocation)
        }
    }

    fun typeCheckTraitDefinition(
        node: TraitDefinitionNode,
        boundVars: MutableSet<TypeVariable>,
        refresh: Boolean
    ) {
        node.methods.forEach { method -> typeCheck(method, boundVars, refresh) }
    }

    /**
     * Infer types for every identifier of the specified class in the symbol table. Be sure to
     * only infer types after all type checking and unification has taken place.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferSymbolTypes(idClass: IdentifierClass, freezeSymbols: Boolean) {
        // Infer types for every identifier of the specified class
        for ((ident, identInfo) in symbolTable.identifiers) {
            if (identInfo.typeShouldBeInferred && !identInfo.typeIsInferred &&
                    identInfo.idClass == idClass) {
                identInfo.type = currentRepType(identInfo.type)

                println("${ident.name} was resolved to type ${identInfo.type}")

                if (freezeSymbols) {
                    identInfo.typeIsInferred = true
                }
            }
        }
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferFunctionTypes(freezeSymbols: Boolean) {
        inferSymbolTypes(IdentifierClass.FUNCTION, freezeSymbols)
    }

    /**
     * Infer types for every function in the symbol table. Be sure to only infer types after all
     * type checking and unification has taken place, and after function types have been inferred.
     * 
     * @param freezeSymbols whether or not to freeze all inferred symbol types
     */
    fun inferVariableTypes(freezeSymbols: Boolean) {
        inferSymbolTypes(IdentifierClass.VARIABLE, freezeSymbols)
    }

    /**
     * Infer types for every IR tree rooted at a list of nodes. Be sure to only infer types after
     * all type checking and unification has taken place, and after function and variable types
     * have been inferred.
     */
    fun inferIRTypes(root: IRNode) {
        // Infer return types for each IRNode
        root.forEach { node ->
            // Wrapper nodes should be ignored
            if (node !is WrapperNode) {
                node.type = currentRepType(node.type)
            }
        }
    }
}
