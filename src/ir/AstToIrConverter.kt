package myte.ir

import myte.eval.builtins.*
import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

class AstToIrConverter(var symbolTable: SymbolTable) {

    private val typeChecker = TypeChecker(symbolTable)

    /**
     * Set the symbol table to new symbol table.
     */
    fun resetSymbolTable(newSymbolTable: SymbolTable) {
        symbolTable = newSymbolTable
        typeChecker.resetSymbolTable(newSymbolTable)
    }

    fun convertMyteFiles(parseFilesResult: ParseFilesResult): List<IRNode> {
        val packages = parseFilesResult.packages

        // First check that all imports actually exist
        assertImportsExist(parseFilesResult.importContexts)

        // First create ADT sigs, then create variants for those sigs so that variants can
        // reference other ADTs regardless of order.
        packages.forEach { pack -> pack.typeDefs.forEach(this::createAdtSig) }
        packages.forEach { pack -> pack.typeDefs.forEach(this::createAdtVariants) }

        // Convert all statements of each package
        val irNodes = packages.map({ pack -> pack.statements.map(this::convert) }).flatten()

        // Infer types and then check structure of IR
        inferTypes(irNodes)
        irNodes.forEach(this::assertIRStructure)

        return irNodes
    }

    fun convertFiles(parseFilesResult: ParseFilesResult): ConvertFilesResult {
        val nodes = convertMyteFiles(parseFilesResult)
        val mainIdent = findMainIdentifier(nodes)
        return ConvertFilesResult(nodes, mainIdent)
    }

    fun convertPackages(parsePackagesResult: ParseFilesResult): ConvertPackagesResult {
        val nodes = convertMyteFiles(parsePackagesResult)
        return ConvertPackagesResult(nodes)
    }

    fun convertReplLine(replLineResult: ParseReplLineResult): ConvertReplLineResult {
        val statement = replLineResult.statement
        if (statement == null) {
            // If no statement this must have been an import, so check that all imports exist
            assertImportsExist(listOf(replLineResult.importContext))
            return ConvertReplLineResult(null)
        } else if (statement is TypeDefinitionExpression) {
            // Create ADT sig and variants
            createAdtSig(statement)
            createAdtVariants(statement)
            return ConvertReplLineResult(null)
        } else if (statement is Statement) {
            // Convert node, infer types, and verify correct IR structure
            val node = convert(statement)
            inferTypes(listOf(node))
            assertIRStructure(node)

            return ConvertReplLineResult(node)
        } else {
            throw Exception("Unkown repl statement ${statement}")  
        }
    }

    /**
     * Convert a node in the ast into a node in the internal representation.
     *
     * @param stmt the ast node
     * @return the exivalent node in the internal representation
     */
    fun convert(stmt: Statement): IRNode {
        return when {
            // Literals
            stmt is BoolLiteralExpression -> BoolLiteralNode(stmt.bool, stmt.startLocation)
            stmt is StringLiteralExpression -> StringLiteralNode(stmt.str, stmt.startLocation)
            stmt is IntLiteral -> IntLiteralNode(stmt.num, stmt.startLocation)
            stmt is FloatLiteral -> FloatLiteralNode(stmt.num, stmt.startLocation)
            stmt is UnitLiteralExpression -> UnitLiteralNode(stmt.startLocation)
            stmt is VectorLiteralExpression -> convertVectorLiteral(stmt)
            stmt is SetLiteralExpression -> convertSetLiteral(stmt)
            stmt is MapLiteralExpression -> convertMapLiteral(stmt)
            stmt is TupleLiteralExpression -> convertTupleLiteral(stmt)
            stmt is LambdaExpression -> convertLambda(stmt)
            // Variables and functions
            stmt is VariableExpression -> convertVariable(stmt)
            stmt is ApplicationExpression -> convertApplication(stmt)
            stmt is TypeConstructorExpression -> convertTypeConstructor(stmt)
            stmt is KeyedAccessExpression -> convertKeyedAccess(stmt)
            stmt is AssignmentExpression -> convertAssignment(stmt)
            stmt is VariableDefinitionStatement -> convertVariableDefinition(stmt)
            stmt is FunctionDefinitionStatement -> convertFunctionDefinition(stmt)
            // Math expressions
            stmt is UnaryPlusExpression -> convertUnaryPlus(stmt)
            stmt is UnaryMinusExpression -> convertUnaryMinus(stmt)
            stmt is BinaryMathOperatorExpression -> convertBinaryMathOperator(stmt)
            // Logical operators
            stmt is LogicalAndExpression -> convertLogicalAnd(stmt)
            stmt is LogicalOrExpression -> convertLogicalOr(stmt)
            stmt is LogicalNotExpression -> convertLogicalNot(stmt)
            // Comparisons
            stmt is EqualityExpression -> convertEquality(stmt)
            stmt is ComparisonExpression -> convertComparison(stmt)
            // Control flow and structure
            stmt is GroupExpression -> convert(stmt.expr)
            stmt is BlockStatement -> BlockNode(stmt.stmts.map(this::convert), stmt.startLocation)
            stmt is IfStatement -> convertIf(stmt)
            stmt is WhileStatement -> convertWhile(stmt)
            stmt is DoWhileStatement -> convertDoWhile(stmt)
            stmt is ForStatement -> convertFor(stmt)
            stmt is MatchStatement -> convertMatch(stmt)
            stmt is ReturnStatement -> convertReturn(stmt)
            stmt is BreakStatement -> BreakNode(stmt.breakLocation)
            stmt is ContinueStatement -> ContinueNode(stmt.continueLocation)
            else -> throw IRConversionException("Unknown statement ${stmt}", stmt.startLocation)
        }
    }

    /**
     * Returns whether an identifier has the given identifier property.
     */
    fun identHasProp(ident: Identifier, prop: IdentifierProperty): Boolean {
        return symbolTable.getInfo(ident)?.props?.contains(prop) ?: false
    }

    /**
     * Returns whether an identifier is immutable.
     */
    fun isImmutable(ident: Identifier): Boolean {
        return identHasProp(ident, IdentifierProperty.IMMUTABLE)
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Conversion functions for each AST statement node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun convertLambda(expr: LambdaExpression): LambdaNode {
        val formalArgs = expr.formalArgs.map { (arg, typeAnnotation) ->
            // Annotate each formal argument with its optional type annotation or new type variable
            val type = if (typeAnnotation != null) {
                convertType(typeAnnotation)
            } else {
                TypeVariable()
            }

            symbolTable.getInfo(arg)?.type = type

            arg
        }

        return LambdaNode(formalArgs, convert(expr.body), expr.startLocation)
    }

    fun convertFunctionDefinition(stmt: FunctionDefinitionStatement): FunctionDefinitionNode {
        val (formalArgs, argTypes) = stmt.formalArgs.map({ (arg, typeAnnotation) ->
            // Annotate each formal argument with its optional type annotation or new type variable
            val type = if (typeAnnotation != null) {
                convertType(typeAnnotation)
            } else {
                TypeVariable()
            }

            symbolTable.getInfo(arg)?.type = type

            Pair(arg, type)
        }).unzip()

        // Find annotated return type
        val returnType = if (stmt.returnTypeAnnotation != null) {
            convertType(stmt.returnTypeAnnotation)
        } else {
            TypeVariable()
        }

        // Annotate function identifier with type from annotations
        symbolTable.getInfo(stmt.ident)?.type = FunctionType(argTypes, returnType)

        return FunctionDefinitionNode(stmt.ident, formalArgs, convert(stmt.body),
                stmt.identLocation, stmt.startLocation)
    }

    fun convertVariableDefinition(stmt: VariableDefinitionStatement): VariableDefinitionNode {
        // Annotate this identifier with type annotation, or new type variable if not annotated
        val type = if (stmt.typeAnnotation != null) {
            convertType(stmt.typeAnnotation)
        } else {
            TypeVariable()
        }

        symbolTable.getInfo(stmt.ident)?.type = type

        return VariableDefinitionNode(stmt.ident, convert(stmt.expr), stmt.identLocation,
                stmt.startLocation)
    }

    fun convertIf(stmt: IfStatement): IfNode {
        val cond = convert(stmt.cond)
        val conseq = convert(stmt.conseq)
        val altern = if (stmt.altern != null) convert(stmt.altern) else null

        return IfNode(cond, conseq, altern, stmt.startLocation)
    }

    fun convertWhile(stmt: WhileStatement): WhileNode {
        return WhileNode(convert(stmt.cond), convert(stmt.body), stmt.startLocation)
    }

    fun convertDoWhile(stmt: DoWhileStatement): DoWhileNode {
        return DoWhileNode(convert(stmt.cond), convert(stmt.body), stmt.startLocation)
    }

    fun convertFor(stmt: ForStatement): ForNode {
        val init = if (stmt.init != null) convert(stmt.init) else null
        val cond = if (stmt.cond != null) convert(stmt.cond) else null
        val update = if (stmt.update != null) convert(stmt.update) else null

        return ForNode(init, cond, update, convert(stmt.body), stmt.startLocation)
    }

    fun convertMatch(stmt: MatchStatement): MatchNode {
        val expr = convert(stmt.expr)
        val cases = stmt.cases.map { (p, g, s) ->
            val pattern = convert(p)

            val guard = if (g != null) {
                convert(g)
            } else {
                null
            }

            val statement = convert(s)

            Triple(pattern, guard, statement)
        }

        return MatchNode(expr, cases, stmt.startLocation)
    }

    fun convertVariable(expr: VariableExpression): IRNode {
        val ident = expr.ident.resolve()

        // If this identifier is for a type constructor, create a type constructor node
        val info = symbolTable.getInfo(ident)!!
        if (info.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            return TypeConstructorNode(info.adtVariant, listOf(), expr.identLocation)
        }

        return VariableNode(ident, expr.identLocation)
    }

    fun convertApplication(expr: ApplicationExpression): IRNode {
        val func = convert(expr.func)
        val args = expr.args.map(this::convert)

        if (func is TypeConstructorNode) {
            return TypeConstructorNode(func.adtVariant, args, func.startLocation)
        } else {
            return FunctionCallNode(func, args, expr.callLocation, expr.startLocation)
        }
    }

    fun convertTypeConstructor(expr: TypeConstructorExpression): TypeConstructorNode {
        val ident = expr.ident.resolve()

        val info = symbolTable.getInfo(ident)
        if (info?.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            throw IRConversionException("${expr.ident.symbol()} is not a type constructor",
                    expr.startLocation)
        }

        val args = expr.actualArgs.map(this::convert)

        return TypeConstructorNode(info.adtVariant, args, expr.startLocation)
    }

    fun convertKeyedAccess(expr: KeyedAccessExpression): KeyedAccessNode {
        return KeyedAccessNode(convert(expr.container), convert(expr.key), expr.accessLocation)
    }

    fun convertAssignment(expr: AssignmentExpression): IRNode {
        val lValue = convert(expr.lValue)

        if (lValue is VariableNode) {
            // Error if assigning to identifier that is not a variable
            if (symbolTable.getInfo(lValue.ident)?.idClass != IdentifierClass.VARIABLE) {
                throw IRConversionException("Can only reassign variables", expr.equalsLocation)
            }

            // Error if assigning to immutable variable
            if (isImmutable(lValue.ident)) {
                throw IRConversionException("Cannot reassign immutable variable " +
                        "${lValue.ident.name}", lValue.startLocation)
            }

            return VariableAssignmentNode(lValue.ident, convert(expr.rValue), lValue.startLocation)
        } else if (lValue is KeyedAccessNode) {
            return KeyedAssignmentNode(lValue.container, lValue.key,
                    convert(expr.rValue), lValue.accessLocation)
        } else {
            throw IRConversionException("Cannot only assign values to variables",
                    expr.equalsLocation)
        }
    }

    fun convertReturn(expr: ReturnStatement): ReturnNode {
        val returnVal = if (expr.expr != null) convert(expr.expr) else null 
        return ReturnNode(returnVal, expr.returnLocation)
    }

    fun convertVectorLiteral(expr: VectorLiteralExpression): VectorLiteralNode {
        return VectorLiteralNode(expr.elements.map(this::convert), expr.startLocation)
    }

    fun convertSetLiteral(expr: SetLiteralExpression): SetLiteralNode {
        return SetLiteralNode(expr.elements.map(this::convert), expr.startLocation)
    }

    fun convertMapLiteral(expr: MapLiteralExpression): MapLiteralNode {
        return MapLiteralNode(expr.keys.map(this::convert), expr.values.map(this::convert),
                expr.startLocation)
    }

    fun convertTupleLiteral(expr: TupleLiteralExpression): TupleLiteralNode {
        return TupleLiteralNode(expr.elements.map(this::convert), expr.startLocation)
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
        return LogicalNotNode(convert(expr.expr), expr.startLocation)
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

    fun convertUnaryPlus(expr: UnaryPlusExpression): IRNode {
        val child = convert(expr.expr)

        // If child is an int or float literal, can simply return the literals
        if (child is IntLiteralNode) {
            return IntLiteralNode(child.num, expr.startLocation)
        } else if (child is FloatLiteralNode) {
            return FloatLiteralNode(child.num, expr.startLocation)
        } else {
            return IdentityNode(convert(expr.expr), expr.startLocation)
        }
    }

    fun convertUnaryMinus(expr: UnaryMinusExpression): IRNode {
        val child = convert(expr.expr)

        // If child is an int or float literal, can simply negate the literals
        if (child is IntLiteralNode) {
            return IntLiteralNode(-child.num, expr.startLocation)
        } else if (child is FloatLiteralNode) {
            return FloatLiteralNode(-child.num, expr.startLocation)
        } else {
            return NegateNode(child, expr.startLocation)
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Conversion functions for type definitions
    //
    ///////////////////////////////////////////////////////////////////////////

    fun createAdtSig(typeDef: TypeDefinitionExpression) {
        // First annotate all type parameters identifiers with new type variables
        val typeParams = typeDef.typeParamIdents.map { typeParamIdent ->
            val typeParam = TypeVariable()
            symbolTable.getInfo(typeParamIdent)?.type = typeParam
            typeParam
        }

        // Create initial ADT signature and annotate identifier with it
        val adtSig = AlgebraicDataTypeSignature(typeDef.typeIdent.name, typeParams)
        symbolTable.getInfo(typeDef.typeIdent)?.adtSig = adtSig
        symbolTable.getInfo(typeDef.typeIdent)?.type = adtSig.getAdtWithParams(adtSig.typeParams)
    }

    fun createAdtVariants(typeDef: TypeDefinitionExpression) {
        val adtSig = symbolTable.getInfo(typeDef.typeIdent)?.adtSig!!

        for ((variantIdent, typeConstructorExprs) in typeDef.variants) {
            // Create variant by parsing type constructor and add to ADT sig
            val typeConstructor = typeConstructorExprs.map(this::convertType)
            val variant = AlgebraicDataTypeVariant(adtSig, variantIdent.name, typeConstructor)
            adtSig.variants.add(variant)

            // Annotate variant's identifier with newly created variant
            symbolTable.getInfo(variantIdent)?.adtVariant = variant
            symbolTable.getInfo(variantIdent)?.type = variant.typeForConstructor()
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Conversion functions for each AST type node
    //
    ///////////////////////////////////////////////////////////////////////////

    fun convertType(typeExpr: TypeExpression): Type {
        return when (typeExpr) {
            is UnitTypeExpression -> UnitType
            is BoolTypeExpression -> BoolType
            is IntTypeExpression -> IntType
            is FloatTypeExpression -> FloatType
            is StringTypeExpression -> StringType
            is VectorTypeExpression -> VectorType(convertType(typeExpr.elementType))
            is SetTypeExpression -> SetType(convertType(typeExpr.elementType))
            is MapTypeExpression -> MapType(convertType(typeExpr.keyType),
                    convertType(typeExpr.valType))
            is TupleTypeExpression -> TupleType(typeExpr.elementTypes.map(this::convertType))
            is FunctionTypeExpression -> FunctionType(typeExpr.argTypes.map(this::convertType),
                    convertType(typeExpr.returnType))
            is VariableTypeExpression -> convertVariableType(typeExpr)
        }
    }

    fun convertVariableType(typeExpr: VariableTypeExpression): Type {
        val ident = typeExpr.ident.resolve()

        val info = symbolTable.getInfo(ident)!!
        if (info.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE) {
            val typeParams = typeExpr.typeParams.map(this::convertType)

            // If this is an ADT, error if number of parameters differs
            val adtSig = info.adtSig
            if (adtSig.typeParams.size != typeParams.size) {
                throw IRConversionException("Type ${adtSig.name} expects " +
                        "${adtSig.typeParams.size} type parameters, but received " +
                        "${typeParams.size}", typeExpr.startLocation)
            }

            return adtSig.getAdtWithParams(typeParams)
        } else if (info.idClass == IdentifierClass.TYPE_PARAMETER) {
            if (!typeExpr.typeParams.isEmpty()) {
                // Error if this is not an ADT but was given type parameters
                throw IRConversionException("Type ${ident.name} does not have any type parameters",
                        typeExpr.startLocation)
            }

            return info.type
        } else {
            throw IRConversionException("${ident.name} is not a type", typeExpr.startLocation)
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Type and structure checks after IR tree has been created
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Check that all imports point to packages (or members within packages) that exist.
     */
    fun assertImportsExist(importContexts: List<ImportContext>) {
        for (importContext in importContexts) {
            for ((_, importName, location) in importContext.imports) {
                // Every import must be either the name of a package
                val pack = importContext.rootPackageNode.getSubPackage(importName)

                // Or an identifier or type within a package
                val identName = importName[importName.size - 1]
                val parentPackage = importContext.rootPackageNode
                        .getSubPackage(importName.dropLast(1))
                val variableInPackage = parentPackage?.scope?.lookupVariable(identName)
                val typeInPackage = parentPackage?.scope?.lookupType(identName)

                // Error if the import statement does not correspond to package or member in package
                if (pack == null && variableInPackage == null && typeInPackage == null) {
                    val name = formatPackageName(importName)
                    throw IRConversionException("No package (or member within package) found " +
                            "with name ${name}", location)
                }
            }
        }
    }

    /**
     * Find the unique identifier for the main function, erroring if a unique main cannot be found.
     */
    fun findMainIdentifier(nodes: List<IRNode>): Identifier {
        var mainIdent: Identifier? = null
        var mainFileName: String = ""
        for (node in nodes) {
            // Find the main function def and save its identifier, erroring if two mains are found
            if (node is FunctionDefinitionNode && node.ident.name == "main") {
                if (mainIdent == null) {
                    mainIdent = node.ident
                    mainFileName = symbolTable.getInfo(node.ident)?.location?.fileName!!
                } else {
                    throw IRConversionException("Main function already defined in ${mainFileName}",
                            node.identLocation)
                }
            }
        }

        if (mainIdent == null) {
            throw ExceptionWithoutLocation("No main function found")
        }

        return mainIdent
    }

    /**
     * Infer the types for the internal representation, and infer types for every symbol in
     * in the symbol table.
     * 
     * @param nodes a list of IRNodes corresponding to the complete internal representation
     * @throws IRConversionException if there are type clashes or if types can not be inferred
     *         for every node in the IR and for every symbol in the symbol table
     */
    fun inferTypes(nodes: List<IRNode>) {
        val boundVars: MutableSet<TypeVariable> = hashSetOf()
        nodes.forEach { node -> typeChecker.typeCheck(node, boundVars, true) }

        typeChecker.inferFunctionTypes()
        typeChecker.inferVariableTypes()

        nodes.forEach(typeChecker::inferIRTypes)
    }

    /**
     * Assert that the IR has the correct physical structure (e.g. functions always return with
     * the correct type, control flow statements like break, continue, return only occur in 
     * valid locations). This must be called after type inference has taken place.
     */
    fun assertIRStructure(root: IRNode) {
        jumpsInAllowedPlaces(root, false, false)
        exhaustiveMatchCases(root)
        allFunctionsHaveReturn(root)
    }

    /**
     * Return whether all functions and lambdas in the entire tree have exhuastive return statements
     * (unless they have the unit return type, in which case returns are not required).
     */
    fun allFunctionsHaveReturn(root: IRNode) {
        root.map { node ->
            if (node is FunctionDefinitionNode) {
                // Check that all paths in a non-unit function return a value
                val funcType = symbolTable.getInfo(node.ident)?.type as FunctionType
                if (funcType.returnType != UnitType && !allPathsHaveReturn(node.body)) {
                    throw IRConversionException("Every branch of ${node.ident.name} must return " +
                            "a value", node.identLocation)
                }
            } else if (node is LambdaNode) {
                // Check that all paths in a non-unit lambda return a value
                val funcType = node.type as FunctionType
                if (funcType.returnType != UnitType && !allPathsHaveReturn(node.body)) {
                    throw IRConversionException("Every branch of lambda expression must return a " +
                            "value", node.startLocation)
                }
            }
        }
    }

    /**
     * Return whether all execution paths in the subtree rooted at an IR node have a return
     * statement.
     */
    fun allPathsHaveReturn(node: IRNode): Boolean {
        return when (node) {
            is ReturnNode -> true
            is IfNode -> allPathsHaveReturn(node.conseq) && 
                    node.altern != null &&
                    allPathsHaveReturn(node.altern)
            is MatchNode -> node.cases.map({ (_, _, stmt) -> allPathsHaveReturn(stmt) })
                                      .all({ x -> x })
            is BlockNode -> allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
            else -> false
        }
    }

    /**
     * Check whether all break, continue, and return statements in the subtree rooted at a node
     * occur in valid locations.
     *
     * @param node the root of the IR tree to check
     * @param allowReturn whether to allow a return statement in this subtree
     * @param allowBreakOrContinue whether to allow a break or continue statement in this subtree
     * @throws IRConversionException if a break, return, or continue occurs at an invalid location
     */
    fun jumpsInAllowedPlaces(node: IRNode, allowReturn: Boolean, allowBreakOrContinue: Boolean) {
        when (node) {
            is BlockNode -> node.nodes.map { n -> 
                jumpsInAllowedPlaces(n, allowReturn, allowBreakOrContinue )
            }
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
                if (node.update != null) {
                    jumpsInAllowedPlaces(node.update, allowReturn, false)
                }
                jumpsInAllowedPlaces(node.body, allowReturn, true)
            }
            is MatchNode -> {
                node.cases.forEach { (_, _, stmt) ->
                    jumpsInAllowedPlaces(stmt, allowReturn, allowBreakOrContinue)
                }
            }
            is FunctionDefinitionNode -> jumpsInAllowedPlaces(node.body, true, false)
            is LambdaNode -> jumpsInAllowedPlaces(node.body, true, false)
            is ReturnNode -> if (!allowReturn) {
                throw IRConversionException("Return must appear in function body",
                        node.returnLocation)
            }
            is BreakNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Break must appear in loop", node.breakLocation)
            }
            is ContinueNode -> if (!allowBreakOrContinue) {
                throw IRConversionException("Continue must appear in loop", node.continueLocation)
            }
        }
    }
}

