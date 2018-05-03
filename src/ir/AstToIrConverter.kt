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

        // Create traits, saving definition nodes in list of all IR nodes
        val irNodes: MutableList<IRNode> = mutableListOf()
        irNodes.addAll(packages.flatMap({ pack ->
            pack.traitDefs.flatMap(this::createTrait)
        }))

        // Create implementations for ADTs, saving definition nodes in list of all IR nodes
        irNodes.addAll(packages.flatMap({ pack ->
            pack.typeImpls.flatMap(this::createTypeImplementation)
        }))

        // Convert all statements of each package
        irNodes.addAll(packages.flatMap({ pack ->
            pack.statements.map({ convert(it, false) })
        }))

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
            return ConvertReplLineResult(listOf(), listOf())
        } else if (statement is TypeDefinitionStatement) {
            // Create ADT sig and variants
            createAdtSig(statement)
            createAdtVariants(statement)
            return ConvertReplLineResult(listOf(), listOf())
        } else if (statement is TraitDefinitionStatement) {
            // Create trait, then infer types and check structure of methods
            val irNodes = createTrait(statement)

            inferTypes(irNodes)
            irNodes.forEach(this::assertIRStructure)

            return ConvertReplLineResult(listOf(), irNodes)
        } else if (statement is TypeImplementationStatement) {
            // Create type implementation, then infer types and check structure of methods
            val irNodes = createTypeImplementation(statement)

            inferTypes(irNodes)
            irNodes.forEach(this::assertIRStructure)

            return ConvertReplLineResult(listOf(), irNodes)
        } else if (statement is Statement) {
            // Convert node, infer types, and verify correct IR structure
            val node = when (statement) {
                // Match statements on top level of REPL should be interpreted as expressions
                is MatchStatement -> convert(statement, true)
                else -> convert(statement, false)
            }

            inferTypes(listOf(node))
            assertIRStructure(node)

            return ConvertReplLineResult(listOf(node), listOf())
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
    fun convert(stmt: Statement, isExpr: Boolean): IRNode {
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
            stmt is RecordTypeConstructorExpression -> convertRecordTypeConstructor(stmt)
            stmt is KeyedAccessExpression -> convertKeyedAccess(stmt)
            stmt is AccessExpression -> convertAccess(stmt)
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
            stmt is GroupExpression -> convert(stmt.expr, true)
            stmt is BlockStatement -> convertBlock(stmt, isExpr)
            stmt is IfStatement -> convertIf(stmt, isExpr)
            stmt is WhileStatement -> convertWhile(stmt)
            stmt is DoWhileStatement -> convertDoWhile(stmt)
            stmt is ForStatement -> convertFor(stmt)
            stmt is MatchStatement -> convertMatch(stmt, isExpr)
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

        return LambdaNode(formalArgs, convert(expr.body, false), expr.startLocation)
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

        return FunctionDefinitionNode(stmt.ident, formalArgs, convert(stmt.body, false),
                stmt.identLocation, stmt.startLocation)
    }

    fun convertAbstractFunctionDefinition(stmt: AbstractFunctionDefinitionStatement) {
        val argTypes = stmt.formalArgs.map { (arg, typeAnnotation) ->
            val type = convertType(typeAnnotation)
            symbolTable.getInfo(arg)?.type = type
            type
        }

        // Annotate function identifier with function type from annotations
        val returnType = convertType(stmt.returnTypeAnnotation)
        symbolTable.getInfo(stmt.ident)?.type = FunctionType(argTypes, returnType)
    }

    fun convertVariableDefinition(stmt: VariableDefinitionStatement): IRNode {
        val pattern = convertLValuePattern(stmt.lValue, true)

        // Annotate this identifier with type annotation, or new type variable if not annotated
        val type = if (stmt.typeAnnotation != null) {
            convertType(stmt.typeAnnotation)
        } else {
            TypeVariable()
        }

        // If this is a simple variable assignment, create variable definition node
        if (pattern is VariableNode) {
            symbolTable.getInfo(pattern.ident)?.type = type
            return VariableDefinitionNode(pattern.ident, convert(stmt.expr, true),
                    pattern.startLocation, stmt.startLocation)
        // Otherwise this is a pattern deconstruction variable definition
        } else {
            return PatternDefinitionNode(pattern, convert(stmt.expr, true), type,
                    pattern.startLocation, stmt.startLocation)
        }
    }

    fun convertBlock(stmt: BlockStatement, isExpr: Boolean): BlockNode {
        if (!isExpr || stmt.stmts.isEmpty()) {
            // If block is a statement, all its children are statements
            val statements = stmt.stmts.map({ convert(it, false ) })
            return BlockNode(statements, isExpr, stmt.startLocation)
        } else {
            // If block is a nonempty expression, the last statement must be an expression
            val firstStmts = stmt.stmts.take(stmt.stmts.size - 1)
            val lastStmt = stmt.stmts[stmt.stmts.size - 1]
            val statements = firstStmts.map({ convert(it, false) }) + convert(lastStmt, true)

            return BlockNode(statements, true, stmt.startLocation)
        }
    }

    fun convertIf(stmt: IfStatement, isExpr: Boolean): IfNode {
        val cond = convert(stmt.cond, true)
        val conseq = convert(stmt.conseq, isExpr)
        val altern = if (stmt.altern != null) convert(stmt.altern, isExpr) else null

        return IfNode(cond, conseq, altern, isExpr, stmt.startLocation)
    }

    fun convertWhile(stmt: WhileStatement): WhileNode {
        return WhileNode(convert(stmt.cond, true), convert(stmt.body, false), stmt.startLocation)
    }

    fun convertDoWhile(stmt: DoWhileStatement): DoWhileNode {
        return DoWhileNode(convert(stmt.cond, true), convert(stmt.body, false), stmt.startLocation)
    }

    fun convertFor(stmt: ForStatement): ForNode {
        val init = if (stmt.init != null) convert(stmt.init, false) else null
        val cond = if (stmt.cond != null) convert(stmt.cond, true) else null
        val update = if (stmt.update != null) convert(stmt.update, false) else null

        return ForNode(init, cond, update, convert(stmt.body, false), stmt.startLocation)
    }

    fun convertMatch(stmt: MatchStatement, isExpr: Boolean): MatchNode {
        val expr = convert(stmt.expr, true)
        val cases = stmt.cases.map { (p, g, s) ->
            val pattern = convert(p, true)

            val guard = if (g != null) {
                convert(g, true)
            } else {
                null
            }

            val case = convert(s, isExpr)

            Triple(pattern, guard, case)
        }

        return MatchNode(expr, cases, isExpr, stmt.startLocation)
    }

    fun convertVariable(expr: VariableExpression): IRNode {
        val ident = expr.ident.resolve()

        // If this identifier is for a type constructor, create a type constructor node
        val info = symbolTable.getInfo(ident)!!
        if (info.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            val adtVariant = info.adtVariant
            if (adtVariant is TupleVariant) {
                return TupleTypeConstructorNode(adtVariant, listOf(), expr.identLocation)
            } else {
                throw IRConversionException("No arguments given to record type constructor",
                        expr.identLocation)
            }
        }

        return VariableNode(ident, expr.identLocation)
    }

    fun convertApplication(expr: ApplicationExpression): IRNode {
        val func = convert(expr.func, true)
        val args = expr.args.map({ convert(it, true) })

        if (func is TupleTypeConstructorNode) {
            return TupleTypeConstructorNode(func.adtVariant, args, func.startLocation)
        } else {
            return FunctionCallNode(func, args, expr.callLocation, expr.startLocation)
        }
    }

    fun convertRecordTypeConstructor(
        expr: RecordTypeConstructorExpression
    ): RecordTypeConstructorNode {
        // Resolve record type constructor
        if (expr.typeConstructor !is VariableExpression) {
            throw IRConversionException("Can only create record with record type constructor",
                    expr.callLocation)
        }

        val ident = expr.typeConstructor.ident.resolve()
        val info = symbolTable.getInfo(ident)!!

        // Make sure that the ident corresponds to record type constructor
        if (info.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            throw IRConversionException("Can only create record with record type constructor",
                    expr.callLocation)
        }

        val adtVariant = info.adtVariant
        if (adtVariant !is RecordVariant) {
            throw IRConversionException("Expected record type constructor instead of tuple " +
                    "type constructor", expr.callLocation)
        }

        val fields = expr.fields.mapValues { (_, field) -> convert(field, true) }

        // Check for fields that were not defined in the record type
        val fieldsNotInType = expr.fields.keys - adtVariant.fields.keys
        if (!fieldsNotInType.isEmpty()) {
            if (fieldsNotInType.size > 1) {
                throw IRConversionException("Record type ${ident.name} does not have fields with " +
                        "names ${fieldsNotInType}", expr.startLocation)
            } else {
                throw IRConversionException("Record type ${ident.name} does not have field with " +
                        "name ${fieldsNotInType}", expr.startLocation)
            }
        }

        // Patterns do not have to define all fields, but expressions need to define all fields
        if (!expr.isPattern) {
            val fieldsNotSpecified = adtVariant.fields.keys - expr.fields.keys
            if (!fieldsNotSpecified.isEmpty()) {
                if (fieldsNotSpecified.size > 1) {
                    throw IRConversionException("Missing fields ${fieldsNotSpecified} for " +
                            "record type ${ident.name}", expr.startLocation)
                } else {
                    throw IRConversionException("Missing field ${fieldsNotSpecified} for " +
                            "record type ${ident.name}", expr.startLocation)
                }
            }
        }

        return RecordTypeConstructorNode(adtVariant, fields, expr.startLocation)
    }

    fun convertKeyedAccess(expr: KeyedAccessExpression): KeyedAccessNode {
        return KeyedAccessNode(convert(expr.container, true),
                convert(expr.key, true), expr.accessLocation)
    }

    fun convertAccess(expr: AccessExpression): AccessNode {
        return AccessNode(convert(expr.expr, true), expr.field, expr.accessLocation)
    }

    fun convertAssignment(expr: AssignmentExpression): IRNode {
        // If the lValue is a keyed access, then this is a keyed assignment
        if (expr.lValue is KeyedAccessExpression) {
            val lValue = convertKeyedAccess(expr.lValue)
            return KeyedAssignmentNode(lValue.container, lValue.key,
                    convert(expr.rValue, true), lValue.accessLocation)
        // Else this must be a deconstruction assignment (which encompasses variable assignment)
        } else {
            val pattern = convertLValuePattern(expr.lValue, false)

            // If this is a simple variable assignment, create node
            if (pattern is VariableNode) {
                return VariableAssignmentNode(pattern.ident, convert(expr.rValue, true),
                        pattern.startLocation)
            // Otherwise this is a deconstruction assignment
            } else {
                return PatternAssignmentNode(pattern, convert(expr.rValue, true),
                        pattern.startLocation)
            }
        }
    }

    fun convertLValuePattern(expr: Expression, inDef: Boolean): IRNode {
        when (expr) {
            is VariableExpression -> {
                val ident = expr.ident.resolve()

                // If in a variable definition, need to annotate ident with new type variable,
                // since every ident needs a type annotated when it is defined.
                if (inDef) {
                    symbolTable.getInfo(ident)?.type = TypeVariable()
                }

                // Error if assigning to identifier that is not a variable
                if (symbolTable.getInfo(ident)?.idClass != IdentifierClass.VARIABLE) {
                    throw IRConversionException("Can only reassign variables", expr.identLocation)
                }

                // Error if assigning to immutable variable outside of its definition
                if (!inDef && isImmutable(ident)) {
                    throw IRConversionException("Cannot reassign immutable variable " +
                            "${ident.name}", expr.identLocation)
                }

                return VariableNode(ident, expr.identLocation)
            }
            // For tuples, simply convert tuple elements
            is TupleLiteralExpression -> {
                return TupleLiteralNode(expr.elements.map({ convertLValuePattern(it, inDef) }),
                        expr.startLocation)
            }
            // Tuple type constructors appear as application expressions
            is ApplicationExpression -> {
                // The only allowable applications are for type constructors
                val typeConstructor = expr.func
                if (typeConstructor !is VariableExpression) {
                    throw IRConversionException("Pattern assignments must consist solely of " +
                            "variables, tuples, and type constructors", expr.func.startLocation)  
                }

                val ident = typeConstructor.ident.resolve()
                val info = symbolTable.getInfo(ident)!!
                if (info.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
                    throw IRConversionException("Pattern assignments must consist solely of " +
                            "variables, tuples, and type constructors", expr.func.startLocation)   
                }

                val adtVariant = info.adtVariant
                val args = expr.args.map({ convertLValuePattern(it, inDef )})
                return TupleTypeConstructorNode(adtVariant as TupleVariant, args,
                            expr.startLocation)
            }
            // Process record type constructors as normal for a pattern
            is RecordTypeConstructorExpression -> {
                // Resolve record type constructor
                if (expr.typeConstructor !is VariableExpression) {
                    throw IRConversionException("Can only create record with record type " +
                            "constructor", expr.typeConstructor.startLocation)
                }

                val ident = expr.typeConstructor.ident.resolve()
                val info = symbolTable.getInfo(ident)!!

                // Make sure that the ident corresponds to record type constructor
                if (info.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
                    throw IRConversionException("Can only create record with record type " +
                            "constructor", expr.typeConstructor.startLocation)
                }

                val adtVariant = info.adtVariant
                if (adtVariant !is RecordVariant) {
                    throw IRConversionException("Expected record type constructor instead of tuple " +
                            "type constructor", expr.callLocation)
                }

                val fields = expr.fields.mapValues { (_, field) ->
                    convertLValuePattern(field, true)
                }

                // Check for fields that were not defined in the record type
                val fieldsNotInType = expr.fields.keys - adtVariant.fields.keys
                if (!fieldsNotInType.isEmpty()) {
                    if (fieldsNotInType.size > 1) {
                        throw IRConversionException("Record type ${ident.name} does not have " +
                                "fields with names ${fieldsNotInType}", expr.startLocation)
                    } else {
                        throw IRConversionException("Record type ${ident.name} does not have " +
                                "field with name ${fieldsNotInType}", expr.startLocation)
                    }
                }

                return RecordTypeConstructorNode(adtVariant, fields, expr.startLocation)
            }
            else -> throw IRConversionException("Pattern assignments must consist solely of " +
                    "variables, tuples, and type constructors", expr.startLocation) 
        }
    }

    fun convertReturn(expr: ReturnStatement): ReturnNode {
        val returnVal = if (expr.expr != null) convert(expr.expr, true) else null 
        return ReturnNode(returnVal, expr.returnLocation)
    }

    fun convertVectorLiteral(expr: VectorLiteralExpression): VectorLiteralNode {
        return VectorLiteralNode(expr.elements.map({ convert(it, true) }), expr.startLocation)
    }

    fun convertSetLiteral(expr: SetLiteralExpression): SetLiteralNode {
        return SetLiteralNode(expr.elements.map({ convert(it, true) }), expr.startLocation)
    }

    fun convertMapLiteral(expr: MapLiteralExpression): MapLiteralNode {
        return MapLiteralNode(expr.keys.map({ convert(it, true) }),
                expr.values.map({ convert(it, true) }), expr.startLocation)
    }

    fun convertTupleLiteral(expr: TupleLiteralExpression): TupleLiteralNode {
        return TupleLiteralNode(expr.elements.map({ convert(it, true) }), expr.startLocation)
    }

    fun convertEquality(expr: EqualityExpression): EqualityNode {
        val left = convert(expr.left, true)
        val right = convert(expr.right, true)

        return when (expr) {
            is EqualsExpression -> EqualsNode(left, right)
            is NotEqualsExpression -> NotEqualsNode(left, right)
        }
    }

    fun convertComparison(expr: ComparisonExpression): ComparisonNode {
        val leftNode = convert(expr.left, true)
        val rightNode = convert(expr.right, true)

        return when (expr) {
            is LessThanExpression -> LessThanNode(leftNode, rightNode)
            is LessThanOrEqualExpression -> LessThanOrEqualNode(leftNode, rightNode)
            is GreaterThanExpression -> GreaterThanNode(leftNode, rightNode)
            is GreaterThanOrEqualExpression -> GreaterThanOrEqualNode(leftNode, rightNode)
        }
    }

    fun convertLogicalAnd(expr: LogicalAndExpression): LogicalAndNode {
        return LogicalAndNode(convert(expr.left, true), convert(expr.right, true))
    }

    fun convertLogicalOr(expr: LogicalOrExpression): LogicalOrNode {
        return LogicalOrNode(convert(expr.left, true), convert(expr.right, true))
    }

    fun convertLogicalNot(expr: LogicalNotExpression): LogicalNotNode {
        return LogicalNotNode(convert(expr.expr, true), expr.startLocation)
    }

    fun convertBinaryMathOperator(expr: BinaryMathOperatorExpression): BinaryMathOperatorNode {
        val leftNode = convert(expr.left, true)
        val rightNode = convert(expr.right, true)

        return when (expr) {
            is AddExpression -> AddNode(leftNode, rightNode)
            is SubtractExpression -> SubtractNode(leftNode, rightNode)
            is MultiplyExpression -> MultiplyNode(leftNode, rightNode)
            is DivideExpression -> DivideNode(leftNode, rightNode)
            is ExponentExpression -> ExponentNode(leftNode, rightNode)
        }
    }

    fun convertUnaryPlus(expr: UnaryPlusExpression): IRNode {
        val child = convert(expr.expr, true)

        // If child is an int or float literal, can simply return the literals
        if (child is IntLiteralNode) {
            return IntLiteralNode(child.num, expr.startLocation)
        } else if (child is FloatLiteralNode) {
            return FloatLiteralNode(child.num, expr.startLocation)
        } else {
            return IdentityNode(child, expr.startLocation)
        }
    }

    fun convertUnaryMinus(expr: UnaryMinusExpression): IRNode {
        val child = convert(expr.expr, true)

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

    fun createAdtSig(typeDef: TypeDefinitionStatement) {
        // First annotate all type parameters identifiers with new type variables
        val typeParams = typeDef.typeParamIdents.map { typeParamIdent ->
            val typeParam = TypeParameter()
            symbolTable.getInfo(typeParamIdent)?.type = typeParam
            typeParam
        }

        // Create initial ADT signature and annotate identifier with it
        val adtSig = AlgebraicDataTypeSignature(typeDef.typeIdent.name, typeParams)
        symbolTable.getInfo(typeDef.typeIdent)?.adtSig = adtSig
        symbolTable.getInfo(typeDef.typeIdent)?.type = adtSig.getAdtWithParams(adtSig.typeParams)
    }

    fun createAdtVariants(typeDef: TypeDefinitionStatement) {
        val adtSig = symbolTable.getInfo(typeDef.typeIdent)?.adtSig!!

        // Convert all tuple variants in type definition
        for ((variantIdent, typeConstructorExprs) in typeDef.tupleVariants) {
            // Create variant by parsing type constructor and add to ADT sig
            val typeConstructor = typeConstructorExprs.map(this::convertType)
            val variant = TupleVariant(adtSig, variantIdent.name, typeConstructor)
            adtSig.variants.add(variant)

            // Annotate variant's identifier with newly created variant
            symbolTable.getInfo(variantIdent)?.adtVariant = variant
        }

        // Convert all record variants in type definition
        for ((variantIdent, fieldExprs) in typeDef.recordVariants) {
            // Create variant by parsing field types and add to ADT sig
            val fields = fieldExprs.mapValues { (_, typeExpr) -> convertType(typeExpr) }
            val variant = RecordVariant(adtSig, variantIdent.name, fields)
            adtSig.variants.add(variant)

            // Annotate variant's identifier with newly created variant
            symbolTable.getInfo(variantIdent)?.adtVariant = variant
        }
    }

    fun createTrait(traitDef: TraitDefinitionStatement): List<IRNode> {
        // First annotate all type parameters identifiers with new type variables
        val typeParams = traitDef.typeParamIdents.map { typeParamIdent ->
            val typeParam = TypeParameter()
            symbolTable.getInfo(typeParamIdent)?.type = typeParam
            typeParam
        }

        // Create initial trait signature and type, and annotate identifier with it
        val traitIdent = traitDef.traitIdent
        val traitSig = TraitSignature(traitDef.traitIdent.name, typeParams)
        val traitType = traitSig.getTraitWithParams(typeParams)
        symbolTable.getInfo(traitIdent)?.traitSig = traitSig
        symbolTable.getInfo(traitIdent)?.type = traitType

        // Annotate this with trait type
        val thisInfo = symbolTable.getInfo(traitDef.thisIdent)!!
        thisInfo.type = traitType

        // Check that every method has a unique name
        val names: MutableSet<String> = mutableSetOf()
        for (abstractDef in traitDef.abstractMethods) {
            if (names.contains(abstractDef.ident.name)) {
                throw IRConversionException("Method with name ${abstractDef.ident.name} " +
                        "already defined for type ${traitIdent.name}", abstractDef.identLocation)
            } else {
                names.add(abstractDef.ident.name)
            }
        }

        for (concreteDef in traitDef.concreteMethods) {
            if (names.contains(concreteDef.ident.name)) {
                throw IRConversionException("Method with name ${concreteDef.ident.name} " +
                        "already defined for type ${traitIdent.name}", concreteDef.identLocation)
            } else {
                names.add(concreteDef.ident.name)
            }
        }

        // Convert abstract method definitions and add to trait
        traitDef.abstractMethods.forEach { abstractDef ->
            convertAbstractFunctionDefinition(abstractDef)
            traitSig.abstractMethods[abstractDef.ident.name] = abstractDef.ident
        }

        // Convert concrete method definitions and add to trait
        val concreteNodes = traitDef.concreteMethods.map { concreteDef ->
            val funcDef = convertFunctionDefinition(concreteDef)
            traitSig.concreteMethods[funcDef.ident.name] = funcDef.ident
            MethodDefinitionNode(funcDef.ident, funcDef.formalArgs, funcDef.body,
                    traitDef.thisIdent, null, funcDef.identLocation, funcDef.startLocation)
        }

        return concreteNodes
    }

    fun createTypeImplementation(typeImpl: TypeImplementationStatement): List<IRNode> {
        val typeIdent = typeImpl.typeIdent.resolve()
        val typeInfo = symbolTable.getInfo(typeIdent)!!
        val thisInfo = symbolTable.getInfo(typeImpl.thisIdent)!!

        if (typeInfo.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE) {
            throw IRConversionException("Can only define methods for user defined data types",
                    thisInfo.location)
        }

        // Make sure number of type parameters is correct
        val adtSig = typeInfo.adtSig
        if (adtSig.typeParams.size != typeImpl.typeParamIdents.size) {
            throw IRConversionException("Type ${adtSig.name} expects " +
                    "${adtSig.typeParams.size} type parameters, but received " +
                    "${typeImpl.typeParamIdents.size}", thisInfo.location)
        }

        // Annotate type parameter identifiers with new type variables
        typeImpl.typeParamIdents.zip(adtSig.typeParams).forEach { (typeParamIdent, adtTypeParam) ->
            symbolTable.getInfo(typeParamIdent)?.type = adtTypeParam
        }

        // Annotate "this" with correctly parameterized type
        thisInfo.type = adtSig.getAdtWithParams(adtSig.typeParams)

        // Check that every method and field has a unique name
        val names = adtSig.getAllNames()
        val abstractNames: MutableSet<String> = mutableSetOf()

        for (methodDef in typeImpl.methods) {
            if (names.contains(methodDef.ident.name)) {
                throw IRConversionException("Field or method with name ${methodDef.ident.name} " +
                        "already defined for type ${typeIdent.name}", methodDef.identLocation)
            } else {
                names.add(methodDef.ident.name)
            }
        }

        for ((traitSymbol, _) in typeImpl.extendedTraits) {
            val traitIdent = traitSymbol.resolve()
            val traitSig = symbolTable.getInfo(traitIdent)?.traitSig!!

            // Make sure that concrete methods defined on traits do not conflict with names in type
            for ((concreteName, _) in traitSig.concreteMethods) {
                if (names.contains(concreteName)) {
                    throw IRConversionException("Field or method with name ${concreteName} " +
                            "already defined for type ${typeIdent.name}, conflicting with method " +
                            "of same name from trait ${traitIdent.name}", thisInfo.location) 
                } else {
                    names.add(concreteName)
                }
            }

            for ((abstractName, _) in traitSig.abstractMethods) {
                // Make sure that no two abstract methods have the same name
                if (abstractNames.contains(abstractName)) {
                    throw IRConversionException("${typeIdent.name} cannot implement two abstract " +
                            "methods with the same name: ${abstractName}", thisInfo.location)
                }

                // Make sure that all abstract methods defined in trait are implemented by type
                if (!names.contains(abstractName)) {
                    throw IRConversionException("Type ${typeIdent.name} cannot implement trait " +
                            "${traitIdent.name}, since method ${abstractName} is " +
                            "not implemented", thisInfo.location)
                }
            }
        }

        val abstractTypes: MutableMap<String, Type> = mutableMapOf()

        // Add implemented traits to adt's signature
        for ((traitSymbol, typeParamExprs) in typeImpl.extendedTraits) {
            val traitIdent = traitSymbol.resolve()
            val traitSig = symbolTable.getInfo(traitIdent)?.traitSig!!

            val typeParams = typeParamExprs.map(this::convertType)
            val traitType = traitSig.getTraitWithParams(typeParams)

            adtSig.traits.add(traitType)

            // Generate method types with correct paramaters for this type implementation
            for ((abstractName, abstractIdent) in traitSig.abstractMethods) {
                val substMap = traitSig.typeParams.zip(typeParams).toMap()
                val methodType = symbolTable.getInfo(abstractIdent)?.type?.substitute(substMap)!!
                abstractTypes[abstractName] = methodType
            }
        }

        // Convert function definitions to method definitions, then add methods to type
        val funcDefs = typeImpl.methods.map(this::convertFunctionDefinition)
        val methodDefs = funcDefs.map { funcDef ->
            adtSig.methods[funcDef.ident.name] = funcDef.ident
            MethodDefinitionNode(funcDef.ident, funcDef.formalArgs, funcDef.body,
                    typeImpl.thisIdent, abstractTypes[funcDef.ident.name],
                    funcDef.identLocation, funcDef.startLocation)
        }

        return methodDefs
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
        } else if (info.idClass == IdentifierClass.TRAIT) {
            val typeParams = typeExpr.typeParams.map(this::convertType)

            // If this is a trait, error if number of parameters differs
            if (info.traitSig.typeParams.size != typeParams.size) {
                throw IRConversionException("Trait ${info.traitSig.name} expects " +
                        "${info.traitSig.typeParams.size} type parameters, but received " +
                        "${typeParams.size}", typeExpr.startLocation)
            }

            return info.traitSig.getTraitWithParams(typeParams)
        } else if (info.idClass == IdentifierClass.TYPE_PARAMETER) {
            if (!typeExpr.typeParams.isEmpty()) {
                // Error if this is not an ADT but was given type parameters
                throw IRConversionException("Type ${ident.name} does not take any type parameters",
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
            is BlockNode -> {
                allPathsHaveReturn(node.nodes.get(node.nodes.lastIndex))
            }
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

