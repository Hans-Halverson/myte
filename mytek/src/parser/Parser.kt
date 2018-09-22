package myte.parser

import myte.lexer.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

class Parser(
    var symbolTable: SymbolTable
) {
    // The tokenizer that is currently in use
    var tokenizer: Tokenizer = Tokenizer(listOf())

    // The root of the package node tree
    val rootPackageNode: PackageTreeNode = PackageTreeNode("", null)

    // The import context that is currently in use
    var importContext: ImportContext = ImportContext(rootPackageNode)

    // Whether to ignore ambiguous line endings, or whether to throw an AmbiguousEndException
    var ignoreAmbiguousEnd: Boolean = true

    /**
     * Reset the stream of tokens that are being parsed to a new stream.
     */
    fun setTokens(newTokens: List<Token>) {
        tokenizer = Tokenizer(newTokens)
    }

    /**
     * Parse a list of files, represented as a list of lists of tokens.
     */
    fun parseFiles(allTokens: List<List<Token>>): ParseFilesResult {
        val allPackages: MutableSet<Package> = mutableSetOf()
        val allImportContexts: MutableList<ImportContext> = mutableListOf()

        for (fileTokens in allTokens) {
            setTokens(fileTokens)

            val (pack, importContext) = parseFile()
            allPackages.add(pack)
            allImportContexts.add(importContext)
        }

        return ParseFilesResult(allPackages.toList(), allImportContexts)
    }

    /**
     * Parse an entire file, represented as a list of tokens.
     *
     * @return a list of statements corresponding to the top level statements in the file
     * @throws ParseException if the stream of tokens does not represent a valid list of statements
     */
    fun parseFile(): Pair<Package, ImportContext> {
        val pack = parsePackageDeclaration()
        importContext = parseImportStatements(ImportContext(rootPackageNode))

        symbolTable.registerPackageScope(pack)

        while (!tokenizer.reachedEnd) {
            parseTopLevelStatement(pack)
        }

        return Pair(pack, importContext)
    }

    /**
     * Parse the current top level statement, and return the statement if a statement can be
     * created, or null if no statement needs to be created.
     */
    fun parseTopLevelStatement(pack: Package) {
        val token = tokenizer.next()

        when (token) {
            is TypeToken -> pack.typeDefs.add(parseTypeDefinition())
            is ImplementToken -> pack.typeImpls.add(parseTypeImplementation())
            is TraitToken -> pack.traitDefs.add(parseTraitDefinition())
            is DefToken -> pack.statements.add(parseFunctionDefinition(true, false, token))
            is LetToken -> pack.statements.add(parseVariableDefinition(false, true, token))
            is ConstToken -> pack.statements.add(parseVariableDefinition(true, true, token))
            else -> throw ParseException("Top level statements can only be type, function, or " +
                    "variable definitions", token)
        }
    }

    fun initRepl() {
        // Create a package with an empty name for the REPL to run inside. Import context has
        // already been set up on parser initialization.
        val replPackage = Package("", Scope(null, ScopeType.REPL))
        rootPackageNode.children[replPackage.name] = replPackage.packageTreeNode
        symbolTable.registerPackageScope(replPackage)
    }

    /**
     * Reset a portion of the parser's state for the next REPL line.
     */
    fun resetForReplLine(
        symbolTable: SymbolTable,
        importContext: ImportContext,
        ignoreAmbiguousEnd: Boolean
    ) {
        this.symbolTable = symbolTable
        this.importContext = importContext
        this.ignoreAmbiguousEnd = ignoreAmbiguousEnd
    }

    /**
     * Parse a single command from the REPL.
     *
     * @return the single statement parsed from the input stream, or null if no statement is created
     * @throws ParseException if the stream does not correspond to a valid statement, or if there
     *         are leftover tokens after the statement has been parsed
     */
    fun parseReplLine(tokens: List<Token>): ParseReplLineResult {
        setTokens(tokens)
        symbolTable.returnToPackageScope()

        val token = tokenizer.next()
        val statement = when (token) {
            // If it is an import statement, parse imports and add to import context. This does
            // not produce a top level statement.
            is ImportToken -> {
                parseImportStatements(importContext, true)
                null
            }
            // Otherwise parse type definition, implementation, or statement
            is TypeToken -> parseTypeDefinition()
            is ImplementToken -> parseTypeImplementation()
            is TraitToken -> parseTraitDefinition()
            // Function and variable definitions should be treated as if they are top level
            is LetToken -> parseVariableDefinition(false, true, token)
            is ConstToken -> parseVariableDefinition(true, true, token)
            is DefToken -> parseFunctionDefinition(true, false, token)
            else -> parseStatement(token)
        }

        if (!tokenizer.reachedEnd) {
            throw ParseException(tokenizer.current)
        }

        return ParseReplLineResult(statement, importContext)
    }

    /**
     * Parse the current statement in the stream of tokens.
     */
    fun parseStatement(): Statement = parseStatement(tokenizer.next())

    /** 
     * Parse the current statement in the stream of tokens.
     * 
     * @param token the first token of the current statement
     */
    fun parseStatement(token: Token): Statement {
        return when (token) {
            is LetToken -> parseVariableDefinition(false, false, token)
            is ConstToken -> parseVariableDefinition(true, false, token)
            is DefToken -> parseFunctionDefinition(false, false, token)
            is WhileToken -> parseWhileStatement(token)
            is DoToken -> parseDoWhileStatement(token)
            is ForToken -> parseForStatement(token)
            is ForEachToken -> parseForEachStatement(token)
            is ReturnToken -> parseReturnStatement(token)
            is BreakToken -> BreakStatement(token.location)
            is ContinueToken -> ContinueStatement(token.location)
            // If no statement was found yet, the current statement must be an expression
            else -> parseExpression(token)
        }
    }

    /**
     * Parse the current expression in the stream.
     * 
     * @param precedence the optional precedence level that governs the current context, if none
     *        is supplied the lowest precedence level is assumed.
     */
    fun parseExpression(precedence: Int = EXPR_NO_PRECEDENCE): Expression {
        return parseExpression(tokenizer.next(), precedence)
    }

    /**
     * Parse the current expression in the stream.
     * 
     * @param firstToken the first token of the current expression
     * @param precedence the optional precedence level that governs the current context, if none
     *        is supplied the lowest precedence level is assumed.
     */
    fun parseExpression(firstToken: Token, precedence: Int = EXPR_NO_PRECEDENCE): Expression {
        // Match on all tokens that signal a prefix operator
        var currentExpr = when (firstToken) {
            // Literals
            is IntegralLiteralToken -> IntegralLiteralExpression(firstToken.num, firstToken.location)
            is DecimalLiteralToken -> DecimalLiteralExpression(firstToken.num, firstToken.location)
            is IdentifierToken -> parseVariableExpression(firstToken)
            is TrueToken -> BoolLiteralExpression(true, firstToken.location)
            is FalseToken -> BoolLiteralExpression(false, firstToken.location)
            is StringLiteralToken -> StringLiteralExpression(firstToken.str, firstToken.location)
            is LeftBracketToken -> parseVectorLiteralExpression(false, firstToken)
            is LeftSetLiteralToken -> parseSetLiteralExpression(false, firstToken)
            is LeftMapLiteralToken -> parseMapLiteralExpression(false, firstToken)
            is FunToken -> parseLambdaExpression(firstToken)
            // Prefixs operators
            is PlusToken -> parseUnaryPlusExpression(firstToken)
            is MinusToken -> parseUnaryMinusExpression(firstToken)
            is LogicalNotToken -> parseLogicalNotExpression(firstToken)
            is LeftParenToken -> parseParenthesizedExpression(false, firstToken)
            // Statements that can be expressions
            is LeftBraceToken -> parseBlock(firstToken)
            is IfToken -> parseIfStatement(firstToken)
            is MatchToken -> parseMatchStatement(firstToken)
            // Builtin calls must have a specific structure
            is BuiltinToken -> parseBuiltin(firstToken)
            // If the expression does not begin with a literal or prefix operator, it is not a
            // valid expression.
            else -> throw ParseException(firstToken)
        }

        // Keep parsing operators that have a higher precedence than the current precedence context,
        // as these will be bound more tightly and therefore should be children of the current,
        // lower precedence expression.
        while (!tokenizer.reachedEnd &&
                precedence < getExprPrecedenceForInfixToken(tokenizer.current.type)) {
            // Match on all tokens that signal an infix operator
            val token = tokenizer.next()
            currentExpr = when (token) {
                // Math operators
                is PlusToken -> parseAddExpression(currentExpr)
                is MinusToken -> parseSubtractExpression(currentExpr)
                is AsteriskToken -> parseMultiplyExpression(currentExpr)
                is ForwardSlashToken -> parseDivideExpression(currentExpr)
                is CaretToken -> parseExponentExpression(currentExpr)
                is PercentToken -> parseRemainderExpression(currentExpr)
                // Comparison operators
                is EqualsToken -> parseAssignmentExpression(currentExpr, token)
                is DoubleEqualsToken -> parseEqualsExpression(currentExpr)
                is NotEqualsToken -> parseNotEqualsExpression(currentExpr)
                is LessThanToken -> parseLessThanExpression(currentExpr)
                is LessThanOrEqualToken -> parseLessThanOrEqualExpression(currentExpr)
                is GreaterThanToken -> parseGreaterThanExpression(currentExpr)
                is GreaterThanOrEqualToken -> parseGreaterThanOrEqualExpression(currentExpr)
                // Logical operators
                is LogicalAndToken -> parseLogicalAndExpression(currentExpr)
                is LogicalOrToken -> parseLogicalOrExpression(currentExpr)
                // Function call or index
                is LeftParenToken -> parseApplicationExpression(currentExpr, token)
                is LeftBracketToken -> parseIndexExpression(currentExpr, token)
                is LeftBraceToken -> parseRecordTypeConstructorExpression(currentExpr, token)
                is PeriodToken -> parseAccessExpression(currentExpr, token)
                else -> throw ParseException(token)
            }
        }

        return currentExpr
    }

    /**
     * Assert that the current token has a certain type.
     * @throws ParseException if the current token does not have the specified type
     */
    fun assertCurrent(tokenType: TokenType) {
        if (tokenizer.current.type != tokenType) {
            throw ParseException(tokenType, tokenizer.current.type, tokenizer.current)
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Parsing functions for statements and expressions
    //
    ///////////////////////////////////////////////////////////////////////////

    fun parseVariableExpression(token: IdentifierToken): Expression {
        // Parse "::" separated list of strings to find scope and identifier name
        val identParts = mutableListOf(token.str)

        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()

            val currentToken = tokenizer.current
            if (currentToken !is IdentifierToken) {
                throw ParseException("Identifier must follow scope", currentToken)
            }

            identParts.add(currentToken.str)
            tokenizer.next()
        }

        // All parts except the last are part of the scope. Add resolve job to symbol table.
        val name = identParts[identParts.size - 1]
        val scopes = identParts.take(identParts.size - 1)
        val ident = symbolTable.lookupVariable(name, scopes, token.location, importContext)

        return VariableExpression(ident, token.location)
    }

    fun parseVectorLiteralExpression(
        isPattern: Boolean,
        leftBracketToken: LeftBracketToken
    ): VectorLiteralExpression {
        // If parseVectorLiteralExpression is called, the previous token must have been a [
        val elements: MutableList<Expression> = mutableListOf()

        // Return empty list if no vector elements are encountered
        if (tokenizer.current is RightBracketToken) {
            tokenizer.next()
            return VectorLiteralExpression(listOf(), leftBracketToken.location)
        }

        // Add expressions, separated by commas, until a right bracket is encountered.
        // If in pattern, only parse valid patterns with a call to parsePattern
        elements.add(parsePatternOrExpr(isPattern))

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            elements.add(parsePatternOrExpr(isPattern))
        }

        assertCurrent(TokenType.RIGHT_BRACKET)
        tokenizer.next()

        return VectorLiteralExpression(elements, leftBracketToken.location)
    }

    fun parseSetLiteralExpression(
        isPattern: Boolean,
        leftSetLiteralToken: LeftSetLiteralToken
    ): SetLiteralExpression {
        // If parseSetLiteralExpression is called, the previous token must have been a {|
        val elements: MutableList<Expression> = mutableListOf()

        // Return empty list if no set elements are encountered
        if (tokenizer.current is RightSetLiteralToken) {
            tokenizer.next()
            return SetLiteralExpression(listOf(), leftSetLiteralToken.location)
        }

        // Add expressions, separated by commas, until the set literal is closed.
        // If in pattern, only parse valid patterns with a call to parsePattern
        elements.add(parsePatternOrExpr(isPattern))

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            elements.add(parsePatternOrExpr(isPattern))
        }

        assertCurrent(TokenType.RIGHT_SET_LITERAL)
        tokenizer.next()

        return SetLiteralExpression(elements, leftSetLiteralToken.location)
    }

    fun parseMapLiteralExpression(
        isPattern: Boolean,
        leftMapLiteralToken: LeftMapLiteralToken
    ): MapLiteralExpression {
        // If parseSetLiteralExpression is called, the previous token must have been a [|
        val keys: MutableList<Expression> = mutableListOf()
        val values: MutableList<Expression> = mutableListOf()

        // Return empty list if no map elements are encountered
        if (tokenizer.current is RightMapLiteralToken) {
            tokenizer.next()
            return MapLiteralExpression(listOf(), listOf(), leftMapLiteralToken.location)
        }

        // Add first key -> value pair to running lists of keys and values
        keys.add(parsePatternOrExpr(isPattern))

        assertCurrent(TokenType.ARROW)
        tokenizer.next()

        values.add(parsePatternOrExpr(isPattern))

        while (tokenizer.current is CommaToken) {
            tokenizer.next()

            // Add key -> value pairs, separated by commas, until the map literal is closed
            keys.add(parsePatternOrExpr(isPattern))

            assertCurrent(TokenType.ARROW)
            tokenizer.next()

            values.add(parsePatternOrExpr(isPattern))
        }

        assertCurrent(TokenType.RIGHT_MAP_LITERAL)
        tokenizer.next()

        return MapLiteralExpression(keys, values, leftMapLiteralToken.location)
    }

    fun parseUnaryPlusExpression(plusToken: PlusToken): UnaryPlusExpression {
        // If parseUnaryPlusExpression is called, the previous token must have been a +
        val expr = parseExpression(EXPR_NUMERIC_PREFIX_PRECEDENCE)
        return UnaryPlusExpression(expr, plusToken.location)
    }

    fun parseUnaryMinusExpression(minusToken: MinusToken): UnaryMinusExpression {
        // If parseUnaryMinusExpression is called, the previous token must have been a -
        val expr = parseExpression(EXPR_NUMERIC_PREFIX_PRECEDENCE)
        return UnaryMinusExpression(expr, minusToken.location)
    }

    fun parseLogicalNotExpression(logicalNotToken: LogicalNotToken): LogicalNotExpression {
        // If parseLogicalNotExpression is called, the previous token must have been a !
        val expr = parseExpression(EXPR_LOGICAL_NOT_PRECEDENCE)
        return LogicalNotExpression(expr, logicalNotToken.location)
    }

    fun parseParenthesizedExpression(
        isPattern: Boolean,
        leftParenToken: LeftParenToken
    ): Expression {
        // If parseParenthesizedExpression is called, the previous token must have been a (
        // If next token is a right paren, this is a unit literal
        if (tokenizer.current is RightParenToken) {
            tokenizer.next()
            return UnitLiteralExpression(leftParenToken.location)
        }

        var expr = parsePatternOrExpr(isPattern)

        // If a right paren is seen after a single expression, this is a group expression
        if (tokenizer.current is RightParenToken) {
            tokenizer.next()
            return GroupExpression(expr, leftParenToken.location)
        }

        // Otherwise this is a tuple literal, so parse comma separated list of expressions
        val exprs = mutableListOf(expr)

        while (tokenizer.current !is RightParenToken) {
            assertCurrent(TokenType.COMMA)
            tokenizer.next()

            expr = parsePatternOrExpr(isPattern)

            exprs.add(expr)
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return TupleLiteralExpression(exprs, leftParenToken.location)
    }

    fun parseAddExpression(prevExpr: Expression): AddExpression {
        // If parseAddExpression is called, the previous token must have been a +
        val expr = parseExpression(EXPR_ADD_PRECEDENCE)
        return AddExpression(prevExpr, expr)
    }

    fun parseSubtractExpression(prevExpr: Expression): SubtractExpression {
        // If parseSubtractExpression is called, the previous token must have been a -
        val expr = parseExpression(EXPR_ADD_PRECEDENCE)
        return SubtractExpression(prevExpr, expr)
    }

    fun parseMultiplyExpression(prevExpr: Expression): MultiplyExpression {
        // If parseMultiplyExpression is called, the previous token must have been a *
        val expr = parseExpression(EXPR_MULTIPLY_PRECEDENCE)
        return MultiplyExpression(prevExpr, expr)
    }

    fun parseDivideExpression(prevExpr: Expression): DivideExpression {
        // If parseDivideExpression is called, the previous token must have been a /
        val expr = parseExpression(EXPR_MULTIPLY_PRECEDENCE)
        return DivideExpression(prevExpr, expr)
    }

    fun parseExponentExpression(prevExpr: Expression): ExponentExpression {
        // If parseExponentExpression is called, the previous token must have been a ^.
        // Subtracting one from the precedence makes this operator right associative.
        val expr = parseExpression(rightAssociative(EXPR_EXPONENT_PRECEDENCE))
        return ExponentExpression(prevExpr, expr)
    }

    fun parseRemainderExpression(prevExpr: Expression): RemainderExpression {
        // If parseRemainderExpression is called, the previous token must have been a %
        val expr = parseExpression(EXPR_MULTIPLY_PRECEDENCE)
        return RemainderExpression(prevExpr, expr)
    }

    fun parseAssignmentExpression(prevExpr: Expression, equalsToken: EqualsToken): Expression {
        // If parseAssignmentExpression is called, the previous token must have been a =
        // Subtracting one from the precedence makes this operator right associative.
        val expr = parseExpression(rightAssociative(EXPR_ASSIGNMENT_PRECEDENCE))
        return AssignmentExpression(prevExpr, expr, equalsToken.location)
    }

    fun parseEqualsExpression(prevExpr: Expression): EqualsExpression {
        // If parseEqualsExpression is called, the previous token must have been a ==
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return EqualsExpression(prevExpr, expr)
    }

    fun parseNotEqualsExpression(prevExpr: Expression): NotEqualsExpression {
        // If parseNotEqualsExpression is called, the previous token must have been a !=
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return NotEqualsExpression(prevExpr, expr)
    }

    fun parseLessThanExpression(prevExpr: Expression): LessThanExpression {
        // If parseLessThanExpression is called, the previous token must have been a <
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return LessThanExpression(prevExpr, expr)
    }

    fun parseLessThanOrEqualExpression(prevExpr: Expression): LessThanOrEqualExpression {
        // If parseLessThanOrEqualExpression is called, the previous token must have been a <=
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return LessThanOrEqualExpression(prevExpr, expr)
    }

    fun parseGreaterThanExpression(prevExpr: Expression): GreaterThanExpression {
        // If parseGreaterThanExpression is called, the previous token must have been a >
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return GreaterThanExpression(prevExpr, expr)
    }

    fun parseGreaterThanOrEqualExpression(prevExpr: Expression): GreaterThanOrEqualExpression {
        // If parseGreaterThanOrEqualExpression is called, the previous token must have been a >=
        val expr = parseExpression(EXPR_COMPARISON_PRECEDENCE)
        return GreaterThanOrEqualExpression(prevExpr, expr)
    }

    fun parseLogicalAndExpression(prevExpr: Expression): LogicalAndExpression {
        // If parseLogicalAndExpression is called, the previous token must have been a &&
        val expr = parseExpression(EXPR_LOGICAL_AND_PRECEDENCE)
        return LogicalAndExpression(prevExpr, expr)
    }

    fun parseLogicalOrExpression(prevExpr: Expression): LogicalOrExpression {
        // If parseLogicalOrExpression is called, the previous token must have been a ||
        val expr = parseExpression(EXPR_LOGICAL_OR_PRECEDENCE)
        return LogicalOrExpression(prevExpr, expr)
    }

    fun parseBuiltin(builtinToken: BuiltinToken): BuiltinExpression {
        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()
        
        val nameToken = tokenizer.next()
        if (nameToken !is StringLiteralToken) {
            throw ParseException("Builtin call expects string containing builtin name", nameToken)
        }

        val args: MutableList<Expression> = mutableListOf()
        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            args.add(parseExpression())
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return BuiltinExpression(nameToken.str, args, builtinToken.location)
    }

    fun parseApplicationExpression(
        prevExpr: Expression,
        leftParenToken: LeftParenToken
    ): ApplicationExpression {
        // If parseApplicationExpression is called, the previous token must have been a (
        val actualArgs: MutableList<Expression> = mutableListOf()

        // If no arguments are supplied, create function call with no argument list
        if (tokenizer.current !is RightParenToken) {
            // Add all arguments (comma separated expressions) until the next right paren is found
            actualArgs.add(parseExpression())

            while (tokenizer.current is CommaToken) {
                tokenizer.next()
                actualArgs.add(parseExpression())
            }
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return ApplicationExpression(prevExpr, actualArgs, leftParenToken.location)
    }

    fun parseIndexExpression(
        prevExpr: Expression,
        leftBracketToken: LeftBracketToken
    ): IndexExpression {
        // If parseIndexExpression is called, the previous token must have been a [
        val keyExpr = parseExpression()

        assertCurrent(TokenType.RIGHT_BRACKET)
        tokenizer.next()

        return IndexExpression(prevExpr, keyExpr, leftBracketToken.location)
    }

    fun parseRecordTypeConstructorExpression(
        prevExpr: Expression,
        leftBraceToken: LeftBraceToken
    ): RecordTypeConstructorExpression {
        // If parseRecordTypeConstructorExpression is called, the previous token must have been a {
        val fields: MutableMap<String, Expression> = mutableMapOf()

        // Parse comma separated list of fields, all within curly braces
        fieldsLoop@ while (tokenizer.current !is RightBraceToken) {
            var token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Field names must be identifiers", token)
            }

            // Fields in a record must be unique
            if (fields.containsKey(token.str)) {
                throw ParseException("Field with name ${token.str} already defined in " +
                        "this record", token)
            }

            assertCurrent(TokenType.COLON)
            tokenizer.next()

            fields[token.str] = parseExpression()

            // If a right brace is found, all fields have been parsed. If a comma is
            // found, there must still be fields to parse. Otherwise, syntax is invalid.
            when (tokenizer.current) {
                is RightBraceToken -> break@fieldsLoop
                is CommaToken -> tokenizer.next()
                else -> throw ParseException(tokenizer.current)
            }
        }

        assertCurrent(TokenType.RIGHT_BRACE)
        tokenizer.next()

        return RecordTypeConstructorExpression(prevExpr, fields, false, leftBraceToken.location)
    }

    fun parseAccessExpression(prevExpr: Expression, periodToken: PeriodToken): AccessExpression {
        // If parseAccessExpression is called, the previous token must have been a .
        val fieldToken = tokenizer.current
        if (fieldToken !is IdentifierToken) {
            throw ParseException("Expected a field name", fieldToken)
        }

        tokenizer.next()

        return AccessExpression(prevExpr, fieldToken.str, periodToken.location)
    }

    fun parseVariableDefinition(
        isConst: Boolean,
        isTopLevel: Boolean,
        defToken: Token
    ): VariableDefinitionStatement {
        // If parseVariableDefinition is called, the previous token must have been a let or const
        val identProps = if (isConst) hashSetOf(IdentifierProperty.IMMUTABLE) else hashSetOf()

        // If not top level, a new definition enters a new scope
        if (!isTopLevel) {
            symbolTable.enterScope(ScopeType.NEW_DEFINITION)
        }

        val lValue = parseLValue(identProps)

        // Parse type if one is supplied
        val typeAnnotation = if (tokenizer.current is ColonToken) {
            parseTypeAnnotation()
        } else {
            null
        }

        assertCurrent(TokenType.EQUALS)
        tokenizer.next()

        val expr = parseExpression()
        return VariableDefinitionStatement(lValue, expr, typeAnnotation, lValue.startLocation,
                defToken.location)
    }

    fun parseLValue(identProps: Set<IdentifierProperty>): Expression {
        val token = tokenizer.next()
        return when (token) {
            // A left paren signals the beginning of a tuple literal (or parenthesized expression)
            is LeftParenToken -> parseLValueParenthesizedExpression(token, identProps)
            // An identifier signals either a single variable or a type constructor
            is IdentifierToken -> parseLValueVariableExpression(token, identProps)
            else -> throw ParseException("Patterns in variable assignment must consist solely of " +
                    "variables, tuples, and type constructors", token)
        }
    }

    fun parseLValueParenthesizedExpression(
        token: LeftParenToken,
        identProps: Set<IdentifierProperty>
    ): Expression {
        // If this is called, the previous token must have been a ( within an lValue
        val lValue = parseLValue(identProps)

        // If a single lValue is between parentheses, simply return it
        if (tokenizer.current is RightParenToken) {
            return lValue
        }

        // Otherwise parse comma separated list of lValues until right paren is seen,
        // as this is a tuple literal.
        val lValues = mutableListOf(lValue)

        while (tokenizer.current !is RightParenToken) {
            assertCurrent(TokenType.COMMA)
            tokenizer.next()

            lValues.add(parseLValue(identProps))
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return TupleLiteralExpression(lValues, token.location)
    }

    fun parseLValueVariableExpression(
        token: IdentifierToken,
        identProps: Set<IdentifierProperty>
    ): Expression {
        // Parse "::" separated list of strings to find scope and identifier name
        val identParts = mutableListOf(token)

        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()

            val currentToken = tokenizer.current
            if (currentToken !is IdentifierToken) {
                throw ParseException("Identifier must follow scope", currentToken)
            }

            identParts.add(currentToken)
            tokenizer.next()
        }

        // All parts except the last are part of the scope. Add resolve job to symbol table.
        val scopeTokens = identParts.take(identParts.size - 1)

        // If there is a left paren following the variable, this is a tuple type constructor
        // pattern definition.
        if (tokenizer.current is LeftParenToken) {
            // Must be a type constructor, so look it up with scopes in context
            val ident = symbolTable.lookupVariable(token.str, scopeTokens.map({ it.str }),
                    token.location, importContext)
            val variableExpr = VariableExpression(ident, token.location)
            val callLocation = tokenizer.current.location

            // Parse comma separated list of lValues
            tokenizer.next()
            val lValues = mutableListOf(parseLValue(identProps))
            while (tokenizer.current !is RightParenToken) {
                assertCurrent(TokenType.COMMA)
                tokenizer.next()

                lValues.add(parseLValue(identProps))
            }

            assertCurrent(TokenType.RIGHT_PAREN)
            tokenizer.next()

            return ApplicationExpression(variableExpr, lValues, callLocation)
        // If there is a left brace following the variable, this is a tuple type constructor
        // pattern definition.
        } else if (tokenizer.current is LeftBraceToken) {
            // Must be a type constructor, so look it up with scopes in context
            val ident = symbolTable.lookupVariable(token.str, scopeTokens.map({ it.str }),
                    token.location, importContext)
            val variableExpr = VariableExpression(ident, token.location)
            val callLocation = tokenizer.current.location

            val fields: MutableMap<String, Expression> = mutableMapOf()

            // Parse comma separated list of fields, all within curly braces
            tokenizer.next()
            fieldsLoop@ while (tokenizer.current !is RightBraceToken) {
                var currentToken = tokenizer.next()
                if (currentToken !is IdentifierToken) {
                    throw ParseException("Field names must be identifiers", currentToken)
                }

                // Fields in a record must be unique
                if (fields.containsKey(currentToken.str)) {
                    throw ParseException("Field with name ${currentToken.str} already defined in " +
                            "this record", currentToken)
                }

                assertCurrent(TokenType.COLON)
                tokenizer.next()

                fields[currentToken.str] = parseLValue(identProps)

                // If a right brace is found, all fields have been parsed. If a comma is
                // found, there must still be fields to parse. Otherwise, syntax is invalid.
                when (tokenizer.current) {
                    is RightBraceToken -> break@fieldsLoop
                    is CommaToken -> tokenizer.next()
                    else -> throw ParseException(tokenizer.current)
                }
            }

            assertCurrent(TokenType.RIGHT_BRACE)
            tokenizer.next()

            return RecordTypeConstructorExpression(variableExpr, fields, true, callLocation)
        // Otherwise this is a defined variable, and it must have no scopes
        } else {
            if (scopeTokens.isEmpty()) {
                val ident = symbolTable.addVariable(token.str, IdentifierClass.VARIABLE,
                        token.location, WhichScope.CURRENT, identProps)
                return VariableExpression(ResolvedIdentifier(ident), token.location)
            } else {
                throw ParseException("Variables in variable definition cannot have scopes",
                        scopeTokens[0].location)
            }
        }
    }

    fun parseLambdaExpression(funToken: FunToken): LambdaExpression {
        // If parseLambdaExpression is called, the previous token must have been a fun

        // Arg list can optionall start with parens
        val hasParens = tokenizer.current is LeftParenToken
        if (hasParens) {
            assertCurrent(TokenType.LEFT_PAREN)
            tokenizer.next()
        }

        val formalArgs: MutableList<Pair<Identifier, TypeExpression?>> = mutableListOf()

        // Enter a new scope so that all variable names and types are scoped to this function
        symbolTable.enterScope(ScopeType.FUNCTION)

        // Keep parsing comma separated formal argument list until a right paren or arrow is found
        argsLoop@ while (tokenizer.current !is RightParenToken &&
                tokenizer.current !is ArrowToken) {
            var token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Formal arguments must be identifiers", token)
            }

            // Parse optional type annotation if they exist
            val typeAnnotation = if (tokenizer.current is ColonToken) {
                parseTypeAnnotation(true)
            } else {
                null
            }

            // Add formal argument as variable to symbol table in new scope
            val formalArg = symbolTable.addVariable(token.str, IdentifierClass.VARIABLE,
                    token.location)
            formalArgs.add(Pair(formalArg, typeAnnotation))

            // If a right paren or arrow is found, all arguments have been found. If a comma is
            // found, there must still be identifiers to parse. Otherwise, syntax is invalid.
            when (tokenizer.current) {
                is ArrowToken -> break@argsLoop
                is RightParenToken -> break@argsLoop
                is CommaToken -> tokenizer.next()
                else -> throw ParseException(tokenizer.current)
            }
        }

        // If arg list started with paren, it must end with paren
        if (hasParens) {
            assertCurrent(TokenType.RIGHT_PAREN)
            tokenizer.next()
        }

        assertCurrent(TokenType.ARROW)
        tokenizer.next()

        // Lambda body must be an expression
        val expr = parseExpression()
        val body = ReturnStatement(expr, expr.startLocation)

        symbolTable.exitScope()

        return LambdaExpression(formalArgs, body, funToken.location)
    }

    fun parseFunctionDefinition(
        isTopLevel: Boolean,
        isMethod: Boolean,
        defToken: DefToken
    ): FunctionDefinitionStatement {
        // If parseFunctionDefinition is called, the previous token must have been a def
        var token = tokenizer.next()

        if (token !is IdentifierToken) {
            throw ParseException("Expected function name", token)
        }

        val funcToken = token
        val formalArgs: MutableList<Pair<Identifier, TypeExpression>> = mutableListOf()

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // If not top level, start a new scope where this function is defined
        if (!isTopLevel) {
            symbolTable.enterScope(ScopeType.NEW_DEFINITION)
        }

        // Add the function to the symbol table
        val ident = if (isMethod) {
            symbolTable.addMethod(funcToken.str, funcToken.location)
        } else {
            symbolTable.addVariable(funcToken.str, IdentifierClass.FUNCTION,
                    funcToken.location)
        }

        // Enter a new scope so that all variable names and types are scoped to this function
        symbolTable.enterScope(ScopeType.FUNCTION)

        // Keep parsing comma separated formal argument identifiers until a right paren is found
        argsLoop@ while (tokenizer.current !is RightParenToken) {
            token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Formal arguments must be identifiers", token)
            }

            // Parse required type annotation
            val typeAnnotation = parseTypeAnnotation(true)

            val formalArg = symbolTable.addVariable(token.str, IdentifierClass.VARIABLE,
                    token.location)
            formalArgs.add(Pair(formalArg, typeAnnotation))

            // If a right paren is found, all arguments have been found. If a comma is found,
            // there must still be identifiers to parse. Otherwise, syntax is invalid.
            when (tokenizer.current) {
                is RightParenToken -> break@argsLoop
                is CommaToken -> tokenizer.next()
                else -> throw ParseException(tokenizer.current)
            }
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        // Parse optional return type annotation
        val returnTypeAnnotation = if (tokenizer.current is ColonToken) {
            parseTypeAnnotation(true)
        } else {
            null
        }

        // Expression function definition bodies begin with an equals sign
        if (tokenizer.current is EqualsToken) {
            tokenizer.next()

            val expr = parseExpression()
            symbolTable.exitScope()
            return FunctionDefinitionStatement(ident, formalArgs, returnTypeAnnotation,
                    ReturnStatement(expr, expr.startLocation), funcToken.location,
                            defToken.location)
        } else {
            // Non-expression function definition bodies must consist of a single block
            token = tokenizer.next()

            if (token !is LeftBraceToken) {
                throw ParseException("Function bodies must consist of either a single expression " +
                        "or a single block", token)
            }

            val block = parseBlock(token)
            symbolTable.exitScope()
            return FunctionDefinitionStatement(ident, formalArgs, returnTypeAnnotation, block,
                    funcToken.location, defToken.location)
        }
    }

    fun parseBlock(leftBraceToken: LeftBraceToken): BlockStatement {
        // If parseBlock is called, the previous token must have been a {
        symbolTable.enterScope(ScopeType.BLOCK)

        // Keep parsing statements in a new scope until a right brace is encountered
        val statements: MutableList<Statement> = mutableListOf()
        while (tokenizer.current !is RightBraceToken) {
            statements.add(parseStatement())
        }

        tokenizer.next()
        symbolTable.exitScope()

        return BlockStatement(statements, leftBraceToken.location)
    }

    fun parseIfStatement(ifToken: IfToken): IfStatement {
        // If parseIfStatement is called, the previous token must have been an if
        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        val condition = parseExpression()

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        val consequent = parseStatement()
        var alternative: Statement? = null

        // If an else is encountered parse it, otherwise alternative does not exist
        if (!tokenizer.reachedEnd && tokenizer.current is ElseToken) {
            tokenizer.next()
            alternative = parseStatement()
        }

        return IfStatement(condition, consequent, alternative, ifToken.location)
    }

    fun parseWhileStatement(whileToken: WhileToken): WhileStatement {
        // If parseWhileStatement is called, the previous token must have been a while
        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        val condition = parseExpression()

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()


        val statement = parseStatement()

        return WhileStatement(condition, statement, whileToken.location)
    }

    fun parseDoWhileStatement(doToken: DoToken): DoWhileStatement {
        // If parseDoWhileStatement is called, the previous token must have been a while
        val statement = parseStatement()

        assertCurrent(TokenType.WHILE)
        tokenizer.next()

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        val condition = parseExpression()

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return DoWhileStatement(condition, statement, doToken.location)
    }

    fun parseForStatement(forToken: ForToken): ForStatement {
        // If parseForStatement is called, the previous token must have been a for
        
        // Entire for loop must be in a new scope, since the init or update could introduce
        // new bindings that should not exist outside the context of the for loop.
        symbolTable.enterScope(ScopeType.BLOCK)

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // Parse initializer statement if a comma is not seen
        var initializer: Statement? = null
        if (tokenizer.current !is CommaToken) {
            initializer = parseStatement()
        }

        // The next token after the initializer must be a comma
        assertCurrent(TokenType.COMMA)
        tokenizer.next()

        // Parse condition expression if a comma is not seen
        var condition: Expression? = null
        if (tokenizer.current !is CommaToken) {
            condition = parseExpression()
        }

        // The next token after the condition must be a comma
        assertCurrent(TokenType.COMMA)
        tokenizer.next()

        // Parse update statement if a right paren is not seen
        var update: Statement? = null
        if (tokenizer.current !is RightParenToken) {
            update = parseStatement()
        }

        // The next token after the update must be a right paren
        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        val statement = parseStatement()

        symbolTable.exitScope()

        return ForStatement(initializer, condition, update, statement, forToken.location)
    }

    fun parseForEachStatement(forEachToken: ForEachToken): ForEachStatement {
        // If parseForEachStatement is called, the previous token must have been a forEach
        
        // Entire for loop must be in a new scope, since the lValue will introduce
        // new bindings that should not exist outside the context of the for loop.
        symbolTable.enterScope(ScopeType.BLOCK)

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()
        
        val lValue = parseLValue(setOf())

        // An optional type annotation can exist after the lValue
        val typeAnnotation = if (tokenizer.current is ColonToken) {
            parseTypeAnnotation()
        } else {
            null
        }

        assertCurrent(TokenType.IN)
        tokenizer.next()

        // Parse expression to loop over and the body of the for loop
        val iterable = parseExpression()

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        val statement = parseStatement()

        symbolTable.exitScope()

        return ForEachStatement(lValue, typeAnnotation, iterable, statement,
                forEachToken.location)
    }

    fun parsePatternOrExpr(isPattern: Boolean): Expression {
        if (isPattern) {
            return parsePattern()
        } else {
            return parseExpression()
        }
    }

    fun parsePattern(): Expression {
        var token = tokenizer.next()
        return when (token) {
            // Patterns only consist of literals and variables
            is IntegralLiteralToken -> IntegralLiteralExpression(token.num, token.location)
            is DecimalLiteralToken -> DecimalLiteralExpression(token.num, token.location)
            is StringLiteralToken -> StringLiteralExpression(token.str, token.location)
            is TrueToken -> BoolLiteralExpression(true, token.location)
            is FalseToken -> BoolLiteralExpression(false, token.location)
            is LeftBracketToken -> parseVectorLiteralExpression(true, token)
            is LeftSetLiteralToken -> parseSetLiteralExpression(true, token)
            is LeftMapLiteralToken -> parseMapLiteralExpression(true, token)
            is LeftParenToken -> parseParenthesizedExpression(true, token)
            // Plus tokens can prefix a number literal
            is PlusToken -> {
                val currentToken = tokenizer.next()
                if (currentToken is IntegralLiteralToken) {
                    IntegralLiteralExpression(currentToken.num, token.location)
                } else if (currentToken is DecimalLiteralToken) {
                    DecimalLiteralExpression(currentToken.num, token.location)
                } else {
                    throw ParseException("Patterns must only consist of literals and variables",
                        token)
                }
            }
            // Minus tokens can prefix a number literal
            is MinusToken -> {
                val currentToken = tokenizer.next()
                if (currentToken is IntegralLiteralToken) {
                    IntegralLiteralExpression(-currentToken.num, token.location)
                } else if (currentToken is DecimalLiteralToken) {
                    DecimalLiteralExpression(-currentToken.num, token.location)
                } else {
                    throw ParseException("Patterns must only consist of literals and variables",
                        token)
                }
            }
            // An identifier may be a new variable or a type constructor
            is IdentifierToken -> {
                // Parse "::" separated list of strings to find scope and identifier name
                val identParts = mutableListOf(token.str)

                while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
                    tokenizer.next()

                    val currentToken = tokenizer.current
                    if (currentToken !is IdentifierToken) {
                        throw ParseException("Identifier must follow scope", currentToken)
                    }

                    identParts.add(currentToken.str)
                    tokenizer.next()
                }

                // All parts except the last are part of the scope. Add resolve job to symbol table.
                val name = identParts[identParts.size - 1]
                val scopes = identParts.take(identParts.size - 1)

                // Identifier may be followed by (optional) comma separated list of patterns within
                // parentheses, meaning this is a tuple type constructor pattern.
                if (tokenizer.current is LeftParenToken) {
                    val ident = symbolTable.addPatternVariable(name, scopes, importContext,
                            token.location, false)

                    val variableExpr = VariableExpression(ident, token.location)
                    val callLocation = tokenizer.current.location
                    val args: MutableList<Expression> = mutableListOf()

                    do {
                        tokenizer.next()
                        args.add(parsePattern())
                    } while (tokenizer.current is CommaToken)

                    assertCurrent(TokenType.RIGHT_PAREN)
                    tokenizer.next()

                    return ApplicationExpression(variableExpr, args, callLocation)
                // Identifier may be followed by (optional) comma separated list of named fields
                // and patterns within braces, meaning this is a record type constructor pattern.
                } else if (tokenizer.current is LeftBraceToken) {
                    val ident = symbolTable.addPatternVariable(name, scopes, importContext,
                            token.location, false)

                    val variableExpr = VariableExpression(ident, token.location)
                    val leftBraceLocation = tokenizer.current.location
                    tokenizer.next()

                    val fields: MutableMap<String, Expression> = mutableMapOf()

                    // Parse comma separated list of fields, all within curly braces
                    fieldsLoop@ while (tokenizer.current !is RightBraceToken) {
                        token = tokenizer.next()
                        if (token !is IdentifierToken) {
                            throw ParseException("Field names must be identifiers", token)
                        }

                        // Fields in a record must be unique
                        if (fields.containsKey(token.str)) {
                            throw ParseException("Field with name ${token.str} already defined in " +
                                    "this record", token)
                        }

                        assertCurrent(TokenType.COLON)
                        tokenizer.next()

                        fields[token.str] = parsePattern()

                        // If a right brace is found, all fields have been parsed. If a comma is
                        // found, there must still be fields to parse. Otherwise, syntax is invalid.
                        when (tokenizer.current) {
                            is RightBraceToken -> break@fieldsLoop
                            is CommaToken -> tokenizer.next()
                            else -> throw ParseException(tokenizer.current)
                        }
                    }

                    assertCurrent(TokenType.RIGHT_BRACE)
                    tokenizer.next()

                    return RecordTypeConstructorExpression(variableExpr, fields, true,
                            leftBraceLocation)
                } else {
                    val ident = symbolTable.addPatternVariable(name, scopes, importContext,
                            token.location, true)
                    return VariableExpression(ident, token.location)
                }
            }
            else -> throw ParseException("Patterns must only consist of literals and variables",
                        token)
        }
    }

    fun parseMatchStatement(matchToken: MatchToken): MatchStatement {
        // If parseMatchStatement is called, the previous token must have been a match.
        val matchExpr = parseExpression()

        val patterns: MutableList<Expression> = mutableListOf()
        val guards: MutableList<Expression?> = mutableListOf()
        val statements: MutableList<Statement> = mutableListOf()

        // Parse nonempty list of cases
        do {
            // The pipe before the first match case is optional
            if (patterns.size == 0 && tokenizer.current is PipeToken) {
                tokenizer.next()
            } else if (patterns.size != 0) {
                // If EOF is reached right after a complete match case, another case could start
                // on the next input line.
                if (tokenizer.reachedEnd) {
                    if (ignoreAmbiguousEnd) {
                        break
                    } else {
                        throw AmbiguousEndException()
                    }
                // Otherwise a pipe signals that another match case is starting
                } else if (tokenizer.current is PipeToken) {
                    tokenizer.next()
                // Otherwise we have parsed all the cases for this match
                } else {
                    break
                }
            }

            // Each case should be in its own scope
            symbolTable.enterScope(ScopeType.PATTERN)

            // Rest of case is pattern and statement separated by an arrow
            patterns.add(parsePattern())

            // Parse optional guard
            val guard = if (tokenizer.current is WhenToken) {
                tokenizer.next()
                parseExpression()
            } else {
                null
            }

            guards.add(guard)

            // Parse arrow and then statement to be evaluated in match case
            assertCurrent(TokenType.ARROW)
            tokenizer.next()

            statements.add(parseStatement())

            symbolTable.exitScope()
        } while (true)

        val cases = patterns.zip(guards).zip(statements).map {
            (pg, s) -> Triple(pg.first, pg.second, s)
        }
        
        return MatchStatement(matchExpr, cases, matchToken.location)
    }

    fun parseReturnStatement(returnToken: ReturnToken): ReturnStatement {
        // If parseReturnStatement is called, the previous token must have been a return.
        return ReturnStatement(parseExpression(), returnToken.location)
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Parsing packages and imports
    //
    ///////////////////////////////////////////////////////////////////////////

    fun parsePackageDeclaration(): Package {
        // Parse initial package declaration
        var token = tokenizer.next()
        if (token !is PackageToken) {
            throw ParseException("Every file must begin with a package declaration", token)
        }

        // Parse package name - a sequence of identifiers separated by scope tokens (::)
        token = tokenizer.next()
        if (token !is IdentifierToken) {
            throw ParseException("Package name must consist of scope separated identifiers", token)
        }

        val packageParts = mutableListOf(token.str)
        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()
            token = tokenizer.next()

            if (token !is IdentifierToken) {
                throw ParseException("Package name must consist of scope separated identifiers",
                        token)
            }

            packageParts.add(token.str)
        }

        // Lookup and return the package if it already exists, or create new package if it doesn't
        val pack = rootPackageNode.getSubPackage(packageParts)
        if (pack != null) {
            return pack
        } else {
            return rootPackageNode.createSubPackage(packageParts)
        }
    }

    fun parseImportStatements(
        importContext: ImportContext,
        seenFirstImport: Boolean = false
    ): ImportContext {
        var firstImport = true

        // Parse sequence of import statements, and enter loop if already seen the first import
        while ((!tokenizer.reachedEnd && tokenizer.current is ImportToken) ||
                    (seenFirstImport && firstImport)) {
            // Ignore the first import if it has already been seen
            if (!(seenFirstImport && firstImport)) {
                tokenizer.next()
            }

            firstImport = false

            // Parse import parts which appear as a sequence of identifiers separated by scopes (::)
            var token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Imports must consist of scope separated identifiers", token)
            }

            val importTokens = mutableListOf(token)
            var isMultipleImport = false

            scopeLoop@while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
                tokenizer.next()
                token = tokenizer.next()

                // If left brace is seen, this is a multiple import statement so stop parsing scopes
                if (token is LeftBraceToken) {
                    isMultipleImport = true
                    break@scopeLoop
                } else if (token !is IdentifierToken) {
                    throw ParseException("Imports must consist of scope separated identifiers",
                            token)
                }

                importTokens.add(token)
            }

            // If this is a multiple import start parsing all final imports with optional aliases
            if (isMultipleImport) {
                // Parse last name part of first import in multiple imports
                token = tokenizer.next()
                if (token !is IdentifierToken) {
                    throw ParseException("Imports must consist of scope separated identifiers",
                            token)
                }

                // Find alias token for first import in multiple imports
                val lastImportTokens = mutableListOf(token)
                val aliasTokens = mutableListOf(parseOptionalAlias(token, true))

                // Parse comma separated list of rest of multiple imports
                while (tokenizer.current is CommaToken) {
                    tokenizer.next()

                    // Parse last name part of import
                    token = tokenizer.next()
                    if (token !is IdentifierToken) {
                        throw ParseException("Imports must consist of scope separated identifiers",
                                token)
                    }

                    // Find alias for import in multiple imports
                    lastImportTokens.add(token)
                    aliasTokens.add(parseOptionalAlias(token, true))
                }

                // Multiple import must end with a closing curly brace
                assertCurrent(TokenType.RIGHT_BRACE)
                tokenizer.next()

                // Cannot add two aliases with the same name into the same context
                for (aliasToken in aliasTokens) {
                    val alias = aliasToken.str

                    // This can occur because the context already contains an alias with that name
                    if (importContext.imports.any { (otherAlias, _, _) -> alias == otherAlias }) {
                        throw ParseException("Import with name ${alias} already found",
                                aliasToken.location)
                    }

                    // Or if the multiple import contains two imports with the same alias
                    if (aliasTokens.any { otherToken -> otherToken.str == aliasToken.str &&
                            otherToken != aliasToken }) {
                        throw ParseException("Import with name ${alias} already found",
                                aliasToken.location)
                    }
                }

                // Add all import parts and aliases to the import context
                for ((aliasToken, lastImportToken) in aliasTokens.zip(lastImportTokens)) {
                    val fullImportParts = importTokens.map({ it.str }).toList() +
                            lastImportToken.str
                    importContext.imports.add(Triple(aliasToken.str, fullImportParts,
                            importTokens[0].location))
                }
            } else {
                // Otherwise this is a single import, so parse optional alias and add it to context
                val aliasToken = parseOptionalAlias(importTokens[importTokens.size - 1], false)
                val alias = aliasToken.str

                // Cannot add two imports with the same alias into same context
                if (importContext.imports.any { (otherAlias, _, _) -> alias == otherAlias }) {
                    throw ParseException("Import with name ${alias} already found",
                            aliasToken.location)
                }

                // Add these import parts and alias to the import context
                importContext.imports.add(Triple(alias, importTokens.map({ it.str}),
                        importTokens[0].location))
            }
        }

        return importContext
    }

    fun parseOptionalAlias(
        lastImportToken: IdentifierToken,
        inMultipleImport: Boolean
    ): IdentifierToken {
        // Parse optional alias part of import
        return if ((inMultipleImport || !tokenizer.reachedEnd)
                && tokenizer.current is AsToken) {
            tokenizer.next()
            val token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Import alias must be an identifier", token)
            }

            token
        } else {
            lastImportToken
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    // 
    // Parsing functions for types
    //
    ///////////////////////////////////////////////////////////////////////////

    fun parseType(
        inFunctionDef: Boolean = false,
        precedence: Int = TYPE_NO_PRECEDENCE
    ): TypeExpression {
        val currentToken = tokenizer.next()

        // Find the first type found in the type expression
        var currentType = when (currentToken) {
            is BoolToken -> BoolTypeExpression
            is StringTypeToken -> StringTypeExpression
            is ByteToken -> ByteTypeExpression
            is IntToken -> IntTypeExpression
            is FloatToken -> FloatTypeExpression
            is DoubleToken -> DoubleTypeExpression
            is UnitToken -> UnitTypeExpression
            is LeftParenToken -> parseParenthesizedType(inFunctionDef)
            is VecToken -> parseVectorType(inFunctionDef)
            is SetToken -> parseSetType(inFunctionDef)
            is MapToken -> parseMapType(inFunctionDef)
            is IdentifierToken -> parseVariableType(currentToken, inFunctionDef)
            else -> throw ParseException("Expected type, got ${currentToken}", currentToken)
        }

        // Keep parsing operators that have a higher precedence than the current precedence context,
        // as these will be bound more tightly and therefore should be children of the current,
        // lower precedence expression.
        while (!tokenizer.reachedEnd &&
                precedence < getTypePrecedenceForInfixToken(tokenizer.current.type)) {
            // Match on all tokens that signal an infix type operator
            val token = tokenizer.next()
            currentType = when (token) {
                is ArrowToken -> parseFunctionType(currentType, inFunctionDef)
                else -> throw ParseException(token)
            }
        }

        return currentType
    }

    fun parseParenthesizedType(inFunctionDef: Boolean): TypeExpression {
        // If parseParenthesizedType is called, the previous token must have been a (
        val types: MutableList<TypeExpression> = mutableListOf(parseType(inFunctionDef))

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            types.add(parseType(inFunctionDef))
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        // If only one type was found in the list, this is just a parenthesized single type
        if (types.size == 1) {
            return types.get(0)
        // Otherwise this is a tuple type with the listed elements
        } else {
            return TupleTypeExpression(types)
        }
    }

    fun parseVectorType(inFunctionDef: Boolean = false): VectorTypeExpression {
        // If parseVectorType is called, previous token must have been vec.
        // Parse element type surrounded by angle braces.
        assertCurrent(TokenType.LESS_THAN)
        tokenizer.next()

        val elementType = parseType(inFunctionDef)

        assertCurrent(TokenType.GREATER_THAN)
        tokenizer.next()

        return VectorTypeExpression(elementType)
    }

    fun parseSetType(inFunctionDef: Boolean = false): SetTypeExpression {
        // If parseSetType is called, previous token must have been set.
        // Parse element type surrounded by angle braces.
        assertCurrent(TokenType.LESS_THAN)
        tokenizer.next()

        val elementType = parseType(inFunctionDef)

        assertCurrent(TokenType.GREATER_THAN)
        tokenizer.next()

        return SetTypeExpression(elementType)
    }

    fun parseMapType(inFunctionDef: Boolean = false): MapTypeExpression {
        // If parseMapType is called, previous token must have been map.
        // Parse comma separated key and val types surrounded by angle braces.
        assertCurrent(TokenType.LESS_THAN)
        tokenizer.next()

        val keyType = parseType(inFunctionDef)

        assertCurrent(TokenType.COMMA)
        tokenizer.next()

        val valType = parseType(inFunctionDef)

        assertCurrent(TokenType.GREATER_THAN)
        tokenizer.next()

        return MapTypeExpression(keyType, valType)
    }

    /**
     * Parse the type represented by a particular identifier. This could be an existing type
     * parameter, a new type parameter (if in a function definition), or the beginning of a
     * defined algebraic data type.
     */
    fun parseVariableType(
        token: IdentifierToken,
        inFunctionDef: Boolean = false
    ): TypeExpression {
        // Parse "::" separated list of strings to find scope and identifier name
        val identParts = mutableListOf(token.str)

        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()

            val currentToken = tokenizer.current
            if (currentToken !is IdentifierToken) {
                throw ParseException("Identifier must follow scope", currentToken)
            }

            identParts.add(currentToken.str)
            tokenizer.next()
        }

        // All parts except the last are part of the scope. Add resolve job to symbol table.
        val name = identParts[identParts.size - 1]
        val scopes = identParts.take(identParts.size - 1)

        val ident = symbolTable.addTypeVariable(name, scopes, importContext,
                token.location, inFunctionDef)

        // Parse (optional) comma separated list of types within < > that parameterize this type
        val typeParams: MutableList<TypeExpression> = mutableListOf()
        if (tokenizer.current is LessThanToken) {
            do {
                tokenizer.next()
                typeParams.add(parseType(inFunctionDef))
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()

        }

        return VariableTypeExpression(ident, typeParams, token.location)
    }

    fun parseFunctionType(
        prevType: TypeExpression,
        inFunctionDef: Boolean
    ): FunctionTypeExpression {
        // If parseFunctionType is called, the previous token must have been a ->
        val type = parseType(inFunctionDef, TYPE_FUNCTION_PRECEDENCE)

        // If the previous type was a function, the next type is its return type
        if (prevType is FunctionTypeExpression) {
            return FunctionTypeExpression(prevType.argTypes + prevType.returnType, type)
        // Otherwise, this is a one argument function
        } else {
            return FunctionTypeExpression(listOf(prevType), type)
        }
    }

    /**
     * Parse the current type annotation in the stream into a single type.
     */
    fun parseTypeAnnotation(inFunctionDef: Boolean = false): TypeExpression {
        assertCurrent(TokenType.COLON)
        tokenizer.next()

        return parseType(inFunctionDef)
    }

    fun parseTypeDefinition(): TypeDefinitionStatement {
        // If parseTypeDefinition is called, the previous token must have been a type
        var currentToken = tokenizer.current
        if (currentToken !is IdentifierToken) {
            throw ParseException("Expected ${currentToken} to be an identifier", currentToken)
        }

        val typeNameToken = currentToken
        val typeParams: MutableList<Identifier> = mutableListOf()
        val typeParamNames: MutableSet<String> = mutableSetOf()

        tokenizer.next()

        // Enter a new scope for the duration of this definition, so that type params will be local
        symbolTable.enterScope(ScopeType.TYPE_DEFINITION)
        
        // Parse optional type parameters for parameterized variant type
        if (tokenizer.current is LessThanToken) {
            // Type parameters must be a comma separated list of identifiers within < >
            do {
                tokenizer.next()
                currentToken = tokenizer.current

                if (currentToken !is IdentifierToken) {
                    throw ParseException("Expected ${currentToken} to be an identifier",
                            currentToken)
                }

                // Cannot have two type parameters with the same name
                if (typeParamNames.contains(currentToken.str)) {
                    throw ParseException("Type ${typeNameToken} cannot have two type " +
                            "parameters with the same name: ${currentToken.str}", currentToken)
                }

                // Add the type parameter to the local environment and save it in the list of params
                typeParams.add(symbolTable.addType(currentToken.str,
                        IdentifierClass.TYPE_PARAMETER, currentToken.location))
                typeParamNames.add(currentToken.str)

                tokenizer.next()
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()
        }

        // Add identifier for ADT name to package scope
        val typeIdent = symbolTable.addType(typeNameToken.str, IdentifierClass.ALGEBRAIC_DATA_TYPE,
                typeNameToken.location, WhichScope.PACKAGE)

        assertCurrent(TokenType.EQUALS)
        tokenizer.next()

        val tupleVariants: MutableList<Pair<Identifier, List<TypeExpression>>> = mutableListOf()
        val recordVariants: MutableList<Pair<Identifier,
                Map<String, Pair<TypeExpression, Boolean>>>> = mutableListOf()
        var firstVariant = true

        // Parse nonempty sequence of variant definitions
        do {
            // The pipe before the first variant is optional
            if (firstVariant && tokenizer.current is PipeToken) {
                tokenizer.next()
            } else if (!firstVariant) {
                // If EOF is reached right after a complete variant, another variant could start
                // on the next input line.
                if (tokenizer.reachedEnd) {
                    if (ignoreAmbiguousEnd) {
                        break
                    } else {
                        throw AmbiguousEndException()
                    }
                // Otherwise a pipe signals that another variant is starting
                } else if (tokenizer.current is PipeToken) {
                    tokenizer.next()
                // Otherwise we have parsed all the variants for this type
                } else {
                    break
                }
            }

            firstVariant = false
            currentToken = tokenizer.current

            if (currentToken !is IdentifierToken) {
                throw ParseException("Algebraic data type variants must be named", currentToken)
            }

            val variantNameToken = currentToken
            tokenizer.next()

            // Type constructor arguments are optional, so an end here is ambiguous
            if (tokenizer.reachedEnd) {
                // If end is unambiguous, need to create a new variant and add it to package scope
                if (ignoreAmbiguousEnd) {
                    val variantIdent = symbolTable.addVariable(variantNameToken.str,
                            IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT, variantNameToken.location,
                            WhichScope.PACKAGE)
                    tupleVariants.add(Pair(variantIdent, listOf()))
                    break
                } else {
                    throw AmbiguousEndException()
                }
            }

            if (tokenizer.current is LeftBraceToken) {
                tokenizer.next()

                val fields: MutableMap<String, Pair<TypeExpression, Boolean>> = mutableMapOf()

                // Parse comma separated list of fields, all within curly braces
                fieldsLoop@ while (tokenizer.current !is RightBraceToken) {
                    // Parse optional mutable flag
                    val isMutable = if (tokenizer.current is MutToken) {
                        tokenizer.next()
                        true
                    } else {
                        false
                    }

                    var token = tokenizer.next()
                    if (token !is IdentifierToken) {
                        throw ParseException("Field names must be identifiers", token)
                    }

                    // Fields in a record must be unique
                    if (fields.containsKey(token.str)) {
                        throw ParseException("Field with name ${token.str} already defined in " +
                                "this record", token)
                    }

                    assertCurrent(TokenType.COLON)
                    tokenizer.next()

                    fields[token.str] = Pair(parseType(), isMutable)

                    // If a right brace is found, all fields have been parsed. If a comma is
                    // found, there must still be fields to parse. Otherwise, syntax is invalid.
                    when (tokenizer.current) {
                        is RightBraceToken -> break@fieldsLoop
                        is CommaToken -> tokenizer.next()
                        else -> throw ParseException(tokenizer.current)
                    }
                }

                assertCurrent(TokenType.RIGHT_BRACE)
                tokenizer.next()

                // Create a new record variant identifier and add it to the package scope
                val variantIdent = symbolTable.addVariable(variantNameToken.str,
                        IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT, variantNameToken.location,
                        WhichScope.PACKAGE)
                recordVariants.add(Pair(variantIdent, fields))
            // Otherwise this is a tuple type variant
            } else {
                // Parse optional type constructor arguments
                val typeAnnotation = if (tokenizer.current is LeftParenToken) {
                    parseType()
                } else {
                    null
                }

                // If tuple type then constructor is elements of tuple, otherwise use parsed type
                // if it exists, or empty constructor if no type annotation was provided
                val typeConstructor = if (typeAnnotation is TupleTypeExpression) {
                    typeAnnotation.elementTypes
                } else if (typeAnnotation != null) {
                    listOf(typeAnnotation)
                } else {
                    listOf()
                }

                // Create a new tuple variant identifier and add it to the package scope
                val variantIdent = symbolTable.addVariable(variantNameToken.str,
                        IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT, variantNameToken.location,
                        WhichScope.PACKAGE)
                tupleVariants.add(Pair(variantIdent, typeConstructor))
            }
        } while (true)

        symbolTable.exitScope()

        return TypeDefinitionStatement(typeIdent, typeParams, tupleVariants, recordVariants)
    }

    fun parseTypeImplementation(): TypeImplementationStatement {
        // Parse "::" separated list of strings to find scope and identifier name
        val typeStartToken = tokenizer.current
        if (typeStartToken !is IdentifierToken) {
            throw ParseException("Expected type name", typeStartToken)
        }

        val identParts = mutableListOf(typeStartToken.str)
        tokenizer.next()

        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()

            val currentToken = tokenizer.current
            if (currentToken !is IdentifierToken) {
                throw ParseException("Identifier must follow scope", currentToken)
            }

            identParts.add(currentToken.str)
            tokenizer.next()
        }

        // All parts except the last are part of the scope. Add resolve job to symbol table.
        val name = identParts[identParts.size - 1]
        val scopes = identParts.take(identParts.size - 1)

        // Determine whether this implementation is on a builtin or user-defined type
        val (typeSig, typeIdent) = if (scopes.isEmpty() &&
                (name == "unit" ||
                 name == "bool" ||
                 name == "byte" ||
                 name == "int" ||
                 name == "float" ||
                 name == "double" ||
                 name == "string" ||
                 name == "vec" ||
                 name == "set" ||
                 name == "map" ||
                 name == "__tuple" ||
                 name == "__function")) {
            Pair(name, null)
        } else {
            // Find type in symbol table
            val typeIdent = symbolTable.addTypeVariable(name, scopes, importContext,
                    typeStartToken.location, false)
            Pair(null, typeIdent)
        }

        val typeParams: MutableList<Identifier> = mutableListOf()
        val typeParamNames: MutableSet<String> = mutableSetOf()

        // Enter a new scope for the duration of this definition, so that type params will be local
        symbolTable.enterScope(ScopeType.TYPE_DEFINITION)
        
        // Parse optional type parameters
        if (tokenizer.current is LessThanToken) {
            // Type parameters must be a comma separated list of identifiers within < >
            do {
                tokenizer.next()
                var token = tokenizer.current

                if (token !is IdentifierToken) {
                    throw ParseException("Expected ${token} to be a type parameter", token)
                }

                // Cannot have two type parameters with the same name
                if (typeParamNames.contains(token.str)) {
                    throw ParseException("Type ${name} cannot have two type " +
                            "parameters with the same name: ${token.str}", token)
                }

                // Add the type parameter to the local environment and save it in the list of params
                typeParams.add(symbolTable.addType(token.str, IdentifierClass.TYPE_PARAMETER,
                        token.location))
                typeParamNames.add(token.str)

                tokenizer.next()
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()
        }

        // Parse optional, comma separated list of traits that this type implements
        val extendedTraits: MutableList<Pair<ResolvableSymbol, List<TypeExpression>>> =
                mutableListOf()
        if (tokenizer.current is ExtendsToken) {
            tokenizer.next()
            extendedTraits.add(parseExtendedTrait())

            while (tokenizer.current is CommaToken) {
                tokenizer.next()
                extendedTraits.add(parseExtendedTrait())
            }
        }

        assertCurrent(TokenType.LEFT_BRACE)
        tokenizer.next()

        // Add "this" to method definition scope and add all method definitions
        val methods: MutableList<Pair<FunctionDefinitionStatement, Boolean>> = mutableListOf()
        val thisIdent = symbolTable.addVariable("this", IdentifierClass.VARIABLE,
                typeStartToken.location)

        while (tokenizer.current !is RightBraceToken) {
            // Parse optional static modifier to function definition
            val isStatic = if (tokenizer.current is StaticToken) {
                tokenizer.next()
                true
            } else {
                false
            }

            val token = tokenizer.current
            assertCurrent(TokenType.DEF)
            tokenizer.next()

            methods.add(Pair(parseFunctionDefinition(true, true, token as DefToken), isStatic))
        }

        assertCurrent(TokenType.RIGHT_BRACE)
        tokenizer.next()

        symbolTable.exitScope()

        return TypeImplementationStatement(typeIdent, typeSig, typeParams, thisIdent, methods,
                extendedTraits, typeStartToken.location)
    }

    fun parseExtendedTrait(): Pair<ResolvableSymbol, List<TypeExpression>> {
        // Parse "::" separated list of strings to find scope and identifier name
        val traitStartToken = tokenizer.current
        if (traitStartToken !is IdentifierToken) {
            throw ParseException("Expected trait name", traitStartToken)
        }

        val identParts = mutableListOf(traitStartToken.str)
        tokenizer.next()

        while (!tokenizer.reachedEnd && tokenizer.current is ScopeToken) {
            tokenizer.next()

            val currentToken = tokenizer.current
            if (currentToken !is IdentifierToken) {
                throw ParseException("Identifier must follow scope", currentToken)
            }

            identParts.add(currentToken.str)
            tokenizer.next()
        }

        // All parts except the last are part of the scope. Add resolve job to symbol table.
        val name = identParts[identParts.size - 1]
        val scopes = identParts.take(identParts.size - 1)

        // Find trait in symbol table
        val traitIdent = symbolTable.addTypeVariable(name, scopes, importContext,
                traitStartToken.location, false)
        val typeParams: MutableList<TypeExpression> = mutableListOf()
        
        // Parse optional type parameters - a comma separated list of types within < >
        if (tokenizer.current is LessThanToken) {
            do {
                tokenizer.next()
                typeParams.add(parseType())
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()
        }

        return Pair(traitIdent, typeParams)
    }

    fun parseTraitDefinition(): TraitDefinitionStatement {
        var token = tokenizer.current
        if (token !is IdentifierToken) {
            throw ParseException("Trait name must be an identifier", token)
        }

        val traitNameToken = token
        val traitIdent = symbolTable.addType(traitNameToken.str, IdentifierClass.TRAIT,
                traitNameToken.location)

        val typeParams: MutableList<Identifier> = mutableListOf()
        val typeParamNames: MutableSet<String> = mutableSetOf()

        // Enter a new scope for the duration of this definition, so that type params will be local
        symbolTable.enterScope(ScopeType.TYPE_DEFINITION)

        tokenizer.next()
        
        // Parse optional type parameters
        if (tokenizer.current is LessThanToken) {
            // Type parameters must be a comma separated list of identifiers within < >
            do {
                tokenizer.next()
                token = tokenizer.current

                if (token !is IdentifierToken) {
                    throw ParseException("Expected ${token} to be an identifier", token)
                }

                // Cannot have two type parameters with the same name
                if (typeParamNames.contains(token.str)) {
                    throw ParseException("Trait ${traitNameToken.str} cannot have two type " +
                            "parameters with the same name: ${token.str}", token)
                }

                // Add the type parameter to the local environment and save it in the list of params
                typeParams.add(symbolTable.addType(token.str, IdentifierClass.TYPE_PARAMETER,
                        token.location))
                typeParamNames.add(token.str)

                tokenizer.next()
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()
        }

        assertCurrent(TokenType.LEFT_BRACE)
        tokenizer.next()

        val signatureDefs: MutableList<Pair<FunctionSignatureDefinitionStatement, Boolean>> =
                mutableListOf()
        val concreteMethods: MutableList<Pair<FunctionDefinitionStatement, Boolean>> =
                mutableListOf()
        val thisIdent = symbolTable.addVariable("this", IdentifierClass.VARIABLE,
                traitNameToken.location)

        while (true) {
            token = tokenizer.current

            // Check whether the current definition is static or not
            val isStatic = if (token is StaticToken) {
                tokenizer.next()
                token = tokenizer.current
                true
            } else {
                false
            }

            if (token is SigToken) {
                tokenizer.next()
                signatureDefs.add(Pair(parseFunctionSignatureDefinition(token), isStatic))
            } else if (token is DefToken) {
                tokenizer.next()
                concreteMethods.add(Pair(parseFunctionDefinition(true, true, token), isStatic))
            } else if (token is RightBraceToken){
                break
            } else {
                throw ParseException("Trait expects concrete or abstract function definitions",
                        token)
            }
        }

        assertCurrent(TokenType.RIGHT_BRACE)
        tokenizer.next()

        symbolTable.exitScope()

        return TraitDefinitionStatement(traitIdent, typeParams, thisIdent, signatureDefs,
                concreteMethods)
    }

    fun parseFunctionSignatureDefinition(
        sigToken: SigToken
    ): FunctionSignatureDefinitionStatement {
        // If parseFunctionSignatureDefinition is called, the previous token must have been abstract

        var token = tokenizer.next()
        if (token !is IdentifierToken) {
            throw ParseException("Expected function name", token)
        }

        val funcToken = token
        val ident = symbolTable.addMethod(funcToken.str, funcToken.location)
        val argTypes: MutableList<TypeExpression> = mutableListOf()

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // Enter a new scope so that all type parameters are scoped to this function
        symbolTable.enterScope(ScopeType.FUNCTION)

        // Keep parsing comma separated formal argument identifiers until a right paren is found
        argsLoop@ while (tokenizer.current !is RightParenToken) {
            argTypes.add(parseType(true))

            // If a right paren is found, all arguments have been found. If a comma is found,
            // there must still be identifiers to parse. Otherwise, syntax is invalid.
            when (tokenizer.current) {
                is RightParenToken -> break@argsLoop
                is CommaToken -> tokenizer.next()
                else -> throw ParseException(tokenizer.current)
            }
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        // Parse optional return type annotation, interpreting it as unit if no annoation supplied
        val returnTypeAnnotation = if (tokenizer.current is ColonToken) {
            parseTypeAnnotation(true)
        } else {
            UnitTypeExpression
        }

        symbolTable.exitScope()

        return FunctionSignatureDefinitionStatement(ident, argTypes, returnTypeAnnotation,
                funcToken.location, sigToken.location)
    }
}
