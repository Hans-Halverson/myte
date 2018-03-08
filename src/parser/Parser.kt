package myte.parser

import myte.lexer.*
import myte.parser.ast.*
import myte.shared.*

class Parser(val symbolTable: SymbolTable, tokens: List<Token> = listOf()) {
    var tokenizer: Tokenizer = Tokenizer(tokens)

    /**
     * Reset the stream of tokens that are being parsed to a new stream.
     */
    fun setTokens(newTokens: List<Token>) {
        tokenizer = Tokenizer(newTokens)
    }

    /**
     * Parse an entire file, represented as a list of tokens.
     *
     * @return a list of statements corresponding to the top level statements in the file
     * @throws ParseException if the stream of tokens does not represent a valid list of statements
     */
    fun parseFile(): List<Statement> {
        symbolTable.returnToGlobalScope()

        val statements: MutableList<Statement> = mutableListOf()
        while (!tokenizer.reachedEnd) {
            val statement = parseTopLevelStatement()
            if (statement != null) {
                statements.add(statement)
            }
        }

        return statements
    }

    /**
     * Parse a single command from the REPL.
     *
     * @return the single statement parsed from the input stream, or null if no statement is created
     * @throws ParseException if the stream does not correspond to a valid statement, or if there
     *         are leftover tokens after the statement has been parsed
     */
    fun parseLine(): Statement? {
        symbolTable.returnToGlobalScope()

        val statement = parseTopLevelStatement()

        if (!tokenizer.reachedEnd) {
            throw ParseException(tokenizer.current)
        }

        return statement
    }


    /**
     * Parse the current top level statement, and return the statement if a statement can be
     * created, or null if no statement needs to be created.
     */
    fun parseTopLevelStatement(): Statement? {
        val token = tokenizer.next()

        return when (token) {
            is TypeToken -> {
                parseTypeDefinition()
                null
            }
            else -> parseStatement(token)
        }
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
            is LeftBraceToken -> parseBlock(token)
            is LetToken -> parseVariableDefinition(false, token)
            is ConstToken -> parseVariableDefinition(true, token)
            is DefToken -> parseFunctionDefinition(token)
            is IfToken -> parseIfStatement(token)
            is WhileToken -> parseWhileStatement(token)
            is DoToken -> parseDoWhileStatement(token)
            is ForToken -> parseForStatement(token)
            is MatchToken -> parseMatchStatement(token)
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
    fun parseExpression(precedence: Int = NO_PRECEDENCE): Expression {
        return parseExpression(tokenizer.next(), precedence)
    }

    /**
     * Parse the current expression in the stream.
     * 
     * @param firstToken the first token of the current expression
     * @param precedence the optional precedence level that governs the current context, if none
     *        is supplied the lowest precedence level is assumed.
     */
    fun parseExpression(firstToken: Token, precedence: Int = NO_PRECEDENCE): Expression {
        // Match on all tokens that signal a prefix operator
        var currentExpr = when (firstToken) {
            // Literals
            is IntLiteralToken -> IntLiteral(firstToken.num, firstToken.location)
            is FloatLiteralToken -> FloatLiteral(firstToken.num, firstToken.location)
            is IdentifierToken -> parseIdentifierExpression(firstToken)
            is TrueToken -> BoolLiteralExpression(true, firstToken.location)
            is FalseToken -> BoolLiteralExpression(false, firstToken.location)
            is StringLiteralToken -> StringLiteralExpression(firstToken.str, firstToken.location)
            is LeftBracketToken -> parseVectorLiteralExpression(false, firstToken)
            // Prefixs operators
            is PlusToken -> parseUnaryPlusExpression(firstToken)
            is MinusToken -> parseUnaryMinusExpression(firstToken)
            is LogicalNotToken -> parseLogicalNotExpression(firstToken)
            is LeftParenToken -> parseParenthesizedExpression(false, firstToken)
            // If the expression does not begin with a literal or prefix operator, it is not a
            // valid expression.
            else -> throw ParseException(firstToken)
        }

        // Keep parsing operators that have a higher precedence than the current precedence context,
        // as these will be bound more tightly and therefore should be children of the current,
        // lower precedence expression.
        while (!tokenizer.reachedEnd &&
                precedence < getPrecedenceForInfixToken(tokenizer.current.type)) {
            // Match on all tokens that signal an infix operator
            val token = tokenizer.next()
            currentExpr = when (token) {
                // Math operators
                is PlusToken -> parseAddExpression(currentExpr)
                is MinusToken -> parseSubtractExpression(currentExpr)
                is AsteriskToken -> parseMultiplyExpression(currentExpr)
                is ForwardSlashToken -> parseDivideExpression(currentExpr)
                is CaretToken -> parseExponentExpression(currentExpr)
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
                // Function call or access
                is LeftParenToken -> parseCallExpression(currentExpr, token)
                is LeftBracketToken -> parseKeyedAccessExpression(currentExpr, token)
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

    fun parseIdentifierExpression(token: IdentifierToken): Expression {
        val ident = symbolTable.lookup(token.str)
        if (ident == null) {
            throw ParseException("No identifier found for symbol ${token.str}", token)
        }

        // If identifier is a variable or function, create a variable
        val info = symbolTable.getInfo(ident)
        if (info?.idClass == IdentifierClass.VARIABLE ||
                info?.idClass == IdentifierClass.FUNCTION) {
            return VariableExpression(ident, token.location)
        // If identifier is a type constructor, create a constructor with no arguments
        } else if (info?.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            return TypeConstructorExpression(info.adtVariant, listOf(), token.location)
        } else {
            throw ParseException("${ident.name} is not a function, variable, or type constructor",
                    token)
        }
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
        if (isPattern) {
            elements.add(parsePattern())
        } else {
            elements.add(parseExpression())
        }

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            // If in pattern, only parse valid patterns with a call to parsePattern
            if (isPattern) {
                elements.add(parsePattern())
            } else {
                elements.add(parseExpression())
            }
        }

        assertCurrent(TokenType.RIGHT_BRACKET)
        tokenizer.next()

        return VectorLiteralExpression(elements, leftBracketToken.location)
    }

    fun parseUnaryPlusExpression(plusToken: PlusToken): UnaryPlusExpression {
        // If parseUnaryPlusExpression is called, the previous token must have been a +
        val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
        return UnaryPlusExpression(expr, plusToken.location)
    }

    fun parseUnaryMinusExpression(minusToken: MinusToken): UnaryMinusExpression {
        // If parseUnaryMinusExpression is called, the previous token must have been a -
        val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
        return UnaryMinusExpression(expr, minusToken.location)
    }

    fun parseLogicalNotExpression(logicalNotToken: LogicalNotToken): LogicalNotExpression {
        // If parseLogicalNotExpression is called, the previous token must have been a !
        val expr = parseExpression(LOGICAL_NOT_PRECEDENCE)
        return LogicalNotExpression(expr, logicalNotToken.location)
    }

    fun parseParenthesizedExpression(
        isPattern: Boolean,
        leftParenToken: LeftParenToken
    ): Expression {
        // If parseParenthesizedExpression is called, the previous token must have been a (
        // If in pattern, only parse valid patterns with call to parsePattern
        var expr = if (isPattern) {
            parsePattern()
        } else {
            parseExpression()
        }

        // If a right paren is seen after a single expression, this is a group expression
        if (tokenizer.current is RightParenToken) {
            tokenizer.next()
            return GroupExpression(expr, leftParenToken.location)
        }

        // Otherwise interpret as a tuple literal by parsing comma separated list of expressions
        val exprs = mutableListOf(expr)

        while (tokenizer.current !is RightParenToken) {
            assertCurrent(TokenType.COMMA)
            tokenizer.next()

            // If in pattern, only parse valid patterns with call to parsePattern
            expr = if (isPattern) {
                parsePattern()
            } else {
                parseExpression()
            }

            exprs.add(expr)
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return TupleLiteralExpression(exprs, leftParenToken.location)
    }

    fun parseAddExpression(prevExpr: Expression): AddExpression {
        // If parseAddExpression is called, the previous token must have been a +
        val expr = parseExpression(ADD_PRECEDENCE)
        return AddExpression(prevExpr, expr)
    }

    fun parseSubtractExpression(prevExpr: Expression): SubtractExpression {
        // If parseSubtractExpression is called, the previous token must have been a -
        val expr = parseExpression(ADD_PRECEDENCE)
        return SubtractExpression(prevExpr, expr)
    }

    fun parseMultiplyExpression(prevExpr: Expression): MultiplyExpression {
        // If parseMultiplyExpression is called, the previous token must have been a *
        val expr = parseExpression(MULTIPLY_PRECEDENCE)
        return MultiplyExpression(prevExpr, expr)
    }

    fun parseDivideExpression(prevExpr: Expression): DivideExpression {
        // If parseDivideExpression is called, the previous token must have been a /
        val expr = parseExpression(MULTIPLY_PRECEDENCE)
        return DivideExpression(prevExpr, expr)
    }

    fun parseExponentExpression(prevExpr: Expression): ExponentExpression {
        // If parseExponentExpression is called, the previous token must have been a ^.
        // Subtracting one from the precedence makes this operator right associative.
        val expr = parseExpression(rightAssociative(EXPONENT_PRECEDENCE))
        return ExponentExpression(prevExpr, expr)
    }

    fun parseAssignmentExpression(prevExpr: Expression, equalsToken: EqualsToken): Expression {
        // If parseAssignmentExpression is called, the previous token must have been a =
        // Subtracting one from the precedence makes this operator right associative.
        if (prevExpr is VariableExpression) {
            if (symbolTable.getInfo(prevExpr.ident)?.idClass != IdentifierClass.VARIABLE) {
                throw ParseException("Can only reassign value to variables", equalsToken)
            }

            val expr = parseExpression(rightAssociative(ASSIGNMENT_PRECEDENCE))
            return VariableAssignmentExpression(prevExpr.ident, expr, prevExpr.identLocation)
        } else if (prevExpr is KeyedAccessExpression) {
            val expr = parseExpression(rightAssociative(ASSIGNMENT_PRECEDENCE))
            return KeyedAssignmentExpression(prevExpr, expr, prevExpr.accessLocation)
        } else {
            throw ParseException("Cannot assign value to ${prevExpr}", equalsToken)
        }
    }

    fun parseEqualsExpression(prevExpr: Expression): EqualsExpression {
        // If parseEqualsExpression is called, the previous token must have been a ==
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return EqualsExpression(prevExpr, expr)
    }

    fun parseNotEqualsExpression(prevExpr: Expression): NotEqualsExpression {
        // If parseNotEqualsExpression is called, the previous token must have been a !=
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return NotEqualsExpression(prevExpr, expr)
    }

    fun parseLessThanExpression(prevExpr: Expression): LessThanExpression {
        // If parseLessThanExpression is called, the previous token must have been a <
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return LessThanExpression(prevExpr, expr)
    }

    fun parseLessThanOrEqualExpression(prevExpr: Expression): LessThanOrEqualExpression {
        // If parseLessThanOrEqualExpression is called, the previous token must have been a <=
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return LessThanOrEqualExpression(prevExpr, expr)
    }

    fun parseGreaterThanExpression(prevExpr: Expression): GreaterThanExpression {
        // If parseGreaterThanExpression is called, the previous token must have been a >
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return GreaterThanExpression(prevExpr, expr)
    }

    fun parseGreaterThanOrEqualExpression(prevExpr: Expression): GreaterThanOrEqualExpression {
        // If parseGreaterThanOrEqualExpression is called, the previous token must have been a >=
        val expr = parseExpression(COMPARISON_PRECEDENCE)
        return GreaterThanOrEqualExpression(prevExpr, expr)
    }

    fun parseLogicalAndExpression(prevExpr: Expression): LogicalAndExpression {
        // If parseLogicalAndExpression is called, the previous token must have been a &&
        val expr = parseExpression(LOGICAL_AND_PRECEDENCE)
        return LogicalAndExpression(prevExpr, expr)
    }

    fun parseLogicalOrExpression(prevExpr: Expression): LogicalOrExpression {
        // If parseLogicalOrExpression is called, the previous token must have been a ||
        val expr = parseExpression(LOGICAL_OR_PRECEDENCE)
        return LogicalOrExpression(prevExpr, expr)
    }

    fun parseCallExpression(prevExpr: Expression, leftParenToken: LeftParenToken): Expression {
        // If parseCallExpression is called, the previous token must have been a (
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

        // If a variable is called create a call expression
        if (prevExpr is VariableExpression) {
            return FunctionCallExpression(prevExpr, actualArgs, prevExpr.identLocation)
        // If a type constructor is called, create a type constructor expression with arguments
        } else if (prevExpr is TypeConstructorExpression) {
            return TypeConstructorExpression(prevExpr.adtVariant, actualArgs,
                    prevExpr.identLocation)
        } else {
            throw ParseException("Can only apply functions or type constructors", leftParenToken)
        }
    }

    fun parseKeyedAccessExpression(
        prevExpr: Expression,
        leftBracketToken: LeftBracketToken
    ): KeyedAccessExpression {
        // If parseKeyedAccessExpression is called, the previous token must have been a [
        val keyExpr = parseExpression()

        assertCurrent(TokenType.RIGHT_BRACKET)
        tokenizer.next()

        return KeyedAccessExpression(prevExpr, keyExpr, leftBracketToken.location)
    }

    fun parseAccessExpression(prevExpr: Expression, periodToken: PeriodToken): AccessExpression {
        // If parseKeyedAccessExpression is called, the previous token must have been a .
        val accessExpr = parseExpression(CALL_ACCESS_PRECEDENCE)

        return AccessExpression(prevExpr, accessExpr, periodToken.location)
    }

    fun parseVariableDefinition(isConst: Boolean, defToken: Token): VariableDefinitionStatement {
        // If parseVariableDefinition is called, the previous token must have been a let or const
        var token = tokenizer.next()

        if (token is NumToken) {
            token = tokenizer.next()

            if (token !is IdentifierToken) {
                throw ParseException("Expected identifier in variable definition", token)
            }

            val identToken = token

            assertCurrent(TokenType.EQUALS)
            tokenizer.next()

            // Const variable definitions are immutable, all others are mutable.
            val identProps: MutableSet<IdentifierProperty> = hashSetOf(IdentifierProperty.NUMERIC)
            if (isConst) {
                identProps.add(IdentifierProperty.IMMUTABLE)
            }

            // Parse the expression and then add the ident to the symbol table, so that the old
            // symbol for the ident will be used in the body (if the ident is being rebound).
            val expr = parseExpression()
            val ident = symbolTable.addSymbol(identToken.str, IdentifierClass.VARIABLE,
                    FloatType, identProps)
            
            return VariableDefinitionStatement(ident, expr, identToken.location, defToken.location)
        } else {
            if (token !is IdentifierToken) {
                throw ParseException("Expected identifier in variable definition", token)
            }

            val identToken = token

            // Parse type if one is specified, otherwise create new type variable
            val type = if (tokenizer.current is ColonToken) {
                parseTypeAnnotation()
            } else {
                TypeVariable()
            }

            // Const variable definitions are immutable, all others are mutable
            val identProps = if (isConst) hashSetOf(IdentifierProperty.IMMUTABLE) else hashSetOf()

            assertCurrent(TokenType.EQUALS)
            tokenizer.next()

            // Parse the expression and then add the ident to the symbol table, so that the old
            // symbol for the ident will be used in the body (if the ident is being rebound).
            val expr = parseExpression()
            val ident = symbolTable.addSymbol(identToken.str, IdentifierClass.VARIABLE, type,
                    identProps)
            
            return VariableDefinitionStatement(ident, expr, identToken.location, defToken.location)
        }
    }

    fun parseFunctionDefinition(defToken: DefToken): FunctionDefinitionStatement {
        // If parseFunctionDefinition is called, the previous token must have been a def
        var token = tokenizer.next()

        // Check whether function is numeric or not
        var isNumeric = false
        if (token is NumToken) {
            token = tokenizer.next()
            isNumeric = true
        }

        if (token !is IdentifierToken) {
            throw ParseException("Expected identifier in variable definition", token)
        }

        val funcToken = token
        val formalArgs: MutableList<Identifier> = mutableListOf()
        val argTypes: MutableList<Type> = mutableListOf()

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // Enter a new scope so that all variable names and types are scoped to this function
        symbolTable.enterScope()

        // Keep parsing comma separated formal argument identifiers until a right paren is found
        argsLoop@ while (tokenizer.current !is RightParenToken) {
            token = tokenizer.next()
            if (token !is IdentifierToken) {
                throw ParseException("Formal arguments must be identifiers", token)
            }

            // Only parse types if the function is non-numeric, otherwise must be floats
            val argType = if (isNumeric) {
                FloatType
            } else if (tokenizer.current is ColonToken) {
                parseTypeAnnotation(true)
            } else {
                TypeVariable()
            }

            // Add formal argument as variable to symbol table in new scope
            argTypes.add(argType)
            formalArgs.add(symbolTable.addSymbol(token.str, IdentifierClass.VARIABLE, argType))

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

        // If function is numeric, it must return float. Otherwise find the type.
        val returnType = if (isNumeric) {
            FloatType
        } else if (tokenizer.current is ColonToken) {
            parseTypeAnnotation(true)
        } else {
            TypeVariable()
        }

        // Add the function to the symbol table with correct type before parsing body, and make
        // sure to add in previous scope, as symbolTable is currently in the scope of the function.
        val ident = symbolTable.addSymbolInPreviousScope(funcToken.str, IdentifierClass.FUNCTION,
                FunctionType(argTypes, returnType))

        // Expression function definition bodies begin with an equals sign
        if (tokenizer.current is EqualsToken) {
            tokenizer.next()

            val expr = parseExpression()
            symbolTable.exitScope()
            return FunctionDefinitionStatement(ident, formalArgs,
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
            return FunctionDefinitionStatement(ident, formalArgs, block, funcToken.location,
                    defToken.location)
        }
    }

    fun parseBlock(leftBraceToken: LeftBraceToken): BlockStatement {
        // If parseBlock is called, the previous token must have been a {
        symbolTable.enterScope()

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
        val condition = parseExpression()
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
        val condition = parseExpression()
        val statement = parseStatement()

        return WhileStatement(condition, statement, whileToken.location)
    }

    fun parseDoWhileStatement(doToken: DoToken): DoWhileStatement {
        // If parseDoWhileStatement is called, the previous token must have been a while
        val statement = parseStatement()

        assertCurrent(TokenType.WHILE)
        tokenizer.next()

        val condition = parseExpression()

        return DoWhileStatement(condition, statement, doToken.location)
    }

    fun parseForStatement(forToken: ForToken): ForStatement {
        // If parseForStatement is called, the previous token must have been a for
        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // Entire for loop must be in a new scope, since the init and update could introduce
        // new bindings that should not exist outside the context of the for loop.
        symbolTable.enterScope()

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

    fun parsePattern(): Expression {
        var token = tokenizer.next()
        return when (token) {
            // Patterns only consist of literals and variables
            is IntLiteralToken -> IntLiteral(token.num, token.location)
            is FloatLiteralToken -> FloatLiteral(token.num, token.location)
            is StringLiteralToken -> StringLiteralExpression(token.str, token.location)
            is TrueToken -> BoolLiteralExpression(true, token.location)
            is FalseToken -> BoolLiteralExpression(false, token.location)
            is LeftBracketToken -> parseVectorLiteralExpression(true, token)
            is LeftParenToken -> parseParenthesizedExpression(true, token)
            // An identifier may be a new variable or a type constructor
            is IdentifierToken -> {
                // If unseen identifier, create new identifier
                val variantIdent = symbolTable.lookup(token.str)
                if (variantIdent == null) {
                    val ident = symbolTable.addSymbol(token.str, IdentifierClass.VARIABLE,
                            TypeVariable())
                    return VariableExpression(ident, token.location)
                }

                // If identifier is seen but not adt variant, create new identifier
                val variantInfo = symbolTable.getInfo(variantIdent)
                if (variantInfo?.idClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
                    val ident = symbolTable.addSymbol(token.str, IdentifierClass.VARIABLE,
                            TypeVariable())
                    return VariableExpression(ident, token.location)
                }

                // If identifier is for type constructor, parse (optional) comma separated list
                // of patterns with parentheses
                val adtVariant = variantInfo.adtVariant
                val args: MutableList<Expression> = mutableListOf()

                if (tokenizer.current is LeftParenToken) {
                    do {
                        tokenizer.next()
                        args.add(parsePattern())
                    } while (tokenizer.current is CommaToken)

                    assertCurrent(TokenType.RIGHT_PAREN)
                    tokenizer.next()
                }

                return TypeConstructorExpression(adtVariant, args, token.location)
            }
            else -> throw ParseException("Patterns must only consist of literals and variables",
                        token)
        }
    }

    fun parseMatchStatement(matchToken: MatchToken): MatchStatement {
        // If parseMatchStatement is called, the previous token must have been a match.
        val matchExpr = parseExpression()

        assertCurrent(TokenType.LEFT_BRACE)
        tokenizer.next()

        val patterns: MutableList<Expression> = mutableListOf()
        val statements: MutableList<Statement> = mutableListOf()

        // Parse nonempty list of cases
        do {
            // Each case should be in its own scope
            symbolTable.enterScope()

            // Pipe for first case is optional, but required for all other cases
            if (patterns.size != 0 || tokenizer.current is PipeToken) {
                assertCurrent(TokenType.PIPE)
                tokenizer.next()
            }

            // Rest of case is pattern and statement separated by an arrow
            patterns.add(parsePattern())

            assertCurrent(TokenType.ARROW)
            tokenizer.next()

            statements.add(parseStatement())

            symbolTable.exitScope()
        } while (tokenizer.current !is RightBraceToken)

        tokenizer.next()

        return MatchStatement(matchExpr, patterns.zip(statements), matchToken.location)
    }

    fun parseReturnStatement(returnToken: ReturnToken): ReturnStatement {
        // If parseReturnStatement is called, the previous token must have been a return.
        // If unit is reurned, do not store a return expression.
        if (tokenizer.current is UnitToken) {
            tokenizer.next()
            return ReturnStatement(null, returnToken.location)
        } else {
            return ReturnStatement(parseExpression(), returnToken.location)
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Parsing functions for types
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Parse the current stream into a single top-level types, which may contain
     * arbitrarily nested types.
     */
    fun parseType(inFunctionDef: Boolean = false): Type {
        val funcTypes: MutableList<Type> = mutableListOf(parseNestedType(inFunctionDef))

        // Parse arrow separated list of types
        while (tokenizer.current is ArrowToken) {
            tokenizer.next()
            funcTypes.add(parseNestedType(inFunctionDef))
        }

        // If only one type is found return it, otherwise construct function type
        if (funcTypes.size > 1) {
            val returnType = funcTypes.removeAt(funcTypes.lastIndex)
            return FunctionType(funcTypes, returnType)
        } else {
            return funcTypes[0]
        }
    }

    /**
     * Parse the current stream into a single nested type, that may contain arbitrarily
     * nested types.
     */
    fun parseNestedType(inFunctionDef: Boolean = false): Type {
        val token = tokenizer.next()
        return when (token) {
            is BoolToken -> BoolType
            is StringTypeToken -> StringType
            is IntToken -> IntType
            is FloatToken -> FloatType
            is UnitToken -> UnitType
            is VecToken -> parseVectorType(inFunctionDef)
            is LeftParenToken -> parseParenthesizedType(inFunctionDef)
            is IdentifierToken -> parseIdentiferType(token, inFunctionDef)
            else -> throw ParseException("Expected type, got ${token}", token)
        }
    }

    fun parseVectorType(inFunctionDef: Boolean = false): VectorType {
        // If parseVectorType is called, previous token must have been vec.
        // Parse element type surrounded by angle braces.
        assertCurrent(TokenType.LESS_THAN)
        tokenizer.next()

        val elementType = parseType(inFunctionDef)

        assertCurrent(TokenType.GREATER_THAN)
        tokenizer.next()

        return VectorType(elementType)
    }

    fun parseParenthesizedType(inFunctionDef: Boolean = false): Type {
        // If parseParenthesizedType is called, previous token must have been (.
        // Regular types or tuple types can be surrounded in parentheses
        val nestedTypes: MutableList<Type> = mutableListOf(
                parseType(inFunctionDef))

        // Parse a comma separated list of types
        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            nestedTypes.add(parseType(inFunctionDef))
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        // If only one type is found return that type, otherwise return tuple type
        if (nestedTypes.size > 1) {
            return TupleType(nestedTypes)
        } else {
            return nestedTypes[0]
        }
    }

    /**
     * Parse the type represented by a particular identifier. This could be an existing type
     * parameter, a new type parameter (if in a function definition), or the beginning of a
     * defined algebraic data type.
     */
    fun parseIdentiferType(token: IdentifierToken, inFunctionDef: Boolean = false): Type {
        val ident = symbolTable.lookup(token.str)
        if (ident != null) {
            val identInfo = symbolTable.getInfo(ident)
            // If this is a type parameter that has been defined, then use the stored type variable
            if (identInfo?.idClass == IdentifierClass.TYPE_PARAMETER) {
                return identInfo.type
            // If this is an algebraic data type, created parameterized adt type
            } else if (identInfo?.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE) {
                val adt = identInfo.type
                if (adt !is AlgebraicDataType) {
                    throw ParseException("Expected ${token.str} to be an algebraic data type",
                            token)
                }

                // Parse optional, comma separated list of types within < > 
                // for parameterized variant type
                val typeParams: MutableList<Type> = mutableListOf()
                if (tokenizer.current is LessThanToken) {
                    do {
                        tokenizer.next()
                        typeParams.add(parseType(inFunctionDef))
                    } while (tokenizer.current is CommaToken)

                    assertCurrent(TokenType.GREATER_THAN)
                    tokenizer.next()
                }

                return adt.adtSig.getAdtWithParams(typeParams)
            } else {
                throw ParseException("Expected ${token.str} to be a type parameter", token)
            }
        } else if (inFunctionDef) {
            // If this type parameter has not been seen, create a new type variable and add
            // the type parameter to the symbol table.
            val newTypeParam = TypeVariable()
            symbolTable.addSymbol(token.str, IdentifierClass.TYPE_PARAMETER, newTypeParam)
            return newTypeParam
        } else {
            throw ParseException("Unknown type ${token.str}", token)
        }
    }

    /**
     * Parse the current type annotation in the stream into a single type.
     */
    fun parseTypeAnnotation(inFunctionDef: Boolean = false): Type {
        assertCurrent(TokenType.COLON)
        tokenizer.next()

        return parseType(inFunctionDef)
    }

    fun parseTypeDefinition() {
        // If parseTypeDefinition is called, the previous token must have been a type
        var currentToken = tokenizer.current
        if (currentToken !is IdentifierToken) {
            throw ParseException("Expected ${currentToken} to be an identifier", currentToken)
        }

        val typeName = currentToken.str
        val typeParams: MutableList<TypeVariable> = mutableListOf()

        tokenizer.next()

        // Enter a new scope for the duration of this definition, so that type params will be local
        symbolTable.enterScope()
        
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

                // Add the type parameter to the local environment and save it in the list of params
                val typeParam = TypeVariable()
                typeParams.add(typeParam)
                symbolTable.addSymbol(currentToken.str, IdentifierClass.TYPE_PARAMETER, typeParam)

                tokenizer.next()
            } while (tokenizer.current is CommaToken)

            assertCurrent(TokenType.GREATER_THAN)
            tokenizer.next()
        }

        // Create adt type signature based off name and type params, and add to global scope
        val adtSig = AlgebraicDataTypeSignature(typeName, typeParams)
        addAdtSigToSymbolTable(adtSig, symbolTable)

        assertCurrent(TokenType.EQUALS)
        tokenizer.next()

        assertCurrent(TokenType.LEFT_BRACE)
        tokenizer.next()

        var firstVariant = true

        // Parse nonempty sequence of variant definitions
        do {
            // Pipe before the first variant is optional, but required for all other variants
            if (!firstVariant || tokenizer.current is PipeToken) {
                assertCurrent(TokenType.PIPE)
                tokenizer.next()
            }

            firstVariant = false
            currentToken = tokenizer.current

            if (currentToken !is IdentifierToken) {
                throw ParseException("Type parameters must identifiers", currentToken)
            }

            val variantName = currentToken.str
            tokenizer.next()

            // Parse optional type constructor arguments
            val typeAnnotation = if (tokenizer.current is LeftParenToken) {
                parseType()
            } else {
                null
            }

            // If tuple type then constructor is elements of tuple, otherwise use parsed type
            // if it exists, or empty constructor if no type annotation was provided
            val typeConstructor = if (typeAnnotation is TupleType) {
                typeAnnotation.elementTypes
            } else if (typeAnnotation != null) {
                listOf(typeAnnotation)
            } else {
                listOf()
            }

            // Create a new variant type and add it to the global scope
            val adtVariant = AlgebraicDataTypeVariant(adtSig, variantName, typeConstructor)
            adtSig.variants.add(adtVariant)
            addAdtVariantToSymbolTable(adtVariant, symbolTable)
        } while (tokenizer.current !is RightBraceToken)

        tokenizer.next()

        symbolTable.exitScope()
    }

}
