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
            statements.add(parseStatement())
        }

        return statements
    }

    /**
     * Parse a single command from the REPL.
     *
     * @return the single statement parsed from the input stream
     * @throws ParseException if the stream does not correspond to a valid statement, or if there
     *         are leftover tokens after the statement has been parsed
     */
    fun parseLine(): Statement {
        symbolTable.returnToGlobalScope()

        val statement = parseStatement()

        if (!tokenizer.reachedEnd) {
            throw ParseException(tokenizer.current)
        }

        return statement
    }

    /** 
     * Parse the current statement in the stream of tokens.
     */
    fun parseStatement(): Statement {
        val token = tokenizer.next()

        return when(token) {
            is LeftBraceToken -> parseBlock()
            is LetToken -> parseVariableDefinition(false)
            is ConstToken -> parseVariableDefinition(true)
            is DefToken -> parseFunctionDefinition()
            is IfToken -> parseIfStatement()
            is WhileToken -> parseWhileStatement()
            is DoToken -> parseDoWhileStatement()
            is ForToken -> parseForStatement()
            is ReturnToken -> parseReturnStatement()
            is BreakToken -> BreakStatement
            is ContinueToken -> ContinueStatement
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
    fun parseExpression(firstToken: Token?, precedence: Int = NO_PRECEDENCE): Expression {
        // Match on all tokens that signal a prefix operator
        var currentExpr = when (firstToken) {
            // Literals
            is IntLiteralToken -> IntLiteral(firstToken.num)
            is FloatLiteralToken -> FloatLiteral(firstToken.num)
            is IdentifierToken -> parseIdentifierExpression(firstToken)
            is TrueToken -> BoolLiteralExpression(true)
            is FalseToken -> BoolLiteralExpression(false)
            is StringLiteralToken -> StringLiteralExpression(firstToken.str)
            is LeftBracketToken -> parseListLiteralExpression()
            // Prefixs operators
            is PlusToken -> parseUnaryPlusExpression()
            is MinusToken -> parseUnaryMinusExpression()
            is LogicalNotToken -> parseLogicalNotExpression()
            is LeftParenToken -> parseGroupExpression()
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
                is EqualsToken -> parseAssignmentExpression(currentExpr)
                is DoubleEqualsToken -> parseEqualsExpression(currentExpr)
                is NotEqualsToken -> parseNotEqualsExpression(currentExpr)
                is LessThanToken -> parseLessThanExpression(currentExpr)
                is LessThanOrEqualToken -> parseLessThanOrEqualExpression(currentExpr)
                is GreaterThanToken -> parseGreaterThanExpression(currentExpr)
                is GreaterThanOrEqualToken -> parseGreaterThanOrEqualExpression(currentExpr)
                // Logical operators
                is LogicalAndToken -> parseLogicalAndExpression(currentExpr)
                is LogicalOrToken -> parseLogicalOrExpression(currentExpr)
                // Function call
                is LeftParenToken -> parseCallExpression(currentExpr)
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
            throw ParseException(tokenType, tokenizer.current.type)
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // 
    // Parsing functions for statement and expression
    //
    ///////////////////////////////////////////////////////////////////////////

    fun parseIdentifierExpression(token: IdentifierToken): IdentifierExpression {
        val ident = symbolTable.lookup(token.str)
        if (ident == null) {
            throw ParseException("No identifier found for symbol ${token.str}")
        }

        return IdentifierExpression(ident)
    }

    fun parseListLiteralExpression(): ListLiteralExpression {
        // If parseListLiteralExpression is called, the previous token must have been a [
        val elements: MutableList<Expression> = mutableListOf()

        // Return empty list if no list elements are encountered
        if (tokenizer.current is RightBracketToken) {
            tokenizer.next()
            return ListLiteralExpression(listOf())
        }

        // Add expressions, separated by commas, until a right bracket is encountered.
        elements.add(parseExpression())

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            elements.add(parseExpression())
        }

        assertCurrent(TokenType.RIGHT_BRACKET)
        tokenizer.next()

        return ListLiteralExpression(elements)
    }

    fun parseUnaryPlusExpression(): UnaryPlusExpression {
        // If parseUnaryPlusExpression is called, the previous token must have been a +
        val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
        return UnaryPlusExpression(expr)
    }

    fun parseUnaryMinusExpression(): UnaryMinusExpression {
        // If parseUnaryMinusExpression is called, the previous token must have been a -
        val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
        return UnaryMinusExpression(expr)
    }

    fun parseLogicalNotExpression(): LogicalNotExpression {
        // If parseLogicalNotExpression is called, the previous token must have been a !
        val expr = parseExpression(LOGICAL_NOT_PRECEDENCE)
        return LogicalNotExpression(expr)
    }

    fun parseGroupExpression(): GroupExpression {
        // If parseGroupExpression is called, the previous token must have been a (
        val expr = parseExpression()

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return GroupExpression(expr)
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
        val expr = parseExpression(EXPONENT_PRECEDENCE - 1)
        return ExponentExpression(prevExpr, expr)
    }

    fun parseAssignmentExpression(prevExpr: Expression): AssignmentExpression {
        // If parseAssignmentExpression is called, the previous token must have been a =
        if (prevExpr !is IdentifierExpression) {
            throw ParseException("Can only assign to identifiers, attempted to "
                    + "assign to ${prevExpr}")
        }
        
        val expr = parseExpression(ASSIGNMENT_PRECEDENCE)
        return AssignmentExpression(prevExpr.ident, expr)
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

    fun parseCallExpression(prevExpr: Expression): CallExpression {
        // If parseCallExpression is called, the previous token must have been a (
        if (prevExpr !is IdentifierExpression) {
            throw ParseException("Function name must be an identifier")
        }

        val actualArgs: MutableList<Expression> = mutableListOf()

        // If no arguments are supplied, create function call with no argument list
        if (tokenizer.current is RightParenToken) {
            tokenizer.next()
            return CallExpression(prevExpr.ident, listOf())
        }

        // Add all arguments (comma separated expressions) until the next right paren is encountered
        actualArgs.add(parseExpression())

        while (tokenizer.current is CommaToken) {
            tokenizer.next()
            actualArgs.add(parseExpression())
        }

        assertCurrent(TokenType.RIGHT_PAREN)
        tokenizer.next()

        return CallExpression(prevExpr.ident, actualArgs)
    }

    /**
     * Parse the current stream into a single top-level type expression, which may contain
     * arbitrarily nested types.
     */
    fun parseType(): TypeExpression {
        val types: MutableList<TypeExpression> = mutableListOf(parseNestedType())

        // If not a function type, only parse first complete type
        if (tokenizer.current !is ArrowToken) {
            return types[0]
        }

        // If a function type, add arrow separated types as args and return type of function type
        do {
            tokenizer.next()
            types.add(parseNestedType())
        } while (tokenizer.current is ArrowToken)

        val returnType = types.removeAt(types.lastIndex)
        return FunctionTypeExpression(types, returnType)
    }

    /**
     * Parse the current stream into a single nested type expression, that may contain arbitrarily
     * nested types.
     */
    fun parseNestedType(): TypeExpression {
        val token = tokenizer.next()
        return when (token) {
            is BoolToken -> BoolTypeExpression
            is StringTypeToken -> StringTypeExpression
            is IntToken -> IntTypeExpression
            is FloatToken -> FloatTypeExpression
            is UnitToken -> UnitTypeExpression
            is ListToken -> {
                assertCurrent(TokenType.LESS_THAN)
                tokenizer.next()

                val elementType = parseType()

                assertCurrent(TokenType.GREATER_THAN)
                tokenizer.next()

                ListTypeExpression(elementType)
            }
            // Nested types can be surrounded in parentheses - and nested function types must
            // have surrounding parentheses.
            is LeftParenToken -> {
                val nestedType = parseType()

                assertCurrent(TokenType.RIGHT_PAREN)
                tokenizer.next()

                nestedType
            }
            else -> throw ParseException("Expected type, got ${token}")
        }
    }

    /**
     * Parse the current type annotation in the stream into a single type expression.
     */
    fun parseTypeAnnotation(): TypeExpression {
        assertCurrent(TokenType.COLON)
        tokenizer.next()

        return parseType()
    }

    fun parseVariableDefinition(isConst: Boolean): Statement {
        // If parseVariableDefinition is called, the previous token must have been a let or const
        var token = tokenizer.next()

        if (token is NumToken) {
            token = tokenizer.next()

            if (token !is IdentifierToken) {
                throw ParseException("No identifier found in definition")
            }

            val identName = token.str

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
            val ident = symbolTable.addSymbol(identName, IdentifierClass.VARIABLE,
                    FloatTypeExpression, identProps)
            
            return VariableDefinitionStatement(ident, expr)
        } else {
            if (token !is IdentifierToken) {
                throw ParseException("No identifier found in definition")
            }

            val identName = token.str

            // Parse type if one is specified, otherwise create new type variable
            val typeExpr = if (tokenizer.current is ColonToken) {
                parseTypeAnnotation()
            } else {
                newTypeVariable()
            }

            // Const variable definitions are immutable, all others are mutable
            val identProps = if (isConst) hashSetOf(IdentifierProperty.IMMUTABLE) else hashSetOf()

            assertCurrent(TokenType.EQUALS)
            tokenizer.next()

            // Parse the expression and then add the ident to the symbol table, so that the old
            // symbol for the ident will be used in the body (if the ident is being rebound).
            val expr = parseExpression()
            val ident = symbolTable.addSymbol(identName, IdentifierClass.VARIABLE,
                    typeExpr, identProps)
            
            return VariableDefinitionStatement(ident, expr)
        }
    }

    fun parseFunctionDefinition(): Statement {
        // If parseFunctionDefinition is called, the previous token must have been a def
        var token = tokenizer.next()

        // Check whether function is numeric or not
        var isNumeric = false
        if (token is NumToken) {
            token = tokenizer.next()
            isNumeric = true
        }

        if (token !is IdentifierToken) {
            throw ParseException("No identifier found in function definition")
        }

        val funcName = token.str
        val argNames: MutableList<String> = mutableListOf()
        val argTypes: MutableList<TypeExpression> = mutableListOf()

        assertCurrent(TokenType.LEFT_PAREN)
        tokenizer.next()

        // Keep parsing comma separated formal argument identifiers until a right paren is found
        argsLoop@ while (tokenizer.current !is RightParenToken) {
            token = tokenizer.next()
            when (token) {
                is IdentifierToken -> {
                    // Only parse types if the function is non-numeric, otherwise must be floats
                    argNames.add(token.str)
                    if (isNumeric) {
                        argTypes.add(FloatTypeExpression)
                    } else {
                        argTypes.add(parseTypeAnnotation())
                    }
                }
                else -> throw ParseException("Formal arguments must be identifiers")
            }

            // If a right paren is found, all arguments have been found. If a comma is found,
            // there must still be identifiers to parse. Otherwise, synatix is invalid.
            when (tokenizer.current) {
                is RightParenToken -> break@argsLoop
                is CommaToken -> tokenizer.next()
                else -> throw ParseException(tokenizer.current)
            }
        }

        tokenizer.next()

        // If function is numeric, it must return float. Otherwise find the type.
        val returnType = if (isNumeric) FloatTypeExpression else parseTypeAnnotation()

        // Add the symbol to the symbol table with correct type before parsing body, then enter
        // scope so that function body is always in new scope and can recursively reference ident.
        val ident = symbolTable.addSymbol(funcName, IdentifierClass.FUNCTION,
                FunctionTypeExpression(argTypes, returnType))
        symbolTable.enterScope()

        // Add all formal arguments as variables to symbol table in new scope.
        val formalArgs: MutableList<Identifier> = mutableListOf()
        for ((argName, argType) in argNames.zip(argTypes)) {
            formalArgs.add(symbolTable.addSymbol(argName, IdentifierClass.VARIABLE, argType))
        }

        // Expression function definition bodies begin with an equals sign
        if (tokenizer.current is EqualsToken) {
            tokenizer.next()

            val expr = parseExpression()
            symbolTable.exitScope()
            return FunctionDefinitionExpression(ident, formalArgs, expr)
        } else {
            // Non-expression function definition bodies must consist of a single block
            assertCurrent(TokenType.LEFT_BRACE)
            tokenizer.next()

            val block = parseBlock()
            symbolTable.exitScope()
            return FunctionDefinitionStatement(ident, formalArgs, block)
        }
    }

    fun parseBlock(): BlockStatement {
        // If parseBlock is called, the previous token must have been a {
        symbolTable.enterScope()

        // Keep parsing statements in a new scope until a right brace is encountered
        val statements: MutableList<Statement> = mutableListOf()
        while (tokenizer.current !is RightBraceToken) {
            statements.add(parseStatement())
        }

        tokenizer.next()
        symbolTable.exitScope()

        return BlockStatement(statements)
    }

    fun parseIfStatement(): IfStatement {
        // If parseIfStatement is called, the previous token must have been an if
        val condition = parseExpression()
        val consequent = parseStatement()
        var alternative: Statement? = null

        // If an else is encountered parse it, otherwise alternative does not exist
        if (!tokenizer.reachedEnd && tokenizer.current is ElseToken) {
            tokenizer.next()
            alternative = parseStatement()
        }

        return IfStatement(condition, consequent, alternative)
    }

    fun parseWhileStatement(): WhileStatement {
        // If parseWhileStatement is called, the previous token must have been a while
        val condition = parseExpression()
        val statement = parseStatement()

        return WhileStatement(condition, statement)
    }

    fun parseDoWhileStatement(): DoWhileStatement {
        // If parseDoWhileStatement is called, the previous token must have been a while
        val statement = parseStatement()

        assertCurrent(TokenType.WHILE)
        tokenizer.next()

        val condition = parseExpression()

        return DoWhileStatement(condition, statement)
    }

    fun parseForStatement(): ForStatement {
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

        return ForStatement(initializer, condition, update, statement)
    }

    fun parseReturnStatement(): ReturnStatement {
        // If parseReturnStatement is called, the previous token must have been a return.
        // If unit is reurned, do not store a return expression.
        if (tokenizer.current is UnitToken) {
            tokenizer.next()
            return ReturnStatement(null)
        } else {
            return ReturnStatement(parseExpression())
        }
    }
}
