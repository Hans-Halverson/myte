package myte.parser

import myte.lexer.*
import myte.parser.ast.*
import myte.shared.*

class Parser(val symbolTable: SymbolTable, tokens: List<Token> = listOf()) {
	var tokenizer: Tokenizer = Tokenizer(tokens)

	fun setTokens(newTokens: List<Token>) {
		tokenizer = Tokenizer(newTokens)
	}

	fun parseFile(): List<Statement> {
		symbolTable.returnToGlobalScope()

		val statements: MutableList<Statement> = mutableListOf()
		while (!tokenizer.reachedEnd) {
			statements.add(parseStatement())
		}

		return statements
	}

	fun parseLine(): Statement {
		symbolTable.returnToGlobalScope()

		val statement = parseStatement()

		if (!tokenizer.reachedEnd) {
			throw ParseException(tokenizer.current)
		}

		return statement
	}

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
			else -> parseExpression(token)
		}
	}

	fun parseExpression(precedence: Int = 0) = parseExpression(tokenizer.next(), precedence)

	fun parseExpression(firstToken: Token?, precedence: Int = 0): Expression {
		// Match on all tokens that signal a prefix operator
		var currentExpr = when (firstToken) {
			// Literals
			is IntLiteralToken -> IntLiteral(firstToken.num)
			is FloatLiteralToken -> FloatLiteral(firstToken.num)
			is IdentifierToken -> parseIdentifierExpression(firstToken)
			is TrueToken -> BooleanLiteralExpression(true)
			is FalseToken -> BooleanLiteralExpression(false)
			is StringLiteralToken -> StringLiteralExpression(firstToken.str)
			is LeftBracketToken -> parseListLiteralExpression()
			// Unary operators
			is PlusToken -> parseUnaryPlusExpression()
			is MinusToken -> parseUnaryMinusExpression()
			is LogicalNotToken -> parseLogicalNotExpression()
			is LeftParenToken -> parseGroupExpression()
			else -> throw ParseException(firstToken)
		}

		while (!tokenizer.reachedEnd && precedence < getPrecedenceForInfixToken(tokenizer.current.type)) {
			// Match on all tokens that signal an infix operator
			val token = tokenizer.next()
			currentExpr = when (token) {
				is PlusToken -> parseAddExpression(currentExpr)
				is MinusToken -> parseSubtractExpression(currentExpr)
				is AsteriskToken -> parseMultiplyExpression(currentExpr)
				is ForwardSlashToken -> parseDivideExpression(currentExpr)
				is CaretToken -> parseExponentExpression(currentExpr)
				is EqualsToken -> parseAssignmentExpression(currentExpr)
				is DoubleEqualsToken -> parseEqualsExpression(currentExpr)
				is NotEqualsToken -> parseNotEqualsExpression(currentExpr)
				is LessThanToken -> parseLessThanExpression(currentExpr)
				is LessThanOrEqualToken -> parseLessThanOrEqualExpression(currentExpr)
				is GreaterThanToken -> parseGreaterThanExpression(currentExpr)
				is GreaterThanOrEqualToken -> parseGreaterThanOrEqualExpression(currentExpr)
				is LogicalAndToken -> parseLogicalAndExpression(currentExpr)
				is LogicalOrToken -> parseLogicalOrExpression(currentExpr)
				is LeftParenToken -> parseCallExpression(currentExpr)
				else -> throw ParseException(token)
			}
		}

		return currentExpr
	}

	fun assertCurrent(tokenType: TokenType) {
		if (tokenizer.current.type != tokenType) {
			throw ParseException(tokenType, tokenizer.current.type)
		}
	}


	fun parseIdentifierExpression(token: IdentifierToken): IdentifierExpression {
		val ident = symbolTable.lookup(token.str)
		if (ident == null) {
			throw ParseException("No identifier found for symbol ${token.str}")
		}

		return IdentifierExpression(ident)
	}

	fun parseListLiteralExpression(): ListLiteralExpression {
		val elements: MutableList<Expression> = mutableListOf()

		if (tokenizer.current is RightBracketToken) {
			tokenizer.next()
			return ListLiteralExpression(listOf())
		}

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
		val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
		return UnaryPlusExpression(expr)
	}

	fun parseUnaryMinusExpression(): UnaryMinusExpression {
		val expr = parseExpression(NUMERIC_PREFIX_PRECEDENCE)
		return UnaryMinusExpression(expr)
	}

	fun parseLogicalNotExpression(): LogicalNotExpression {
		val expr = parseExpression(LOGICAL_NOT_PRECEDENCE)
		return LogicalNotExpression(expr)
	}

	fun parseGroupExpression(): GroupExpression {
		val expr = parseExpression()

		assertCurrent(TokenType.RIGHT_PAREN)
		tokenizer.next()

		return GroupExpression(expr)
	}

	fun parseAddExpression(prevExpr: Expression): AddExpression {
		val expr = parseExpression(ADD_PRECEDENCE)
		return AddExpression(prevExpr, expr)
	}

	fun parseSubtractExpression(prevExpr: Expression): SubtractExpression {
		val expr = parseExpression(ADD_PRECEDENCE)
		return SubtractExpression(prevExpr, expr)
	}

	fun parseMultiplyExpression(prevExpr: Expression): MultiplyExpression {
		val expr = parseExpression(MULTIPLY_PRECEDENCE)
		return MultiplyExpression(prevExpr, expr)
	}

	fun parseDivideExpression(prevExpr: Expression): DivideExpression {
		val expr = parseExpression(MULTIPLY_PRECEDENCE)
		return DivideExpression(prevExpr, expr)
	}

	fun parseExponentExpression(prevExpr: Expression): ExponentExpression {
		val expr = parseExpression(EXPONENT_PRECEDENCE - 1)
		return ExponentExpression(prevExpr, expr)
	}

	fun parseAssignmentExpression(prevExpr: Expression): AssignmentExpression {
		if (prevExpr !is IdentifierExpression) {
			throw ParseException("Can only assign to identifiers, attempted to assign to ${prevExpr}")
		}
		
		val expr = parseExpression(ASSIGNMENT_PRECEDENCE)
		return AssignmentExpression(prevExpr.ident, expr)
	}

	fun parseEqualsExpression(prevExpr: Expression): EqualsExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return EqualsExpression(prevExpr, expr)
	}

	fun parseNotEqualsExpression(prevExpr: Expression): NotEqualsExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return NotEqualsExpression(prevExpr, expr)
	}

	fun parseLessThanExpression(prevExpr: Expression): LessThanExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return LessThanExpression(prevExpr, expr)
	}

	fun parseLessThanOrEqualExpression(prevExpr: Expression): LessThanOrEqualExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return LessThanOrEqualExpression(prevExpr, expr)
	}

	fun parseGreaterThanExpression(prevExpr: Expression): GreaterThanExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return GreaterThanExpression(prevExpr, expr)
	}

	fun parseGreaterThanOrEqualExpression(prevExpr: Expression): GreaterThanOrEqualExpression {
		val expr = parseExpression(COMPARISON_PRECEDENCE)
		return GreaterThanOrEqualExpression(prevExpr, expr)
	}

	fun parseLogicalAndExpression(prevExpr: Expression): LogicalAndExpression {
		val expr = parseExpression(LOGICAL_AND_PRECEDENCE)
		return LogicalAndExpression(prevExpr, expr)
	}

	fun parseLogicalOrExpression(prevExpr: Expression): LogicalOrExpression {
		val expr = parseExpression(LOGICAL_OR_PRECEDENCE)
		return LogicalOrExpression(prevExpr, expr)
	}

	fun parseCallExpression(prevExpr: Expression): CallExpression {
		if (prevExpr !is IdentifierExpression) {
			throw ParseException("Function name must be an identifier")
		}

		val actualArgs: MutableList<Expression> = mutableListOf()

		if (tokenizer.current is RightParenToken) {
			tokenizer.next()
			return CallExpression(prevExpr.ident, listOf())
		}

		actualArgs.add(parseExpression())

		while (tokenizer.current is CommaToken) {
			tokenizer.next()
			actualArgs.add(parseExpression())
		}

		assertCurrent(TokenType.RIGHT_PAREN)
		tokenizer.next()

		return CallExpression(prevExpr.ident, actualArgs)
	}

	fun parseType(): TypeExpression {
		val types: MutableList<TypeExpression> = mutableListOf(parseSingleType())

		if (tokenizer.current !is ArrowToken) {
			return types[0]
		}

		do {
			tokenizer.next()
			types.add(parseSingleType())
		} while (tokenizer.current is ArrowToken)

		val returnType = types.removeAt(types.lastIndex)
		return FunctionTypeExpression(types, returnType)
	}

	fun parseSingleType(): TypeExpression {
		val token = tokenizer.next()
		return when (token) {
			is BoolToken -> BoolTypeExpression
			is StringTypeToken -> StringTypeExpression
			is IntToken -> IntTypeExpression
			is FloatToken -> FloatTypeExpression
			is UnitToken -> UnitTypeExpression
			is LeftParenToken -> {
				val functionType = parseType()

				assertCurrent(TokenType.RIGHT_PAREN)
				tokenizer.next()

				functionType
			}
			is ListToken -> {
				assertCurrent(TokenType.LESS_THAN)
				tokenizer.next()

				val typeParam = parseType()

				assertCurrent(TokenType.GREATER_THAN)
				tokenizer.next()

				ListTypeExpression(typeParam)
			}
			else -> throw ParseException("Expected type, got ${token}")
		}
	}

	fun parseTypeAnnotation(): TypeExpression {
		assertCurrent(TokenType.COLON)
		tokenizer.next()

		return parseType()
	}

	fun parseVariableDefinition(isConst: Boolean): Statement {
		var token = tokenizer.next()

		if (token is NumToken) {
			token = tokenizer.next()

			if (token !is IdentifierToken) {
				throw ParseException("No identifier found in definition")
			}

			val identName = token.str

			assertCurrent(TokenType.EQUALS)
			tokenizer.next()

			val identProps: MutableSet<IdentifierProperty> = hashSetOf(IdentifierProperty.NUMERIC)
			if (isConst) {
				identProps.add(IdentifierProperty.IMMUTABLE)
			}

			val expr = parseExpression()
			val ident = symbolTable.addSymbol(identName, IdentifierClass.VARIABLE, FloatTypeExpression, identProps)
			
			return VariableDefinitionStatement(ident, expr)
		} else {
			if (token !is IdentifierToken) {
				throw ParseException("No identifier found in definition")
			}

			val identName = token.str

			// Parse type if one is specified, otherwise create new type variable
			val typeExpr = if (tokenizer.current is ColonToken) parseTypeAnnotation() else newTypeVariable()

			val identProps = if (isConst) hashSetOf(IdentifierProperty.IMMUTABLE) else hashSetOf()

			assertCurrent(TokenType.EQUALS)
			tokenizer.next()

			val expr = parseExpression()
			val ident = symbolTable.addSymbol(identName, IdentifierClass.VARIABLE, typeExpr, identProps)
			
			return VariableDefinitionStatement(ident, expr)
		}
	}

	fun parseFunctionDefinition(): Statement {
		var token = tokenizer.next()

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

		argsLoop@ while (tokenizer.current !is RightParenToken) {
			token = tokenizer.next()
			when (token) {
				is IdentifierToken -> {
					argNames.add(token.str)
					if (isNumeric) {
						argTypes.add(FloatTypeExpression)
					} else {
						argTypes.add(parseTypeAnnotation())
					}
				}
				else -> throw ParseException("Formal arguments must be identifiers")
			}

			when (tokenizer.current) {
				is RightParenToken -> break@argsLoop
				is CommaToken -> tokenizer.next()
				else -> throw ParseException(tokenizer.current)
			}
		}

		tokenizer.next()

		val returnType = if (isNumeric) FloatTypeExpression else parseTypeAnnotation()
		val ident = symbolTable.addSymbol(funcName, IdentifierClass.FUNCTION, FunctionTypeExpression(argTypes, returnType))
		val formalArgs: MutableList<Identifier> = mutableListOf()

		symbolTable.enterScope()

		for ((argName, argType) in argNames.zip(argTypes)) {
			formalArgs.add(symbolTable.addSymbol(argName, IdentifierClass.VARIABLE, argType))
		}

		if (tokenizer.current is EqualsToken) {
			tokenizer.next()

			val expr = parseExpression()
			symbolTable.exitScope()
			return FunctionDefinitionExpression(ident, formalArgs, expr)
		} else {
			assertCurrent(TokenType.LEFT_BRACE)
			tokenizer.next()

			val block = parseBlock()
			symbolTable.exitScope()
			return FunctionDefinitionStatement(ident, formalArgs, block)
		}
	}

	fun parseBlock(): BlockStatement {
		symbolTable.enterScope()

		val statements: MutableList<Statement> = mutableListOf()
		while (tokenizer.current !is RightBraceToken) {
			statements.add(parseStatement())
		}

		tokenizer.next()

		symbolTable.exitScope()

		return BlockStatement(statements)
	}

	fun parseIfStatement(): IfStatement {
		val condition = parseExpression()
		val consequent = parseStatement()
		var alternative: Statement? = null

		if (!tokenizer.reachedEnd && tokenizer.current is ElseToken) {
			tokenizer.next()
			alternative = parseStatement()
		}

		return IfStatement(condition, consequent, alternative)
	}

	fun parseWhileStatement(): WhileStatement {
		val condition = parseExpression()
		val statement = parseStatement()

		return WhileStatement(condition, statement)
	}

	fun parseDoWhileStatement(): DoWhileStatement {
		val statement = parseStatement()

		assertCurrent(TokenType.WHILE)
		tokenizer.next()

		val condition = parseExpression()

		return DoWhileStatement(condition, statement)
	}

	fun parseForStatement(): ForStatement {
		assertCurrent(TokenType.LEFT_PAREN)
		tokenizer.next()

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
		if (tokenizer.current is UnitToken) {
			tokenizer.next()
			return ReturnStatement(null)
		} else {
			return ReturnStatement(parseExpression())
		}
	}
}
