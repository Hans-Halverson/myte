package myte.ir

import myte.ir.nodes.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

val NEGATIVE_ONE_NODE = ConstantNode(-1.0)

class AstToIrConverter(val symbolTable: SymbolTable) {

	fun convertStatement(stmt: Statement): IRNode {
		return when {
			stmt is DefineNumericFunctionStatement -> DefineNumericFunction(stmt.ident,
																	        stmt.formalArgs,
																	        convertNumericExpression(stmt.expr))
			stmt is DefineNumericVariableStatement -> DefineNumericVariable(stmt.ident,
																	        convertNumericExpression(stmt.expr))
			stmt is FunctionDefinitionStatement -> FunctionDefinitionNode(stmt.ident,
																	      stmt.formalArgs,
																	      convertStatement(stmt.stmt))
			stmt is VariableDefinitionStatement -> VariableDefinitionNode(stmt.ident,
																		  convertStatement(stmt.expr))
			stmt is BlockStatement -> BlockNode(stmt.stmts.map(this::convertStatement))
			stmt is IfStatement -> IfNode(convertBooleanExpression(stmt.cond),
				                          convertStatement(stmt.conseq),
				                          if (stmt.altern != null) convertStatement(stmt.altern) else null)
			stmt is WhileStatement -> WhileNode(convertBooleanExpression(stmt.cond),
										        convertStatement(stmt.stmt))
			stmt is DoWhileStatement -> DoWhileNode(convertBooleanExpression(stmt.cond),
				                                    convertStatement(stmt.stmt))
			stmt is ForStatement -> ForNode(if (stmt.init != null) convertStatement(stmt.init) else null,
				                            if (stmt.cond != null) convertBooleanExpression(stmt.cond) else null,
									        if (stmt.update != null) convertStatement(stmt.update) else null,
									        convertStatement(stmt.stmt))
			stmt is IdentifierExpression -> VariableNode(stmt.ident)
			stmt is CallExpression && isFunction(stmt.func) ->
				FunctionCallNode(stmt.func, stmt.actualArgs.map(this::convertStatement))
			stmt is AssignmentExpression && isVariable(stmt.ident) ->
				AssignmentNode(stmt.ident, convertStatement(stmt.expr))
			stmt is Expression -> {
				try {
					return convertBooleanExpression(stmt)
				} catch (e: IRConversionException) {
					return convertNumericExpression(stmt)
				}			
			}
			else -> {
				throw IRConversionException("Unexpected statement ${stmt}")
			}
		}
	}

	fun convertBooleanExpression(expr: Expression): IRBooleanNode {
		return when (expr) {
			is BooleanLiteralExpression -> BooleanLiteralNode(expr.bool)
			is EqualsExpression -> EqualsNode(convertStatement(expr.left),
											  convertStatement(expr.right))
			is NotEqualsExpression -> NotEqualsNode(convertStatement(expr.left),
											        convertStatement(expr.right))
			is LessThanExpression -> LessThanNode(convertNumericExpression(expr.left),
											      convertNumericExpression(expr.right))
			is LessThanOrEqualExpression -> LessThanOrEqualNode(convertNumericExpression(expr.left),
											                    convertNumericExpression(expr.right))
			is GreaterThanExpression -> GreaterThanNode(convertNumericExpression(expr.left),
											            convertNumericExpression(expr.right))
			is GreaterThanOrEqualExpression -> GreaterThanOrEqualNode(convertNumericExpression(expr.left),
											                          convertNumericExpression(expr.right))
			is LogicalAndExpression -> LogicalAndNode(convertBooleanExpression(expr.left),
													  convertBooleanExpression(expr.right))
			is LogicalOrExpression -> LogicalOrNode(convertBooleanExpression(expr.left),
												    convertBooleanExpression(expr.right))
			is LogicalNotExpression -> LogicalNotNode(convertBooleanExpression(expr.expr))
			is GroupExpression -> convertBooleanExpression(expr.expr)
			else -> throw IRConversionException("Expected expression that evaluates to boolean, got ${expr}")
		}
	}

	fun convertNumericExpression(expr: Expression): IRNumericNode {
		return when {
			expr is NumberLiteral -> ConstantNode(expr.num)
			expr is IdentifierExpression && isNumber(expr.ident) -> NumericVariableNode(expr.ident)
			expr is AddExpression -> AddNode(convertNumericExpression(expr.left),
				                             convertNumericExpression(expr.right))
			expr is SubtractExpression -> SubtractNode(convertNumericExpression(expr.left),
												       convertNumericExpression(expr.right))
			expr is MultiplyExpression -> MultiplyNode(convertNumericExpression(expr.left),
				                                       convertNumericExpression(expr.right))
			expr is DivideExpression -> DivideNode(convertNumericExpression(expr.numer),
				                                   convertNumericExpression(expr.denom))
			expr is ExponentExpression -> ExponentNode(convertNumericExpression(expr.base),
				                                       convertNumericExpression(expr.exponent))
			expr is UnaryPlusExpression -> convertNumericExpression(expr.expr)
			expr is UnaryMinusExpression -> MultiplyNode(NEGATIVE_ONE_NODE,
				                                         convertNumericExpression(expr.expr))
			expr is GroupExpression -> convertNumericExpression(expr.expr)
			expr is CallExpression && isNumericFunction(expr.func) ->
				NumericCallNode(expr.func, expr.actualArgs.map(this::convertNumericExpression))
			expr is AssignmentExpression && isNumber(expr.ident) ->
				NumericAssignmentNode(expr.ident, convertNumericExpression(expr.expr))
			else -> throw NumericException("Mixed numerics and non-numerics")
		}
	}

	fun isNumber(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.NUMBER
	}

	fun isVariable(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.VARIABLE
	}

	fun isNumericFunction(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.NUMERIC_FUNCTION
	}

	fun isFunction(ident: Identifier): Boolean {
		return symbolTable.getInfo(ident)?.idClass == IdentifierClass.FUNCTION
	}

}
