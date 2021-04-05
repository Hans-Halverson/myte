#pragma once

#include "parser/Lexer.h"
#include "parser/ast/Expression.h"

class Parser {
 public:
  Parser(std::unique_ptr<Lexer>);

  std::unique_ptr<Expression> parseExpression();

 private:
  std::unique_ptr<Lexer> lexer_;
  LexResult lexResult_;

  Token token() { return this->lexResult_.token; }
  Loc *loc() { return this->lexResult_.loc; }

  void advance();

  std::unique_ptr<Expression> parseExpressionPrefix();
  std::unique_ptr<Expression> parseExpressionInfix(std::unique_ptr<Expression>);
  std::unique_ptr<BinaryOperationExpression> parseBinaryOperationExpression(
      std::unique_ptr<Expression>, BinaryOperator);
};