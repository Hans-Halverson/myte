#include "parser/Parser.h"

Parser::Parser(std::unique_ptr<Lexer> lexer) : lexer_(std::move(lexer)) {
  this->lexResult_ = this->lexer_->next();
}

void Parser::advance() { this->lexResult_ = this->lexer_->next(); }

std::unique_ptr<Expression> Parser::parseExpression() {
  std::unique_ptr<Expression> expr = this->parseExpressionPrefix();

  while (this->token() != Token::Eof) {
    expr = this->parseExpressionInfix(std::move(expr));
  }

  return expr;
}

std::unique_ptr<Expression> Parser::parseExpressionPrefix() {
  Loc *loc = this->loc();
  switch (this->token()) {
    case Token::Identifier: {
      std::string string = this->lexer_->lexString();
      this->advance();
      return std::make_unique<IdentifierExpression>(loc, this->lexer_->lexString());
    }
    case Token::IntLiteral:
      this->advance();
      return std::make_unique<IntLiteralExpression>(loc, this->lexer_->lexInt());
  }
}

std::unique_ptr<Expression> Parser::parseExpressionInfix(std::unique_ptr<Expression> left) {
  switch (this->token()) {
    case Token::Plus:
      return this->parseBinaryOperationExpression(std::move(left), BinaryOperator::Add);
    case Token::Minus:
      return this->parseBinaryOperationExpression(std::move(left), BinaryOperator::Subtract);
    case Token::Multiply:
      return this->parseBinaryOperationExpression(std::move(left), BinaryOperator::Multiply);
    case Token::Divide:
      return this->parseBinaryOperationExpression(std::move(left), BinaryOperator::Divide);
    default:
      return left;
  }
}

std::unique_ptr<BinaryOperationExpression> Parser::parseBinaryOperationExpression(
    std::unique_ptr<Expression> left, BinaryOperator op) {
  this->advance();
  std::unique_ptr<Expression> right = this->parseExpression();
  return std::make_unique<BinaryOperationExpression>(Loc::between(left->loc, right->loc), op,
                                                     std::move(left), std::move(right));
}
