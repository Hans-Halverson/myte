#pragma once

#include <memory>
#include <string>

#include "common/Loc.h"
#include "parser/ast/Node.h"

class Expression : public Node {
 public:
  Expression(Loc *);
  virtual ~Expression(){};
};

enum class BinaryOperator { Add, Subtract, Multiply, Divide };

std::string binaryOperatorToString(BinaryOperator);

class BinaryOperationExpression : public Expression {
 public:
  BinaryOperationExpression(Loc *, BinaryOperator, std::unique_ptr<Expression>,
                            std::unique_ptr<Expression>);

  BinaryOperator op;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;

  std::unique_ptr<PPNode> toPrettyPrintNode();
};

class IntLiteralExpression : public Expression {
 public:
  IntLiteralExpression(Loc *, int64_t);

  int64_t value;

  std::unique_ptr<PPNode> toPrettyPrintNode();
};

class IdentifierExpression : public Expression {
 public:
  IdentifierExpression(Loc *, std::string);

  std::string name;

  std::unique_ptr<PPNode> toPrettyPrintNode();
};
