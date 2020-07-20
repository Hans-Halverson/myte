#include "parser/ast/Expression.h"

#include <memory>

Expression::Expression(Loc *loc) : Node(loc) {}

std::string binaryOperatorToString(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::Add:
      return "Add";
    case BinaryOperator::Subtract:
      return "Subtract";
    case BinaryOperator::Multiply:
      return "Multiply";
    case BinaryOperator::Divide:
      return "Divide";
  }
}

BinaryOperationExpression::BinaryOperationExpression(Loc *loc, BinaryOperator op,
                                                     std::unique_ptr<Expression> left,
                                                     std::unique_ptr<Expression> right)
    : Expression(loc), op(op), left(std::move(left)), right(std::move(right)) {}

std::unique_ptr<PPNode> BinaryOperationExpression::toPrettyPrintNode() {
  auto node = NewAstNode("BinaryOperationExpression", this->loc);
  node->addKVPair("op", std::make_unique<RawPPNode>(binaryOperatorToString(this->op)));
  node->addKVPair("left", this->left->toPrettyPrintNode());
  node->addKVPair("right", this->right->toPrettyPrintNode());

  return node;
}

IntLiteralExpression::IntLiteralExpression(Loc *loc, int64_t value)
    : Expression(loc), value(value) {}

std::unique_ptr<PPNode> IntLiteralExpression::toPrettyPrintNode() {
  auto node = NewAstNode("IntLiteralExpression", this->loc);
  node->addKVPair("value", std::make_unique<IntPPNode>(this->value));

  return node;
}

IdentifierExpression::IdentifierExpression(Loc *loc, std::string name)
    : Expression(loc), name(name) {}

std::unique_ptr<PPNode> IdentifierExpression::toPrettyPrintNode() {
  auto node = NewAstNode("IdentifierExpression", this->loc);
  node->addKVPair("name", std::make_unique<StringPPNode>(this->name));

  return node;
}