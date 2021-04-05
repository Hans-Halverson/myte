#pragma once

#include "common/Loc.h"
#include "parser/ast/PrettyPrint.h"

class Node {
 public:
  Node(Loc *);
  virtual ~Node(){};
  Loc *loc;

  virtual std::unique_ptr<PPNode> toPrettyPrintNode() = 0;
  std::string toString();
};