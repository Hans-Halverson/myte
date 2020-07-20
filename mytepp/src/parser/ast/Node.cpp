#include "parser/ast/Node.h"

Node::Node(Loc *loc) : loc(loc) {}

std::string Node::toString() { return this->toPrettyPrintNode()->toString(); }
