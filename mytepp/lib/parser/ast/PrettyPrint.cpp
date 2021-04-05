#include "parser/ast/PrettyPrint.h"

#include <string>

std::string PPNode::toString() {
  std::string buffer = std::string();
  this->format(buffer, 0);

  return buffer;
}

void indent(std::string& buffer, int depth) { buffer.append(depth * 2, ' '); }

void IntPPNode::format(std::string& buffer, int) { buffer.append(std::to_string(this->value)); }

void StringPPNode::format(std::string& buffer, int) {
  buffer.append("\"");
  buffer.append(this->value);
  buffer.append("\"");
}

void BoolPPNode::format(std::string& buffer, int) { buffer.append(this->value ? "true" : "false"); }

void RawPPNode::format(std::string& buffer, int) { buffer.append(this->value); }

void ListPPNode::format(std::string& buffer, int depth) {
  if (this->values.empty()) {
    buffer.append("[]");
    return;
  }

  buffer.append("[\n");

  for (auto item = this->values.begin(); item != this->values.end(); item++) {
    indent(buffer, depth + 1);
    (*item)->format(buffer, depth + 1);
    buffer.append(",\n");
  }

  indent(buffer, depth);
  buffer.append("]");
}

void MapPPNode::format(std::string& buffer, int depth) {
  if (this->values.empty()) {
    buffer.append("{}");
    return;
  }

  buffer.append("{\n");

  for (auto item = this->values.begin(); item != this->values.end(); item++) {
    indent(buffer, depth + 1);
    buffer.append((*item).first);
    buffer.append(": ");
    ((*item).second)->format(buffer, depth + 1);
    buffer.append(",\n");
  }

  indent(buffer, depth);
  buffer.append("}");
}

void MapPPNode::addKVPair(std::string key, std::unique_ptr<PPNode> value) {
  this->values.push_back(
      std::make_pair<std::string, std::unique_ptr<PPNode>>(std::move(key), std::move(value)));
}

std::unique_ptr<MapPPNode> NewAstNode(std::string name, Loc* loc) {
  auto map = std::make_unique<MapPPNode>();
  map->addKVPair("name", std::make_unique<RawPPNode>(name));
  map->addKVPair("loc", std::make_unique<RawPPNode>(loc->toString()));

  return map;
}