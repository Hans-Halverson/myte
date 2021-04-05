#pragma once

#include <string>
#include <utility>
#include <vector>

#include "common/Loc.h"

struct PPNode {
  virtual ~PPNode(){};
  std::string toString();
  virtual void format(std::string &, int) = 0;
};

struct NonePPNode : public PPNode {
  void format(std::string &, int) override;
};

struct IntPPNode : public PPNode {
  int value;

  IntPPNode(int value) : value(value) {}
  void format(std::string &, int) override;
};

struct StringPPNode : public PPNode {
  std::string value;

  StringPPNode(std::string value) : value(value) {}
  void format(std::string &, int) override;
};

struct BoolPPNode : public PPNode {
  bool value;
  void format(std::string &, int) override;
};

struct RawPPNode : public PPNode {
  std::string value;

  RawPPNode(std::string value) : value(value) {}
  void format(std::string &, int) override;
};

struct ListPPNode : public PPNode {
  std::vector<std::unique_ptr<PPNode>> values;
  void format(std::string &, int) override;
};

struct MapPPNode : public PPNode {
  std::vector<std::pair<std::string, std::unique_ptr<PPNode>>> values =
      std::vector<std::pair<std::string, std::unique_ptr<PPNode>>>();
  void format(std::string &, int) override;
  void addKVPair(std::string, std::unique_ptr<PPNode>);
};

std::unique_ptr<MapPPNode> NewAstNode(std::string name, Loc *loc);
