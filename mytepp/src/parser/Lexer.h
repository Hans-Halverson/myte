#pragma once

#include <fstream>

#include "common/Loc.h"

enum class Token { Identifier, IntLiteral, Plus, Minus, Multiply, Divide, Eof };

const char *tokenToString(Token);

struct LexResult {
  Token token;
  Loc *loc;
};

class Lexer {
 public:
  Lexer(char *file);

  LexResult next();
  std::string lexString() { return lexString_; }
  int64_t lexInt() { return lexInt_; }

 private:
  char *file_;
  std::ifstream istream_;

  bool isPrimed_ = false;
  char currentChar_ = 0;
  char nextChar_ = 0;
  bool isCurrentEOF_ = false;
  bool isNextEOF_ = false;
  uint32_t line_ = 1;
  uint32_t col_ = 0;
  Pos startPos_ = Pos{1, 0};

  LexResult lexResult_ = LexResult{Token::Eof, nullptr};
  std::string lexString_ = std::string();
  int64_t lexInt_ = 0;

  void lex();
  void newStringWithChar();
  void addCharToString();
  Loc *finalizeLoc();
  void finalizeLexResult(Token);
  void nextChar();
};
