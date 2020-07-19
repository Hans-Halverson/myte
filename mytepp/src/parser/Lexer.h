#pragma once

#include <fstream>

#include "common/Loc.h"

enum Token { Plus, Minus, Multiply, Divide, Eof };

struct LexResult {
  Token token;
  Loc *loc;
};

class Lexer {
 public:
  Lexer(char *file);

  Token Peek();
  Token Advance();
  Loc *GetLoc();

 private:
  char *file_;
  std::ifstream istream_;

  uint32_t line_;
  uint32_t col_;
  Pos start_pos_;

  LexResult lex_result_;

  void Lex();
  Loc *FinalizeLoc();
  void FinalizeLexResult(Token);
  char NextChar();
};
