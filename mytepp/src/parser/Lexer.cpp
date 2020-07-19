#include "parser/Lexer.h"

#include <string>
#include <vector>

#include "common/Loc.h"

Lexer::Lexer(char *file)
    : file_(file),
      line_(1),
      col_(-1),
      start_pos_(Pos{0, 0}),
      lex_result_(LexResult{Token::Eof, nullptr}) {
  std::ifstream istream_(file);
}

Token Lexer::Peek() { return this->lex_result_.token; }

Token Lexer::Advance() {
  this->Lex();
  return this->lex_result_.token;
}

Loc *Lexer::GetLoc() { return this->lex_result_.loc; }

char Lexer::NextChar() {
  char c;
  this->istream_.get(c);
  this->col_++;

  return c;
}

void Lexer::Lex() {
  char c = this->NextChar();
  this->start_pos_ = Pos{this->line_, this->col_};

  switch (c) {
    case '\n':
      this->line_++;
      break;
    case ' ':
    case '\t':
      this->Lex();
      break;
    case '+':
      this->FinalizeLexResult(Token::Plus);
      break;
    case '-':
      this->FinalizeLexResult(Token::Minus);
      break;
    case '*':
      this->FinalizeLexResult(Token::Multiply);
      break;
    case '/':
      this->FinalizeLexResult(Token::Divide);
      break;
    case EOF:
      this->FinalizeLexResult(Token::Eof);
      break;
  }
}

Loc *Lexer::FinalizeLoc() {
  return new Loc{this->file_, this->start_pos_, Pos{this->line_, this->col_}};
}

void Lexer::FinalizeLexResult(Token token) {
  this->lex_result_ = LexResult{token, this->FinalizeLoc()};
}
