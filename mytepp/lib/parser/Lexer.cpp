#include "parser/Lexer.h"

#include <string>

#include "common/Loc.h"

Lexer::Lexer(char *file) : file_(file), istream_(std::ifstream(file)) {}

LexResult Lexer::next() {
  this->lex();
  return this->lexResult_;
}

void Lexer::nextChar() {
  if (!this->isPrimed_) {
    this->istream_.get(this->currentChar_);
    this->isCurrentEOF_ = this->istream_.eof();
    this->istream_.get(this->nextChar_);
    this->isNextEOF_ = this->istream_.eof();
    this->isPrimed_ = true;
    return;
  }

  this->currentChar_ = this->nextChar_;
  this->isCurrentEOF_ = this->isNextEOF_;
  this->istream_.get(this->nextChar_);
  this->isNextEOF_ = this->istream_.eof();
  this->col_++;
}

bool isAlphabetic(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }

bool isNumeric(char c) { return (c >= '0' && c <= '9'); }

void Lexer::newStringWithChar() { this->lexString_ = std::string(1, this->currentChar_); }

void Lexer::addCharToString() { this->lexString_.push_back(this->currentChar_); }

void Lexer::lex() {
  this->nextChar();
  this->startPos_ = Pos{this->line_, this->col_};

  if (this->isCurrentEOF_) {
    this->finalizeLexResult(Token::Eof);
    return;
  }

  // Identifiers consist of alphabetic, numeric, and underscore characters and must start with an
  // alphabetic or underscore character.
  if (isAlphabetic(this->currentChar_) || this->currentChar_ == '_') {
    this->newStringWithChar();
    while (!this->isNextEOF_ && (isAlphabetic(this->nextChar_) || isNumeric(this->nextChar_) ||
                                 this->nextChar_ == '_')) {
      this->nextChar();
      this->addCharToString();
    }

    return this->finalizeLexResult(Token::Identifier);
  }

  // Int literals consist of a sequence of numeric characters
  if (isNumeric(this->currentChar_)) {
    this->newStringWithChar();
    this->lexInt_ = this->currentChar_ - '0';
    while (!this->isNextEOF_ && isNumeric(this->nextChar_)) {
      this->nextChar();
      this->addCharToString();
      this->lexInt_ *= 10;
      this->lexInt_ += this->currentChar_ - '0';
    }

    return this->finalizeLexResult(Token::IntLiteral);
  }

  switch (this->currentChar_) {
    case '\n':
      this->line_++;
      this->col_ = 0;
      break;
    case ' ':
    case '\t':
      this->lex();
      break;
    case '+':
      this->finalizeLexResult(Token::Plus);
      break;
    case '-':
      this->finalizeLexResult(Token::Minus);
      break;
    case '*':
      this->finalizeLexResult(Token::Multiply);
      break;
    case '/':
      this->finalizeLexResult(Token::Divide);
      break;
  }
}

Loc *Lexer::finalizeLoc() {
  return new Loc{this->file_, this->startPos_, Pos{this->line_, this->col_ + 1}};
}

void Lexer::finalizeLexResult(Token token) {
  this->lexResult_ = LexResult{token, this->finalizeLoc()};
}

const char *tokenToString(Token token) {
  switch (token) {
    case Token::Plus:
      return "+";
    case Token::Minus:
      return "-";
    case Token::Multiply:
      return "*";
    case Token::Divide:
      return "/";
    case Token::Eof:
      return "<EOF>";
    case Token::Identifier:
      return "<IDENTIFIER>";
    case Token::IntLiteral:
      return "<INT_LITERAL>";
  }
}
