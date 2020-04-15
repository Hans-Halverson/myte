#include <string>
#include <vector>
#include <fstream>

namespace lex {

enum TokenTypes {
  EQUALS,
  NUMBER_LITERAL,
  PLUS,
  MINUS,
  ASTERISK,
  FORWARD_SLASH
};

struct Loc {
  uint64_t line;
  uint64_t col; 
  char *file;
};

struct Token {
  TokenTypes type;
  Loc loc;
};

class FileTokenizer {
  public:
    FileTokenizer(char *file);

    std::vector<Token> tokenize();

  private:
    char *file_;
    std::ifstream istream_;
    uint64_t line_;
    uint64_t col_; 

    char peek();
    char advance();
    Token *next();
};

FileTokenizer::FileTokenizer(char *file): file_(file), line_(1), col_(0) {
  std::ifstream istream_(file);
}

char FileTokenizer::peek() {

}

char FileTokenizer::advance() {
  char c;
  istream_.get(&c);

  if (c == '\n') {
    line_++;
  }
}

Token *FileTokenizer::next() {
  switch ()
}

std::vector<Token> FileTokenizer::tokenize() {
  std::vector<Token> tokens;

  return tokens;
}


} // namespace
