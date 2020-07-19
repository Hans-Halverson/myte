#include <iostream>

#include "parser/Lexer.h"

int main(int argc, char** argv) {
  if (argc == 2) {
    Lexer lexer(argv[1]);

    Token token = lexer.advance();
    std::cout << tokenToString(token) << std::endl;
    while (token != Token::Eof) {
      token = lexer.advance();
      std::cout << tokenToString(token) << std::endl;
    }
  }

  return 0;
}
