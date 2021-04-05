#include <iostream>
#include <memory>

#include "parser/Lexer.h"
#include "parser/Parser.h"

int main(int argc, char** argv) {
  if (argc == 2) {
    auto lexer = std::make_unique<Lexer>(argv[1]);
    auto parser = std::make_unique<Parser>(std::move(lexer));
    auto expression = parser->parseExpression();
    std::cout << expression->toString() << std::endl;
  }

  return 0;
}
