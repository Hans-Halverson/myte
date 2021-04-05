#pragma once

#include <cstdint>
#include <string>

struct Pos {
  uint32_t line;
  uint32_t col;

  std::string toString() { return std::to_string(line) + ":" + std::to_string(col); }
};

struct Loc {
  char *file;
  Pos start;
  Pos end;

  std::string toString();
  static Loc *between(Loc *, Loc *);
};
