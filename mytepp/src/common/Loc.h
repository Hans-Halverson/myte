#pragma once

#include <cstdint>

struct Pos {
  uint32_t line;
  uint32_t col;
};

struct Loc {
  char *file;
  Pos start;
  Pos end;
};
