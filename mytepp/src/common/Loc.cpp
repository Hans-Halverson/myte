#include "common/Loc.h"

#include <string>

std::string Loc::toString() {
  std::string source = this->file != nullptr ? std::string(this->file) : "";
  return source + ":" + this->start.toString() + "-" + this->end.toString();
}

Loc *Loc::between(Loc *start, Loc *end) { return new Loc{start->file, start->start, end->end}; }
