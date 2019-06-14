#include "Object.h"
#include "../llvm-objcopy.h"

namespace llvm {
namespace objcopy {
namespace macho {

SymbolEntry *SymbolTable::getSymbolByIndex(uint32_t Index) {
  assert(Index < Symbols.size() && "invalid symbol index");
  return const_cast<SymbolEntry *>(Symbols[Index].get());
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
