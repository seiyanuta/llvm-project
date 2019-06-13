#include "Object.h"
#include "../llvm-objcopy.h"

namespace llvm {
namespace objcopy {
namespace macho {

SymbolEntry *SymbolTable::getSymbolByIndex(uint32_t Index) {
  assert(Index < Symbols.size() && "invalid symbol index");
  return const_cast<SymbolEntry *>(Symbols[Index].get());
}

void SymbolTable::removeSymbols(
    function_ref<bool(const std::unique_ptr<SymbolEntry> &)> ToRemove) {
  Symbols.erase(
      std::remove_if(std::begin(Symbols), std::end(Symbols), ToRemove),
      std::end(Symbols));
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm