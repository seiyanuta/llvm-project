#include "Object.h"
#include "../llvm-objcopy.h"

namespace llvm {
namespace objcopy {
namespace macho {

const SymbolEntry *SymbolTable::getSymbolByIndex(uint32_t Index) const {
  assert(Index < Symbols.size() && "invalid symbol index");
  return Symbols[Index].get();
}

SymbolEntry *SymbolTable::getSymbolByIndex(uint32_t Index) {
  return const_cast<SymbolEntry *>(
      static_cast<const SymbolTable *>(this)->getSymbolByIndex(Index));
}

void SymbolTable::removeSymbols(
    function_ref<bool(const std::unique_ptr<SymbolEntry> &)> ToRemove) {
  Symbols.erase(
      std::remove_if(std::begin(Symbols), std::end(Symbols), ToRemove),
      std::end(Symbols));
}

void Object::removeSections(function_ref<bool(const Section &)> ToRemove) {
  for (LoadCommand &LC : LoadCommands)
    LC.Sections.erase(std::remove_if(std::begin(LC.Sections),
                                     std::end(LC.Sections), ToRemove),
                      std::end(LC.Sections));
}

void Object::addLoadCommand(LoadCommand LC) {
  LoadCommands.push_back(std::move(LC));
}

LoadCommand &Object::addSegment(StringRef SegName) {
  assert(is64Bit() && "adding segment_command is not yet supported");

  LoadCommand LC;
  MachO::segment_command_64 &Seg = LC.MachOLoadCommand.segment_command_64_data;

  assert(SegName.size() <= sizeof(Seg.segname) && "too long segment name");

  memset(&Seg, 0, sizeof(MachO::segment_command_64));
  Seg.cmd = MachO::LC_SEGMENT_64;
  strncpy(Seg.segname, SegName.data(), SegName.size());
  LoadCommands.push_back(LC);

  return LoadCommands.back();
}

/// Extracts a segment name from a string which is possibly non-null-terminated.
StringRef extractSegmentName(const char *SegName) {
  return StringRef(SegName,
                   strnlen(SegName, sizeof(MachO::segment_command::segname)));
}

Optional<StringRef> LoadCommand::getSegmentName() const {
  const MachO::macho_load_command &MLC = MachOLoadCommand;
  switch (MLC.load_command_data.cmd) {
  case MachO::LC_SEGMENT:
    return extractSegmentName(MLC.segment_command_data.segname);
  case MachO::LC_SEGMENT_64:
    return extractSegmentName(MLC.segment_command_64_data.segname);
  default:
    return None;
  }
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
