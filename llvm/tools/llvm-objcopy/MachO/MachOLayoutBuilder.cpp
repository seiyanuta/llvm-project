//===- MachOLayoutBuilder.cpp -----------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MachOLayoutBuilder.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {
namespace objcopy {
namespace macho {

uint32_t MachOLayoutBuilder::computeSizeOfCmds() const {
  uint32_t Size = 0;
  for (const auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    auto cmd = MLC.load_command_data.cmd;
    switch (cmd) {
    case MachO::LC_SEGMENT:
      Size += sizeof(MachO::segment_command) +
              sizeof(MachO::section) * LC.Sections.size();
      continue;
    case MachO::LC_SEGMENT_64:
      Size += sizeof(MachO::segment_command_64) +
              sizeof(MachO::section_64) * LC.Sections.size();
      continue;
    }

    switch (cmd) {
#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct)                         \
  case MachO::LCName:                                                          \
    Size += sizeof(MachO::LCStruct) + LC.Payload.size();                       \
    break;
#include "llvm/BinaryFormat/MachO.def"
#undef HANDLE_LOAD_COMMAND
    }
  }

  return Size;
}

void MachOLayoutBuilder::constructStringTable() {
  for (std::unique_ptr<SymbolEntry> &Sym : O.SymTable.Symbols)
    StrTableBuilder.add(Sym->Name);
  StrTableBuilder.finalize();
}

void MachOLayoutBuilder::updateSymbolIndexes() {
  uint32_t Index = 0;
  for (auto &Symbol : O.SymTable.Symbols)
    Symbol->Index = Index++;
}

// Updates the index and the number of local/external/undefined symbols. Here we
// assume that MLC is a LC_DYSYMTAB and the nlist entries in the symbol table
// are already sorted by the those types.
void MachOLayoutBuilder::updateDySymTab(MachO::macho_load_command &MLC) {
  assert(MLC.load_command_data.cmd == MachO::LC_DYSYMTAB);
  assert(std::is_sorted(O.SymTable.Symbols.begin(), O.SymTable.Symbols.end(),
                        [](const std::unique_ptr<SymbolEntry> &A, const std::unique_ptr<SymbolEntry> &B) {
                          // The order of types is: local < external < undefined.
                          errs() << A->Name << "<>" << B->Name << ": " << "f=" << ((A->isLocalSymbol() && !B->isLocalSymbol()) ? "y" : "n") << ", s=" <<  ((A->isExternalSymbol() && B->isUndefinedSymbol()) ? "y" : "n") << "\n";
                          return false;
                          return (A->isLocalSymbol() && !B->isLocalSymbol()) ||  (A->isExternalSymbol() && B->isUndefinedSymbol());
                        }));

  uint32_t NumLocalSymbols = 0;
  auto Iter = O.SymTable.Symbols.begin();
  auto End = O.SymTable.Symbols.end();
  for (; Iter != End; ++Iter) {
    if ((*Iter)->isExternalSymbol())
      break;

    ++NumLocalSymbols;
  }

  uint32_t NumExtDefSymbols = 0;
  for (; Iter != End; ++Iter) {
    if ((*Iter)->isUndefinedSymbol())
      break;

    ++NumExtDefSymbols;
  }

  MLC.dysymtab_command_data.ilocalsym = 0;
  MLC.dysymtab_command_data.nlocalsym = NumLocalSymbols;
  MLC.dysymtab_command_data.iextdefsym = NumLocalSymbols;
  MLC.dysymtab_command_data.nextdefsym = NumExtDefSymbols;
  MLC.dysymtab_command_data.iundefsym = NumLocalSymbols + NumExtDefSymbols;
  MLC.dysymtab_command_data.nundefsym =
      O.SymTable.Symbols.size() - (NumLocalSymbols + NumExtDefSymbols);
}

// Recomputes and updates offset and size fields in load commands and sections
// since they could be modified.
uint64_t MachOLayoutBuilder::layoutSegments() {
  auto HeaderSize =
      Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
  auto Offset = HeaderSize + O.Header.SizeOfCmds;

  // Lay out sections.
  for (auto &LC : O.LoadCommands) {
    uint64_t FileOff = Offset;
    auto &MLC = LC.MachOLoadCommand;
    StringRef Segname;
    switch (MLC.load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      Segname = StringRef(MLC.segment_command_data.segname,
                          strnlen(MLC.segment_command_data.segname,
                                  sizeof(MLC.segment_command_data.segname)));
      break;
    case MachO::LC_SEGMENT_64:
      Segname = StringRef(MLC.segment_command_64_data.segname,
                          strnlen(MLC.segment_command_64_data.segname,
                                  sizeof(MLC.segment_command_64_data.segname)));
      break;
    default:
      continue;
    }

    if (Segname == "__LINKEDIT") {
      // We update the __LINKEDIT segment later (in layoutTail).
      assert(LC.Sections.empty() && "__LINKEDIT segment has sections");
      LinkEditLoadCommand = &MLC;
      continue;
    }

    // Update file offsets and sizes of sections.
    uint64_t VMSize = 0;
    uint64_t FileOffsetInSegment = 0;
    for (auto &Sec : LC.Sections) {
      if (!Sec.isVirtualSection()) {
        auto FilePaddingSize =
            OffsetToAlignment(FileOffsetInSegment, 1ull << Sec.Align);
        Sec.Offset = Offset + FileOffsetInSegment + FilePaddingSize;
        Sec.Size = Sec.Content.size();
        FileOffsetInSegment += FilePaddingSize + Sec.Size;
      }

      VMSize = std::max(VMSize, Sec.Addr + Sec.Size);
    }

    // TODO: Handle the __PAGEZERO segment.
    switch (MLC.load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      MLC.segment_command_data.cmdsize =
          sizeof(MachO::segment_command) +
          sizeof(MachO::section) * LC.Sections.size();
      MLC.segment_command_data.nsects = LC.Sections.size();
      MLC.segment_command_data.fileoff = FileOff;
      MLC.segment_command_data.vmsize = VMSize;
      MLC.segment_command_data.filesize = FileOffsetInSegment;
      break;
    case MachO::LC_SEGMENT_64:
      MLC.segment_command_64_data.cmdsize =
          sizeof(MachO::segment_command_64) +
          sizeof(MachO::section_64) * LC.Sections.size();
      MLC.segment_command_64_data.nsects = LC.Sections.size();
      MLC.segment_command_64_data.fileoff = FileOff;
      MLC.segment_command_64_data.vmsize = VMSize;
      MLC.segment_command_64_data.filesize = FileOffsetInSegment;
      break;
    }

    Offset += FileOffsetInSegment;
  }

  return Offset;
}

uint64_t MachOLayoutBuilder::layoutRelocations(uint64_t Offset) {
  for (auto &LC : O.LoadCommands)
    for (auto &Sec : LC.Sections) {
      Sec.RelOff = Sec.Relocations.empty() ? 0 : Offset;
      Sec.NReloc = Sec.Relocations.size();
      Offset += sizeof(MachO::any_relocation_info) * Sec.NReloc;
    }

  return Offset;
}

Error MachOLayoutBuilder::layoutTail(uint64_t Offset) {
  // The order of LINKEDIT elements is as follows:
  // rebase info, binding info, weak binding info, lazy binding info, export
  // trie, data-in-code, symbol table, indirect symbol table, symbol table
  // strings.
  uint64_t NListSize = Is64Bit ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist);
  uint64_t StartOfLinkEdit = Offset;
  uint64_t StartOfRebaseInfo = StartOfLinkEdit;
  uint64_t StartOfBindingInfo = StartOfRebaseInfo + O.Rebases.Opcodes.size();
  uint64_t StartOfWeakBindingInfo = StartOfBindingInfo + O.Binds.Opcodes.size();
  uint64_t StartOfLazyBindingInfo =
      StartOfWeakBindingInfo + O.WeakBinds.Opcodes.size();
  uint64_t StartOfExportTrie =
      StartOfLazyBindingInfo + O.LazyBinds.Opcodes.size();
  uint64_t StartOfFunctionStarts = StartOfExportTrie + O.Exports.Trie.size();
  uint64_t StartOfDataInCode =
      StartOfFunctionStarts + O.FunctionStarts.Data.size();
  uint64_t StartOfSymbols = StartOfDataInCode + O.DataInCode.Data.size();
  uint64_t StartOfIndirectSymbols =
      StartOfSymbols + NListSize * O.SymTable.Symbols.size();
  uint64_t StartOfSymbolStrings =
      StartOfIndirectSymbols +
      sizeof(uint32_t) * O.IndirectSymTable.Symbols.size();
  uint64_t LinkEditSize =
      (StartOfSymbolStrings + StrTableBuilder.getSize()) - StartOfLinkEdit;

  // Now we have determined the layout of the contents of the __LINKEDIT
  // segment. Update its load command.
  if (LinkEditLoadCommand) {
    MachO::macho_load_command *MLC = LinkEditLoadCommand;
    switch (LinkEditLoadCommand->load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      MLC->segment_command_data.cmdsize = sizeof(MachO::segment_command);
      MLC->segment_command_data.fileoff = StartOfLinkEdit;
      MLC->segment_command_data.vmsize = alignTo(LinkEditSize, PageSize);
      MLC->segment_command_data.filesize = LinkEditSize;
      break;
    case MachO::LC_SEGMENT_64:
      MLC->segment_command_64_data.cmdsize = sizeof(MachO::segment_command_64);
      MLC->segment_command_64_data.fileoff = StartOfLinkEdit;
      MLC->segment_command_64_data.vmsize = alignTo(LinkEditSize, PageSize);
      MLC->segment_command_64_data.filesize = LinkEditSize;
      break;
    }
  }

  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    auto cmd = MLC.load_command_data.cmd;
    switch (cmd) {
    case MachO::LC_SYMTAB:
      MLC.symtab_command_data.symoff = StartOfSymbols;
      MLC.symtab_command_data.nsyms = O.SymTable.Symbols.size();
      MLC.symtab_command_data.stroff = StartOfSymbolStrings;
      MLC.symtab_command_data.strsize = StrTableBuilder.getSize();
      break;
    case MachO::LC_DYSYMTAB: {
      if (MLC.dysymtab_command_data.ntoc != 0 ||
          MLC.dysymtab_command_data.nmodtab != 0 ||
          MLC.dysymtab_command_data.nextrefsyms != 0 ||
          MLC.dysymtab_command_data.nlocrel != 0 ||
          MLC.dysymtab_command_data.nextrel != 0)
        return createStringError(llvm::errc::not_supported,
                                 "shared library is not yet supported");

      if (!O.IndirectSymTable.Symbols.empty()) {
        MLC.dysymtab_command_data.indirectsymoff = StartOfIndirectSymbols;
        MLC.dysymtab_command_data.nindirectsyms =
            O.IndirectSymTable.Symbols.size();
      }

      updateDySymTab(MLC);
      break;
    }
    case MachO::LC_DATA_IN_CODE:
      MLC.linkedit_data_command_data.dataoff = StartOfDataInCode;
      MLC.linkedit_data_command_data.datasize = O.DataInCode.Data.size();
      break;
    case MachO::LC_FUNCTION_STARTS:
      MLC.linkedit_data_command_data.dataoff = StartOfFunctionStarts;
      MLC.linkedit_data_command_data.datasize = O.FunctionStarts.Data.size();
      break;
    case MachO::LC_DYLD_INFO:
    case MachO::LC_DYLD_INFO_ONLY:
      MLC.dyld_info_command_data.rebase_off =
          O.Rebases.Opcodes.empty() ? 0 : StartOfRebaseInfo;
      MLC.dyld_info_command_data.rebase_size = O.Rebases.Opcodes.size();
      MLC.dyld_info_command_data.bind_off =
          O.Binds.Opcodes.empty() ? 0 : StartOfBindingInfo;
      MLC.dyld_info_command_data.bind_size = O.Binds.Opcodes.size();
      MLC.dyld_info_command_data.weak_bind_off =
          O.WeakBinds.Opcodes.empty() ? 0 : StartOfWeakBindingInfo;
      MLC.dyld_info_command_data.weak_bind_size = O.WeakBinds.Opcodes.size();
      MLC.dyld_info_command_data.lazy_bind_off =
          O.LazyBinds.Opcodes.empty() ? 0 : StartOfLazyBindingInfo;
      MLC.dyld_info_command_data.lazy_bind_size = O.LazyBinds.Opcodes.size();
      MLC.dyld_info_command_data.export_off =
          O.Exports.Trie.empty() ? 0 : StartOfExportTrie;
      MLC.dyld_info_command_data.export_size = O.Exports.Trie.size();
      break;
    case MachO::LC_LOAD_DYLINKER:
    case MachO::LC_MAIN:
    case MachO::LC_RPATH:
    case MachO::LC_SEGMENT:
    case MachO::LC_SEGMENT_64:
    case MachO::LC_VERSION_MIN_MACOSX:
    case MachO::LC_BUILD_VERSION:
    case MachO::LC_ID_DYLIB:
    case MachO::LC_LOAD_DYLIB:
    case MachO::LC_UUID:
    case MachO::LC_SOURCE_VERSION:
      // Nothing to update.
      break;
    default:
      // Abort if it's unsupported in order to prevent corrupting the object.
      return createStringError(llvm::errc::not_supported,
                               "unsupported load command (cmd=0x%x)", cmd);
    }
  }

  return Error::success();
}

Error MachOLayoutBuilder::layout() {
  O.Header.NCmds = O.LoadCommands.size();
  O.Header.SizeOfCmds = computeSizeOfCmds();
  constructStringTable();
  updateSymbolIndexes();
  uint64_t Offset = layoutSegments();
  Offset = layoutRelocations(Offset);
  return layoutTail(Offset);
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
