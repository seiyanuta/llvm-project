//===- MachOWriter.cpp ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MachOWriter.h"
#include "Object.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/ErrorHandling.h"
#include <memory>

namespace llvm {
namespace objcopy {
namespace macho {

size_t MachOWriter::headerSize() const {
  return Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
}

size_t MachOWriter::loadCommandsSize() const { return O.Header.SizeOfCmds; }

size_t MachOWriter::symTableSize() const {
  return O.SymTable.NameList.size() *
         (Is64Bit ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist));
}

size_t MachOWriter::strTableSize() const {
  size_t S = 0;
  for (const auto &Str : O.StrTable.Strings)
    S += Str.size();
  S += (O.StrTable.Strings.empty() ? 0 : O.StrTable.Strings.size() - 1);
  return S;
}

size_t MachOWriter::totalSize() const {
  // Going from tail to head and looking for an appropriate "anchor" to
  // calculate the total size assuming that all the offsets are either valid
  // ("true") or 0 (0 indicates that the corresponding part is missing).

  SmallVector<size_t, 7> Ends;
  if (O.SymTabCommandIndex) {
    const MachO::symtab_command &SymTabCommand =
        O.LoadCommands[*O.SymTabCommandIndex]
            .MachOLoadCommand.symtab_command_data;
    if (SymTabCommand.symoff) {
      assert((SymTabCommand.nsyms == O.SymTable.NameList.size()) &&
             "Incorrect number of symbols");
      Ends.push_back(SymTabCommand.symoff + symTableSize());
    }
    if (SymTabCommand.stroff) {
      assert((SymTabCommand.strsize == strTableSize()) &&
             "Incorrect string table size");
      Ends.push_back(SymTabCommand.stroff + SymTabCommand.strsize);
    }
  }
  if (O.DyLdInfoCommandIndex) {
    const MachO::dyld_info_command &DyLdInfoCommand =
        O.LoadCommands[*O.DyLdInfoCommandIndex]
            .MachOLoadCommand.dyld_info_command_data;
    if (DyLdInfoCommand.rebase_off) {
      assert((DyLdInfoCommand.rebase_size == O.Rebases.Opcodes.size()) &&
             "Incorrect rebase opcodes size");
      Ends.push_back(DyLdInfoCommand.rebase_off + DyLdInfoCommand.rebase_size);
    }
    if (DyLdInfoCommand.bind_off) {
      assert((DyLdInfoCommand.bind_size == O.Binds.Opcodes.size()) &&
             "Incorrect bind opcodes size");
      Ends.push_back(DyLdInfoCommand.bind_off + DyLdInfoCommand.bind_size);
    }
    if (DyLdInfoCommand.weak_bind_off) {
      assert((DyLdInfoCommand.weak_bind_size == O.WeakBinds.Opcodes.size()) &&
             "Incorrect weak bind opcodes size");
      Ends.push_back(DyLdInfoCommand.weak_bind_off +
                     DyLdInfoCommand.weak_bind_size);
    }
    if (DyLdInfoCommand.lazy_bind_off) {
      assert((DyLdInfoCommand.lazy_bind_size == O.LazyBinds.Opcodes.size()) &&
             "Incorrect lazy bind opcodes size");
      Ends.push_back(DyLdInfoCommand.lazy_bind_off +
                     DyLdInfoCommand.lazy_bind_size);
    }
    if (DyLdInfoCommand.export_off) {
      assert((DyLdInfoCommand.export_size == O.Exports.Trie.size()) &&
             "Incorrect trie size");
      Ends.push_back(DyLdInfoCommand.export_off + DyLdInfoCommand.export_size);
    }
  }

  if (O.DySymTabCommandIndex) {
    const MachO::dysymtab_command &DySymTabCommand =
        O.LoadCommands[*O.DySymTabCommandIndex]
            .MachOLoadCommand.dysymtab_command_data;

    if (DySymTabCommand.indirectsymoff)
      Ends.push_back(DySymTabCommand.indirectsymoff +
                     sizeof(uint32_t) * O.IndirectSymTable.Symbols.size());
  }

  if (O.DataInCodeCommandIndex) {
    const MachO::linkedit_data_command &LinkEditDataCommand =
        O.LoadCommands[*O.DataInCodeCommandIndex]
            .MachOLoadCommand.linkedit_data_command_data;

    if (LinkEditDataCommand.dataoff)
      Ends.push_back(LinkEditDataCommand.dataoff +
                     LinkEditDataCommand.datasize);
  }

  if (O.FunctionStartsCommandIndex) {
    const MachO::linkedit_data_command &LinkEditDataCommand =
        O.LoadCommands[*O.FunctionStartsCommandIndex]
            .MachOLoadCommand.linkedit_data_command_data;

    if (LinkEditDataCommand.dataoff)
      Ends.push_back(LinkEditDataCommand.dataoff +
                     LinkEditDataCommand.datasize);
  }

  // Otherwise, use the last section / reloction.
  for (const auto &LC : O.LoadCommands) {
    switch (LC.MachOLoadCommand.load_command_data.cmd) {
    case MachO::LC_SEGMENT: {
      auto &Seg = LC.MachOLoadCommand.segment_command_data;
      Ends.push_back(Seg.fileoff + Seg.filesize);
      break;
    }
    case MachO::LC_SEGMENT_64: {
      auto &Seg = LC.MachOLoadCommand.segment_command_64_data;
      Ends.push_back(Seg.fileoff + Seg.filesize);
      break;
    }
    }

    for (const auto &S : LC.Sections) {
      Ends.push_back(S.Offset + S.Size);
      if (S.RelOff)
        Ends.push_back(S.RelOff +
                       S.NReloc * sizeof(MachO::any_relocation_info));
    }
  }

  if (!Ends.empty())
    return *std::max_element(Ends.begin(), Ends.end());

  // Otherwise, we have only Mach header and load commands.
  return headerSize() + loadCommandsSize();
}

void MachOWriter::writeHeader() {
  MachO::mach_header_64 Header;

  Header.magic = O.Header.Magic;
  Header.cputype = O.Header.CPUType;
  Header.cpusubtype = O.Header.CPUSubType;
  Header.filetype = O.Header.FileType;
  Header.ncmds = O.Header.NCmds;
  Header.sizeofcmds = O.Header.SizeOfCmds;
  Header.flags = O.Header.Flags;
  Header.reserved = O.Header.Reserved;

  if (IsLittleEndian != sys::IsLittleEndianHost)
    MachO::swapStruct(Header);

  auto HeaderSize =
      Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
  memcpy(B.getBufferStart(), &Header, HeaderSize);
}

void MachOWriter::writeLoadCommands() {
  uint8_t *Begin = B.getBufferStart() + headerSize();
  for (const auto &LC : O.LoadCommands) {
    // Construct a load command.
    MachO::macho_load_command MLC = LC.MachOLoadCommand;
    switch (MLC.load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      if (IsLittleEndian != sys::IsLittleEndianHost)
        MachO::swapStruct(MLC.segment_command_data);
      memcpy(Begin, &MLC.segment_command_data, sizeof(MachO::segment_command));
      Begin += sizeof(MachO::segment_command);

      for (const auto &Sec : LC.Sections)
        writeSectionInLoadCommand<MachO::section>(Sec, Begin);
      continue;
    case MachO::LC_SEGMENT_64:
      if (IsLittleEndian != sys::IsLittleEndianHost)
        MachO::swapStruct(MLC.segment_command_64_data);
      memcpy(Begin, &MLC.segment_command_64_data,
             sizeof(MachO::segment_command_64));
      Begin += sizeof(MachO::segment_command_64);

      for (const auto &Sec : LC.Sections)
        writeSectionInLoadCommand<MachO::section_64>(Sec, Begin);
      continue;
    }

#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct)                         \
  case MachO::LCName:                                                          \
    assert(sizeof(MachO::LCStruct) + LC.Payload.size() ==                      \
           MLC.load_command_data.cmdsize);                                     \
    if (IsLittleEndian != sys::IsLittleEndianHost)                             \
      MachO::swapStruct(MLC.LCStruct##_data);                                  \
    memcpy(Begin, &MLC.LCStruct##_data, sizeof(MachO::LCStruct));              \
    Begin += sizeof(MachO::LCStruct);                                          \
    memcpy(Begin, LC.Payload.data(), LC.Payload.size());                       \
    Begin += LC.Payload.size();                                                \
    break;

    // Copy the load command as it is.
    switch (MLC.load_command_data.cmd) {
    default:
      assert(sizeof(MachO::load_command) + LC.Payload.size() ==
             MLC.load_command_data.cmdsize);
      if (IsLittleEndian != sys::IsLittleEndianHost)
        MachO::swapStruct(MLC.load_command_data);
      memcpy(Begin, &MLC.load_command_data, sizeof(MachO::load_command));
      Begin += sizeof(MachO::load_command);
      memcpy(Begin, LC.Payload.data(), LC.Payload.size());
      Begin += LC.Payload.size();
      break;
#include "llvm/BinaryFormat/MachO.def"
    }
  }
}

template <typename StructType>
void MachOWriter::writeSectionInLoadCommand(const Section &Sec, uint8_t *&Out) {
  StructType Temp;
  assert(Sec.Segname.size() <= sizeof(Temp.segname) && "too long segment name");
  assert(Sec.Sectname.size() <= sizeof(Temp.sectname) &&
         "too long section name");
  memset(&Temp, 0, sizeof(StructType));
  memcpy(Temp.segname, Sec.Segname.data(), Sec.Segname.size());
  memcpy(Temp.sectname, Sec.Sectname.data(), Sec.Sectname.size());
  Temp.addr = Sec.Addr;
  Temp.size = Sec.Size;
  Temp.offset = Sec.Offset;
  Temp.align = Sec.Align;
  Temp.reloff = Sec.RelOff;
  Temp.nreloc = Sec.NReloc;
  Temp.flags = Sec.Flags;
  Temp.reserved1 = Sec.Reserved1;
  Temp.reserved2 = Sec.Reserved2;

  if (IsLittleEndian != sys::IsLittleEndianHost)
    MachO::swapStruct(Temp);
  memcpy(Out, &Temp, sizeof(StructType));
  Out += sizeof(StructType);
}

void MachOWriter::writeSections() {
  for (const auto &LC : O.LoadCommands)
    for (const auto &Sec : LC.Sections) {
      if (Sec.isVirtualSection())
        continue;

      assert(Sec.Offset && "Section offset can not be zero");
      assert((Sec.Size == Sec.Content.size()) && "Incorrect section size");
      memcpy(B.getBufferStart() + Sec.Offset, Sec.Content.data(),
             Sec.Content.size());
      for (size_t Index = 0; Index < Sec.Relocations.size(); ++Index) {
        MachO::any_relocation_info R = Sec.Relocations[Index];
        if (IsLittleEndian != sys::IsLittleEndianHost)
          MachO::swapStruct(R);
        memcpy(B.getBufferStart() + Sec.RelOff +
                   Index * sizeof(MachO::any_relocation_info),
               &R, sizeof(R));
      }
    }
}

template <typename NListType>
void writeNListEntry(const NListEntry &NLE, bool IsLittleEndian, char *&Out) {
  NListType ListEntry;
  ListEntry.n_strx = NLE.n_strx;
  ListEntry.n_type = NLE.n_type;
  ListEntry.n_sect = NLE.n_sect;
  ListEntry.n_desc = NLE.n_desc;
  ListEntry.n_value = NLE.n_value;

  if (IsLittleEndian != sys::IsLittleEndianHost)
    MachO::swapStruct(ListEntry);
  memcpy(Out, reinterpret_cast<const char *>(&ListEntry), sizeof(NListType));
  Out += sizeof(NListType);
}

void MachOWriter::writeSymbolTable() {
  if (!O.SymTabCommandIndex)
    return;
  const MachO::symtab_command &SymTabCommand =
      O.LoadCommands[*O.SymTabCommandIndex]
          .MachOLoadCommand.symtab_command_data;
  assert((SymTabCommand.nsyms == O.SymTable.NameList.size()) &&
         "Incorrect number of symbols");
  char *Out = (char *)B.getBufferStart() + SymTabCommand.symoff;
  for (auto NLE : O.SymTable.NameList) {
    if (Is64Bit)
      writeNListEntry<MachO::nlist_64>(NLE, IsLittleEndian, Out);
    else
      writeNListEntry<MachO::nlist>(NLE, IsLittleEndian, Out);
  }
}

void MachOWriter::writeStringTable() {
  if (!O.SymTabCommandIndex)
    return;
  const MachO::symtab_command &SymTabCommand =
      O.LoadCommands[*O.SymTabCommandIndex]
          .MachOLoadCommand.symtab_command_data;
  char *Out = (char *)B.getBufferStart() + SymTabCommand.stroff;
  assert((SymTabCommand.strsize == strTableSize()) &&
         "Incorrect string table size");
  for (size_t Index = 0; Index < O.StrTable.Strings.size(); ++Index) {
    memcpy(Out, O.StrTable.Strings[Index].data(),
           O.StrTable.Strings[Index].size());
    Out += O.StrTable.Strings[Index].size();
    if (Index + 1 != O.StrTable.Strings.size()) {
      memcpy(Out, "\0", 1);
      Out += 1;
    }
  }
}

void MachOWriter::writeRebaseInfo() {
  if (!O.DyLdInfoCommandIndex)
    return;
  const MachO::dyld_info_command &DyLdInfoCommand =
      O.LoadCommands[*O.DyLdInfoCommandIndex]
          .MachOLoadCommand.dyld_info_command_data;
  char *Out = (char *)B.getBufferStart() + DyLdInfoCommand.rebase_off;
  assert((DyLdInfoCommand.rebase_size == O.Rebases.Opcodes.size()) &&
         "Incorrect rebase opcodes size");
  memcpy(Out, O.Rebases.Opcodes.data(), O.Rebases.Opcodes.size());
}

void MachOWriter::writeBindInfo() {
  if (!O.DyLdInfoCommandIndex)
    return;
  const MachO::dyld_info_command &DyLdInfoCommand =
      O.LoadCommands[*O.DyLdInfoCommandIndex]
          .MachOLoadCommand.dyld_info_command_data;
  char *Out = (char *)B.getBufferStart() + DyLdInfoCommand.bind_off;
  assert((DyLdInfoCommand.bind_size == O.Binds.Opcodes.size()) &&
         "Incorrect bind opcodes size");
  memcpy(Out, O.Binds.Opcodes.data(), O.Binds.Opcodes.size());
}

void MachOWriter::writeWeakBindInfo() {
  if (!O.DyLdInfoCommandIndex)
    return;
  const MachO::dyld_info_command &DyLdInfoCommand =
      O.LoadCommands[*O.DyLdInfoCommandIndex]
          .MachOLoadCommand.dyld_info_command_data;
  char *Out = (char *)B.getBufferStart() + DyLdInfoCommand.weak_bind_off;
  assert((DyLdInfoCommand.weak_bind_size == O.WeakBinds.Opcodes.size()) &&
         "Incorrect weak bind opcodes size");
  memcpy(Out, O.WeakBinds.Opcodes.data(), O.WeakBinds.Opcodes.size());
}

void MachOWriter::writeLazyBindInfo() {
  if (!O.DyLdInfoCommandIndex)
    return;
  const MachO::dyld_info_command &DyLdInfoCommand =
      O.LoadCommands[*O.DyLdInfoCommandIndex]
          .MachOLoadCommand.dyld_info_command_data;
  char *Out = (char *)B.getBufferStart() + DyLdInfoCommand.lazy_bind_off;
  assert((DyLdInfoCommand.lazy_bind_size == O.LazyBinds.Opcodes.size()) &&
         "Incorrect lazy bind opcodes size");
  memcpy(Out, O.LazyBinds.Opcodes.data(), O.LazyBinds.Opcodes.size());
}

void MachOWriter::writeExportInfo() {
  if (!O.DyLdInfoCommandIndex)
    return;
  const MachO::dyld_info_command &DyLdInfoCommand =
      O.LoadCommands[*O.DyLdInfoCommandIndex]
          .MachOLoadCommand.dyld_info_command_data;
  char *Out = (char *)B.getBufferStart() + DyLdInfoCommand.export_off;
  assert((DyLdInfoCommand.export_size == O.Exports.Trie.size()) &&
         "Incorrect export trie size");
  memcpy(Out, O.Exports.Trie.data(), O.Exports.Trie.size());
}

void MachOWriter::writeIndirectSymbolTable() {
  if (!O.DySymTabCommandIndex)
    return;

  const MachO::dysymtab_command &DySymTabCommand =
      O.LoadCommands[*O.DySymTabCommandIndex]
          .MachOLoadCommand.dysymtab_command_data;

  char *Out = (char *)B.getBufferStart() + DySymTabCommand.indirectsymoff;
  assert((DySymTabCommand.nindirectsyms == O.IndirectSymTable.Symbols.size()) &&
         "Incorrect indirect symbol table size");
  memcpy(Out, O.IndirectSymTable.Symbols.data(),
         sizeof(uint32_t) * O.IndirectSymTable.Symbols.size());
}

void MachOWriter::writeDataInCodeData() {
  if (!O.DataInCodeCommandIndex)
    return;
  const MachO::linkedit_data_command &LinkEditDataCommand =
      O.LoadCommands[*O.DataInCodeCommandIndex]
          .MachOLoadCommand.linkedit_data_command_data;
  char *Out = (char *)B.getBufferStart() + LinkEditDataCommand.dataoff;
  assert((LinkEditDataCommand.datasize == O.DataInCode.Data.size()) &&
         "Incorrect data in code data size");
  memcpy(Out, O.DataInCode.Data.data(), O.DataInCode.Data.size());
}

void MachOWriter::writeFunctionStartsData() {
  if (!O.FunctionStartsCommandIndex)
    return;
  const MachO::linkedit_data_command &LinkEditDataCommand =
      O.LoadCommands[*O.FunctionStartsCommandIndex]
          .MachOLoadCommand.linkedit_data_command_data;
  char *Out = (char *)B.getBufferStart() + LinkEditDataCommand.dataoff;
  assert((LinkEditDataCommand.datasize == O.FunctionStarts.Data.size()) &&
         "Incorrect function starts data size");
  memcpy(Out, O.FunctionStarts.Data.data(), O.FunctionStarts.Data.size());
}

void MachOWriter::writeTail() {
  typedef void (MachOWriter::*WriteHandlerType)(void);
  typedef std::pair<uint64_t, WriteHandlerType> WriteOperation;
  SmallVector<WriteOperation, 7> Queue;

  if (O.SymTabCommandIndex) {
    const MachO::symtab_command &SymTabCommand =
        O.LoadCommands[*O.SymTabCommandIndex]
            .MachOLoadCommand.symtab_command_data;
    if (SymTabCommand.symoff)
      Queue.push_back({SymTabCommand.symoff, &MachOWriter::writeSymbolTable});
    if (SymTabCommand.stroff)
      Queue.push_back({SymTabCommand.stroff, &MachOWriter::writeStringTable});
  }

  if (O.DyLdInfoCommandIndex) {
    const MachO::dyld_info_command &DyLdInfoCommand =
        O.LoadCommands[*O.DyLdInfoCommandIndex]
            .MachOLoadCommand.dyld_info_command_data;
    if (DyLdInfoCommand.rebase_off)
      Queue.push_back(
          {DyLdInfoCommand.rebase_off, &MachOWriter::writeRebaseInfo});
    if (DyLdInfoCommand.bind_off)
      Queue.push_back({DyLdInfoCommand.bind_off, &MachOWriter::writeBindInfo});
    if (DyLdInfoCommand.weak_bind_off)
      Queue.push_back(
          {DyLdInfoCommand.weak_bind_off, &MachOWriter::writeWeakBindInfo});
    if (DyLdInfoCommand.lazy_bind_off)
      Queue.push_back(
          {DyLdInfoCommand.lazy_bind_off, &MachOWriter::writeLazyBindInfo});
    if (DyLdInfoCommand.export_off)
      Queue.push_back(
          {DyLdInfoCommand.export_off, &MachOWriter::writeExportInfo});
  }

  if (O.DySymTabCommandIndex) {
    const MachO::dysymtab_command &DySymTabCommand =
        O.LoadCommands[*O.DySymTabCommandIndex]
            .MachOLoadCommand.dysymtab_command_data;

    if (DySymTabCommand.indirectsymoff)
      Queue.push_back({DySymTabCommand.indirectsymoff,
                       &MachOWriter::writeIndirectSymbolTable});
  }

  if (O.DataInCodeCommandIndex) {
    const MachO::linkedit_data_command &LinkEditDataCommand =
        O.LoadCommands[*O.DataInCodeCommandIndex]
            .MachOLoadCommand.linkedit_data_command_data;

    if (LinkEditDataCommand.dataoff)
      Queue.push_back(
          {LinkEditDataCommand.dataoff, &MachOWriter::writeDataInCodeData});
  }

  if (O.FunctionStartsCommandIndex) {
    const MachO::linkedit_data_command &LinkEditDataCommand =
        O.LoadCommands[*O.FunctionStartsCommandIndex]
            .MachOLoadCommand.linkedit_data_command_data;

    if (LinkEditDataCommand.dataoff)
      Queue.push_back(
          {LinkEditDataCommand.dataoff, &MachOWriter::writeFunctionStartsData});
  }

  llvm::sort(Queue, [](const WriteOperation &LHS, const WriteOperation &RHS) {
    return LHS.first < RHS.first;
  });

  for (auto WriteOp : Queue)
    (this->*WriteOp.second)();
}

uint32_t MachOWriter::computeSizeOfCmds() {
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

// Updates the index and the number of local/external/undefined symbols. Here we
// assume that MLC is a LC_DYSYMTAB and the nlist entries in the symbol table
// are already sorted by the those types.
void MachOWriter::updateDySymTab(MachO::macho_load_command &MLC) {
  uint32_t NumLocalSymbols = 0;
  auto Iter = O.SymTable.NameList.begin();
  auto End = O.SymTable.NameList.end();
  for (; Iter != End; Iter++) {
    if (Iter->n_type & MachO::N_EXT)
      break;

    NumLocalSymbols++;
  }

  uint32_t NumExtDefSymbols = 0;
  for (; Iter != End; Iter++) {
    if ((Iter->n_type & MachO::N_TYPE) == MachO::N_UNDF)
      break;

    NumExtDefSymbols++;
  }

  MLC.dysymtab_command_data.ilocalsym = 0;
  MLC.dysymtab_command_data.nlocalsym = NumLocalSymbols;
  MLC.dysymtab_command_data.iextdefsym = NumLocalSymbols;
  MLC.dysymtab_command_data.nextdefsym = NumExtDefSymbols;
  MLC.dysymtab_command_data.iundefsym = NumLocalSymbols + NumExtDefSymbols;
  MLC.dysymtab_command_data.nundefsym =
      O.SymTable.NameList.size() - (NumLocalSymbols + NumExtDefSymbols);
}

uint64_t MachOWriter::layoutSegments() {
  bool IsExecutable = O.Header.FileType == MachO::HeaderFileType::MH_EXECUTE;
  uint64_t Offset = IsExecutable ? 0 : (headerSize() + O.Header.SizeOfCmds);
  errs() << "IsExecutable: " << IsExecutable << "\n";
  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    StringRef Segname;
    uint64_t SegmentVmAddr = 0;
    uint64_t SegmentVmSize = 0;
    switch (MLC.load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      SegmentVmAddr = MLC.segment_command_data.vmaddr;
      SegmentVmSize = MLC.segment_command_data.vmsize;
      Segname = StringRef(MLC.segment_command_data.segname,
                          strnlen(MLC.segment_command_data.segname,
                                  sizeof(MLC.segment_command_data.segname)));
      break;
    case MachO::LC_SEGMENT_64:
      SegmentVmAddr = MLC.segment_command_64_data.vmaddr;
      SegmentVmSize = MLC.segment_command_64_data.vmsize;
      Segname = StringRef(MLC.segment_command_64_data.segname,
                          strnlen(MLC.segment_command_64_data.segname,
                                  sizeof(MLC.segment_command_64_data.segname)));
      break;
    default:
      continue;
    }

    if (Segname == "__LINKEDIT") {
      // We update the __LINKEDIT segment later (in layoutTail).
      assert(LC.Sections.empty() && "the __LINKEDIT segment has sections");
      LinkEditLoadCommand = &MLC;
      continue;
    }

    uint64_t SegOffset = Offset;
    uint64_t SegFileSize = 0;
    uint64_t VMSize = 0;
    for (auto &Sec : LC.Sections) {
      if (IsExecutable) {
        if (Sec.isVirtualSection()) {
          VMSize += Sec.Size;
        } else {
          errs() << "SectOff: " << Sec.Sectname << ", addr=" << Sec.Addr
                 << "\n";
          uint32_t SectOffset = Sec.Addr - SegmentVmAddr;
          Sec.Offset = SegOffset + SectOffset;
          Sec.Size = Sec.Content.size();
          SegFileSize = std::max(SegFileSize, SectOffset + Sec.Size);
          VMSize = std::max(VMSize, SegFileSize);
        }
      } else {
        if (!Sec.isVirtualSection()) {
          errs() << "SectOff: " << Sec.Sectname << ", segOff=" << SegOffset
                 << ", sz=" << SegFileSize << "\n";
          uint64_t PaddingSize = OffsetToAlignment(SegFileSize, 1 << Sec.Align);
          Sec.Offset = SegOffset + SegFileSize + PaddingSize;
          Sec.Size = Sec.Content.size();
          SegFileSize += PaddingSize + Sec.Size;
        }

        VMSize = std::max(VMSize, Sec.Addr + Sec.Size);
      }
    }

    if (IsExecutable) {
      Offset = alignTo(Offset + SegFileSize, PageSize);
      SegFileSize = alignTo(SegFileSize, PageSize);
      // Use the original vmsize if the segment is __PAGEZERO.
      VMSize =
          Segname == "__PAGEZERO" ? SegmentVmSize : alignTo(VMSize, PageSize);
      errs() << "VMSIZE(" << Segname << "): " << SegFileSize
             << ", to=" << VMSize << "\n";
    } else {
      Offset += SegFileSize;
    }

    errs() << "layout: seg='" << Segname << "', offset=" << SegOffset
           << ", size=" << SegFileSize << "\n";

    switch (MLC.load_command_data.cmd) {
    case MachO::LC_SEGMENT:
      MLC.segment_command_data.cmdsize =
          sizeof(MachO::segment_command) +
          sizeof(MachO::section) * LC.Sections.size();
      MLC.segment_command_data.nsects = LC.Sections.size();
      MLC.segment_command_data.fileoff = SegOffset;
      MLC.segment_command_data.vmsize = VMSize;
      MLC.segment_command_data.filesize = SegFileSize;
      break;
    case MachO::LC_SEGMENT_64:
      MLC.segment_command_64_data.cmdsize =
          sizeof(MachO::segment_command_64) +
          sizeof(MachO::section_64) * LC.Sections.size();
      MLC.segment_command_64_data.nsects = LC.Sections.size();
      MLC.segment_command_64_data.fileoff = SegOffset;
      MLC.segment_command_64_data.vmsize = VMSize;
      MLC.segment_command_64_data.filesize = SegFileSize;
      break;
    }
  }

  return Offset;
}

uint64_t MachOWriter::layoutRelocations(uint64_t Offset) {
  for (auto &LC : O.LoadCommands)
    for (auto &Sec : LC.Sections) {
      Sec.RelOff = Sec.Relocations.empty() ? 0 : Offset;
      Sec.NReloc = Sec.Relocations.size();
      Offset += sizeof(MachO::any_relocation_info) * Sec.NReloc;
    }

  return Offset;
}

Error MachOWriter::layoutTail(uint64_t Offset) {
  // The order of LINKEDIT elements as follows:
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
      StartOfSymbols + NListSize * O.SymTable.NameList.size();
  uint64_t StartOfSymbolStrings =
      StartOfIndirectSymbols +
      sizeof(uint32_t) * O.IndirectSymTable.Symbols.size();
  uint64_t LinkEditSize =
      (StartOfSymbolStrings + strTableSize()) - StartOfLinkEdit;

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
      MLC.symtab_command_data.nsyms = O.SymTable.NameList.size();
      MLC.symtab_command_data.stroff = StartOfSymbolStrings;
      MLC.symtab_command_data.strsize = strTableSize();
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
      MLC.dyld_info_command_data.rebase_off = O.Rebases.Opcodes.empty() ? 0 : StartOfRebaseInfo;
      MLC.dyld_info_command_data.rebase_size = O.Rebases.Opcodes.size();
      MLC.dyld_info_command_data.bind_off = O.Binds.Opcodes.empty() ? 0 : StartOfBindingInfo;
      MLC.dyld_info_command_data.bind_size = O.Binds.Opcodes.size();
      MLC.dyld_info_command_data.weak_bind_off = O.WeakBinds.Opcodes.empty() ? 0 : StartOfWeakBindingInfo;
      MLC.dyld_info_command_data.weak_bind_size = O.WeakBinds.Opcodes.size();
      MLC.dyld_info_command_data.lazy_bind_off = O.LazyBinds.Opcodes.empty() ? 0 : StartOfLazyBindingInfo;
      MLC.dyld_info_command_data.lazy_bind_size = O.LazyBinds.Opcodes.size();
      MLC.dyld_info_command_data.export_off = O.Exports.Trie.empty() ? 0 : StartOfExportTrie;
      MLC.dyld_info_command_data.export_size = O.Exports.Trie.size();
      break;
    case MachO::LC_LOAD_DYLINKER:
    case MachO::LC_MAIN:
    case MachO::LC_SEGMENT:
    case MachO::LC_SEGMENT_64:
    case MachO::LC_VERSION_MIN_MACOSX:
    case MachO::LC_BUILD_VERSION:
    case MachO::LC_ID_DYLIB:
    case MachO::LC_LOAD_DYLIB:
    case MachO::LC_UUID:
    case MachO::LC_RPATH:
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

// Recomputes and updates offset and size fields in load commands and sections
// since they could be modified.
Error MachOWriter::layout() {
  uint64_t Offset = layoutSegments();
  Offset = layoutRelocations(Offset);
  return layoutTail(Offset);
}

Error MachOWriter::finalize() {
  O.Header.NCmds = O.LoadCommands.size();
  O.Header.SizeOfCmds = computeSizeOfCmds();

  if (auto E = layout())
    return E;

  return Error::success();
}

Error MachOWriter::write() {
  if (Error E = B.allocate(totalSize()))
    return E;
  memset(B.getBufferStart(), 0, totalSize());
  writeHeader();
  writeLoadCommands();
  writeSections();
  writeTail();
  return B.commit();
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
