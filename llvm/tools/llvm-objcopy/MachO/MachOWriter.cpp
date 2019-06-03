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
#include <memory>

namespace llvm {
namespace objcopy {
namespace macho {

size_t MachOWriter::headerSize() const {
  return Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
}

size_t MachOWriter::loadCommandsSize() const { return O.Header.SizeOfCmds; }

size_t MachOWriter::symTableSize() const {
  return O.SymTable.Symbols.size() *
         (Is64Bit ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist));
}

size_t MachOWriter::totalSize() const {
  // Going from tail to head and looking for an appropriate "anchor" to
  // calculate the total size assuming that all the offsets are either valid
  // ("true") or 0 (0 indicates that the corresponding part is missing).

  SmallVector<size_t, 7> Ends;
  if (O.SymTabCommand) {
    if (O.SymTabCommand->symoff)
      Ends.push_back(O.SymTabCommand->symoff + symTableSize());
    if (O.SymTabCommand->stroff)
      Ends.push_back(O.SymTabCommand->stroff + O.SymTabCommand->strsize);
  }

  if (O.DyLdInfoCommand) {
    if (O.DyLdInfoCommand->rebase_off) {
      assert((O.DyLdInfoCommand->rebase_size == O.Rebases.Opcodes.size()) &&
             "Incorrect rebase opcodes size");
      Ends.push_back(O.DyLdInfoCommand->rebase_off + O.DyLdInfoCommand->rebase_size);
    }
    if (O.DyLdInfoCommand->bind_off) {
      assert((O.DyLdInfoCommand->bind_size == O.Binds.Opcodes.size()) &&
             "Incorrect bind opcodes size");
      Ends.push_back(O.DyLdInfoCommand->bind_off + O.DyLdInfoCommand->bind_size);
    }
    if (O.DyLdInfoCommand->weak_bind_off) {
      assert((O.DyLdInfoCommand->weak_bind_size == O.WeakBinds.Opcodes.size()) &&
             "Incorrect weak bind opcodes size");
      Ends.push_back(O.DyLdInfoCommand->weak_bind_off +
                     O.DyLdInfoCommand->weak_bind_size);
    }
    if (O.DyLdInfoCommand->lazy_bind_off) {
      assert((O.DyLdInfoCommand->lazy_bind_size == O.LazyBinds.Opcodes.size()) &&
             "Incorrect lazy bind opcodes size");
      Ends.push_back(O.DyLdInfoCommand->lazy_bind_off +
                     O.DyLdInfoCommand->lazy_bind_size);
    }
    if (O.DyLdInfoCommand->export_off) {
      assert((O.DyLdInfoCommand->export_size == O.Exports.Trie.size()) &&
             "Incorrect trie size");
      Ends.push_back(O.DyLdInfoCommand->export_off + O.DyLdInfoCommand->export_size);
    }
  }

  // Otherwise, use the last section / reloction.
  for (const auto &LC : O.LoadCommands)
    for (const auto &S : LC->Sections) {
      Ends.push_back(S.Offset + S.Size);
      if (S.RelOff)
        Ends.push_back(S.RelOff +
                       S.NReloc * sizeof(MachO::any_relocation_info));
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
  MachO::macho_load_command MLC;
  for (const auto &LC : O.LoadCommands) {
#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct)                         \
  case MachO::LCName:                                                          \
    assert(sizeof(MachO::LCStruct) + LC->Payload.size() ==                     \
           LC->MachOLoadCommand.load_command_data.cmdsize);                    \
    MLC = LC->MachOLoadCommand;                                                \
    if (IsLittleEndian != sys::IsLittleEndianHost)                             \
      MachO::swapStruct(MLC.LCStruct##_data);                                  \
    memcpy(Begin, &MLC.LCStruct##_data, sizeof(MachO::LCStruct));              \
    Begin += sizeof(MachO::LCStruct);                                          \
    memcpy(Begin, LC->Payload.data(), LC->Payload.size());                     \
    Begin += LC->Payload.size();                                               \
    break;

    switch (LC->MachOLoadCommand.load_command_data.cmd) {
    default:
      assert(sizeof(MachO::load_command) + LC->Payload.size() ==
             LC->MachOLoadCommand.load_command_data.cmdsize);
      MLC = LC->MachOLoadCommand;
      if (IsLittleEndian != sys::IsLittleEndianHost)
        MachO::swapStruct(MLC.load_command_data);
      memcpy(Begin, &MLC.load_command_data, sizeof(MachO::load_command));
      Begin += sizeof(MachO::load_command);
      memcpy(Begin, LC->Payload.data(), LC->Payload.size());
      Begin += LC->Payload.size();
      break;
#include "llvm/BinaryFormat/MachO.def"
    }
  }
}

void MachOWriter::updateSymbolIndexes() {
  auto Index = 0;
  for (auto &Symbol : O.SymTable.Symbols) {
    Symbol->Index = Index;
    Index++;
  }
}

void MachOWriter::writeSections() {
  for (const auto &LC : O.LoadCommands)
    for (const auto &Sec : LC->Sections) {
      if (!Sec.Offset) continue; // FIXME:
      assert(Sec.Offset && "Section offset can not be zero");
      assert((Sec.Size == Sec.Content.size()) && "Incorrect section size");
      memcpy(B.getBufferStart() + Sec.Offset, Sec.Content.data(),
             Sec.Content.size());
      for (size_t Index = 0; Index < Sec.Relocations.size(); ++Index) {
        auto RelocInfo = Sec.Relocations[Index];
        RelocInfo.Info.r_symbolnum = RelocInfo.Symbol->Index;
        if (IsLittleEndian != sys::IsLittleEndianHost)
          MachO::swapStruct(reinterpret_cast<MachO::any_relocation_info &>(RelocInfo.Info));
        memcpy(B.getBufferStart() + Sec.RelOff +
                   Index * sizeof(MachO::any_relocation_info),
               &RelocInfo.Info, sizeof(RelocInfo.Info));
      }
    }
}

template <typename NListType>
void writeNListEntry(const SymbolEntry &SE, bool IsLittleEndian, char *&Out, uint32_t Nstrx) {
  NListType ListEntry;
  ListEntry.n_strx = Nstrx;
  ListEntry.n_type = SE.n_type;
  ListEntry.n_sect = SE.n_sect;
  ListEntry.n_desc = SE.n_desc;
  ListEntry.n_value = SE.n_value;

  if (IsLittleEndian != sys::IsLittleEndianHost)
    MachO::swapStruct(ListEntry);
  memcpy(Out, reinterpret_cast<const char *>(&ListEntry), sizeof(NListType));
  Out += sizeof(NListType);
}

void MachOWriter::writeSymbolTable() {
  if (!O.SymTabCommand)
    return;

  // FIXME: do this before writeLoadCommands()
  O.SymTabCommand->nsyms = O.SymTable.Symbols.size();

  char *StrTable = (char *)B.getBufferStart() + O.SymTabCommand->stroff;
  char *SymTable = (char *)B.getBufferStart() + O.SymTabCommand->symoff;
  auto Offset = 1; // SKip the first empty string.
  for (auto Iter = O.SymTable.Symbols.begin(); Iter != O.SymTable.Symbols.end(); Iter++) {
    std::unique_ptr<SymbolEntry> &Sym = *Iter;
    uint32_t Nstrx = Offset;
    // Write the string table entry corresponding to the symbol.
    memcpy(StrTable + Offset, Sym->Name.data(), Sym->Name.size());
    Offset += Sym->Name.size();
    if (Iter + 1 != O.SymTable.Symbols.end()) {
      StrTable[Offset] = '\0';
      Offset += 1;
    }

    if (Is64Bit)
      writeNListEntry<MachO::nlist_64>(*Sym, IsLittleEndian, SymTable, Nstrx);
    else
      writeNListEntry<MachO::nlist>(*Sym, IsLittleEndian, SymTable, Nstrx);
  }
}

void MachOWriter::writeRebaseInfo() {
  if (!O.DyLdInfoCommand)
    return;

  char *Out = (char *)B.getBufferStart() + O.DyLdInfoCommand->rebase_off;
  assert((O.DyLdInfoCommand->rebase_size == O.Rebases.Opcodes.size()) &&
         "Incorrect rebase opcodes size");
  memcpy(Out, O.Rebases.Opcodes.data(), O.Rebases.Opcodes.size());
}

void MachOWriter::writeBindInfo() {
  if (!O.DyLdInfoCommand)
    return;

  char *Out = (char *)B.getBufferStart() + O.DyLdInfoCommand->bind_off;
  assert((O.DyLdInfoCommand->bind_size == O.Binds.Opcodes.size()) &&
         "Incorrect bind opcodes size");
  memcpy(Out, O.Binds.Opcodes.data(), O.Binds.Opcodes.size());
}

void MachOWriter::writeWeakBindInfo() {
  if (!O.DyLdInfoCommand)
    return;

  char *Out = (char *)B.getBufferStart() + O.DyLdInfoCommand->weak_bind_off;
  assert((O.DyLdInfoCommand->weak_bind_size == O.WeakBinds.Opcodes.size()) &&
         "Incorrect weak bind opcodes size");
  memcpy(Out, O.WeakBinds.Opcodes.data(), O.WeakBinds.Opcodes.size());
}

void MachOWriter::writeLazyBindInfo() {
  if (!O.DyLdInfoCommand)
    return;

  char *Out = (char *)B.getBufferStart() + O.DyLdInfoCommand->lazy_bind_off;
  assert((O.DyLdInfoCommand->lazy_bind_size == O.LazyBinds.Opcodes.size()) &&
         "Incorrect lazy bind opcodes size");
  memcpy(Out, O.LazyBinds.Opcodes.data(), O.LazyBinds.Opcodes.size());
}

void MachOWriter::writeExportInfo() {
  if (!O.DyLdInfoCommand)
    return;

  char *Out = (char *)B.getBufferStart() + O.DyLdInfoCommand->export_off;
  assert((O.DyLdInfoCommand->export_size == O.Exports.Trie.size()) &&
         "Incorrect export trie size");
  memcpy(Out, O.Exports.Trie.data(), O.Exports.Trie.size());
}

void MachOWriter::writeTail() {
  typedef void (MachOWriter::*WriteHandlerType)(void);
  typedef std::pair<uint64_t, WriteHandlerType> WriteOperation;
  SmallVector<WriteOperation, 7> Queue;

  if (O.SymTabCommand && O.SymTabCommand->symoff)
      Queue.push_back({O.SymTabCommand->symoff, &MachOWriter::writeSymbolTable});

  if (O.DyLdInfoCommand) {
    if (O.DyLdInfoCommand->rebase_off)
      Queue.push_back(
          {O.DyLdInfoCommand->rebase_off, &MachOWriter::writeRebaseInfo});
    if (O.DyLdInfoCommand->bind_off)
      Queue.push_back({O.DyLdInfoCommand->bind_off, &MachOWriter::writeBindInfo});
    if (O.DyLdInfoCommand->weak_bind_off)
      Queue.push_back(
          {O.DyLdInfoCommand->weak_bind_off, &MachOWriter::writeWeakBindInfo});
    if (O.DyLdInfoCommand->lazy_bind_off)
      Queue.push_back(
          {O.DyLdInfoCommand->lazy_bind_off, &MachOWriter::writeLazyBindInfo});
    if (O.DyLdInfoCommand->export_off)
      Queue.push_back(
          {O.DyLdInfoCommand->export_off, &MachOWriter::writeExportInfo});
  }

  llvm::sort(Queue, [](const WriteOperation &LHS, const WriteOperation &RHS) {
    return LHS.first < RHS.first;
  });

  for (auto WriteOp : Queue)
    (this->*WriteOp.second)();
}

Error MachOWriter::write() {
  if (Error E = B.allocate(totalSize()))
    return E;
  memset(B.getBufferStart(), 0, totalSize());
  writeHeader();
  updateSymbolIndexes();
  writeLoadCommands();
  writeSections();
  writeTail();
  return B.commit();
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
