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

  // Otherwise, use the last section / reloction.
  for (const auto &LC : O.LoadCommands)
    for (const auto &S : LC.Sections) {
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

static void copyFixedSizedString(char *Dest, StringRef Src, size_t DestLen) {
  size_t CopyLen = std::min(Src.size(), DestLen);
  memcpy(reinterpret_cast<void *>(Dest), Src.data(), CopyLen);
  memset(reinterpret_cast<void *>(Dest + CopyLen), '\0', DestLen - CopyLen);
}

void MachOWriter::writeLoadCommands() {
  uint8_t *Begin = B.getBufferStart() + headerSize();
  MachO::macho_load_command MLC;
  for (const auto &LC : O.LoadCommands) {
    // Construct new load command.
    MLC = LC.MachOLoadCommand;
    switch (MLC.load_command_data.cmd) {
      case MachO::LC_SEGMENT_64:
        errs() << MLC.segment_command_64_data.cmdsize << "\n";
        MLC.segment_command_64_data.nsects = LC.Sections.size();
        MLC.segment_command_64_data.cmdsize = sizeof(MachO::segment_command_64) + sizeof(MachO::section_64) * LC.Sections.size();

        if (IsLittleEndian != sys::IsLittleEndianHost)
          MachO::swapStruct(MLC.segment_command_64_data);
        memcpy(Begin, &MLC.segment_command_64_data, sizeof(MachO::segment_command_64));
        Begin += sizeof(MachO::segment_command_64);

        for (auto &Sec : LC.Sections) {
          struct MachO::section_64 Temp;
          copyFixedSizedString(Temp.sectname, Sec.Sectname, 16);
          copyFixedSizedString(Temp.segname, Sec.Segname, 16);
          Temp.addr = Sec.Addr;
          Temp.size = Sec.Size;
          Temp.offset = Sec.Offset;
          Temp.align = Sec.Align;
          Temp.reloff = Sec.RelOff;
          Temp.nreloc = Sec.NReloc;
          Temp.flags = Sec.Flags;
          Temp.reserved1 = Sec.Reserved1;
          Temp.reserved2 = Sec.Reserved2;
          Temp.reserved3 = Sec.Reserved3;

          if (IsLittleEndian != sys::IsLittleEndianHost)
            MachO::swapStruct(Temp);
          memcpy(Begin, &Temp, sizeof(MachO::section_64));
          Begin += sizeof(MachO::section_64);
        }
        continue;
      case MachO::LC_SYMTAB:
        // FIXME: delete me
        errs() << "LC_SYMTAB: " << MLC.symtab_command_data.symoff << " addr=" << &MLC.symtab_command_data.symoff << "\n";
        if (IsLittleEndian != sys::IsLittleEndianHost)
          MachO::swapStruct(MLC.symtab_command_data);
        memcpy(Begin, &MLC.dysymtab_command_data, sizeof(MachO::symtab_command));
        Begin += sizeof(MachO::symtab_command);
        continue;
      case MachO::LC_DYSYMTAB:
        // FIXME: delete me
        errs() << "LC_DYSYM " << MLC.dysymtab_command_data.nextrefsyms << "\n";
        if (IsLittleEndian != sys::IsLittleEndianHost)
          MachO::swapStruct(MLC.dysymtab_command_data);
        memcpy(Begin, &MLC.dysymtab_command_data, sizeof(MachO::dysymtab_command));
        Begin += sizeof(MachO::dysymtab_command);
        continue;
    }

#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct)                         \
  case MachO::LCName:                                                          \
    assert(sizeof(MachO::LCStruct) + LC.Payload.size() ==                      \
           LC.MachOLoadCommand.load_command_data.cmdsize);                     \
    MLC = LC.MachOLoadCommand;                                                 \
    if (IsLittleEndian != sys::IsLittleEndianHost)                             \
      MachO::swapStruct(MLC.LCStruct##_data);                                  \
    memcpy(Begin, &MLC.LCStruct##_data, sizeof(MachO::LCStruct));              \
    Begin += sizeof(MachO::LCStruct);                                          \
    memcpy(Begin, LC.Payload.data(), LC.Payload.size());                       \
    Begin += LC.Payload.size();                                                \
    break;

    // Copy the load command as it is.
    switch (LC.MachOLoadCommand.load_command_data.cmd) {
    default:
      assert(sizeof(MachO::load_command) + LC.Payload.size() ==
             LC.MachOLoadCommand.load_command_data.cmdsize);
      MLC = LC.MachOLoadCommand;
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

void MachOWriter::writeSections() {
  for (const auto &LC : O.LoadCommands)
    for (const auto &Sec : LC.Sections) {
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

  llvm::sort(Queue, [](const WriteOperation &LHS, const WriteOperation &RHS) {
    return LHS.first < RHS.first;
  });

  for (auto WriteOp : Queue)
    (this->*WriteOp.second)();
}

void MachOWriter::updateLoadCommandsSize() {
  auto Size = 0;
  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    auto cmd = MLC.load_command_data.cmd;

    switch (cmd) {
      case MachO::LC_SEGMENT_64:
        Size += sizeof(MachO::segment_command_64) + sizeof(MachO::section_64) * LC.Sections.size();
        continue;
    }

    switch (cmd) {
#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct) \
  case MachO::LCName:                                  \
    errs() << "lc size: " << sizeof(MachO::LCStruct) << "\n"; \
    Size += sizeof(MachO::LCStruct);                   \
    break;
#include "llvm/BinaryFormat/MachO.def"
#undef HANDLE_LOAD_COMMAND
    }
  }

  O.Header.SizeOfCmds = Size;
}

// Updates offset fields in load commands and sections since they would be
// corrupted if some sections are added, removed, or modified.
Error MachOWriter::updateOffsets() {
  // Compute and update the size of load commands.
  auto SizeOfCmds = loadCommandsSize();
  O.Header.NCmds = O.LoadCommands.size();
  O.Header.SizeOfCmds = SizeOfCmds;

  auto Offset = headerSize() + SizeOfCmds;
  errs() << "SizeOfCmds: " << (SizeOfCmds) << "\n";
  errs() << "Offset:     " << (Offset) << "\n";
  size_t SegSize;
  // Section data.
  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    switch (MLC.load_command_data.cmd) {
      case MachO::LC_SEGMENT_64:
        MLC.segment_command_64_data.fileoff = Offset;
        SegSize = 0;
        for (auto &Sec : LC.Sections) {
          Sec.Size = Sec.Content.size(); // FIXME: is this really the size of contents?
          if (Sec.Align)
            Offset = alignTo(Offset, 1 << Sec.Align);
          Sec.Offset = Offset; // TODO: alignment
          Offset += Sec.Size;

          Sec.NReloc = Sec.Relocations.size();
          SegSize += Sec.Size + sizeof(MachO::any_relocation_info) * Sec.NReloc;
        }
        // MLC.segment_command_64_data.filesize = SegSize;
        //MLC.segment_command_64_data.vmsize = SegSize; // TODO:
      break;
    }
  }

  // Relocations.
  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    switch (MLC.load_command_data.cmd) {
      case MachO::LC_SEGMENT_64:
        for (auto &Sec : LC.Sections) {
          auto RelSize = sizeof(MachO::any_relocation_info) * Sec.Relocations.size();
          if (RelSize == 0)
            continue;

          Sec.RelOff = Offset;
          Offset += RelSize;
        }
      break;
    }
  }

  // Tail stuff.
  errs() << "Tail start: " << Offset << "\n";
  auto NListSize = Is64Bit ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist);
  for (auto &LC : O.LoadCommands) {
    auto &MLC = LC.MachOLoadCommand;
    auto cmd = MLC.load_command_data.cmd;
    switch (cmd) {
      case MachO::LC_SYMTAB:
        errs() << "Updating symtab: " << Offset << ", size=" << NListSize * O.SymTable.NameList.size() << " addr=" << &MLC.symtab_command_data.symoff << "\n";
        MLC.symtab_command_data.symoff = Offset;
        MLC.symtab_command_data.nsyms = O.SymTable.NameList.size();
        Offset += NListSize * MLC.symtab_command_data.nsyms;
        MLC.symtab_command_data.stroff = Offset;
        Offset += MLC.symtab_command_data.strsize;
        break;
      case MachO::LC_DYSYMTAB:
        // FIXME:
        errs() <<"LC_DYSYMTAB\n";
        MLC.dysymtab_command_data.tocoff = 0;
        MLC.dysymtab_command_data.ntoc = 0;
        MLC.dysymtab_command_data.modtaboff = 0;
        MLC.dysymtab_command_data.nmodtab = 0;
        MLC.dysymtab_command_data.extrefsymoff = 0;
        MLC.dysymtab_command_data.nextrefsyms = 0;
        MLC.dysymtab_command_data.indirectsymoff = 0;
        MLC.dysymtab_command_data.nindirectsyms = 0;
        MLC.dysymtab_command_data.extreloff = 0;
        MLC.dysymtab_command_data.nextrel = 0;
        MLC.dysymtab_command_data.locreloff = 0;
        MLC.dysymtab_command_data.nlocrel = 0;
        break;
      case MachO::LC_SEGMENT_64:
        // Do nothing.
        break;
      default:
        // Abort if it's unsupported to prevent corrupting the object.
        break;
        // TODO: return createStringError(llvm::errc::not_supported, "unsupported load command (cmd=%d)", cmd);
    }
  }

  return Error::success();
}

Error MachOWriter::finalize() {
  errs() << "prev cmdsize: " << loadCommandsSize() << "\n";
  updateLoadCommandsSize();

  if (auto E = updateOffsets())
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
