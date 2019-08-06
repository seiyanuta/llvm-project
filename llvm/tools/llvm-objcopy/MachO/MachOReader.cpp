//===- MachOReader.cpp ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MachOReader.h"
#include "../llvm-objcopy.h"
#include "Object.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"
#include <memory>

namespace llvm {
namespace objcopy {
namespace macho {

void MachOReader::readHeader(Object &O) const {
  O.Header.Magic = MachOObj.getHeader().magic;
  O.Header.CPUType = MachOObj.getHeader().cputype;
  O.Header.CPUSubType = MachOObj.getHeader().cpusubtype;
  O.Header.FileType = MachOObj.getHeader().filetype;
  O.Header.NCmds = MachOObj.getHeader().ncmds;
  O.Header.SizeOfCmds = MachOObj.getHeader().sizeofcmds;
  O.Header.Flags = MachOObj.getHeader().flags;
}

template <typename SectionType>
Section constructSectionCommon(SectionType Sec) {
  StringRef Sectname(Sec.sectname, strnlen(Sec.sectname, sizeof(Sec.sectname)));
  StringRef Segname(Sec.segname, strnlen(Sec.segname, sizeof(Sec.sectname)));
  Section S(Segname, Sectname);
  S.Addr = Sec.addr;
  S.Size = Sec.size;
  S.Offset = Sec.offset;
  S.Align = Sec.align;
  S.RelOff = Sec.reloff;
  S.NReloc = Sec.nreloc;
  S.Flags = Sec.flags;
  S.Reserved1 = Sec.reserved1;
  S.Reserved2 = Sec.reserved2;
  S.Reserved3 = 0;
  return S;
}

template <typename SectionType> Section constructSection(SectionType Sec);

template <> Section constructSection(MachO::section Sec) {
  return constructSectionCommon(Sec);
}

template <> Section constructSection(MachO::section_64 Sec) {
  Section S = constructSectionCommon(Sec);
  S.Reserved3 = Sec.reserved3;
  return S;
}

// TODO: get rid of reportError and make MachOReader return Expected<> instead.
template <typename SectionType, typename SegmentType>
std::vector<Section>
extractSections(const object::MachOObjectFile::LoadCommandInfo &LoadCmd,
                const object::MachOObjectFile &MachOObj,
                size_t &NextSectionIndex) {
  auto End = LoadCmd.Ptr + LoadCmd.C.cmdsize;
  const SectionType *Curr =
      reinterpret_cast<const SectionType *>(LoadCmd.Ptr + sizeof(SegmentType));
  std::vector<Section> Sections;
  for (; reinterpret_cast<const void *>(Curr) < End; Curr++) {
    if (MachOObj.isLittleEndian() != sys::IsLittleEndianHost) {
      SectionType Sec;
      memcpy((void *)&Sec, Curr, sizeof(SectionType));
      MachO::swapStruct(Sec);
      Sections.push_back(constructSection(Sec));
    } else {
      Sections.push_back(constructSection(*Curr));
    }

    Section &S = Sections.back();

    Expected<object::SectionRef> SecRef =
        MachOObj.getSection(NextSectionIndex++);
    if (!SecRef)
      reportError(MachOObj.getFileName(), SecRef.takeError());

    if (Expected<ArrayRef<uint8_t>> E =
            MachOObj.getSectionContents(SecRef->getRawDataRefImpl()))
      S.Content =
          StringRef(reinterpret_cast<const char *>(E->data()), E->size());
    else
      reportError(MachOObj.getFileName(), E.takeError());

    S.Relocations.reserve(S.NReloc);
    for (auto RI = MachOObj.section_rel_begin(SecRef->getRawDataRefImpl()),
              RE = MachOObj.section_rel_end(SecRef->getRawDataRefImpl());
         RI != RE; ++RI) {
      RelocationInfo R;
      R.Symbol = nullptr; // We'll fill this field later.
      R.Info = MachOObj.getRelocation(RI->getRawDataRefImpl());
      R.Scattered = MachOObj.isRelocationScattered(R.Info);
      S.Relocations.push_back(R);
    }

    assert(S.NReloc == S.Relocations.size() &&
           "Incorrect number of relocations");
  }
  return Sections;
}

void MachOReader::readLoadCommands(Object &O) const {
  // For MachO sections indices start from 1.
  size_t NextSectionIndex = 1;
  for (auto LoadCmd : MachOObj.load_commands()) {
    LoadCommand LC;
    switch (LoadCmd.C.cmd) {
    case MachO::LC_SEGMENT:
      LC.Sections = extractSections<MachO::section, MachO::segment_command>(
          LoadCmd, MachOObj, NextSectionIndex);
      break;
    case MachO::LC_SEGMENT_64:
      LC.Sections =
          extractSections<MachO::section_64, MachO::segment_command_64>(
              LoadCmd, MachOObj, NextSectionIndex);
      break;
    case MachO::LC_SYMTAB:
      O.SymTabCommandIndex = O.LoadCommands.size();
      break;
    case MachO::LC_DYSYMTAB:
      O.DySymTabCommandIndex = O.LoadCommands.size();
      break;
    case MachO::LC_DYLD_INFO:
    case MachO::LC_DYLD_INFO_ONLY:
      O.DyLdInfoCommandIndex = O.LoadCommands.size();
      break;
    case MachO::LC_DATA_IN_CODE:
      O.DataInCodeCommandIndex = O.LoadCommands.size();
      break;
    case MachO::LC_FUNCTION_STARTS:
      O.FunctionStartsCommandIndex = O.LoadCommands.size();
      break;
    }
#define HANDLE_LOAD_COMMAND(LCName, LCValue, LCStruct)                         \
  case MachO::LCName:                                                          \
    memcpy((void *)&(LC.MachOLoadCommand.LCStruct##_data), LoadCmd.Ptr,        \
           sizeof(MachO::LCStruct));                                           \
    if (MachOObj.isLittleEndian() != sys::IsLittleEndianHost)                  \
      MachO::swapStruct(LC.MachOLoadCommand.LCStruct##_data);                  \
    LC.Payload = ArrayRef<uint8_t>(                                            \
        reinterpret_cast<uint8_t *>(const_cast<char *>(LoadCmd.Ptr)) +         \
            sizeof(MachO::LCStruct),                                           \
        LoadCmd.C.cmdsize - sizeof(MachO::LCStruct));                          \
    break;

    switch (LoadCmd.C.cmd) {
    default:
      memcpy((void *)&(LC.MachOLoadCommand.load_command_data), LoadCmd.Ptr,
             sizeof(MachO::load_command));
      if (MachOObj.isLittleEndian() != sys::IsLittleEndianHost)
        MachO::swapStruct(LC.MachOLoadCommand.load_command_data);
      LC.Payload = ArrayRef<uint8_t>(
          reinterpret_cast<uint8_t *>(const_cast<char *>(LoadCmd.Ptr)) +
              sizeof(MachO::load_command),
          LoadCmd.C.cmdsize - sizeof(MachO::load_command));
      break;
#include "llvm/BinaryFormat/MachO.def"
    }
    O.LoadCommands.push_back(std::move(LC));
  }
}

template <typename nlist_t>
SymbolEntry constructSymbolEntry(StringRef StrTable, const nlist_t &nlist) {
  assert(nlist.n_strx < StrTable.size() &&
         "n_strx exceeds the size of the string table");
  return SymbolEntry(StringRef(StrTable.data() + nlist.n_strx).str(),
                     nlist.n_type, nlist.n_sect, nlist.n_desc, nlist.n_value);
}

void MachOReader::readSymbolTable(Object &O) const {
  StringRef StrTable = MachOObj.getStringTableData();
  for (auto Symbol : MachOObj.symbols()) {
    SymbolEntry SE =
        (MachOObj.is64Bit()
             ? constructSymbolEntry(
                   StrTable,
                   MachOObj.getSymbol64TableEntry(Symbol.getRawDataRefImpl()))
             : constructSymbolEntry(
                   StrTable,
                   MachOObj.getSymbolTableEntry(Symbol.getRawDataRefImpl())));

    O.SymTable.Symbols.push_back(std::make_unique<SymbolEntry>(SE));
  }
}

void MachOReader::setSymbolInRelocationInfo(Object &O) const {
  for (auto &LC : O.LoadCommands)
    for (auto &Sec : LC.Sections)
      for (auto &Reloc : Sec.Relocations)
        if (!Reloc.Scattered) {
          auto *Info = reinterpret_cast<MachO::relocation_info *>(&Reloc.Info);
          Reloc.Symbol = O.SymTable.getSymbolByIndex(Info->r_symbolnum);
        }
}

void MachOReader::readRebaseInfo(Object &O) const {
  O.Rebases.Opcodes = MachOObj.getDyldInfoRebaseOpcodes();
}

void MachOReader::readBindInfo(Object &O) const {
  O.Binds.Opcodes = MachOObj.getDyldInfoBindOpcodes();
}

void MachOReader::readWeakBindInfo(Object &O) const {
  O.WeakBinds.Opcodes = MachOObj.getDyldInfoWeakBindOpcodes();
}

void MachOReader::readLazyBindInfo(Object &O) const {
  O.LazyBinds.Opcodes = MachOObj.getDyldInfoLazyBindOpcodes();
}

void MachOReader::readExportInfo(Object &O) const {
  O.Exports.Trie = MachOObj.getDyldInfoExportsTrie();
}

void MachOReader::readDataInCodeData(Object &O) const {
  if (!O.DataInCodeCommandIndex)
    return;
  const MachO::linkedit_data_command &LDC =
      O.LoadCommands[*O.DataInCodeCommandIndex]
          .MachOLoadCommand.linkedit_data_command_data;

  O.DataInCode.Data = arrayRefFromStringRef(
      MachOObj.getData().substr(LDC.dataoff, LDC.datasize));
}

void MachOReader::readFunctionStartsData(Object &O) const {
  if (!O.FunctionStartsCommandIndex)
    return;
  const MachO::linkedit_data_command &LDC =
      O.LoadCommands[*O.FunctionStartsCommandIndex]
          .MachOLoadCommand.linkedit_data_command_data;

  O.FunctionStarts.Data = arrayRefFromStringRef(
      MachOObj.getData().substr(LDC.dataoff, LDC.datasize));
}

void MachOReader::readIndirectSymbolTable(Object &O) const {
  MachO::dysymtab_command DySymTab = MachOObj.getDysymtabLoadCommand();
  for (uint32_t i = 0; i < DySymTab.nindirectsyms; ++i) {
    uint32_t Index = MachOObj.getIndirectSymbolTableEntry(DySymTab, i);
    if ((Index &
        (MachO::INDIRECT_SYMBOL_LOCAL | MachO::INDIRECT_SYMBOL_ABS)) != 0)
      O.IndirectSymTable.Symbols.emplace_back(Index, None);
    else
      O.IndirectSymTable.Symbols.emplace_back(
          Index, O.SymTable.getSymbolByIndex(Index));
  }
}

std::unique_ptr<Object> MachOReader::create() const {
  auto Obj = std::make_unique<Object>();
  readHeader(*Obj);
  readLoadCommands(*Obj);
  readSymbolTable(*Obj);
  setSymbolInRelocationInfo(*Obj);
  readRebaseInfo(*Obj);
  readBindInfo(*Obj);
  readWeakBindInfo(*Obj);
  readLazyBindInfo(*Obj);
  readExportInfo(*Obj);
  readDataInCodeData(*Obj);
  readFunctionStartsData(*Obj);
  readIndirectSymbolTable(*Obj);
  return Obj;
}

std::unique_ptr<Object> BinaryReader::create() const {
  assert(MI.Is64Bit && "32-bit object is not yet supported");
  auto Obj = std::make_unique<Object>();

  size_t HeaderSize =
      MI.Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
  size_t SegLoadCommandSize =
      MI.Is64Bit
          ? (sizeof(MachO::segment_command_64) + sizeof(MachO::section_64))
          : (sizeof(MachO::segment_command) + sizeof(MachO::section));
  size_t SizeOfCmds = SegLoadCommandSize + sizeof(MachO::symtab_command) +
                      sizeof(MachO::dysymtab_command);
  uint64_t SegOffset = HeaderSize + SizeOfCmds;
  Obj->Header.Magic = MI.Is64Bit ? MachO::MH_MAGIC_64 : MachO::MH_MAGIC;
  Obj->Header.CPUType = MI.MachOCPUType;
  Obj->Header.CPUSubType = MI.MachOCPUSubType;
  Obj->Header.FileType = MachO::MH_OBJECT;
  Obj->Header.NCmds = 1;
  Obj->Header.Flags = 0;
  Obj->Header.SizeOfCmds = SizeOfCmds;

  LoadCommand &LC = Obj->addSegment("");
  MachO::segment_command_64 &Seg = LC.MachOLoadCommand.segment_command_64_data;
  Seg.vmsize = SectionContent->getBufferSize();
  Seg.fileoff = SegOffset;
  Seg.filesize = SectionContent->getBufferSize();
  Seg.maxprot =
      MachO::VM_PROT_READ | MachO::VM_PROT_WRITE | MachO::VM_PROT_EXECUTE;
  Seg.initprot =
      MachO::VM_PROT_READ | MachO::VM_PROT_WRITE | MachO::VM_PROT_EXECUTE;

  Section Sec("__DATA", "__data");
  Sec.Size = SectionContent->getBufferSize();
  Sec.Offset = SegOffset;
  memcpy(SectionContent->getBufferStart(), Input.getBufferStart(),
         Input.getBufferSize());
  Sec.setOwnedContentData(ArrayRef<uint8_t>(
      reinterpret_cast<const uint8_t *>(SectionContent->getBufferStart()),
      SectionContent->getBufferSize()));
  LC.Sections.push_back(Sec);

  LoadCommand DySymTab;
  MachO::dysymtab_command &DySymTabData =
      DySymTab.MachOLoadCommand.dysymtab_command_data;
  memset(&DySymTabData, 0, sizeof(DySymTabData));
  DySymTabData.cmd = MachO::LC_DYSYMTAB;
  DySymTabData.cmdsize = sizeof(MachO::dysymtab_command);

  LoadCommand SymTab;
  MachO::symtab_command &SymTabData =
      SymTab.MachOLoadCommand.symtab_command_data;
  memset(&SymTabData, 0, sizeof(SymTabData));
  SymTabData.cmd = MachO::LC_SYMTAB;
  SymTabData.cmdsize = sizeof(MachO::symtab_command);

  Obj->LoadCommands.push_back(DySymTab);
  Obj->DySymTabCommandIndex = Obj->LoadCommands.size() - 1;
  Obj->LoadCommands.push_back(SymTab);
  Obj->SymTabCommandIndex = Obj->LoadCommands.size() - 1;

  std::string SanitizedFilename = Input.getBufferIdentifier().str();
  // Replace characters that are not valid for symbol name.
  std::replace_if(
      std::begin(SanitizedFilename), std::end(SanitizedFilename),
      [](char C) { return !isalnum(C); }, '_');
  Twine Prefix = Twine("__binary_") + SanitizedFilename;

  Obj->SymTable.addSymbol(Prefix + "_size", MachO::N_ABS | MachO::N_EXT, 0,
                          MachO::REFERENCE_FLAG_DEFINED, Input.getBufferSize());
  Obj->SymTable.addSymbol(Prefix + "_start", MachO::N_SECT | MachO::N_EXT, 1,
                          MachO::REFERENCE_FLAG_DEFINED, 0);
  Obj->SymTable.addSymbol(Prefix + "_end", MachO::N_SECT | MachO::N_EXT, 1,
                          MachO::REFERENCE_FLAG_DEFINED,
                          SectionContent->getBufferSize());

  return Obj;
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
