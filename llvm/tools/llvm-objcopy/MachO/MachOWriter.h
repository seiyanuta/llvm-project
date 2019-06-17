//===- MachOWriter.h --------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../Buffer.h"
#include "MachOObjcopy.h"
#include "Object.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"

namespace llvm {
class Error;

namespace objcopy {
namespace macho {

class MachOWriter {
  Object &O;
  bool Is64Bit;
  bool IsLittleEndian;
  uint64_t PageSize;
  Buffer &B;
  StringTableBuilder StrTableBuilder{StringTableBuilder::MachO};
  // Points to the __LINKEDIT segment if it exists.
  MachO::macho_load_command *LinkEditLoadCommand = nullptr;

  size_t headerSize() const;
  size_t loadCommandsSize() const;
  size_t symTableSize() const;
  size_t strTableSize() const;

  void updateDySymTab(MachO::macho_load_command &MLC);
  void updateSizeOfCmds();
  void updateSymbolIndexes();
  void constructStringTable();
  uint32_t computeSizeOfCmds();
  uint64_t layoutSegments();
  uint64_t layoutRelocations(uint64_t Offset);
  Error layoutTail(uint64_t Offset);
  Error layout();

  void writeHeader();
  void writeLoadCommands();
  template <typename StructType>
  void writeSectionInLoadCommand(const Section &Sec, uint8_t *&Out);
  void writeSections();
  void writeSymbolTable();
  void writeStringTable();
  void writeRebaseInfo();
  void writeBindInfo();
  void writeWeakBindInfo();
  void writeLazyBindInfo();
  void writeExportInfo();
  void writeIndirectSymbolTable();
  void writeDataInCodeData();
  void writeFunctionStartsData();
  void writeTail();

public:
  MachOWriter(Object &O, bool Is64Bit, bool IsLittleEndian, uint64_t PageSize,
              Buffer &B)
      : O(O), Is64Bit(Is64Bit), IsLittleEndian(IsLittleEndian),
        PageSize(PageSize), B(B) {}

  size_t totalSize() const;
  Error finalize();
  Error write();
};

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
