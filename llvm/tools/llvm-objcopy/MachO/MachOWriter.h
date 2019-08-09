//===- MachOWriter.h --------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../Buffer.h"
#include "MachOLayoutBuilder.h"
#include "MachOObjcopy.h"
#include "Object.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"

namespace llvm {
class Error;

namespace objcopy {
namespace macho {

class Writer {
public:
  virtual ~Writer() {}
  virtual Error finalize() = 0;
  virtual Error write() = 0;
};

class MachOWriter : public Writer {
  Object &O;
  bool Is64Bit;
  bool IsLittleEndian;
  uint64_t PageSize;
  Buffer &B;
  MachOLayoutBuilder LayoutBuilder;

  size_t headerSize() const;
  size_t loadCommandsSize() const;
  size_t symTableSize() const;
  size_t strTableSize() const;

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
        PageSize(PageSize), B(B), LayoutBuilder(O, Is64Bit, PageSize) {}
  ~MachOWriter() {}
  size_t totalSize() const;
  Error finalize() override;
  Error write() override;
};

class BinaryWriter : public Writer {
  Object &O;
  Buffer &B;
  MachOLayoutBuilder LayoutBuilder;
  std::vector<Section *> OrderedSections;

public:
  BinaryWriter(Object &O, bool Is64Bit, bool IsLittleEndian, uint64_t PageSize,
               Buffer &B)
      : O(O), B(B), LayoutBuilder(O, Is64Bit, PageSize) {}
  ~BinaryWriter() {}
  Error finalize() override;
  Error write() override;
};

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
