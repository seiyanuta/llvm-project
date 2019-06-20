//===- MachOLayoutBuilder.h -------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MachOObjcopy.h"
#include "Object.h"

namespace llvm {
namespace objcopy {
namespace macho {

class MachOLayoutBuilder {
  Object &O;
  bool Is64Bit;
  uint64_t PageSize;

  // Points to the __LINKEDIT segment if it exists.
  MachO::macho_load_command *LinkEditLoadCommand = nullptr;

  void updateDySymTab(MachO::macho_load_command &MLC);
  uint32_t computeSizeOfCmds();
  uint64_t layoutSegments();
  uint64_t layoutRelocations(uint64_t Offset);
  Error layoutTail(uint64_t Offset);
  // FIXME: remove this
  size_t strTableSize() const;


public:
  MachOLayoutBuilder(Object &O, bool Is64Bit, uint64_t PageSize)
      : O(O), Is64Bit(Is64Bit),
        PageSize(PageSize) {}

  Error layout();
};

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
