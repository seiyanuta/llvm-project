//===- MachOObjcopy.cpp -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Utils.h"
#include "llvm/BinaryFormat/MachO.h"

namespace llvm {
namespace objcopy {
namespace macho {

// Make sure that the binary format has not changed.
static_assert(sizeof(MachO::segment_command::segname) ==
                  sizeof(MachO::segment_command_64::segname),
              "The MachO format has been changed.");

/// Extracts a segment name from a string which is possibly non-null-terminated.
StringRef extractSegmentName(const char *SegName) {
  return StringRef(SegName,
                   strnlen(SegName, sizeof(MachO::segment_command::segname)));
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
