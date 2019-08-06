//===- Utils.h --------------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_OBJCOPY_MACHO_UTILS_H
#define LLVM_OBJCOPY_MACHO_UTILS_H

#include "llvm/ADT/StringRef.h"

namespace llvm {
namespace objcopy {
namespace macho {

/// Extracts a segment name from a string which is possibly non-null-terminated.
StringRef extractSegmentName(const char *segName);

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm

#endif
