//===- MachOObjcopy.h -------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_OBJCOPY_MACHOOBJCOPY_H
#define LLVM_TOOLS_OBJCOPY_MACHOOBJCOPY_H

#include "llvm/Support/MemoryBuffer.h"

namespace llvm {
class Error;
class MemoryBuffer;

namespace object {
class MachOObjectFile;
class MachOUniversalBinary;
} // end namespace object

namespace objcopy {
struct CopyConfig;
class Buffer;

namespace macho {
Error executeObjcopyOnRawBinary(const CopyConfig &Config, MemoryBuffer &In,
                                Buffer &Out);
Error executeObjcopyOnBinary(const CopyConfig &Config,
                             object::MachOObjectFile &In, Buffer &Out);
} // end namespace macho
} // end namespace objcopy
} // end namespace llvm

#endif // LLVM_TOOLS_OBJCOPY_MACHOOBJCOPY_H
