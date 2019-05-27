//===- Object.cpp ---------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Object.h"

namespace llvm {
namespace objcopy {
namespace macho {

void Object::removeSections(function_ref<bool(const Section &)> ToRemove) {
    // Unlike ELF, sections are not refered by other data structures so
    // simply remove the sections.
    for (auto &LC : LoadCommands)
        LC.Sections.erase(
            std::remove_if(
                std::begin(LC.Sections), std::end(LC.Sections),
                [ToRemove](const Section &Sec) {
                    return ToRemove(Sec);
                }
            ),
            std::end(LC.Sections)
        );
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm