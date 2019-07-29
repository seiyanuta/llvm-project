# RUN: llvm-mc -filetype=obj -triple=x86_64 %s -o %t
# Use '|' to show where the tabs line up.
# RUN: llvm-objdump -d -print-imm-hex -insn-width=4 %t | tr '\t' '|' | FileCheck -strict-whitespace %s

# CHECK:      0: 48 b8 f0 de                  |movabsq|$0x123456789abcdef0, %rax
# CHECK-NEXT: 4: bc 9a 78 56
# CHECK-NEXT: 8: 34 12

# RUN: not llvm-objdump -d -insn-width=0 %t 2>&1 | FileCheck --check-prefix=INVALID-WIDTH %s
# INVALID-WIDTH: error: instruction width must be a positive integer.


.text
  movabs $0x123456789abcdef0,%rax
