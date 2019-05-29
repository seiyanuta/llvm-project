# RUN: llvm-mc -assemble -triple x86_64-apple-darwin9 -filetype=obj %p/Inputs/using-dynamic-library.s -o %t.o
# RUN: llvm-objcopy %t.o %t.copy.o
# RUN: cmp %t.o %t.copy.o

# REQUIRES: x86-registered-target
