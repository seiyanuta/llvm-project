//===- MCInstPrinter.h - MCInst to target assembly syntax -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCINSTPRINTER_H
#define LLVM_MC_MCINSTPRINTER_H

#include "llvm/Support/Format.h"
#include <cstdint>

namespace llvm {

class MCAsmInfo;
class MCInst;
class MCInstrInfo;
class MCRegisterInfo;
class MCSubtargetInfo;
class raw_ostream;
class StringRef;

/// Convert `Bytes' to a hex string and output to `OS'
void dumpBytes(ArrayRef<uint8_t> Bytes, raw_ostream &OS);

namespace HexStyle {

enum Style {
  C,  ///< 0xff
  Asm ///< 0ffh
};

} // end namespace HexStyle

enum class MarkupType {
  Reg,
  Imm,
  Mem,
};

// MarkupSpan represents a marked up range in the disassembly. For example:
//
//      Pos   InnerPos
//       v    v
//  ...  <mem:256(<reg:%rip>)> ...
//            ~~~~~~~~~~~~~~~ InnerLenth
//       ~~~~~~~~~~~~~~~~~~~~~ Length
//
struct MarkupSpan {
  MarkupType Type;
  // The offset of the beginning of the marked up range.
  size_t Pos;
  // The length of the marked up range.
  size_t Length;
  // The offset of the beginning of the inner text.
  size_t InnerPos;
  // The length of the inner text.
  size_t InnerLength;
  // Marked up ranges in the inner text. In the example above,
  // InnerSpans contains one MarkupSpan which represents `<reg:%rip>`.
  std::unique_ptr<std::vector<MarkupSpan>> InnerSpans;

  MarkupSpan(MarkupType Type, size_t Pos, size_t Length, size_t InnerPos,
             size_t InnerLength)
      : Type(Type), Pos(Pos), Length(Length), InnerPos(InnerPos),
        InnerLength(InnerLength), InnerSpans(new std::vector<MarkupSpan>()) {}
};

// MarkupState holds the state used by llvm::operator<<(raw_ostream&) implementaions
// for MarkupStart and MarkupEnd.
struct MarkupState {
  // True if the marked up disassembly is enabled. Defaults to false in case
  // resetMarkup() isn't called in the printer.
  bool Enabled = false;
  // The offset of the beginning of the disassembly in the stream.
  size_t StartOffset;
  // A stack which holds pointers to SpansOut_ and InnerSpans of unclosed Spans.
  // TODO: Rename to a appropriate name.
  std::vector<std::vector<MarkupSpan> *> UnclosedSpanInner;

  void reset(raw_ostream &OS, bool Enabled_,
             std::vector<MarkupSpan> *SpansOut_);
  size_t offset(raw_ostream &OS) const;
};

struct MarkupStart {
  MarkupState &State;
  MarkupType Type;

  MarkupStart(MarkupState &State, MarkupType Type) : State(State), Type(Type) {}
  friend raw_ostream &operator<<(raw_ostream &OS, const MarkupStart &M);
};

struct MarkupEnd {
  MarkupState &State;

  MarkupEnd(MarkupState &State) : State(State) {}
  friend raw_ostream &operator<<(raw_ostream &OS, const MarkupEnd &M);
};

/// This is an instance of a target assembly language printer that
/// converts an MCInst to valid target assembly syntax.
class MCInstPrinter {
protected:
  /// A stream that comments can be emitted to if desired.  Each comment
  /// must end with a newline.  This will be null if verbose assembly emission
  /// is disabled.
  raw_ostream *CommentStream = nullptr;
  const MCAsmInfo &MAI;
  const MCInstrInfo &MII;
  const MCRegisterInfo &MRI;

  /// True if we are printing marked up assembly.
  bool UseMarkup = false;

  /// True if we are printing immediates as hex.
  bool PrintImmHex = false;

  /// Which style to use for printing hexadecimal values.
  HexStyle::Style PrintHexStyle = HexStyle::C;

  /// Markup states.
  MarkupState MarkupState;
  std::vector<MarkupSpan> *MarkupSpans = nullptr;

  /// Utility function for printing annotations.
  void printAnnotation(raw_ostream &OS, StringRef Annot);

public:
  MCInstPrinter(const MCAsmInfo &mai, const MCInstrInfo &mii,
                const MCRegisterInfo &mri) : MAI(mai), MII(mii), MRI(mri) {}

  virtual ~MCInstPrinter();

  /// Customize the printer according to a command line option.
  /// @return true if the option is recognized and applied.
  virtual bool applyTargetSpecificCLOption(StringRef Opt) { return false; }

  /// Specify a stream to emit comments to.
  void setCommentStream(raw_ostream &OS) { CommentStream = &OS; }

  /// Print the specified MCInst to the specified raw_ostream.
  virtual void printInst(const MCInst *MI, raw_ostream &OS, StringRef Annot,
                         const MCSubtargetInfo &STI) = 0;

  /// Return the name of the specified opcode enum (e.g. "MOV32ri") or
  /// empty if we can't resolve it.
  StringRef getOpcodeName(unsigned Opcode) const;

  /// Print the assembler register name.
  virtual void printRegName(raw_ostream &OS, unsigned RegNo) const;

  bool getUseMarkup() const { return UseMarkup; }
  void setUseMarkup(bool Value) { UseMarkup = Value; }

  // Specify an output vector of marked up ranges.
  void setMarkupSpans(std::vector<MarkupSpan> &MS) { MarkupSpans = &MS; }

  // Resets the MarkupState. This should be called first in printInst().
  void resetMarkup(raw_ostream &OS);

  /// Utility functions to make adding mark ups simpler.
  StringRef markup(StringRef s) const;
  StringRef markup(StringRef a, StringRef b) const;
  MarkupStart startMarkup(MarkupType Type);
  MarkupEnd endMarkup();

  bool getPrintImmHex() const { return PrintImmHex; }
  void setPrintImmHex(bool Value) { PrintImmHex = Value; }

  HexStyle::Style getPrintHexStyle() const { return PrintHexStyle; }
  void setPrintHexStyle(HexStyle::Style Value) { PrintHexStyle = Value; }

  /// Utility function to print immediates in decimal or hex.
  format_object<int64_t> formatImm(int64_t Value) const {
    return PrintImmHex ? formatHex(Value) : formatDec(Value);
  }

  /// Utility functions to print decimal/hexadecimal values.
  format_object<int64_t> formatDec(int64_t Value) const;
  format_object<int64_t> formatHex(int64_t Value) const;
  format_object<uint64_t> formatHex(uint64_t Value) const;
};

} // end namespace llvm

#endif // LLVM_MC_MCINSTPRINTER_H
