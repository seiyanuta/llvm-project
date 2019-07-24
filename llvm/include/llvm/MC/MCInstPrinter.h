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
#include <stack>

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

/// MarkupSpan represents a marked up range in the disassembly. For example:
///
///     Pos   InnerPos
///      v    v
///  ... <mem:256(<reg:%rip>)> ...
///           ~~~~~~~~~~~~~~~ InnerLenth
///      ~~~~~~~~~~~~~~~~~~~~~ Length
///
struct MarkupSpan {
  MarkupType Type;
  /// The offset of the beginning of the marked up range in the stream.
  size_t Pos;
  /// The length of the marked up range.
  size_t Length;
  /// The offset of the beginning of the inner text in the stream.
  size_t InnerPos;
  /// The length of the inner text.
  size_t InnerLength;
  /// Marked up ranges in the inner text. For example:
  ///
  ///   movq <reg:%rax>, <mem:256(<reg:%rdi>)>
  ///        ^^^^^^^^^^  ^^^^^^^^^----------^^
  ///         1st span    2nd span (*)
  ///                             ^^^^^^^^^^
  ///                              1st span in the (*)'s InnerSpans
  ///
  /// In this example, the top-level InnerSpans contains two elements that
  /// represent <reg:%rax> and <mem:...> respectively. InnerSpans in the
  /// latter one contains a span which represents <reg:%rdi>.
  std::vector<MarkupSpan> InnerSpans;

  MarkupSpan(MarkupType Type, size_t Pos, size_t Length, size_t InnerPos,
             size_t InnerLength)
      : Type(Type), Pos(Pos), Length(Length), InnerPos(InnerPos),
        InnerLength(InnerLength) {}
};

/// An object which creates a new markup range. When it is streamed to a
/// raw_ostream, along with printing the beginning part of markup string
/// (e.g. "<reg:"), a new incomplete MarkupSpan is appended to the given Spans.
///
/// The incomplete MarkupSpan will be updated by the corresponding MarkupEnd.
///
/// MarkupStart should be instantiated through MCInstPrinter::startMarkup():
///
///   OS << startMarkup(MarkupType::Reg) << "%rax" << endMarkup();
///
class MarkupStart {
  bool Enabled;
  std::stack<std::vector<MarkupSpan> *> &Spans;
  MarkupType Type;

public:
  MarkupStart(bool Enabled, std::stack<std::vector<MarkupSpan> *> &Spans,
              MarkupType Type)
      : Enabled(Enabled), Spans(Spans), Type(Type) {}
  friend raw_ostream &operator<<(raw_ostream &OS, const MarkupStart &M);
};

/// An object which closes the current markup range created by MarkupStart.
///
/// When it is streamed to a raw_ostream, along with printing the ending part
/// of markup string (">"), the last MarkupSpan in Spans will be updated with
/// correct values.
///
/// MarkupEnd should be instantiated through MCInstPrinter::endMarkup():
///
///   OS << startMarkup(MarkupType::Reg) << "%rax" << endMarkup();
///
class MarkupEnd {
  bool Enabled;
  std::stack<std::vector<MarkupSpan> *> &Spans;

public:
  MarkupEnd(bool Enabled, std::stack<std::vector<MarkupSpan> *> &Spans)
      : Enabled(Enabled), Spans(Spans) {}
  friend raw_ostream &operator<<(raw_ostream &OS, const MarkupEnd &M);
};

/// A RAII object which automatically closes the markup range to be used
/// through MCInstPrinter::withMarkup() like:
///
///   withMarkup(OS, MarkupType::Reg) << "%rax";
///
/// This is equivalent to:
///
///   OS << startMarkup(MarkupType::Reg) << "%rax" << endMarkup();
///
class WithMarkup {
  raw_ostream &OS;
  bool Enabled;
  std::stack<std::vector<MarkupSpan> *> &Spans;

public:
  WithMarkup(raw_ostream &OS, bool Enabled,
             std::stack<std::vector<MarkupSpan> *> &Spans, MarkupType Type);
  ~WithMarkup();

  template <typename T> WithMarkup &operator<<(const T &O) {
    OS << O;
    return *this;
  }
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

  /// A stack of pointers which points to &MS set by setMarkupSpans or
  /// InnerSpans of unclosed MarkupSpans. This is mutable because it will be
  /// updated in a const virtual method "printRegName" and making it non-const
  /// requires relatively large changes to the existing code base.
  mutable std::stack<std::vector<MarkupSpan> *> MarkupSpans;

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

  /// Set the vector to write marked up ranges to.
  void setMarkupSpans(std::vector<MarkupSpan> &MS) {
    while (!MarkupSpans.empty())
      MarkupSpans.pop();
    MarkupSpans.push(&MS);
  }

  /// Utility functions to make adding mark ups simpler.
  StringRef markup(StringRef s) const;
  MarkupStart startMarkup(MarkupType Type) const;
  MarkupEnd endMarkup() const;
  WithMarkup withMarkup(raw_ostream &OS, MarkupType Type) const;

  bool getPrintImmHex() const { return PrintImmHex; }
  void setPrintImmHex(bool Value) { PrintImmHex = Value; }

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
