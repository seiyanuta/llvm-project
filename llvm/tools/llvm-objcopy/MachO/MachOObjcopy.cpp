//===- MachOObjcopy.cpp -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MachOObjcopy.h"
#include "../CopyConfig.h"
#include "MachOReader.h"
#include "MachOWriter.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/Error.h"

namespace llvm {
namespace objcopy {
namespace macho {

using namespace object;
using SectionPred = std::function<bool(const Section &Sec)>;

static void removeSections(const CopyConfig &Config, Object &Obj) {
  SectionPred RemovePred = [](const Section &) { return false; };

  if (!Config.ToRemove.empty()) {
    RemovePred = [&Config, RemovePred](const Section &Sec) {
      return is_contained(Config.ToRemove, Sec.CannonicalName);
    };
  }

  if (Config.StripAll) {
    // Remove all debug sections.
    RemovePred = [RemovePred](const Section &Sec) {
      if (Sec.Segname == "__DWARF")
        return true;

      return RemovePred(Sec);
    };
  }

  if (!Config.OnlySection.empty()) {
    RemovePred = [&Config, RemovePred](const Section &Sec) {
      return !is_contained(Config.OnlySection, Sec.CannonicalName);
    };
  }

  return Obj.removeSections(RemovePred);
}

static void markSymbols(const CopyConfig &Config, Object &Obj) {
  // Symbols referenced from the indirect symbol table must not be removed.
  for (IndirectSymbolEntry &ISE : Obj.IndirectSymTable.Symbols)
    if (ISE.Symbol)
      (*ISE.Symbol)->Referenced = true;
}

static void removeSymbols(const CopyConfig &Config, Object &Obj) {
  auto RemovePred = [Config](const std::unique_ptr<SymbolEntry> &N) {
    if (N->Referenced)
      return false;
    return Config.StripAll;
  };

  Obj.SymTable.removeSymbols(RemovePred);
}

static Error addSection(StringRef SecName, StringRef Filename, Object &Obj) {
  // FIXME: Read the file in CopyConfig.
  ErrorOr<std::unique_ptr<MemoryBuffer>> BufOrErr =
      MemoryBuffer::getFile(Filename);
  if (!BufOrErr)
    return createFileError(Filename, errorCodeToError(BufOrErr.getError()));
  std::unique_ptr<MemoryBuffer> Buf = std::move(*BufOrErr);
  ArrayRef<uint8_t> Content(
      reinterpret_cast<const uint8_t *>(Buf->getBufferStart()),
      Buf->getBufferSize());

  std::pair<StringRef, StringRef> Pair = SecName.split(',');
  StringRef TargetSegName = Pair.first;
  Section Sec(TargetSegName, Pair.second);
  Sec.setOwnedContentData(Content);

  for (LoadCommand &LC : Obj.LoadCommands) {
    Optional<StringRef> SegName = LC.getSegmentName();
    if (SegName && SegName == TargetSegName) {
      LC.Sections.push_back(Sec);
      return Error::success();
    }
  }

  // There's no segment named TargetSegName. Create a new load command and
  // Insert a new section into it.
  // TODO: 32-bit
  LoadCommand &NewSegment = Obj.addSegment(TargetSegName);
  NewSegment.Sections.push_back(Sec);
  return Error::success();
}

static Error dumpSectionToFile(StringRef SecName, StringRef Filename,
                               Object &Obj) {
  for (LoadCommand &LC : Obj.LoadCommands)
    for (Section &Sec : LC.Sections) {
      if (Sec.CannonicalName == SecName) {
        Expected<std::unique_ptr<FileOutputBuffer>> BufferOrErr =
            FileOutputBuffer::create(Filename, Sec.Content.size());
        if (!BufferOrErr)
          return BufferOrErr.takeError();
        std::unique_ptr<FileOutputBuffer> Buf = std::move(*BufferOrErr);
        std::copy(Sec.Content.begin(), Sec.Content.end(),
                  Buf->getBufferStart());
        if (Error E = Buf->commit())
          return E;
        return Error::success();
      }
    }

  return createStringError(object_error::parse_failed, "section '%s' not found",
                           SecName.str().c_str());
}

static uint32_t getNewSectionFlags(const SectionFlag &SF) {
  if ((SF & SecCode) != 0)
    return MachO::S_REGULAR | MachO::S_ATTR_PURE_INSTRUCTIONS |
           MachO::S_ATTR_SOME_INSTRUCTIONS;
  else if ((SF & SecAlloc) != 0 && (SF & SecLoad) == 0)
    return MachO::S_ZEROFILL;
  else if ((SF & SecDebug) != 0)
    return MachO::S_ATTR_DEBUG;
  else
    return MachO::S_REGULAR;
}

static Error validateOptions(const CopyConfig &Config) {
  // TODO: Support section renaming in GNU objcopy for compatibility (see
  // http://lists.llvm.org/pipermail/llvm-dev/2019-May/132570.html).

  if (!Config.OnlySection.empty()) {
    for (const NameOrRegex &NR : Config.OnlySection)
      if (Error E = NR.isMachOCannonicalName())
        return E;
  }

  if (!Config.ToRemove.empty()) {
    for (const NameOrRegex &NR : Config.ToRemove)
      if (Error E = NR.isMachOCannonicalName())
        return E;
  }

  if (!Config.SetSectionFlags.empty()) {
    for (const auto &Entry : Config.SetSectionFlags)
      if (Error E = isValidMachOCannonicalName(Entry.getKey()))
        return E;
  }

  return Error::success();
}

static Error handleArgs(const CopyConfig &Config, Object &Obj) {
  if (Config.AllowBrokenLinks || !Config.BuildIdLinkDir.empty() ||
      Config.BuildIdLinkInput || Config.BuildIdLinkOutput ||
      !Config.SplitDWO.empty() || !Config.SymbolsPrefix.empty() ||
      !Config.AllocSectionsPrefix.empty() || !Config.KeepSection.empty() ||
      !Config.SymbolsToGlobalize.empty() || !Config.SymbolsToKeep.empty() ||
      !Config.SymbolsToLocalize.empty() || !Config.SymbolsToWeaken.empty() ||
      !Config.SymbolsToKeepGlobal.empty() || !Config.SectionsToRename.empty() ||
      !Config.SymbolsToRename.empty() ||
      !Config.UnneededSymbolsToRemove.empty() || Config.ExtractDWO ||
      Config.KeepFileSymbols || Config.LocalizeHidden || Config.PreserveDates ||
      Config.StripAllGNU || Config.StripDWO || Config.StripNonAlloc ||
      Config.StripSections || Config.Weaken || Config.DecompressDebugSections ||
      Config.StripDebug || Config.StripNonAlloc || Config.StripSections ||
      Config.StripUnneeded || Config.DiscardMode != DiscardType::None ||
      !Config.SymbolsToAdd.empty() || Config.EntryExpr) {
    return createStringError(llvm::errc::invalid_argument,
                             "option not supported by llvm-objcopy for MachO");
  }

  if (auto E = validateOptions(Config))
    return E;

  removeSections(Config, Obj);

  // Mark symbols to determine which symbols are still needed.
  if (Config.StripAll)
    markSymbols(Config, Obj);

  removeSymbols(Config, Obj);

  if (!Config.SetSectionFlags.empty())
    for (LoadCommand &LC : Obj.LoadCommands)
      for (Section &Sec : LC.Sections) {
        const auto Iter = Config.SetSectionFlags.find(Sec.CannonicalName);
        if (Iter != Config.SetSectionFlags.end())
          Sec.Flags = getNewSectionFlags(Iter->second.NewFlags);
      }

  for (const auto &Flag : Config.AddSection) {
    std::pair<StringRef, StringRef> SecPair = Flag.split("=");
    StringRef SecName = SecPair.first;
    StringRef File = SecPair.second;
    if (Error E = isValidMachOCannonicalName(SecName))
      return E;
    if (Error E = addSection(SecName, File, Obj))
      return E;
  }

  if (Config.StripAll)
    for (LoadCommand &LC : Obj.LoadCommands)
      for (Section &Sec : LC.Sections)
        Sec.Relocations.clear();

  for (const auto &Flag : Config.DumpSection) {
    std::pair<StringRef, StringRef> SecPair = Flag.split("=");
    StringRef SecName = SecPair.first;
    StringRef File = SecPair.second;
    if (Error E = isValidMachOCannonicalName(SecName))
      return E;
    if (Error E = dumpSectionToFile(SecName, File, Obj))
      return E;
  }

  return Error::success();
}

static std::unique_ptr<Writer> createWriter(const CopyConfig &Config, Object &O,
                                            Buffer &Out, bool Is64Bit,
                                            bool IsLittleEndian) {
  // TODO: Support 16KB pages which are employed in iOS arm64 binaries:
  //       https://github.com/llvm/llvm-project/commit/1bebb2832ee312d3b0316dacff457a7a29435edb
  const uint64_t PageSize = 4096;

  switch (Config.OutputFormat) {
  case FileFormat::Binary:
    return std::make_unique<BinaryWriter>(
        BinaryWriter(O, Is64Bit, IsLittleEndian, PageSize, Out));
  default:
    return std::make_unique<MachOWriter>(
        MachOWriter(O, Is64Bit, IsLittleEndian, PageSize, Out));
  }
}

Error writeOutput(const CopyConfig &Config, Object &O, Buffer &Out,
                  bool Is64Bit, bool IsLittleEndian) {
  std::unique_ptr<Writer> W =
      createWriter(Config, O, Out, Is64Bit, IsLittleEndian);
  if (auto E = W->finalize())
    return E;
  return W->write();
}

Error executeObjcopyOnRawBinary(const CopyConfig &Config, MemoryBuffer &In,
                                Buffer &Out) {
  const MachineInfo &MI = Config.BinaryArch;
  BinaryReader Reader(MI, In);
  std::unique_ptr<Object> O = Reader.create();

  if (Error E = handleArgs(Config, *O))
    return createFileError(Config.InputFilename, std::move(E));

  return writeOutput(Config, *O, Out, MI.Is64Bit, MI.IsLittleEndian);
}

Error executeObjcopyOnBinary(const CopyConfig &Config,
                             object::MachOObjectFile &In, Buffer &Out) {
  MachOReader Reader(In);
  std::unique_ptr<Object> O = Reader.create();
  if (!O)
    return createFileError(
        Config.InputFilename,
        createStringError(object_error::parse_failed,
                          "unable to deserialize MachO object"));

  if (Error E = handleArgs(Config, *O))
    return createFileError(Config.InputFilename, std::move(E));

  return writeOutput(Config, *O, Out, In.is64Bit(), In.isLittleEndian());
}

} // end namespace macho
} // end namespace objcopy
} // end namespace llvm
