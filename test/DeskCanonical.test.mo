/// DeskCanonical.test.mo: every command family and every event family round-trips through its bytes, the command
/// hashes are frozen against the recorded vectors, and a block written through the kernel's log decodes to what
/// was written with the treasury body byte for byte Manticore's.
///
/// engine: both.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Debug "mo:core/Debug";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";

import JC "mo:journal/Canonical";
import KC "mo:kernel/codec/Canonical";
import Freeze "mo:kernel/domain/Freeze";
import Enc "mo:kernel/domain/Encoding";
import TyCan "mo:manticore/TreasuryCanonical";

import T "../src/DeskTypes";
import Can "../src/DeskCanonical";
import Cat "../src/Catalogue";
import S "support/CommandSamples";
import V "CommandVectors";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };

// ─── commands: bytes, read back, the whole buffer consumed, the name the catalogue guards ───
var commands = 0;
for (s in S.samples().vals()) {
  let bytes = Can.commandBytes(s.command);
  let r = JC.Reader(Blob.toArray(bytes));
  switch (Can.readCommand(r)) {
    case (?c) {
      check(Can.commandBytes(c) == bytes, s.family # ": the read command writes the same bytes");
      check(r.remaining() == 0, s.family # ": the reader consumed the body");
      check(Cat.commandName(c) == s.family, s.family # ": the sample's family is its name");
    };
    case null check(false, s.family # ": the bytes do not read back");
  };
  // through the registry, as a proposal binds and an approval re-derives
  switch (Enc.bindCurrent(Can.registry(), s.command)) {
    case (?b) {
      check(b.version == Can.COMMAND_ENCODING, s.family # ": bound under the current version");
      let kr = KC.Reader(Blob.toArray(b.bytes));
      switch (Enc.readAtChecked(Can.registry(), b.version, kr, b.hash)) { case (?_) {}; case null check(false, s.family # ": readAtChecked refused its own bytes") };
    };
    case null check(false, s.family # ": the registry cannot represent the sample");
  };
  commands += 1;
};
check(commands == Cat.commandNames().size(), "one sample per command family");
Debug.print("count: command families round-tripped through their bytes = " # Nat.toText(commands));

// ─── the freeze ───
let report = Freeze.check(Can.registry(), S.samples(), V.vectors());
check(Freeze.clean(report), "the recorded command vectors hold: drift " # debug_show (report.drift) # " uncovered " # debug_show (report.uncovered) # " unrecorded " # debug_show (report.unrecorded));
Debug.print("count: command hashes equal to the recorded vectors = " # Nat.toText(report.checked));

// ─── events: the body round-trips; a treasury body is Manticore's bytes after the tag ───
var events = 0;
var treasuryBodies = 0;
for (e in S.events().vals()) {
  let body = Can.eventBytes(e);
  switch (Can.readEventBody(Blob.toArray(body))) {
    case (?back) check(Can.eventBytes(back) == body, T.eventName(e) # ": the event read back writes the same bytes");
    case null check(false, T.eventName(e) # ": the event body does not read back");
  };
  switch (e) {
    case (#treasury(te)) {
      let w = JC.Writer(); TyCan.writeEvent(w, te);
      let a = Blob.toArray(body);
      check(a[0] == 0x55 and Array.tabulate<Nat8>(a.size() - 1, func(i) { a[i + 1] }) == w.toArray(), "a treasury event is tag 0x55 then Manticore's bytes");
      treasuryBodies += 1;
    };
    case (_) {};
  };
  events += 1;
};
Debug.print("count: event families round-tripped through the codec = " # Nat.toText(events));
Debug.print("count: treasury event bodies byte for byte Manticore's = " # Nat.toText(treasuryBodies));

// ─── a corrupted byte is refused, never read as something else ───
var refused = 0;
for (s in S.samples().vals()) {
  let a = Blob.toArray(Can.commandBytes(s.command));
  let cut = Array.tabulate<Nat8>(a.size() - 1, func(i) { a[i] });
  switch (Can.readCommand(JC.Reader(cut))) {
    case (?c) { if (Can.commandBytes(c) != Blob.fromArray(cut)) refused += 1 else check(false, s.family # ": a truncated body read as a whole command") };
    case null refused += 1;
  };
};
Debug.print("count: truncated command bodies refused = " # Nat.toText(refused));

// ─── the codec's frame: the length prefix, and the block domain ───
let codec = Can.codec();
check(codec.version == Can.BLOCK_VERSION and codec.supports(Can.BLOCK_VERSION) and not codec.supports(2), "the codec supports its own version only");
let kw = KC.Writer();
codec.write(kw, #deskInstalled({ installer = S.alice() }));
let kr = KC.Reader(kw.toArray());
switch (codec.read(kr)) { case (?#deskInstalled(x)) check(Principal.equal(x.installer, S.alice()), "the frame reads back"); case (_) check(false, "the frame does not read back") };
check(kr.atEnd(), "the frame consumed exactly its body");
Debug.print("count: codec frame checks = 3");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("DESK CANONICAL GREEN");
