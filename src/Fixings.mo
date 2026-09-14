/// Fixings.mo: the recorded fixings of a floating index by day, the desk's own table.
///
/// An interest-rate swap's period settles against the fixing of its index at the period's start, and its mark
/// discounts the floating leg of a period already fixed at that fixing (Manticore's treasury reads them from the
/// facilities book of its corporate lending domain; a desk has no facilities book, so it records them here). The
/// rule is the curve's: a fixing recorded for a day is data; recording the same figure again records nothing; a
/// different figure for the same day is refused, so no valuation depends on which block a reader stopped at. The
/// fixing in force on a day is the latest recorded on or before it, as Manticore reads it.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Blob "mo:core/Blob";
import Result "mo:core/Result";
import Text "mo:core/Text";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";

import R "mo:kernel/rows/StableRows";

module {

  public let MAX_INDEX_BYTES : Nat = 32;
  public let MAX_RATE_BPS : Nat = 100_000;
  let MAX_PAGE = 512;

  public type State = { rows : RI.State; var count : Nat };

  public func newState(arena : RI.Arena) : State {
    { rows = RI.newStateIn(arena, { keyBytes = 12; valBytes = 4 }); var count = 0 }
  };

  func indexHash(index : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(index))), 0, 8) };
  func key(index : Text, day : Nat) : Blob { R.key2(indexHash(index), 8, day, 4) };

  public type Event = { index : Text; day : Nat; rateBps : Nat };

  /// Null when the same fixing is already recorded (nothing to record).
  public func plan(s : State, index : Text, day : Nat, rateBps : Nat) : Result.Result<?Event, Text> {
    let n = Text.encodeUtf8(index).size();
    if (n == 0 or n > MAX_INDEX_BYTES) return #err("a rate index is named in 1.." # debug_show MAX_INDEX_BYTES # " bytes");
    if (rateBps > MAX_RATE_BPS) return #err("a fixing above 1000 percent a year");
    switch (RI.get(s.rows, key(index, day))) {
      case (?v) {
        if (R.getNat(Blob.toArray(v), 0, 4) == rateBps) return #ok(null);
        #err("a different fixing of " # index # " is already recorded for day " # debug_show day)
      };
      case null #ok(?{ index; day; rateBps });
    }
  };

  public func fold(s : State, e : Event) {
    if (RI.put(s.rows, key(e.index, e.day), R.key(e.rateBps, 4)) == null) s.count += 1;
  };

  /// The fixing in force on `day`: the latest recorded on or before it.
  public func fixingOn(s : State, index : Text, day : Nat) : ?Nat {
    let h = indexHash(index);
    let (lo, _) = R.prefixRange(h, 8, 4);
    let hi = R.key2(h, 8, day, 4);
    var cursor : ?Blob = null;
    var latest : ?Nat = null;
    label walk loop {
      let page = RI.range(s.rows, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) latest := ?R.getNat(Blob.toArray(v), 0, 4);
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    latest
  };

  public func count(s : State) : Nat { s.count };

  public func fingerprintInto(w : C.Writer, s : State) {
    w.nat(s.count);
    let (lo, hi) = R.fullRange(12);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.rows, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v) };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
  };
}
