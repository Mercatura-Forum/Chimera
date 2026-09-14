/// Attribution.mo: the result of every deal attributed by period as a fold over the recorded treasury events:
/// new (the mark and the realised result of the capture day), carry (accruals, amortisation, the coupon's
/// difference to the accrual, the theta recorded beside each mark), market move (the rest); the whole is their
/// sum by construction, and the battery holds the sum over a book against the journal's own result accounts.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import CivilDate "mo:journal/CivilDate";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";

import VT "ValuationTypes";

module {

  public let ROW_BYTES : Nat = 43;
  let MAX_PAGE = 512;

  public type Row = { new : Int; carry : Int; market : Int; marks : Nat; accruals : Nat };

  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  func encode(r : Row) : Blob { let b = R.buf(); putInt(b, r.new); putInt(b, r.carry); putInt(b, r.market); R.putNat(b, r.marks, 8); R.putNat(b, r.accruals, 8); R.done(b, ROW_BYTES) };
  func decode(v : Blob) : Row { let a = Blob.toArray(v); { new = getInt(a, 0); carry = getInt(a, 9); market = getInt(a, 18); marks = R.getNat(a, 27, 8); accruals = R.getNat(a, 35, 8) } };
  /// The period of a day: its month, as the journal's periods are declared.
  public func periodOf(day : Nat) : Text { let t = CivilDate.toText(day); Text.fromArray(Array.sliceToArray<Char>(Text.toArray(t), 0, 7)) };
  func key(deal : Nat, period : Text) : Blob { Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.key(deal, 8)), Blob.toArray(R.textKey(period, 8)))) };

  public type State = {
    rows : RI.State;        // deal(8) ‖ period(8) -> row
    captureDay : RI.State;  // deal(8) -> day(4)
    var rowCount : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    { rows = RI.newStateIn(arena, { keyBytes = 16; valBytes = ROW_BYTES }); captureDay = RI.newStateIn(arena, { keyBytes = 8; valBytes = 4 }); var rowCount = 0 }
  };

  public func row(s : State, deal : Nat, period : Text) : ?Row { switch (RI.get(s.rows, key(deal, period))) { case (?v) ?decode(v); case null null } };
  func captureDayOf(s : State, deal : Nat) : ?Nat { switch (RI.get(s.captureDay, R.key(deal, 8))) { case (?v) ?R.getNat(Blob.toArray(v), 0, 4); case null null } };
  func bump(s : State, deal : Nat, day : Nat, f : Row -> Row) {
    let k = key(deal, periodOf(day));
    let cur = switch (RI.get(s.rows, k)) { case (?v) decode(v); case null { s.rowCount += 1; { new = 0; carry = 0; market = 0; marks = 0; accruals = 0 } } };
    ignore RI.put(s.rows, k, encode(f(cur)));
  };
  func isNew(s : State, deal : Nat, day : Nat) : Bool { switch (captureDayOf(s, deal)) { case (?d) d == day; case null false } };

  /// The treasury events that carry a result: the deal's row before the fold is what a coupon's difference to
  /// the accrual is measured against.
  public func observeTreasury(s : State, block : Nat, ev : TT.TreasuryEvent, before : ?TreasuryCore.DealRow) {
    switch (ev) {
      case (#dealCaptured(x)) ignore RI.put(s.captureDay, R.key(block, 8), R.key(x.day, 4));
      case (#accrued(x)) bump(s, x.deal, x.day, func(r) { { r with carry = r.carry + x.interest + x.amortisation; accruals = r.accruals + 1 } });
      case (#couponPaid(x)) { let posted : Int = switch (before) { case (?b) b.accruedPosted; case null 0 }; bump(s, x.deal, x.day, func(r) { { r with carry = r.carry + (x.amount : Int) - posted } }) };
      case (#marked(x)) {
        let delta = x.value - x.previous;
        if (isNew(s, x.deal, x.day)) bump(s, x.deal, x.day, func(r) { { r with new = r.new + delta; marks = r.marks + 1 } })
        else bump(s, x.deal, x.day, func(r) { { r with market = r.market + delta; marks = r.marks + 1 } });
      };
      case (#legSettled(x)) {
        // the realised result, and the mark or the fair-value adjustment the settlement reverses; an option's
        // premium leg capitalises the premium as the option's value and moves no result
        let premiumLeg = switch (before) { case (?b) b.kind == 6 and x.leg == 0; case null false };
        let moved = x.realised + (if (premiumLeg) 0 else x.fv);
        if (moved != 0) {
          if (isNew(s, x.deal, x.day)) bump(s, x.deal, x.day, func(r) { { r with new = r.new + moved } })
          else bump(s, x.deal, x.day, func(r) { { r with market = r.market + moved } });
        };
      };
      case (_) {};
    }
  };
  /// The theta recorded beside a mark: carry, taken out of the market move.
  public func observeTheta(s : State, deal : Nat, day : Nat, theta : Int) {
    if (theta != 0) bump(s, deal, day, func(r) { { r with carry = r.carry + theta; market = r.market - theta } });
  };

  /// A row as a view, in the deal's currency: a sum over a book is a sum per currency.
  public func view(deal : Nat, period : Text, currency : Text, r : Row) : VT.AttributionView { { deal; period; currency; new = r.new; carry = r.carry; market = r.market; total = r.new + r.carry + r.market; marks = r.marks; accruals = r.accruals } };
  /// Every row of a deal, by period.
  public func rowsOfDeal(s : State, deal : Nat, currency : Text) : [VT.AttributionView] {
    let (lo, hi) = R.prefixRange(deal, 8, 8);
    let out = List.empty<VT.AttributionView>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.rows, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, view(deal, R.getText(Blob.toArray(k), 8, 8), currency, decode(v)));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func fingerprintInto(w : C.Writer, s : State) {
    w.nat(s.rowCount);
    for ((idx, width) in [(s.rows, 16), (s.captureDay, 8)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      var cursor : ?Blob = null;
      label walk loop {
        let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
        for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v); n += 1 };
        switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
      };
      w.nat(n);
    };
  };
}
