/// HedgeCore.mo: hedge relationships as recorded events, IFRS 9 chapter 6: a designation of a hedging instrument
/// (a swap or a forward) against a hedged item, assessed over a period by the dollar-offset method, the effective
/// portion of a cash-flow hedge to the reserve and the ineffective in the result, a fair-value hedge adjusting
/// the hedged lot's carrying amount, a dedesignation reclassifying the reserve.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Result "mo:core/Result";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import M "mo:manticore/TreasuryMath";
import Posting "mo:manticore/Posting";

import VT "ValuationTypes";

module {

  public let ROW_BYTES : Nat = 82;
  let MAX_PAGE = 512;

  public type Row = {
    id : VT.HedgeId; hedging : TT.DealId; hedged : TT.DealId; cashFlow : Bool; hedgedAmount : Nat; state : VT.HedgeState; designatedDay : Nat;
    hedgingMark : Int; hedgedValue : Int; reserve : Int; basisAdjustment : Int; lastEffectivenessBps : Nat; lastAssessedDay : Nat; lastBlock : Nat;
  };
  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  func encode(r : Row) : Blob {
    let b = R.buf();
    R.putNat(b, r.hedging, 8); R.putNat(b, r.hedged, 8); R.putBool(b, r.cashFlow); R.putNat(b, r.hedgedAmount, 8); R.putByte(b, switch (r.state) { case (#designated) 1; case (#dedesignated) 2 }); R.putNat(b, r.designatedDay, 4);
    putInt(b, r.hedgingMark); putInt(b, r.hedgedValue); putInt(b, r.reserve); putInt(b, r.basisAdjustment); R.putNat(b, r.lastEffectivenessBps, 4); R.putNat(b, r.lastAssessedDay, 4); R.putNat(b, r.lastBlock, 8);
    R.done(b, ROW_BYTES)
  };
  func decode(id : Nat, v : Blob) : Row {
    let a = Blob.toArray(v);
    { id; hedging = R.getNat(a, 0, 8); hedged = R.getNat(a, 8, 8); cashFlow = R.getBool(a, 16); hedgedAmount = R.getNat(a, 17, 8); state = if (a[25] == 1) #designated else #dedesignated; designatedDay = R.getNat(a, 26, 4);
      hedgingMark = getInt(a, 30); hedgedValue = getInt(a, 39); reserve = getInt(a, 48); basisAdjustment = getInt(a, 57); lastEffectivenessBps = R.getNat(a, 66, 4); lastAssessedDay = R.getNat(a, 70, 4); lastBlock = R.getNat(a, 74, 8) }
  };

  public type State = {
    rows : RI.State;   // id(8) -> row
    var policy : ?VT.Policy;
    var count : Nat;
    var open : Nat;
    var quotes : Nat;
    var thetas : Nat;
  };
  public func newState(arena : RI.Arena) : State { { rows = RI.newStateIn(arena, { keyBytes = 8; valBytes = ROW_BYTES }); var policy = null; var count = 0; var open = 0; var quotes = 0; var thetas = 0 } };
  public func policy(s : State) : ?VT.Policy { s.policy };
  public func hedge(s : State, id : Nat) : ?Row { switch (RI.get(s.rows, R.key(id, 8))) { case (?v) ?decode(id, v); case null null } };
  func put(s : State, r : Row) { ignore RI.put(s.rows, R.key(r.id, 8), encode(r)) };
  public func hedges(s : State) : [Row] {
    let out = List.empty<Row>();
    let (lo, hi) = R.fullRange(8);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.rows, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, decode(R.getNat(Blob.toArray(k), 0, 8), v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func hedgeSub(id : Nat) : JT.SubledgerKey { Posting.subledgerOf("hedge/" # Nat.toText(id)) };

  type Res<X> = Result.Result<X, VT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidTerms({ reason })) };

  public func planPolicy(p : VT.Policy) : Res<VT.Event> { if (Text.encodeUtf8(p.hedgeReserve).size() == 0) return bad("the hedge reserve is named"); #ok(#policySet(p)) };

  /// The effectiveness of a period by dollar offset, in basis points: the hedged change against the hedging
  /// change, both signed, so a perfect offset is 10,000; and the effective portion of the hedging change by the
  /// lower-of test: the hedging change up to the hedged change's size, with the hedging change's sign.
  public func effectiveness(hedgingChange : Int, hedgedChange : Int) : (Nat, Int) {
    if (hedgingChange == 0) return (if (hedgedChange == 0) 10_000 else 0, 0);
    let ratio = M.roundNat(M.q(Int.abs(hedgedChange) * 10_000, Int.abs(hedgingChange)));
    let effective : Int = if ((hedgingChange > 0) == (hedgedChange < 0) or hedgedChange == 0) {
      if (hedgedChange == 0) 0 else (if (Int.abs(hedgedChange) >= Int.abs(hedgingChange)) hedgingChange else (if (hedgingChange > 0) Int.abs(hedgedChange) else -(Int.abs(hedgedChange) : Int)))
    } else 0;   // the two moved the same way: nothing offsets
    (ratio, effective)
  };

  /// The legs of an assessment: a cash-flow hedge moves the effective portion of the hedging instrument's result
  /// out of the unrealised result into the reserve; a fair-value hedge adjusts the hedged lot's carrying amount
  /// by the hedged change against the unrealised result.
  public func legsOf(p : VT.Policy, tp : TT.Policy, r : Row, currency : Text, lotAccount : Text, ev : VT.Event) : [JT.Leg] {
    let ls = List.empty<JT.Leg>();
    func add(account : Text, sub : ?JT.SubledgerKey, side : JT.Side, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, sub, side, currency, amount)) };
    let sub = ?hedgeSub(r.id);
    switch (ev) {
      case (#hedgeAssessed(x)) {
        if (r.cashFlow) {
          if (x.effective > 0) { add(tp.unrealisedTradingGain, null, #debit, Int.abs(x.effective)); add(p.hedgeReserve, sub, #credit, Int.abs(x.effective)) }
          else if (x.effective < 0) { add(p.hedgeReserve, sub, #debit, Int.abs(x.effective)); add(tp.unrealisedTradingLoss, null, #credit, Int.abs(x.effective)) };
        } else {
          let lotSub = ?TreasuryCore.dealSub(r.hedged);
          if (x.hedgedChange > 0) { add(lotAccount, lotSub, #debit, Int.abs(x.hedgedChange)); add(tp.unrealisedTradingGain, null, #credit, Int.abs(x.hedgedChange)) }
          else if (x.hedgedChange < 0) { add(tp.unrealisedTradingLoss, null, #debit, Int.abs(x.hedgedChange)); add(lotAccount, lotSub, #credit, Int.abs(x.hedgedChange)) };
        };
      };
      case (#hedgeDedesignated(x)) {
        if (x.reclassified > 0) { add(p.hedgeReserve, sub, #debit, Int.abs(x.reclassified)); add(tp.unrealisedTradingGain, null, #credit, Int.abs(x.reclassified)) }
        else if (x.reclassified < 0) { add(tp.unrealisedTradingLoss, null, #debit, Int.abs(x.reclassified)); add(p.hedgeReserve, sub, #credit, Int.abs(x.reclassified)) };
      };
      case (_) {};
    };
    List.toArray(ls)
  };

  public func planAssess(s : State, id : Nat, hedgingMark : Int, hedgedValue : Int, day : Nat) : Res<VT.Event> {
    let ?r = hedge(s, id) else return #err(#UnknownHedge({ hedge = id }));
    if (r.state != #designated) return #err(#HedgeNotIn({ hedge = id; state = "dedesignated"; wanted = "designated" }));
    if (day <= r.lastAssessedDay) return bad("an assessment is after the last");
    let hedgingChange = hedgingMark - r.hedgingMark;
    let hedgedChange = hedgedValue - r.hedgedValue;
    let (bps, effective) = effectiveness(hedgingChange, hedgedChange);
    #ok(#hedgeAssessed({ hedge = id; hedgingChange; hedgedChange; effectivenessBps = bps; effective; ineffective = hedgingChange - effective; day }))
  };
  public func planDedesignate(s : State, id : Nat, day : Nat) : Res<VT.Event> {
    let ?r = hedge(s, id) else return #err(#UnknownHedge({ hedge = id }));
    if (r.state != #designated) return #err(#HedgeNotIn({ hedge = id; state = "dedesignated"; wanted = "designated" }));
    #ok(#hedgeDedesignated({ hedge = id; reclassified = r.reserve; day }))
  };

  public func fold(s : State, block : Nat, ev : VT.Event) {
    switch (ev) {
      case (#policySet(p)) s.policy := ?p;
      case (#yieldQuoted(_)) s.quotes += 1;
      case (#thetaRecorded(_)) s.thetas += 1;
      case (#hedgeDesignated(x)) {
        let (cf, amount) = switch (x.kind) { case (#cashFlow(c)) (true, c.hedgedAmount); case (#fairValue) (false, 0) };
        put(s, { id = block; hedging = x.hedging; hedged = x.hedged; cashFlow = cf; hedgedAmount = amount; state = #designated; designatedDay = x.day; hedgingMark = x.hedgingMark; hedgedValue = x.hedgedValue; reserve = 0; basisAdjustment = 0; lastEffectivenessBps = 0; lastAssessedDay = x.day; lastBlock = block });
        s.count += 1; s.open += 1;
      };
      case (#hedgeAssessed(x)) {
        switch (hedge(s, x.hedge)) {
          case (?r) put(s, { r with hedgingMark = r.hedgingMark + x.hedgingChange; hedgedValue = r.hedgedValue + x.hedgedChange; reserve = if (r.cashFlow) r.reserve + x.effective else r.reserve; basisAdjustment = if (r.cashFlow) r.basisAdjustment else r.basisAdjustment + x.hedgedChange; lastEffectivenessBps = x.effectivenessBps; lastAssessedDay = x.day; lastBlock = block });
          case null {};
        };
      };
      case (#hedgeDedesignated(x)) { switch (hedge(s, x.hedge)) { case (?r) { put(s, { r with state = #dedesignated; reserve = 0; lastBlock = block }); if (s.open > 0) s.open -= 1 }; case null {} } };
    }
  };

  public func view(r : Row) : VT.HedgeView {
    { id = r.id; hedging = r.hedging; hedged = r.hedged; kind = if (r.cashFlow) "cashFlow" else "fairValue"; state = switch (r.state) { case (#designated) "designated"; case (#dedesignated) "dedesignated" }; designatedDay = r.designatedDay;
      hedgingMark = r.hedgingMark; hedgedValue = r.hedgedValue; reserve = r.reserve; basisAdjustment = r.basisAdjustment; lastEffectivenessBps = r.lastEffectivenessBps; lastAssessedDay = r.lastAssessedDay; lastBlock = r.lastBlock }
  };
  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.policy) { case null w.byte(0); case (?p) { w.byte(1); w.text(p.hedgeReserve) } };
    w.nat(s.count); w.nat(s.open); w.nat(s.quotes); w.nat(s.thetas);
    let (lo, hi) = R.fullRange(8);
    var n = 0;
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.rows, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v); n += 1 };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    w.nat(n);
  };
}
