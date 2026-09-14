/// CollateralCore.mo: the collateral agreements, their pools and their calls folded from the desk log in stable
/// memory; the haircut schedules; the credit support arithmetic; the legs every cash event posts.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import DC "mo:manticore/DayCount";
import TT "mo:manticore/TreasuryTypes";
import M "mo:manticore/TreasuryMath";
import Posting "mo:manticore/Posting";

import CuT "CustodyTypes";
import CoT "CollateralTypes";

module {

  public let AGREEMENT_ROW_BYTES : Nat = 197;
  public let CASH_ROW_BYTES : Nat = 29;
  public let SECURITIES_ROW_BYTES : Nat = 115;
  public let CALL_ROW_BYTES : Nat = 58;
  public let HAIRCUT_ROW_BYTES : Nat = 8;
  public let MAX_SCHEDULE_ROWS : Nat = 64;
  /// The supervisory add-on for a currency mismatch between the collateral and the exposure (CRE22.52).
  public let FX_MISMATCH_BPS : Nat = 800;
  let MAX_PAGE = 512;

  public type AgreementRow = {
    id : Text; cpHash : Nat; counterparty : Text; currency : Text; threshold : Nat; minimumTransfer : Nat; rounding : Nat; netting : Bool; covers : Nat8; bilateral : Bool;
    cashRateBps : Nat; dayCount : Nat8; cashAccount : Text; cashSub : ?Text; graceDays : Nat;
    exposure : Int; balance : Int; exposureDay : Nat; pendingExposure : Int; pendingRows : Nat; openCall : Nat; lastBlock : Nat;
  };
  public type CashRow = { agreement : Text; currency : Text; received : Nat; given : Nat; interestAccrued : Int; accrualFrom : Nat };
  public type SecuritiesRow = { id : Nat; agreement : Text; lot : ?TT.DealId; isin : Text; depot : Text; nominal : Nat; given : Bool; state : CoT.SecuritiesState; cashReturned : Nat; currency : Text; day : Nat };
  public type CallRow = { id : Nat; agreement : Text; amount : Nat; outstanding : Nat; deliver : Bool; day : Nat; due : Nat; state : Nat8 };
  public let CALL_OPEN : Nat8 = 1;
  public let CALL_MET : Nat8 = 2;
  public let CALL_SUPERSEDED : Nat8 = 3;
  public func callStateText(c : Nat8) : Text { switch (c) { case 1 "open"; case 2 "met"; case _ "superseded" } };

  public func hash8(t : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(t))), 0, 8) };
  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  func convCode(c : DC.Convention) : Nat8 { switch (c) { case (#a001_ActActIcma(_)) 1; case (#a003_Act360) 3; case (#a004_Act365Fixed) 4; case (#a005_ActActIsda) 5; case (#a006_Thirty360Isda) 6; case (#a007_ThirtyE360) 7; case (#a011_Thirty365) 11 } };
  public func convOf(c : Nat8) : DC.Convention { switch (c) { case 1 #a001_ActActIcma({ couponsPerYear = 1 }); case 3 #a003_Act360; case 4 #a004_Act365Fixed; case 5 #a005_ActActIsda; case 6 #a006_Thirty360Isda; case 7 #a007_ThirtyE360; case _ #a011_Thirty365 } };
  public func coverageBit(c : CoT.Coverage) : Nat8 { switch (c) { case (#treasury) 1; case (#calls) 2; case (#repos) 4; case (#loans) 8 } };
  public func coversOf(mask : Nat8) : [Text] {
    let out = List.empty<Text>();
    for (c in [#treasury, #calls, #repos, #loans].vals()) { if ((mask & coverageBit(c)) != 0) List.add(out, CoT.coverageText(c)) };
    List.toArray(out)
  };
  public func covers(r : AgreementRow, c : CoT.Coverage) : Bool { (r.covers & coverageBit(c)) != 0 };
  func classCode(c : CuT.Classification) : Nat8 { switch (c) { case (#sovereign) 1; case (#supranational) 2; case (#financial) 3; case (#corporate) 4 } };
  func stateCode(s : CoT.SecuritiesState) : Nat8 { switch (s) { case (#pledged) 1; case (#instructed) 2; case (#live) 3; case (#returned) 4 } };
  func stateOf(c : Nat8) : CoT.SecuritiesState { switch (c) { case 1 #pledged; case 2 #instructed; case 3 #live; case _ #returned } };
  public func agreementSub(id : Text) : JT.SubledgerKey { Posting.subledgerOf("collateral/" # id) };

  func encodeAgreement(r : AgreementRow) : Blob {
    let b = R.buf();
    R.putNat(b, r.cpHash, 8); R.putText(b, r.counterparty, 32); R.putText(b, r.currency, 8); R.putNat(b, r.threshold, 8); R.putNat(b, r.minimumTransfer, 8); R.putNat(b, r.rounding, 8);
    R.putBool(b, r.netting); R.putByte(b, r.covers); R.putBool(b, r.bilateral); R.putNat(b, r.cashRateBps, 4); R.putByte(b, r.dayCount); R.putText(b, r.cashAccount, 32);
    switch (r.cashSub) { case null { R.putByte(b, 0); R.putText(b, "", 32) }; case (?t) { R.putByte(b, 1); R.putText(b, t, 32) } };
    R.putNat(b, r.graceDays, 1); putInt(b, r.exposure); putInt(b, r.balance); R.putNat(b, r.exposureDay, 4); putInt(b, r.pendingExposure); R.putNat(b, r.pendingRows, 4); R.putNat(b, r.openCall, 8); R.putNat(b, r.lastBlock, 8);
    R.done(b, AGREEMENT_ROW_BYTES)
  };
  func decodeAgreement(id : Text, v : Blob) : AgreementRow {
    let a = Blob.toArray(v);
    { id; cpHash = R.getNat(a, 0, 8); counterparty = R.getText(a, 8, 32); currency = R.getText(a, 40, 8); threshold = R.getNat(a, 48, 8); minimumTransfer = R.getNat(a, 56, 8); rounding = R.getNat(a, 64, 8);
      netting = R.getBool(a, 72); covers = a[73]; bilateral = R.getBool(a, 74); cashRateBps = R.getNat(a, 75, 4); dayCount = a[79]; cashAccount = R.getText(a, 80, 32);
      cashSub = if (a[112] == 1) ?R.getText(a, 113, 32) else null;
      graceDays = R.getNat(a, 145, 1); exposure = getInt(a, 146); balance = getInt(a, 155); exposureDay = R.getNat(a, 164, 4); pendingExposure = getInt(a, 168); pendingRows = R.getNat(a, 177, 4); openCall = R.getNat(a, 181, 8); lastBlock = R.getNat(a, 189, 8) }
  };
  func encodeCash(r : CashRow) : Blob { let b = R.buf(); R.putNat(b, r.received, 8); R.putNat(b, r.given, 8); putInt(b, r.interestAccrued); R.putNat(b, r.accrualFrom, 4); R.done(b, CASH_ROW_BYTES) };
  func decodeCash(agreement : Text, currency : Text, v : Blob) : CashRow { let a = Blob.toArray(v); { agreement; currency; received = R.getNat(a, 0, 8); given = R.getNat(a, 8, 8); interestAccrued = getInt(a, 16); accrualFrom = R.getNat(a, 25, 4) } };
  func encodeSecurities(r : SecuritiesRow) : Blob {
    let b = R.buf();
    R.putText(b, r.agreement, 32);
    switch (r.lot) { case null { R.putByte(b, 0); R.putNat(b, 0, 8) }; case (?l) { R.putByte(b, 1); R.putNat(b, l, 8) } };
    R.putText(b, r.isin, 12); R.putText(b, r.depot, 32); R.putNat(b, r.nominal, 8); R.putBool(b, r.given); R.putByte(b, stateCode(r.state)); R.putNat(b, r.cashReturned, 8); R.putText(b, r.currency, 8); R.putNat(b, r.day, 4);
    R.done(b, SECURITIES_ROW_BYTES)
  };
  func decodeSecurities(id : Nat, v : Blob) : SecuritiesRow {
    let a = Blob.toArray(v);
    { id; agreement = R.getText(a, 0, 32); lot = if (a[32] == 1) ?R.getNat(a, 33, 8) else null; isin = R.getText(a, 41, 12); depot = R.getText(a, 53, 32); nominal = R.getNat(a, 85, 8); given = R.getBool(a, 93); state = stateOf(a[94]);
      cashReturned = R.getNat(a, 95, 8); currency = R.getText(a, 103, 8); day = R.getNat(a, 111, 4) }
  };
  func encodeCall(r : CallRow) : Blob { let b = R.buf(); R.putText(b, r.agreement, 32); R.putNat(b, r.amount, 8); R.putNat(b, r.outstanding, 8); R.putBool(b, r.deliver); R.putNat(b, r.day, 4); R.putNat(b, r.due, 4); R.putByte(b, r.state); R.done(b, CALL_ROW_BYTES) };
  func decodeCall(id : Nat, v : Blob) : CallRow { let a = Blob.toArray(v); { id; agreement = R.getText(a, 0, 32); amount = R.getNat(a, 32, 8); outstanding = R.getNat(a, 40, 8); deliver = R.getBool(a, 48); day = R.getNat(a, 49, 4); due = R.getNat(a, 53, 4); state = a[57] } };

  public type State = {
    agreements : RI.State;      // id(32) -> row
    byCounterparty : RI.State;  // cpHash(8) -> id(32)
    haircuts : RI.State;        // agreement(32) ‖ class(1) ‖ fromDays(4) -> toDays(4) ‖ bps(4)
    cash : RI.State;            // agreement(32) ‖ currency(8) -> row
    securities : RI.State;      // id(8) -> row
    securitiesByAgreement : RI.State; // agreement(32) ‖ id(8) -> 1
    calls : RI.State;           // id(8) -> row
    callsByAgreement : RI.State;      // agreement(32) ‖ id(8) -> 1
    var policy : ?CoT.Policy;
    var agreementCount : Nat;
    var cashRows : Nat;
    var securitiesRows : Nat;
    var callCount : Nat;
    var openCalls : Nat;
    var exposures : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      agreements = RI.newStateIn(arena, { keyBytes = 32; valBytes = AGREEMENT_ROW_BYTES });
      byCounterparty = RI.newStateIn(arena, { keyBytes = 8; valBytes = 32 });
      haircuts = RI.newStateIn(arena, { keyBytes = 37; valBytes = HAIRCUT_ROW_BYTES });
      cash = RI.newStateIn(arena, { keyBytes = 40; valBytes = CASH_ROW_BYTES });
      securities = RI.newStateIn(arena, { keyBytes = 8; valBytes = SECURITIES_ROW_BYTES });
      securitiesByAgreement = RI.newStateIn(arena, { keyBytes = 40; valBytes = 1 });
      calls = RI.newStateIn(arena, { keyBytes = 8; valBytes = CALL_ROW_BYTES });
      callsByAgreement = RI.newStateIn(arena, { keyBytes = 40; valBytes = 1 });
      var policy = null; var agreementCount = 0; var cashRows = 0; var securitiesRows = 0; var callCount = 0; var openCalls = 0; var exposures = 0;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func policy(s : State) : ?CoT.Policy { s.policy };
  public func agreement(s : State, id : Text) : ?AgreementRow { switch (RI.get(s.agreements, R.textKey(id, 32))) { case (?v) ?decodeAgreement(id, v); case null null } };
  func putAgreement(s : State, r : AgreementRow) { ignore RI.put(s.agreements, R.textKey(r.id, 32), encodeAgreement(r)) };
  public func agreementOfCounterpartyHash(s : State, h : Nat) : ?AgreementRow {
    switch (RI.get(s.byCounterparty, R.key(h, 8))) { case (?v) agreement(s, R.getText(Blob.toArray(v), 0, 32)); case null null }
  };
  public func agreementOfCounterparty(s : State, name : Text) : ?AgreementRow { agreementOfCounterpartyHash(s, hash8(name)) };
  func cashKey(agreement : Text, currency : Text) : Blob { Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(agreement, 32)), Blob.toArray(R.textKey(currency, 8)))) };
  public func cashRow(s : State, agreement : Text, currency : Text) : CashRow {
    switch (RI.get(s.cash, cashKey(agreement, currency))) { case (?v) decodeCash(agreement, currency, v); case null ({ agreement; currency; received = 0; given = 0; interestAccrued = 0; accrualFrom = 0 }) }
  };
  func putCash(s : State, r : CashRow) { if (RI.get(s.cash, cashKey(r.agreement, r.currency)) == null) s.cashRows += 1; ignore RI.put(s.cash, cashKey(r.agreement, r.currency), encodeCash(r)) };
  func textPrefixRange(t : Text, width : Nat, rest : Nat) : (Blob, Blob) {
    let p = Blob.toArray(R.textKey(t, width));
    (Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(0, rest))), Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(255, rest))))
  };
  func walk(idx : RI.State, lo : Blob, hi : Blob, visit : (Blob, Blob) -> ()) {
    var cursor : ?Blob = null;
    label go loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) visit(k, v);
      switch (page.cursor) { case null break go; case (?c) cursor := ?c };
    };
  };
  public func cashOf(s : State, agreement : Text) : [CashRow] {
    let out = List.empty<CashRow>();
    let (lo, hi) = textPrefixRange(agreement, 32, 8);
    walk(s.cash, lo, hi, func(k, v) { List.add(out, decodeCash(agreement, R.getText(Blob.toArray(k), 32, 8), v)) });
    List.toArray(out)
  };
  public func securitiesRow(s : State, id : Nat) : ?SecuritiesRow { switch (RI.get(s.securities, R.key(id, 8))) { case (?v) ?decodeSecurities(id, v); case null null } };
  func putSecurities(s : State, r : SecuritiesRow) {
    if (RI.get(s.securities, R.key(r.id, 8)) == null) { s.securitiesRows += 1; ignore RI.put(s.securitiesByAgreement, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.agreement, 32)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1])) };
    ignore RI.put(s.securities, R.key(r.id, 8), encodeSecurities(r));
  };
  public func securitiesOf(s : State, agreement : Text) : [SecuritiesRow] {
    let out = List.empty<SecuritiesRow>();
    let (lo, hi) = textPrefixRange(agreement, 32, 8);
    walk(s.securitiesByAgreement, lo, hi, func(k, _) { switch (securitiesRow(s, R.getNat(Blob.toArray(k), 32, 8))) { case (?r) List.add(out, r); case null {} } });
    List.toArray(out)
  };
  public func call(s : State, id : Nat) : ?CallRow { switch (RI.get(s.calls, R.key(id, 8))) { case (?v) ?decodeCall(id, v); case null null } };
  func putCall(s : State, r : CallRow) {
    if (RI.get(s.calls, R.key(r.id, 8)) == null) { s.callCount += 1; ignore RI.put(s.callsByAgreement, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.agreement, 32)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1])) };
    ignore RI.put(s.calls, R.key(r.id, 8), encodeCall(r));
  };
  public func callsOf(s : State, agreement : Text) : [CallRow] {
    let out = List.empty<CallRow>();
    let (lo, hi) = textPrefixRange(agreement, 32, 8);
    walk(s.callsByAgreement, lo, hi, func(k, _) { switch (call(s, R.getNat(Blob.toArray(k), 32, 8))) { case (?r) List.add(out, r); case null {} } });
    List.toArray(out)
  };
  public func agreements(s : State) : [AgreementRow] {
    let out = List.empty<AgreementRow>();
    let (lo, hi) = R.fullRange(32);
    walk(s.agreements, lo, hi, func(k, v) { List.add(out, decodeAgreement(R.getText(Blob.toArray(k), 0, 32), v)) });
    List.toArray(out)
  };

  // ─── haircuts ─────────────────────────────────────────────────────────────

  /// The supervisory haircuts of CRE22.49 for the highest rating band of each issuer class, by residual maturity:
  /// sovereigns and supranationals at 0.5, 2 and 4 percent, financials and corporates at 1, 4 and 8, over the
  /// buckets up to a year, one to five years, beyond five. An agreement whose counterparty negotiated finer cuts
  /// records a bilateral schedule instead.
  public func supervisoryHaircutBps(c : CuT.Classification, remainingDays : Nat) : Nat {
    let sovereign = switch (c) { case (#sovereign or #supranational) true; case (_) false };
    if (remainingDays <= 365) { if (sovereign) 50 else 100 }
    else if (remainingDays <= 5 * 365) { if (sovereign) 200 else 400 }
    else { if (sovereign) 400 else 800 }
  };
  func haircutKey(agreement : Text, c : CuT.Classification, fromDays : Nat) : Blob {
    Blob.fromArray(Array.concat<Nat8>(Array.concat<Nat8>(Blob.toArray(R.textKey(agreement, 32)), [classCode(c)]), Blob.toArray(R.key(fromDays, 4))))
  };
  /// The haircut an agreement applies to an instrument of a class with a residual maturity: the bilateral row
  /// whose bucket holds the days, or the supervisory figure when the agreement records no schedule.
  public func haircutBps(s : State, a : AgreementRow, c : CuT.Classification, remainingDays : Nat) : ?Nat {
    if (not a.bilateral) return ?supervisoryHaircutBps(c, remainingDays);
    let p = Array.concat<Nat8>(Blob.toArray(R.textKey(a.id, 32)), [classCode(c)]);
    let lo = Blob.fromArray(Array.concat<Nat8>(p, [0, 0, 0, 0]));
    let hi = Blob.fromArray(Array.concat<Nat8>(p, [255, 255, 255, 255]));
    var found : ?Nat = null;
    walk(s.haircuts, lo, hi, func(k, v) {
      let fromDays = R.getNat(Blob.toArray(k), 33, 4); let vv = Blob.toArray(v); let toDays = R.getNat(vv, 0, 4);
      if (found == null and remainingDays >= fromDays and remainingDays <= toDays) found := ?R.getNat(vv, 4, 4);
    });
    found
  };
  /// A security's value in the pool: the clean value at the day's price, less the haircut and the currency
  /// mismatch add-on, floored at zero.
  public func securitiesValue(nominal : Nat, priceMicro : Nat, haircutBps_ : Nat, mismatch : Bool) : Nat {
    let cut = haircutBps_ + (if (mismatch) FX_MISMATCH_BPS else 0);
    if (cut >= M.BPS) return 0;
    M.roundNat(M.q(M.cleanCost(nominal, priceMicro) * (M.BPS - cut), M.BPS))
  };
  public func cashValue(amount : Nat, mismatch : Bool) : Nat { if (mismatch) M.roundNat(M.q(amount * (M.BPS - FX_MISMATCH_BPS), M.BPS)) else amount };

  // ─── the credit support arithmetic ─────────────────────────────────────────

  /// The requirement an exposure makes: nothing within the threshold either way, the excess beyond it signed by
  /// who is exposed (positive: the counterparty owes the desk credit support).
  public func requirement(a : AgreementRow, exposure : Int) : Int {
    let t : Int = a.threshold;
    if (exposure > t) exposure - t else if (exposure < -t) exposure + t else 0
  };
  func roundUp(x : Nat, r : Nat) : Nat { if (r == 0) x else ((x + r - 1) / r) * r };
  func roundDown(x : Nat, r : Nat) : Nat { if (r == 0) x else (x / r) * r };
  /// The call the figures raise: the shortfall of the credit support balance against the requirement, rounded
  /// up when it is a delivery and down when it returns excess, and only when it passes the minimum transfer
  /// amount. `deliver` is true when the desk delivers.
  public func callFor(a : AgreementRow, exposure : Int, balance : Int) : ?(Nat, Bool) {
    let delta = requirement(a, exposure) - balance;
    if (delta > 0) {
      let d = Int.abs(delta);
      if (d < a.minimumTransfer) return null;
      // the desk receives: a delivery by the counterparty, or a return of what the desk posted in excess
      let amount = if (balance < 0) roundDown(d, a.rounding) else roundUp(d, a.rounding);
      if (amount == 0) null else ?(amount, false)
    } else if (delta < 0) {
      let d = Int.abs(delta);
      if (d < a.minimumTransfer) return null;
      let amount = if (balance > 0) roundDown(d, a.rounding) else roundUp(d, a.rounding);
      if (amount == 0) null else ?(amount, true)
    } else null
  };
  /// The interest on a currency's net cash to a day: on the net given at the agreement's rate as income, on the
  /// net received as expense; nothing before the first movement.
  public func interestTarget(a : AgreementRow, c : CashRow, day : Nat) : Int {
    if (c.accrualFrom == 0 or day <= c.accrualFrom or a.cashRateBps == 0) return c.interestAccrued;
    let net : Int = (c.given : Int) - c.received;
    if (net == 0) return c.interestAccrued;
    let i : Int = M.simpleInterestTo(Int.abs(net), a.cashRateBps, convOf(a.dayCount), c.accrualFrom, day);
    c.interestAccrued + (if (net > 0) i else -i)
  };

  // ─── planners ─────────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, CoT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidAgreement({ reason })) };
  func badMove<X>(reason : Text) : Res<X> { #err(#InvalidMove({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planPolicy(p : CoT.Policy) : Res<CoT.Event> {
    for (a in accountsOf(p).vals()) { if (bytesOf(a) == 0 or bytesOf(a) > 32) return bad("every account is named in 1..32 bytes") };
    #ok(#policySet(p))
  };
  public func accountsOf(p : CoT.Policy) : [Text] { [p.cashReceivedPayable, p.cashGivenReceivable, p.interestPayable, p.interestReceivable, p.interestExpense, p.interestIncome] };

  /// An agreement set or re-set: one per counterparty, its terms bounded, its schedule's buckets in order.
  public func planSetAgreement(s : State, a : CoT.Agreement, day : Nat) : Res<CoT.Event> {
    if (bytesOf(a.id) == 0 or bytesOf(a.id) > 32) return bad("an agreement is named in 1..32 bytes");
    if (bytesOf(a.counterparty.name) == 0 or bytesOf(a.counterparty.name) > 32) return bad("a counterparty is named in 1..32 bytes");
    if (bytesOf(a.currency) != 3) return bad("a currency code has three letters");
    if (a.covers.size() == 0) return bad("an agreement covers at least one family");
    if (a.cashRateBps > 100_000) return bad("the cash rate is at most 1,000 percent");
    if (a.graceDays > 30) return bad("the grace is at most 30 days");
    if (bytesOf(a.cash.account) == 0 or bytesOf(a.cash.account) > 32) return bad("the cash account is named in 1..32 bytes");
    switch (a.cash.sub) { case (?t) { if (bytesOf(t) == 0 or bytesOf(t) > 32) return bad("the cash sub-ledger is named in 1..32 bytes") }; case null {} };
    switch (a.schedule) {
      case (?rows) {
        if (rows.size() == 0 or rows.size() > MAX_SCHEDULE_ROWS) return bad("a bilateral schedule holds 1.." # Nat.toText(MAX_SCHEDULE_ROWS) # " rows");
        for (r in rows.vals()) { if (r.toDays < r.fromDays) return bad("a schedule bucket runs from its lower bound to its upper"); if (r.haircutBps >= M.BPS) return bad("a haircut is below 100 percent") };
      };
      case null {};
    };
    switch (agreementOfCounterparty(s, a.counterparty.name)) { case (?e) { if (not Text.equal(e.id, a.id)) return #err(#AgreementExists({ counterparty = a.counterparty.name; agreement = e.id })) }; case null {} };
    switch (agreement(s, a.id)) { case (?e) { if (e.cpHash != hash8(a.counterparty.name)) return bad("an agreement keeps its counterparty"); if (not Text.equal(e.currency, a.currency)) return bad("an agreement keeps its currency") }; case null {} };
    #ok(#agreementSet({ agreement = a; day }))
  };
  public func requireAgreement(s : State, id : Text) : Res<AgreementRow> { switch (agreement(s, id)) { case (?r) #ok(r); case null #err(#UnknownAgreement({ agreement = id })) } };
  /// A cash movement: a return is within what the side holds.
  public func planCashMove(s : State, id : Text, move : CoT.CashMove, amount : Nat, currency : Text, callCredit : Nat, day : Nat) : Res<CoT.Event> {
    let a = switch (requireAgreement(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    if (amount == 0) return badMove("a movement is positive");
    if (bytesOf(currency) != 3) return badMove("a currency code has three letters");
    let c = cashRow(s, a.id, currency);
    switch (move) {
      case (#receivedReturned) { if (c.received < amount) return #err(#PoolShort({ agreement = id; currency; held = c.received; wanted = amount })) };
      case (#givenReturned) { if (c.given < amount) return #err(#PoolShort({ agreement = id; currency; held = c.given; wanted = amount })) };
      case (_) {};
    };
    if (day < c.accrualFrom) return badMove("a movement is not dated before the last accrual");
    #ok(#cashMoved({ agreement = id; move; amount; currency; callCredit; interestCatchUp = interestTarget(a, c, day) - c.interestAccrued; day }))
  };
  public func planSettleInterest(s : State, id : Text, currency : Text, day : Nat) : Res<CoT.Event> {
    switch (requireAgreement(s, id)) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    let c = cashRow(s, id, currency);
    if (c.interestAccrued == 0) return badMove("no interest is accrued in " # currency);
    #ok(#interestSettled({ agreement = id; currency; amount = c.interestAccrued; day }))
  };
  public func requireCall(s : State, id : Nat) : Res<CallRow> { switch (call(s, id)) { case (?c) #ok(c); case null #err(#UnknownPledge({ agreement = ""; id })) } };
  public func requireSecurities(s : State, agreement : Text, id : Nat, wanted : CoT.SecuritiesState) : Res<SecuritiesRow> {
    let ?r = securitiesRow(s, id) else return #err(#UnknownPledge({ agreement; id }));
    if (not Text.equal(r.agreement, agreement)) return #err(#UnknownPledge({ agreement; id }));
    if (r.state != wanted) return #err(#PledgeNotIn({ id; state = CoT.securitiesStateText(r.state); wanted = CoT.securitiesStateText(wanted) }));
    #ok(r)
  };

  func interestLegs(add : (Text, ?JT.SubledgerKey, JT.Side, Text, Nat) -> (), p : CoT.Policy, sub : ?JT.SubledgerKey, currency : Text, interest : Int) {
    if (interest > 0) { add(p.interestReceivable, sub, #debit, currency, Int.abs(interest)); add(p.interestIncome, null, #credit, currency, Int.abs(interest)) }
    else if (interest < 0) { add(p.interestExpense, null, #debit, currency, Int.abs(interest)); add(p.interestPayable, sub, #credit, currency, Int.abs(interest)) };
  };
  /// The legs a cash event posts: received cash against the payable, given cash against the receivable, the
  /// returns the other way, each after the interest caught up on the net it changes; the interest accrued to
  /// income or expense against its receivable or payable, and its settlement through cash.
  public func legsOf(p : CoT.Policy, a : AgreementRow, ev : CoT.Event) : [JT.Leg] {
    let ls = List.empty<JT.Leg>();
    let sub = ?agreementSub(a.id);
    let cs = switch (a.cashSub) { case (?t) ?Posting.subledgerOf(t); case null null };
    func add(account : Text, s : ?JT.SubledgerKey, side : JT.Side, ccy : Text, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, s, side, ccy, amount)) };
    switch (ev) {
      case (#cashMoved(x)) {
        interestLegs(add, p, sub, x.currency, x.interestCatchUp);
        switch (x.move) {
          case (#received) { add(a.cashAccount, cs, #debit, x.currency, x.amount); add(p.cashReceivedPayable, sub, #credit, x.currency, x.amount) };
          case (#given) { add(p.cashGivenReceivable, sub, #debit, x.currency, x.amount); add(a.cashAccount, cs, #credit, x.currency, x.amount) };
          case (#receivedReturned) { add(p.cashReceivedPayable, sub, #debit, x.currency, x.amount); add(a.cashAccount, cs, #credit, x.currency, x.amount) };
          case (#givenReturned) { add(a.cashAccount, cs, #debit, x.currency, x.amount); add(p.cashGivenReceivable, sub, #credit, x.currency, x.amount) };
        };
      };
      case (#substitutionSettled(_)) {};
      case (#interestAccrued(x)) interestLegs(add, p, sub, x.currency, x.interest);
      case (#interestSettled(x)) {
        if (x.amount > 0) { add(a.cashAccount, cs, #debit, x.currency, Int.abs(x.amount)); add(p.interestReceivable, sub, #credit, x.currency, Int.abs(x.amount)) }
        else if (x.amount < 0) { add(p.interestPayable, sub, #debit, x.currency, Int.abs(x.amount)); add(a.cashAccount, cs, #credit, x.currency, Int.abs(x.amount)) };
      };
      case (_) {};
    };
    List.toArray(ls)
  };
  /// The cash a substitution returns to the desk: the given cash comes back as the securities go out.
  public func substitutionLegs(p : CoT.Policy, a : AgreementRow, r : SecuritiesRow, interestCatchUp : Int) : [JT.Leg] {
    if (r.cashReturned == 0) return [];
    let ls = List.empty<JT.Leg>();
    let cs = switch (a.cashSub) { case (?t) ?Posting.subledgerOf(t); case null null };
    func add(account : Text, s : ?JT.SubledgerKey, side : JT.Side, ccy : Text, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, s, side, ccy, amount)) };
    interestLegs(add, p, ?agreementSub(a.id), r.currency, interestCatchUp);
    add(a.cashAccount, cs, #debit, r.currency, r.cashReturned); add(p.cashGivenReceivable, ?agreementSub(a.id), #credit, r.currency, r.cashReturned);
    List.toArray(ls)
  };

  // ─── the fold ─────────────────────────────────────────────────────────────

  public func fold(s : State, block : Nat, ev : CoT.Event) {
    switch (ev) {
      case (#policySet(p)) s.policy := ?p;
      case (#agreementSet(x)) {
        let a = x.agreement;
        var mask : Nat8 = 0;
        for (c in a.covers.vals()) mask := mask | coverageBit(c);
        let base : AgreementRow = switch (agreement(s, a.id)) {
          case (?e) e;
          case null { s.agreementCount += 1; ignore RI.put(s.byCounterparty, R.key(hash8(a.counterparty.name), 8), R.textKey(a.id, 32)); { id = a.id; cpHash = hash8(a.counterparty.name); counterparty = a.counterparty.name; currency = a.currency; threshold = 0; minimumTransfer = 0; rounding = 0; netting = false; covers = 0; bilateral = false; cashRateBps = 0; dayCount = 3; cashAccount = ""; cashSub = null; graceDays = 0; exposure = 0; balance = 0; exposureDay = 0; pendingExposure = 0; pendingRows = 0; openCall = 0; lastBlock = block } };
        };
        putAgreement(s, { base with threshold = a.threshold; minimumTransfer = a.minimumTransfer; rounding = a.rounding; netting = a.netting; covers = mask; bilateral = a.schedule != null; cashRateBps = a.cashRateBps; dayCount = convCode(a.dayCount); cashAccount = a.cash.account; cashSub = a.cash.sub; graceDays = a.graceDays; lastBlock = block });
        switch (a.schedule) {
          case (?rows) { for (r in rows.vals()) { let b = R.buf(); R.putNat(b, r.toDays, 4); R.putNat(b, r.haircutBps, 4); ignore RI.put(s.haircuts, haircutKey(a.id, r.classification, r.fromDays), R.done(b, HAIRCUT_ROW_BYTES)) } };
          case null {};
        };
      };
      case (#cashMoved(x)) {
        let c = cashRow(s, x.agreement, x.currency);
        let (received, given) = switch (x.move) {
          case (#received) (c.received + x.amount, c.given);
          case (#given) (c.received, c.given + x.amount);
          case (#receivedReturned) (if (c.received > x.amount) (c.received - x.amount : Nat) else 0, c.given);
          case (#givenReturned) (c.received, if (c.given > x.amount) (c.given - x.amount : Nat) else 0);
        };
        putCash(s, { c with received; given; interestAccrued = c.interestAccrued + x.interestCatchUp; accrualFrom = x.day });
        credit(s, x.agreement, x.callCredit);
        touch(s, x.agreement, block);
      };
      case (#securitiesPledged(x)) { putSecurities(s, { id = x.id; agreement = x.agreement; lot = ?x.lot; isin = x.isin; depot = x.depot; nominal = x.nominal; given = true; state = #live; cashReturned = 0; currency = ""; day = x.day }); credit(s, x.agreement, x.callCredit); touch(s, x.agreement, block) };
      case (#securitiesReleased(x)) { switch (securitiesRow(s, x.pledge)) { case (?r) putSecurities(s, { r with state = #returned }); case null {} }; credit(s, x.agreement, x.callCredit); touch(s, x.agreement, block) };
      case (#securitiesReceived(x)) { putSecurities(s, { id = x.id; agreement = x.agreement; lot = null; isin = x.isin; depot = x.depot; nominal = x.nominal; given = false; state = #live; cashReturned = 0; currency = ""; day = x.day }); credit(s, x.agreement, x.callCredit); touch(s, x.agreement, block) };
      case (#securitiesReturned(x)) { switch (securitiesRow(s, x.receipt)) { case (?r) putSecurities(s, { r with state = #returned }); case null {} }; credit(s, x.agreement, x.callCredit); touch(s, x.agreement, block) };
      case (#substitutionOpened(x)) { putSecurities(s, { id = x.id; agreement = x.agreement; lot = ?x.lot; isin = x.isin; depot = x.depot; nominal = x.nominal; given = true; state = #pledged; cashReturned = x.cashReturned; currency = x.currency; day = x.day }); touch(s, x.agreement, block) };
      case (#substitutionSettled(x)) {
        switch (securitiesRow(s, x.substitution)) {
          case (?r) {
            putSecurities(s, { r with state = #live });
            if (r.cashReturned > 0) { let c = cashRow(s, x.agreement, r.currency); putCash(s, { c with given = if (c.given > r.cashReturned) (c.given - r.cashReturned : Nat) else 0; interestAccrued = c.interestAccrued + x.interestCatchUp; accrualFrom = x.day }) };
          };
          case null {};
        };
        credit(s, x.agreement, x.callCredit);
        touch(s, x.agreement, block);
      };
      case (#interestAccrued(x)) { let c = cashRow(s, x.agreement, x.currency); putCash(s, { c with interestAccrued = c.interestAccrued + x.interest; accrualFrom = x.day }) };
      case (#interestSettled(x)) { let c = cashRow(s, x.agreement, x.currency); putCash(s, { c with interestAccrued = c.interestAccrued - x.amount }); touch(s, x.agreement, block) };
      case (#exposureRecorded(x)) { switch (agreement(s, x.agreement)) { case (?a) { putAgreement(s, { a with exposure = x.exposure; balance = x.balance; exposureDay = x.day; pendingExposure = 0; pendingRows = 0; lastBlock = block }); s.exposures += 1 }; case null {} } };
      case (#callRaised(x)) {
        putCall(s, { id = x.id; agreement = x.agreement; amount = x.amount; outstanding = x.amount; deliver = x.deliver; day = x.day; due = x.due; state = CALL_OPEN });
        s.openCalls += 1;
        switch (agreement(s, x.agreement)) { case (?a) putAgreement(s, { a with openCall = x.id; lastBlock = block }); case null {} };
      };
      case (#callMet(x)) { closeCall(s, x.agreement, x.call, CALL_MET, block) };
      case (#callSuperseded(x)) { closeCall(s, x.agreement, x.call, CALL_SUPERSEDED, block) };
    }
  };
  func touch(s : State, agreement_ : Text, block : Nat) { switch (agreement(s, agreement_)) { case (?a) putAgreement(s, { a with lastBlock = block }); case null {} } };
  func closeCall(s : State, agreement_ : Text, id : Nat, state : Nat8, block : Nat) {
    switch (call(s, id)) { case (?c) { if (c.state == CALL_OPEN) s.openCalls -= 1; putCall(s, { c with state; outstanding = if (state == CALL_MET) 0 else c.outstanding }) }; case null {} };
    switch (agreement(s, agreement_)) { case (?a) { if (a.openCall == id) putAgreement(s, { a with openCall = 0; lastBlock = block }) }; case null {} };
  };
  /// A slice's contribution to an agreement's exposure, accumulated until the publication records it.
  public func accumulate(s : State, agreement_ : Text, contribution : Int, rows : Nat) {
    switch (agreement(s, agreement_)) { case (?a) putAgreement(s, { a with pendingExposure = a.pendingExposure + contribution; pendingRows = a.pendingRows + rows }); case null {} }
  };
  /// A sweep opened: every agreement's partial sums start from nothing.
  public func resetPending(s : State) {
    for (a in agreements(s).vals()) { if (a.pendingExposure != 0 or a.pendingRows != 0) putAgreement(s, { a with pendingExposure = 0; pendingRows = 0 }) };
  };
  /// A call's outstanding reduced by a movement in its direction, in the fold of the movement's value.
  public func credit(s : State, agreement_ : Text, value : Nat) {
    switch (agreement(s, agreement_)) {
      case (?a) { if (a.openCall != 0) { switch (call(s, a.openCall)) { case (?c) putCall(s, { c with outstanding = if (c.outstanding > value) c.outstanding - value else 0 }); case null {} } } };
      case null {};
    }
  };

  public func agreementView(r : AgreementRow, interestAccrued : Int) : CoT.AgreementView {
    { id = r.id; counterparty = r.counterparty; currency = r.currency; threshold = r.threshold; minimumTransfer = r.minimumTransfer; rounding = r.rounding; netting = r.netting; covers = coversOf(r.covers);
      bilateral = r.bilateral; cashRateBps = r.cashRateBps; graceDays = r.graceDays; exposure = r.exposure; balance = r.balance; exposureDay = r.exposureDay; openCall = if (r.openCall == 0) null else ?r.openCall; interestAccrued; lastBlock = r.lastBlock }
  };
  public func cashView(c : CashRow) : CoT.CashView { { agreement = c.agreement; currency = c.currency; received = c.received; given = c.given; interestAccrued = c.interestAccrued } };
  public func securitiesView(r : SecuritiesRow) : CoT.SecuritiesView { { agreement = r.agreement; id = r.id; lot = r.lot; isin = r.isin; depot = r.depot; nominal = r.nominal; given = r.given; state = CoT.securitiesStateText(r.state); cashReturned = r.cashReturned } };
  public func callView(c : CallRow) : CoT.CallView { { agreement = c.agreement; id = c.id; amount = c.amount; outstanding = c.outstanding; deliver = c.deliver; day = c.day; due = c.due; state = callStateText(c.state) } };
  public func status(s : State) : CoT.Status { { agreements = s.agreementCount; cashRows = s.cashRows; securitiesRows = s.securitiesRows; calls = s.callCount; openCalls = s.openCalls; exposures = s.exposures } };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.policy) { case null w.byte(0); case (?p) { w.byte(1); for (a in accountsOf(p).vals()) w.text(a) } };
    w.nat(s.agreementCount); w.nat(s.cashRows); w.nat(s.securitiesRows); w.nat(s.callCount); w.nat(s.openCalls); w.nat(s.exposures);
    for ((idx, width) in [(s.agreements, 32), (s.byCounterparty, 8), (s.haircuts, 37), (s.cash, 40), (s.securities, 8), (s.securitiesByAgreement, 40), (s.calls, 8), (s.callsByAgreement, 40)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      walk(idx, lo, hi, func(k, v) { w.blob(k); w.blob(v); n += 1 });
      w.nat(n);
    };
  };
}
