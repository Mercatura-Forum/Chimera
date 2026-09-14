/// CallCore.mo: the call-money book folded from the desk log in stable memory: one row per call with the balance,
/// the rate in force and the accrual the postings have made.
///
/// The accrual is piecewise: the row carries the day the current rate and balance became effective and the
/// interest accrued before it, so the target at any day is `accruedBefore + interest(balance, rate, from, day)` by
/// Manticore's `simpleInterestTo`, exact and rounded once. A reset or an adjustment catches the accrual up to its
/// day at the old figures first, then moves the base. Interest settled is paid or capitalised; a repayment on the
/// notice's repayment day settles the balance and the interest to the day and closes the call.
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

import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import M "mo:manticore/TreasuryMath";
import DC "mo:manticore/DayCount";
import Posting "mo:manticore/Posting";

import CT "CallTypes";

module {

  public let ROW_BYTES : Nat = 125;
  let MAX_PAGE = 512;
  public let F_PLACEMENT : Nat8 = 1;
  public let F_CAPITALISE : Nat8 = 2;
  public let F_FUNDED : Nat8 = 4;
  public let F_WITHIN : Nat8 = 8;
  func has(flags : Nat8, f : Nat8) : Bool { (flags & f) != 0 };

  public type Row = {
    id : CT.CallId; state : CT.State; flags : Nat8; book : Text; cpHash : Nat; currency : Text; balance : Nat; rateBps : Nat; dayCount : Nat8; noticeDays : Nat; interestEveryDays : Nat;
    start : Nat; accrualFrom : Nat; accruedBefore : Int; accruedPosted : Int; lastInterestDay : Nat; repayDay : Nat; interestPaid : Nat; lastBlock : Nat; refHash : Nat;
  };

  func stateCode(s : CT.State) : Nat8 { switch (s) { case (#open) 1; case (#noticed) 2; case (#closed) 3 } };
  func stateOf(c : Nat8) : CT.State { switch (c) { case 1 #open; case 2 #noticed; case _ #closed } };
  func convCode(c : DC.Convention) : Nat8 { switch (c) { case (#a001_ActActIcma(_)) 1; case (#a003_Act360) 3; case (#a004_Act365Fixed) 4; case (#a005_ActActIsda) 5; case (#a006_Thirty360Isda) 6; case (#a007_ThirtyE360) 7; case (#a011_Thirty365) 11 } };
  public func convOf(c : Nat8) : DC.Convention { switch (c) { case 1 #a001_ActActIcma({ couponsPerYear = 1 }); case 3 #a003_Act360; case 4 #a004_Act365Fixed; case 5 #a005_ActActIsda; case 6 #a006_Thirty360Isda; case 7 #a007_ThirtyE360; case _ #a011_Thirty365 } };
  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  public func hash8(t : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(t))), 0, 8) };
  /// The sub-ledger of a call on every account it posts to.
  public func callSub(id : CT.CallId) : JT.SubledgerKey { Posting.subledgerOf("call/" # Nat.toText(id)) };

  func encode(r : Row) : Blob {
    let b = R.buf();
    R.putByte(b, stateCode(r.state)); R.putByte(b, r.flags); R.putText(b, r.book, 32); R.putNat(b, r.cpHash, 8); R.putText(b, r.currency, 8); R.putNat(b, r.balance, 8);
    R.putNat(b, r.rateBps, 4); R.putByte(b, r.dayCount); R.putNat(b, r.noticeDays, 2); R.putNat(b, r.interestEveryDays, 2); R.putNat(b, r.start, 4); R.putNat(b, r.accrualFrom, 4);
    putInt(b, r.accruedBefore); putInt(b, r.accruedPosted); R.putNat(b, r.lastInterestDay, 4); R.putNat(b, r.repayDay, 4); R.putNat(b, r.interestPaid, 8); R.putNat(b, r.lastBlock, 8); R.putNat(b, r.refHash, 8);
    R.done(b, ROW_BYTES)
  };
  func decode(id : Nat, v : Blob) : Row {
    let a = Blob.toArray(v);
    { id; state = stateOf(a[0]); flags = a[1]; book = R.getText(a, 2, 32); cpHash = R.getNat(a, 34, 8); currency = R.getText(a, 42, 8); balance = R.getNat(a, 50, 8);
      rateBps = R.getNat(a, 58, 4); dayCount = a[62]; noticeDays = R.getNat(a, 63, 2); interestEveryDays = R.getNat(a, 65, 2); start = R.getNat(a, 67, 4); accrualFrom = R.getNat(a, 71, 4);
      accruedBefore = getInt(a, 75); accruedPosted = getInt(a, 84); lastInterestDay = R.getNat(a, 93, 4); repayDay = R.getNat(a, 97, 4); interestPaid = R.getNat(a, 101, 8); lastBlock = R.getNat(a, 109, 8); refHash = R.getNat(a, 117, 8) }
  };

  public type State = {
    rows : RI.State;           // id(8) -> row
    byBook : RI.State;         // book(32) ‖ id(8)
    byState : RI.State;        // state(1) ‖ id(8)
    byCounterparty : RI.State; // cpHash(8) ‖ id(8)
    var opened : Nat;
    var open : Nat;
    var interestTotal : Int;
  };

  public func newState(arena : RI.Arena) : State {
    {
      rows = RI.newStateIn(arena, { keyBytes = 8; valBytes = ROW_BYTES });
      byBook = RI.newStateIn(arena, { keyBytes = 40; valBytes = 1 });
      byState = RI.newStateIn(arena, { keyBytes = 9; valBytes = 1 });
      byCounterparty = RI.newStateIn(arena, { keyBytes = 16; valBytes = 1 });
      var opened = 0; var open = 0; var interestTotal = 0;
    }
  };

  public func row(s : State, id : CT.CallId) : ?Row { switch (RI.get(s.rows, R.key(id, 8))) { case (?v) ?decode(id, v); case null null } };
  func putRow(s : State, r : Row) { ignore RI.put(s.rows, R.key(r.id, 8), encode(r)) };
  public func isOpen(r : Row) : Bool { r.state != #closed };
  func idsUnder(idx : RI.State, lo : Blob, hi : Blob, offset : Nat) : [Nat] {
    let out = List.empty<Nat>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, _) in page.entries.vals()) List.add(out, R.getNat(Blob.toArray(k), offset, 8));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  func textPrefixRange(t : Text, width : Nat, rest : Nat) : (Blob, Blob) {
    let p = Blob.toArray(R.textKey(t, width));
    (Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(0, rest))), Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(255, rest))))
  };
  /// Calls of a book, all states, ascending by id.
  public func callsOfBook(s : State, book : Text) : [Row] {
    let (lo, hi) = textPrefixRange(book, 32, 8);
    Array.filterMap<Nat, Row>(idsUnder(s.byBook, lo, hi, 32), func(id) { row(s, id) })
  };
  public func openInBook(s : State, book : Text) : [Row] { Array.filter<Row>(callsOfBook(s, book), isOpen) };
  public func listByCounterparty(s : State, name : Text, cursor : ?Blob, limit : Nat) : { ids : [CT.CallId]; cursor : ?Blob } {
    let (lo, hi) = R.prefixRange(hash8(name), 8, 8);
    let page = RI.range(s.byCounterparty, lo, hi, cursor, Nat.min(limit, MAX_PAGE));
    { ids = Array.map<(Blob, Blob), Nat>(page.entries, func((k, _)) { R.getNat(Blob.toArray(k), 8, 8) }); cursor = page.cursor }
  };

  // ─── the arithmetic ────────────────────────────────────────────────────────

  /// The interest a call has earned or owed to `day`: what was accrued before the current base, plus the
  /// balance at the rate in force from the base day.
  public func accrualTarget(r : Row, day : Nat) : Int {
    let to = if (r.repayDay > 0 and day > r.repayDay) r.repayDay else day;
    if (to <= r.accrualFrom) return r.accruedBefore;
    r.accruedBefore + M.simpleInterestTo(r.balance, r.rateBps, convOf(r.dayCount), r.accrualFrom, to)
  };
  /// The next interest date after the last one, or none when interest settles at repayment only.
  public func nextInterestDay(r : Row) : ?Nat { if (r.interestEveryDays == 0) null else ?(r.lastInterestDay + r.interestEveryDays) };

  // ─── planners ─────────────────────────────────────────────────────────────

  public type Act = { ev : CT.Event; legs : [JT.Leg] };
  type Res<X> = Result.Result<X, CT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidTerms({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func validateTerms(t : CT.Terms, day : Nat) : ?Text {
    if (bytesOf(t.currency) != 3) return ?"the currency is a three-letter code";
    if (t.principal == 0) return ?"the principal is positive";
    if (t.rateBps > 1_000_000) return ?"the rate exceeds 10000 %";
    if (t.noticeDays > 365) return ?"the notice is at most 365 days";
    if (t.interestEveryDays > 366) return ?"interest settles at most 366 days apart";
    if (t.start < day) return ?"the start is not in the past";
    if (bytesOf(t.cash.account) == 0) return ?"the settlement account is named";
    null
  };
  public func rowOpen(s : State, id : CT.CallId) : Res<Row> {
    switch (row(s, id)) { case null #err(#UnknownCall({ call = id })); case (?r) { if (isOpen(r)) #ok(r) else #err(#CallNotIn({ call = id; state = "closed"; wanted = "open|noticed" })) } }
  };
  public func planReset(s : State, id : CT.CallId, rateBps : Nat, day : Nat) : Res<CT.Event> {
    let r = switch (rowOpen(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    if (rateBps > 1_000_000) return bad("the rate exceeds 10000 %");
    if (not has(r.flags, F_FUNDED)) return #err(#NotFunded({ call = id }));
    if (day < r.accrualFrom) return bad("a reset is not dated before the current base");
    #ok(#rateReset({ call = id; rateBps; day; catchUp = accrualTarget(r, day) - r.accruedPosted }))
  };
  public func planAdjust(s : State, id : CT.CallId, delta : Int, day : Nat) : Res<CT.Event> {
    let r = switch (rowOpen(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    if (delta == 0) return bad("an adjustment moves a positive amount");
    if (not has(r.flags, F_FUNDED)) return #err(#NotFunded({ call = id }));
    if (r.state == #noticed) return #err(#CallNotIn({ call = id; state = "noticed"; wanted = "open" }));
    if (day < r.accrualFrom) return bad("an adjustment is not dated before the current base");
    if (delta < 0 and Int.abs(delta) > r.balance) return #err(#InsufficientBalance({ call = id; balance = r.balance; wanted = Int.abs(delta) }));
    #ok(#balanceAdjusted({ call = id; delta; day; catchUp = accrualTarget(r, day) - r.accruedPosted }))
  };
  /// A notice served on `day`: the repayment day is the notice period ahead, moved to the next business day by the
  /// calendar the caller supplies.
  public func planNotice(s : State, id : CT.CallId, day : Nat, nextBusinessDay : Nat -> Nat) : Res<CT.Event> {
    let r = switch (rowOpen(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    if (r.state == #noticed) return #err(#CallNotIn({ call = id; state = "noticed"; wanted = "open" }));
    if (not has(r.flags, F_FUNDED)) return #err(#NotFunded({ call = id }));
    let repayDay = nextBusinessDay(day + (if (r.noticeDays == 0) 1 else r.noticeDays));
    #ok(#noticeServed({ call = id; day; repayDay }))
  };

  func addLeg(ls : List.List<JT.Leg>, account : Text, sub : ?JT.SubledgerKey, side : JT.Side, ccy : Text, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, sub, side, ccy, amount)) };
  func cashSub(c : TT.CashAccount) : ?JT.SubledgerKey { TreasuryCore.cashSub(c) };
  /// The accrual's legs: a placement's receivable against income, a taking's payable against expense.
  func accrualLegs(ls : List.List<JT.Leg>, p : TT.Policy, placement : Bool, sub : ?JT.SubledgerKey, ccy : Text, delta : Int) {
    if (delta == 0) return;
    let a = Int.abs(delta);
    if (placement) {
      if (delta > 0) { addLeg(ls, p.mmInterestReceivable, sub, #debit, ccy, a); addLeg(ls, p.mmInterestIncome, null, #credit, ccy, a) }
      else { addLeg(ls, p.mmInterestIncome, null, #debit, ccy, a); addLeg(ls, p.mmInterestReceivable, sub, #credit, ccy, a) };
    } else {
      if (delta > 0) { addLeg(ls, p.mmInterestExpense, null, #debit, ccy, a); addLeg(ls, p.mmInterestPayable, sub, #credit, ccy, a) }
      else { addLeg(ls, p.mmInterestPayable, sub, #debit, ccy, a); addLeg(ls, p.mmInterestExpense, null, #credit, ccy, a) };
    };
  };
  func principalAccount(p : TT.Policy, placement : Bool) : Text { if (placement) p.mmPlacements else p.mmTakings };
  func interestAccount(p : TT.Policy, placement : Bool) : Text { if (placement) p.mmInterestReceivable else p.mmInterestPayable };

  /// The legs an event posts, given the policy and the terms' cash account; the events that move money.
  public func legsOf(p : TT.Policy, r : Row, cash : TT.CashAccount, ev : CT.Event) : [JT.Leg] {
    let ls = List.empty<JT.Leg>();
    let sub = ?callSub(r.id);
    let cs = cashSub(cash);
    let placement = has(r.flags, F_PLACEMENT);
    let ccy = r.currency;
    switch (ev) {
      case (#funded(x)) {
        if (placement) { addLeg(ls, p.mmPlacements, sub, #debit, ccy, x.amount); addLeg(ls, cash.account, cs, #credit, ccy, x.amount) }
        else { addLeg(ls, cash.account, cs, #debit, ccy, x.amount); addLeg(ls, p.mmTakings, sub, #credit, ccy, x.amount) };
      };
      case (#rateReset(x)) accrualLegs(ls, p, placement, sub, ccy, x.catchUp);
      case (#balanceAdjusted(x)) {
        accrualLegs(ls, p, placement, sub, ccy, x.catchUp);
        let a = Int.abs(x.delta);
        // an addition moves like the funding; a draw the other way
        if ((x.delta > 0) == placement) { addLeg(ls, principalAccount(p, placement), sub, #debit, ccy, a); addLeg(ls, cash.account, cs, #credit, ccy, a) }
        else { addLeg(ls, cash.account, cs, #debit, ccy, a); addLeg(ls, principalAccount(p, placement), sub, #credit, ccy, a) };
      };
      case (#accrued(x)) accrualLegs(ls, p, placement, sub, ccy, x.interest);
      case (#interestSettled(x)) {
        // the accrued interest leaves the receivable or payable: into the balance, or through cash
        let to = if (x.capitalised) principalAccount(p, placement) else cash.account;
        let toSub = if (x.capitalised) sub else cs;
        if (placement) { addLeg(ls, to, toSub, #debit, ccy, x.amount); addLeg(ls, interestAccount(p, true), sub, #credit, ccy, x.amount) }
        else { addLeg(ls, interestAccount(p, false), sub, #debit, ccy, x.amount); addLeg(ls, to, toSub, #credit, ccy, x.amount) };
      };
      case (#repaid(x)) {
        if (placement) {
          addLeg(ls, cash.account, cs, #debit, ccy, x.principal + x.interest);
          addLeg(ls, p.mmPlacements, sub, #credit, ccy, x.principal); addLeg(ls, p.mmInterestReceivable, sub, #credit, ccy, x.interest);
        } else {
          addLeg(ls, p.mmTakings, sub, #debit, ccy, x.principal); addLeg(ls, p.mmInterestPayable, sub, #debit, ccy, x.interest);
          addLeg(ls, cash.account, cs, #credit, ccy, x.principal + x.interest);
        };
      };
      case (_) {};
    };
    List.toArray(ls)
  };

  /// What falls due for a call on `day`, in order: the funding on the start day, the accrual to the day, the
  /// interest settlement at an interest date, the repayment on the repayment day. Each is its own act; the caller
  /// posts and folds one before asking for the next.
  public func planDue(s : State, id : CT.CallId, day : Nat) : Res<?CT.Event> {
    let ?r = row(s, id) else return #err(#UnknownCall({ call = id }));
    if (not isOpen(r)) return #ok(null);
    if (not has(r.flags, F_FUNDED)) {
      if (day < r.start) return #ok(null);
      return #ok(?#funded({ call = id; amount = r.balance; day }));
    };
    let target = accrualTarget(r, day);
    if (target != r.accruedPosted) return #ok(?#accrued({ call = id; interest = target - r.accruedPosted; day }));
    if (r.state == #noticed and day >= r.repayDay) {
      return #ok(?#repaid({ call = id; principal = r.balance; interest = Int.abs(r.accruedPosted); day }));
    };
    switch (nextInterestDay(r)) {
      case (?d) { if (day >= d and r.accruedPosted > 0) return #ok(?#interestSettled({ call = id; amount = Int.abs(r.accruedPosted); capitalised = has(r.flags, F_CAPITALISE); day })) };
      case null {};
    };
    #ok(null)
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  func index(s : State, r : Row) {
    ignore RI.put(s.byBook, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.book, 32)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1]));
    ignore RI.put(s.byState, R.key2(Nat8.toNat(stateCode(r.state)), 1, r.id, 8), Blob.fromArray([1]));
    ignore RI.put(s.byCounterparty, R.key2(r.cpHash, 8, r.id, 8), Blob.fromArray([1]));
  };
  func withState(s : State, r : Row, st : CT.State, block : Nat) : Row {
    ignore RI.put(s.byState, R.key2(Nat8.toNat(stateCode(st)), 1, r.id, 8), Blob.fromArray([1]));
    if (isOpen(r) and st == #closed) s.open -= 1;
    { r with state = st; lastBlock = block }
  };

  public func fold(s : State, block : Nat, ev : CT.Event) {
    switch (ev) {
      case (#opened(x)) {
        let t = x.terms;
        let r : Row = {
          id = block; state = #open; flags = (if (t.placement) F_PLACEMENT else 0) | (if (t.capitalise) F_CAPITALISE else 0) | (if (x.withinLimits) F_WITHIN else 0); book = x.book; cpHash = hash8(x.counterparty.name);
          currency = t.currency; balance = t.principal; rateBps = t.rateBps; dayCount = convCode(t.dayCount); noticeDays = t.noticeDays; interestEveryDays = t.interestEveryDays;
          start = t.start; accrualFrom = t.start; accruedBefore = 0; accruedPosted = 0; lastInterestDay = t.start; repayDay = 0; interestPaid = 0; lastBlock = block; refHash = hash8(x.reference);
        };
        putRow(s, r); index(s, r); s.opened += 1; s.open += 1;
      };
      case (#funded(x)) { switch (row(s, x.call)) { case (?r) putRow(s, { r with flags = r.flags | F_FUNDED; lastBlock = block }); case null {} } };
      case (#rateReset(x)) {
        switch (row(s, x.call)) {
          case (?r) { let posted = r.accruedPosted + x.catchUp; putRow(s, { r with rateBps = x.rateBps; accrualFrom = x.day; accruedBefore = posted; accruedPosted = posted; lastBlock = block }) };
          case null {};
        };
      };
      case (#balanceAdjusted(x)) {
        switch (row(s, x.call)) {
          case (?r) {
            let posted = r.accruedPosted + x.catchUp;
            let balance = if (x.delta < 0) (if (Int.abs(x.delta) > r.balance) 0 else r.balance - Int.abs(x.delta)) else r.balance + Int.abs(x.delta);
            putRow(s, { r with balance; accrualFrom = x.day; accruedBefore = posted; accruedPosted = posted; lastBlock = block });
          };
          case null {};
        };
      };
      case (#noticeServed(x)) { switch (row(s, x.call)) { case (?r) putRow(s, withState(s, { r with repayDay = x.repayDay }, #noticed, block)); case null {} } };
      case (#accrued(x)) { switch (row(s, x.call)) { case (?r) { putRow(s, { r with accruedPosted = r.accruedPosted + x.interest; lastBlock = block }); s.interestTotal += x.interest }; case null {} } };
      case (#interestSettled(x)) {
        switch (row(s, x.call)) {
          case (?r) {
            let balance = if (x.capitalised) r.balance + x.amount else r.balance;
            putRow(s, { r with balance; accrualFrom = x.day; accruedBefore = 0; accruedPosted = 0; lastInterestDay = x.day; interestPaid = r.interestPaid + x.amount; lastBlock = block });
          };
          case null {};
        };
      };
      case (#repaid(x)) {
        switch (row(s, x.call)) {
          case (?r) putRow(s, withState(s, { r with balance = 0; accruedPosted = 0; accruedBefore = 0; accrualFrom = x.day; interestPaid = r.interestPaid + x.interest }, #closed, block));
          case null {};
        };
      };
    }
  };

  // ─── reads ────────────────────────────────────────────────────────────────

  public func view(r : Row, cpName : Text, reference : Text) : CT.View {
    {
      id = r.id; book = r.book; counterparty = cpName; reference; placement = has(r.flags, F_PLACEMENT); currency = r.currency; balance = r.balance; rateBps = r.rateBps; noticeDays = r.noticeDays;
      interestEveryDays = r.interestEveryDays; capitalise = has(r.flags, F_CAPITALISE); start = r.start; funded = has(r.flags, F_FUNDED); state = CT.stateText(r.state); accruedPosted = r.accruedPosted;
      interestPaid = r.interestPaid; lastInterestDay = r.lastInterestDay; repayDay = if (r.repayDay == 0) null else ?r.repayDay; lastBlock = r.lastBlock;
    }
  };
  /// The call positions of a book: open calls aggregated by placement or taking and currency, the nominal signed
  /// from the desk's side, the carrying amount the balance plus the accrual.
  public func positions(s : State, book : Text) : [TT.PositionView] {
    let acc = List.empty<TT.PositionView>();
    for (r in openInBook(s, book).vals()) {
      let placement = has(r.flags, F_PLACEMENT);
      let instrument = if (placement) "call placement" else "call taking";
      let nominal : Int = if (placement) r.balance else -(r.balance : Int);
      let carrying : Int = (r.balance : Int) + r.accruedPosted;
      var merged = false;
      let arr = List.toArray(acc);
      List.clear(acc);
      for (p in arr.vals()) {
        if (not merged and Text.equal(p.instrument, instrument) and Text.equal(p.currency, r.currency)) { List.add(acc, { p with nominal = p.nominal + nominal; carrying = p.carrying + carrying; deals = p.deals + 1 }); merged := true }
        else List.add(acc, p);
      };
      if (not merged) List.add(acc, { book; instrument; currency = r.currency; kind = "callDeposit"; nominal; carrying; mark = 0; deals = 1 });
    };
    List.toArray(acc)
  };
  /// The open balances of a counterparty in a currency across a book: what the counterparty limit counts.
  public func exposureOf(s : State, book : Text, cpName : Text, currency : Text) : Nat {
    var total = 0;
    let h = hash8(cpName);
    for (r in openInBook(s, book).vals()) { if (r.cpHash == h and Text.equal(r.currency, currency)) total += r.balance };
    total
  };
  public func status(s : State) : { opened : Nat; open : Nat; interestTotal : Int } { { opened = s.opened; open = s.open; interestTotal = s.interestTotal } };

  public func fingerprintInto(w : C.Writer, s : State) {
    w.nat(s.opened); w.nat(s.open); w.bool(s.interestTotal < 0); w.nat(Int.abs(s.interestTotal));
    for ((idx, width) in [(s.rows, 8), (s.byBook, 40), (s.byState, 9), (s.byCounterparty, 16)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var cursor : ?Blob = null;
      var n = 0;
      label walk loop {
        let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
        for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v); n += 1 };
        switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
      };
      w.nat(n);
    };
  };
}
