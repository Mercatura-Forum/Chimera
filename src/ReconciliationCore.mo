/// ReconciliationCore.mo: the reconciliation rows folded from the desk log in stable memory: the entries a
/// notification matched, the custodian's statements, the depot and cash breaks with their ageing and their
/// resolution, the cash reconciliations per currency; the matching rules, pure over what the caller reads.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Text "mo:core/Text";
import VarArray "mo:core/VarArray";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";

import RT "ReconciliationTypes";

module {

  public let STATEMENT_ROW_BYTES : Nat = 65;
  public let DEPOT_BREAK_ROW_BYTES : Nat = 157;
  public let CASH_ROW_BYTES : Nat = 79;
  public let CASH_BREAK_ROW_BYTES : Nat = 53;
  public let MAX_ENTRIES : Nat = 500;
  public let MAX_REPORTED : Nat = 2_000;
  let MAX_PAGE = 512;

  public let BREAK_OPEN : Nat8 = 1;
  public let BREAK_RESOLVED : Nat8 = 2;
  public let BREAK_CLEARED : Nat8 = 3;
  public func breakStateText(c : Nat8) : Text { switch (c) { case 1 "open"; case 2 "resolved"; case _ "cleared" } };

  public type DepotStatementRow = { statement : Blob; depot : Text; kind : RT.StatementKind; statementDate : Nat; from : Nat; to : Nat; reported : Nat; matched : Nat; explained : Nat; breaks : Nat; day : Nat };
  public type DepotBreakRow = {
    id : Nat; depot : Text; statement : Blob; kind : RT.BreakKind; side : RT.BreakSide; isin : Text; ours : Nat; theirs : Nat; reference : Text; instruction : ?Nat;
    openedDay : Nat; state : Nat8; resolvedDay : Nat; correction : ?Nat; alerted : Bool;
  };
  public type CashRow = { currency : Text; ledger : ?Principal; ledgerBalance : Nat; bookBalance : Int; difference : Int; day : Nat; tipHeight : ?Nat; openBreak : Nat; inFlight : Bool };
  public type CashBreakRow = { id : Nat; currency : Text; ledgerBalance : Nat; bookBalance : Int; difference : Int; openedDay : Nat; state : Nat8; resolvedDay : Nat; correction : ?Nat; alerted : Bool };

  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  func putOptNat(b : R.Buf, v : ?Nat) { switch (v) { case null { R.putByte(b, 0); R.putNat(b, 0, 8) }; case (?n) { R.putByte(b, 1); R.putNat(b, n, 8) } } };
  func getOptNat(a : [Nat8], off : Nat) : ?Nat { if (a[off] == 1) ?R.getNat(a, off + 1, 8) else null };
  func kindCode(k : RT.StatementKind) : Nat8 { switch (k) { case (#holdings) 1; case (#transactions) 2 } };
  func kindOf(c : Nat8) : RT.StatementKind { if (c == 1) #holdings else #transactions };
  func breakKindCode(k : RT.BreakKind) : Nat8 { switch (k) { case (#position) 1; case (#transaction) 2 } };
  func breakKindOf(c : Nat8) : RT.BreakKind { if (c == 1) #position else #transaction };
  func sideCode(s : RT.BreakSide) : Nat8 { switch (s) { case (#onStatementOnly) 1; case (#inOurBooksOnly) 2 } };
  func sideOf(c : Nat8) : RT.BreakSide { if (c == 1) #onStatementOnly else #inOurBooksOnly };
  public func hash8(t : Text) : Nat { TreasuryCore.hash8(t) };

  func encodeStatement(r : DepotStatementRow) : Blob {
    let b = R.buf();
    R.putText(b, r.depot, 32); R.putByte(b, kindCode(r.kind)); R.putNat(b, r.statementDate, 4); R.putNat(b, r.from, 4); R.putNat(b, r.to, 4); R.putNat(b, r.reported, 4); R.putNat(b, r.matched, 4); R.putNat(b, r.explained, 4); R.putNat(b, r.breaks, 4); R.putNat(b, r.day, 4);
    R.done(b, STATEMENT_ROW_BYTES)
  };
  func decodeStatement(statement : Blob, v : Blob) : DepotStatementRow {
    let a = Blob.toArray(v);
    { statement; depot = R.getText(a, 0, 32); kind = kindOf(a[32]); statementDate = R.getNat(a, 33, 4); from = R.getNat(a, 37, 4); to = R.getNat(a, 41, 4); reported = R.getNat(a, 45, 4); matched = R.getNat(a, 49, 4); explained = R.getNat(a, 53, 4); breaks = R.getNat(a, 57, 4); day = R.getNat(a, 61, 4) }
  };
  func encodeDepotBreak(r : DepotBreakRow) : Blob {
    let b = R.buf();
    R.putText(b, r.depot, 32); R.putBlob(b, r.statement, 32); R.putByte(b, breakKindCode(r.kind)); R.putByte(b, sideCode(r.side)); R.putText(b, r.isin, 12); R.putNat(b, r.ours, 8); R.putNat(b, r.theirs, 8);
    R.putText(b, r.reference, 35); putOptNat(b, r.instruction); R.putNat(b, r.openedDay, 4); R.putByte(b, r.state); R.putNat(b, r.resolvedDay, 4); putOptNat(b, r.correction); R.putBool(b, r.alerted);
    R.done(b, DEPOT_BREAK_ROW_BYTES)
  };
  func decodeDepotBreak(id : Nat, v : Blob) : DepotBreakRow {
    let a = Blob.toArray(v);
    { id; depot = R.getText(a, 0, 32); statement = R.getBlob(a, 32, 32); kind = breakKindOf(a[64]); side = sideOf(a[65]); isin = R.getText(a, 66, 12); ours = R.getNat(a, 78, 8); theirs = R.getNat(a, 86, 8);
      reference = R.getText(a, 94, 35); instruction = getOptNat(a, 129); openedDay = R.getNat(a, 138, 4); state = a[142]; resolvedDay = R.getNat(a, 143, 4); correction = getOptNat(a, 147); alerted = R.getBool(a, 156) }
  };
  func encodeCash(r : CashRow) : Blob {
    let b = R.buf();
    switch (r.ledger) { case null { R.putByte(b, 0); R.putByte(b, 0); R.putBlob(b, zeros(29), 29) }; case (?p) { let raw = Principal.toBlob(p); R.putByte(b, 1); R.putByte(b, Nat8.fromNat(raw.size())); R.putBlob(b, padded(raw, 29), 29) } };
    R.putNat(b, r.ledgerBalance, 8); putInt(b, r.bookBalance); putInt(b, r.difference); R.putNat(b, r.day, 4);
    putOptNat(b, r.tipHeight);
    R.putNat(b, r.openBreak, 8); R.putBool(b, r.inFlight);
    R.done(b, CASH_ROW_BYTES)
  };
  func decodeCash(currency : Text, v : Blob) : CashRow {
    let a = Blob.toArray(v);
    let ledger : ?Principal = if (a[0] == 1) { let n = Nat8.toNat(a[1]); ?Principal.fromBlob(Blob.fromArray(Array.sliceToArray<Nat8>(a, 2, 2 + n))) } else null;
    { currency; ledger; ledgerBalance = R.getNat(a, 31, 8); bookBalance = getInt(a, 39); difference = getInt(a, 48); day = R.getNat(a, 57, 4); tipHeight = getOptNat(a, 61); openBreak = R.getNat(a, 70, 8); inFlight = R.getBool(a, 78) }
  };
  /// A principal is stored with its length in a fixed 29-byte field padded with zeros.
  func zeros(n : Nat) : Blob { Blob.fromArray(Array.repeat<Nat8>(0, n)) };
  func padded(b : Blob, width : Nat) : Blob { let a = Blob.toArray(b); Blob.fromArray(Array.tabulate<Nat8>(width, func(i) { if (i < a.size()) a[i] else 0 })) };
  func encodeCashBreak(r : CashBreakRow) : Blob {
    let b = R.buf();
    R.putText(b, r.currency, 8); R.putNat(b, r.ledgerBalance, 8); putInt(b, r.bookBalance); putInt(b, r.difference); R.putNat(b, r.openedDay, 4); R.putByte(b, r.state); R.putNat(b, r.resolvedDay, 4); putOptNat(b, r.correction); R.putBool(b, r.alerted);
    R.done(b, CASH_BREAK_ROW_BYTES)
  };
  func decodeCashBreak(id : Nat, v : Blob) : CashBreakRow {
    let a = Blob.toArray(v);
    { id; currency = R.getText(a, 0, 8); ledgerBalance = R.getNat(a, 8, 8); bookBalance = getInt(a, 16); difference = getInt(a, 25); openedDay = R.getNat(a, 34, 4); state = a[38]; resolvedDay = R.getNat(a, 39, 4); correction = getOptNat(a, 43); alerted = R.getBool(a, 52) }
  };

  public let NOTIFICATION_ROW_BYTES : Nat = 48;
  public type NotificationRow = { notification : Blob; nostro : Text; day : Nat; entries : Nat; matched : Nat; unmatched : Nat };
  public type State = {
    notifications : RI.State;   // sha256(32) -> nostro(32) ‖ day(4) ‖ entries(4) ‖ matched(4) ‖ unmatched(4)
    notified : RI.State;        // entryKey(32) -> posting(8): the entries a notification matched
    statements : RI.State;      // sha256(32) -> row
    depotBreaks : RI.State;     // id(8) -> row
    cash : RI.State;            // currency(8) -> row
    cashBreaks : RI.State;      // id(8) -> row
    var policy : ?RT.Policy;
    var notificationCount : Nat;
    var depotStatements : Nat;
    var depotBreakCount : Nat;
    var depotBreaksOpen : Nat;
    var explainedFails : Nat;
    var cashReconciliations : Nat;
    var cashBreakCount : Nat;
    var cashBreaksOpen : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      notifications = RI.newStateIn(arena, { keyBytes = 32; valBytes = NOTIFICATION_ROW_BYTES });
      notified = RI.newStateIn(arena, { keyBytes = 32; valBytes = 8 });
      statements = RI.newStateIn(arena, { keyBytes = 32; valBytes = STATEMENT_ROW_BYTES });
      depotBreaks = RI.newStateIn(arena, { keyBytes = 8; valBytes = DEPOT_BREAK_ROW_BYTES });
      cash = RI.newStateIn(arena, { keyBytes = 8; valBytes = CASH_ROW_BYTES });
      cashBreaks = RI.newStateIn(arena, { keyBytes = 8; valBytes = CASH_BREAK_ROW_BYTES });
      var policy = null; var notificationCount = 0; var depotStatements = 0; var depotBreakCount = 0; var depotBreaksOpen = 0; var explainedFails = 0; var cashReconciliations = 0; var cashBreakCount = 0; var cashBreaksOpen = 0;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func policy(s : State) : ?RT.Policy { s.policy };
  /// The key of a statement entry: the nostro, the reference, the amount, the side and the value day, hashed.
  public func entryKey(nostro : Text, e : TT.StatementEntry) : Blob {
    let w = C.Writer();
    w.text(nostro); w.text(e.reference); w.nat(e.amount); w.bool(e.credit); w.nat(e.valueDay);
    Sha256.fromBlob(#sha256, w.toBlob())
  };
  public func notification(s : State, hash : Blob) : ?NotificationRow {
    if (hash.size() != 32) return null;
    switch (RI.get(s.notifications, hash)) { case (?v) { let a = Blob.toArray(v); ?{ notification = hash; nostro = R.getText(a, 0, 32); day = R.getNat(a, 32, 4); entries = R.getNat(a, 36, 4); matched = R.getNat(a, 40, 4); unmatched = R.getNat(a, 44, 4) } }; case null null }
  };
  public func notifiedPosting(s : State, nostro : Text, e : TT.StatementEntry) : ?Nat { switch (RI.get(s.notified, entryKey(nostro, e))) { case (?v) ?R.getNat(Blob.toArray(v), 0, 8); case null null } };
  public func statement(s : State, hash : Blob) : ?DepotStatementRow { if (hash.size() != 32) return null; switch (RI.get(s.statements, hash)) { case (?v) ?decodeStatement(hash, v); case null null } };
  public func depotBreak(s : State, id : Nat) : ?DepotBreakRow { switch (RI.get(s.depotBreaks, R.key(id, 8))) { case (?v) ?decodeDepotBreak(id, v); case null null } };
  func putDepotBreak(s : State, r : DepotBreakRow) { ignore RI.put(s.depotBreaks, R.key(r.id, 8), encodeDepotBreak(r)) };
  public func cashRow(s : State, currency : Text) : ?CashRow { switch (RI.get(s.cash, R.textKey(currency, 8))) { case (?v) ?decodeCash(currency, v); case null null } };
  func putCash(s : State, r : CashRow) { ignore RI.put(s.cash, R.textKey(r.currency, 8), encodeCash(r)) };
  public func cashBreak(s : State, id : Nat) : ?CashBreakRow { switch (RI.get(s.cashBreaks, R.key(id, 8))) { case (?v) ?decodeCashBreak(id, v); case null null } };
  func putCashBreak(s : State, r : CashBreakRow) { ignore RI.put(s.cashBreaks, R.key(r.id, 8), encodeCashBreak(r)) };
  func walk(idx : RI.State, width : Nat, visit : (Blob, Blob) -> ()) {
    let (lo, hi) = R.fullRange(width);
    var cursor : ?Blob = null;
    label go loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) visit(k, v);
      switch (page.cursor) { case null break go; case (?c) cursor := ?c };
    };
  };
  public func depotBreaks(s : State) : [DepotBreakRow] { let out = List.empty<DepotBreakRow>(); walk(s.depotBreaks, 8, func(k, v) { List.add(out, decodeDepotBreak(R.getNat(Blob.toArray(k), 0, 8), v)) }); List.toArray(out) };
  public func openDepotBreaks(s : State) : [DepotBreakRow] { Array.filter<DepotBreakRow>(depotBreaks(s), func(r) { r.state == BREAK_OPEN }) };
  public func statements(s : State) : [DepotStatementRow] { let out = List.empty<DepotStatementRow>(); walk(s.statements, 32, func(k, v) { List.add(out, decodeStatement(k, v)) }); List.toArray(out) };
  public func cashRows(s : State) : [CashRow] { let out = List.empty<CashRow>(); walk(s.cash, 8, func(k, v) { List.add(out, decodeCash(R.getText(Blob.toArray(k), 0, 8), v)) }); List.toArray(out) };
  public func cashBreaks(s : State) : [CashBreakRow] { let out = List.empty<CashBreakRow>(); walk(s.cashBreaks, 8, func(k, v) { List.add(out, decodeCashBreak(R.getNat(Blob.toArray(k), 0, 8), v)) }); List.toArray(out) };
  public func openCashBreaks(s : State) : [CashBreakRow] { Array.filter<CashBreakRow>(cashBreaks(s), func(r) { r.state == BREAK_OPEN }) };
  public func cashAccountOf(p : RT.Policy, currency : Text) : ?TT.CashAccount { for (c in p.cashAccounts.vals()) { if (Text.equal(c.currency, currency)) return ?c.cash }; null };

  // ─── the matching rules ────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, RT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#BadDocument({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planPolicy(p : RT.Policy) : Res<RT.Event> {
    if (p.cashAccounts.size() == 0) return #err(#InvalidPolicy({ reason = "at least one settlement cash account is named" }));
    for (c in p.cashAccounts.vals()) {
      if (bytesOf(c.currency) != 3) return #err(#InvalidPolicy({ reason = "a currency code has three letters" }));
      if (bytesOf(c.cash.account) == 0 or bytesOf(c.cash.account) > 32) return #err(#InvalidPolicy({ reason = "a cash account is named in 1..32 bytes" }));
    };
    if (p.breakAgeAlertDays == 0 or p.breakAgeAlertDays > 90) return #err(#InvalidPolicy({ reason = "the age alert is 1..90 days" }));
    #ok(#policySet(p))
  };

  /// A notification's entries against the nostro's open legs by the statement's rule: by reference first, then
  /// by amount and value day within the tolerance, each leg taken once. The unmatched entries are not breaks:
  /// the statement of the day decides them.
  public func matchNotification(entries : [TT.StatementEntry], legs : [TreasuryCore.NostroLegRow], tolerance : Nat) : { matches : [(Nat, Nat)]; unmatched : [Nat] } {
    let taken = VarArray.repeat<Bool>(false, legs.size());
    let matches = List.empty<(Nat, Nat)>();
    let unmatched = List.empty<Nat>();
    func within(a : Nat, b : Nat) : Bool { (if (a > b) a - b else b - a) <= tolerance };
    var e = 0;
    while (e < entries.size()) {
      let x = entries[e];
      let rh = hash8(x.reference);
      var found : ?Nat = null;
      var i = 0;
      while (i < legs.size() and found == null) {
        let l = legs[i];
        if (not taken[i] and l.status == TreasuryCore.LEG_OPEN and l.debit == x.credit and l.amount == x.amount and within(l.valueDay, x.valueDay) and l.refHash == rh and bytesOf(x.reference) > 0) found := ?i;
        i += 1;
      };
      i := 0;
      while (i < legs.size() and found == null) {
        let l = legs[i];
        if (not taken[i] and l.status == TreasuryCore.LEG_OPEN and l.debit == x.credit and l.amount == x.amount and within(l.valueDay, x.valueDay)) found := ?i;
        i += 1;
      };
      switch (found) { case (?j) { taken[j] := true; List.add(matches, (e, legs[j].posting)) }; case null List.add(unmatched, e) };
      e += 1;
    };
    { matches = List.toArray(matches); unmatched = List.toArray(unmatched) }
  };

  /// The custodian's holdings against the depot's positions: every difference a break, on the statement's side
  /// when the custodian holds more, on the desk's when it holds less or the custodian reports nothing.
  public func matchHoldings(reported : [RT.ReportedHolding], ours : [(Text, Nat)]) : { matched : Nat; breaks : [(Text, Nat, Nat, RT.BreakSide)] } {
    let out = List.empty<(Text, Nat, Nat, RT.BreakSide)>();
    var matched = 0;
    let seen = List.empty<Text>();
    for (h in reported.vals()) {
      var mine = 0;
      for ((isin, n) in ours.vals()) { if (Text.equal(isin, h.isin)) mine += n };
      List.add(seen, h.isin);
      if (mine == h.nominal) matched += 1
      else List.add(out, (h.isin, mine, h.nominal, if (h.nominal > mine) #onStatementOnly else #inOurBooksOnly));
    };
    for ((isin, n) in ours.vals()) {
      var reportedIt = false;
      for (x in List.values(seen)) { if (Text.equal(x, isin)) reportedIt := true };
      if (not reportedIt and n > 0) List.add(out, (isin, n, 0, #inOurBooksOnly));
    };
    { matched; breaks = List.toArray(out) }
  };

  /// What the desk knows of an instruction the custodian may have posted: its reference hash, the instrument,
  /// the quantity, the direction, whether it settled, and the day.
  public type DeskInstruction = { id : Nat; referenceHash : Nat; isin : Text; nominal : Nat; delivered : Bool; settled : Bool; failed : Bool; day : Nat };
  /// The custodian's postings against the desk's instructions of the window, by reference: a posting whose
  /// instruction settled with the same instrument, quantity and direction is matched; one the desk has not
  /// settled, or does not know, is a break on the statement's side; a settled instruction the custodian did
  /// not post is a break on the desk's side; an instruction that failed and was not posted is explained by the
  /// fail.
  public func matchTransactions(reported : [RT.ReportedTransaction], ours : [DeskInstruction]) : { matched : [(Nat, Nat)]; explained : [Nat]; breaks : [(RT.BreakSide, Text, Nat, Nat, Text, ?Nat)] } {
    let taken = VarArray.repeat<Bool>(false, ours.size());
    let matched = List.empty<(Nat, Nat)>();
    let breaks = List.empty<(RT.BreakSide, Text, Nat, Nat, Text, ?Nat)>();
    var t = 0;
    while (t < reported.size()) {
      let x = reported[t];
      let rh = hash8(x.reference);
      var found : ?Nat = null;
      var i = 0;
      while (i < ours.size() and found == null) {
        let o = ours[i];
        if (not taken[i] and o.referenceHash == rh and o.settled and Text.equal(o.isin, x.isin) and o.nominal == x.nominal and o.delivered == x.delivered) found := ?i;
        i += 1;
      };
      switch (found) {
        case (?i) { taken[i] := true; List.add(matched, (t, ours[i].id)) };
        case null {
          // the instruction, if the desk knows it, is named on the break
          var known : ?Nat = null;
          for (o in ours.vals()) { if (o.referenceHash == rh) known := ?o.id };
          List.add(breaks, (#onStatementOnly, x.isin, 0, x.nominal, x.reference, known));
        };
      };
      t += 1;
    };
    let explained = List.empty<Nat>();
    var i = 0;
    while (i < ours.size()) {
      let o = ours[i];
      if (not taken[i]) {
        if (o.settled) List.add(breaks, (#inOurBooksOnly, o.isin, o.nominal, 0, "", ?o.id))
        else if (o.failed) List.add(explained, o.id);
      };
      i += 1;
    };
    { matched = List.toArray(matched); explained = List.toArray(explained); breaks = List.toArray(breaks) }
  };

  public func planResolveDepotBreak(s : State, id : Nat, resolution : Text, correction : ?Nat, day : Nat) : Res<RT.Event> {
    let ?b = depotBreak(s, id) else return #err(#UnknownBreak({ break_ = id }));
    if (b.state != BREAK_OPEN) return #err(#BreakNotOpen({ break_ = id; state = breakStateText(b.state) }));
    if (bytesOf(resolution) == 0 or bytesOf(resolution) > 256) return bad("a resolution is stated in 1..256 bytes");
    #ok(#depotBreakResolved({ break_ = id; resolution; correction; day }))
  };
  public func planResolveCashBreak(s : State, id : Nat, resolution : Text, correction : ?Nat, day : Nat) : Res<RT.Event> {
    let ?b = cashBreak(s, id) else return #err(#UnknownBreak({ break_ = id }));
    if (b.state != BREAK_OPEN) return #err(#BreakNotOpen({ break_ = id; state = breakStateText(b.state) }));
    if (bytesOf(resolution) == 0 or bytesOf(resolution) > 256) return bad("a resolution is stated in 1..256 bytes");
    #ok(#cashBreakResolved({ break_ = id; resolution; correction; day }))
  };
  /// The open breaks past the policy's age, each once.
  public func agedBreaks(s : State, day : Nat, threshold : Nat) : [RT.Event] {
    let out = List.empty<RT.Event>();
    for (b in openDepotBreaks(s).vals()) { if (not b.alerted and day >= b.openedDay + threshold) List.add(out, #depotBreakAged({ break_ = b.id; ageDays = day - b.openedDay; day })) };
    List.toArray(out)
  };
  public func agedCashBreaks(s : State, day : Nat, threshold : Nat) : [CashBreakRow] {
    Array.filter<CashBreakRow>(openCashBreaks(s), func(b) { not b.alerted and day >= b.openedDay + threshold })
  };

  // ─── the fold ─────────────────────────────────────────────────────────────

  public func fold(s : State, block : Nat, ev : RT.Event) {
    switch (ev) {
      case (#policySet(p)) s.policy := ?p;
      case (#notificationRecorded(x)) {
        if (RI.get(s.notifications, x.notification) == null) s.notificationCount += 1;
        let b = R.buf(); R.putText(b, x.nostro, 32); R.putNat(b, x.day, 4); R.putNat(b, x.entries, 4); R.putNat(b, x.matched.size(), 4); R.putNat(b, x.unmatched, 4);
        ignore RI.put(s.notifications, x.notification, R.done(b, NOTIFICATION_ROW_BYTES));
        noteMatched(s, x.nostro, x.matched);
      };
      case (#statementEntriesNotified(_)) {};
      case (#depotStatementRecorded(x)) {
        if (RI.get(s.statements, x.statement) == null) s.depotStatements += 1;
        ignore RI.put(s.statements, x.statement, encodeStatement({ statement = x.statement; depot = x.depot; kind = x.kind; statementDate = x.statementDate; from = x.from; to = x.to; reported = x.reported; matched = x.matched; explained = x.explained; breaks = x.breaks; day = x.day }));
      };
      case (#depotBreak(x)) {
        putDepotBreak(s, { id = block; depot = x.depot; statement = x.statement; kind = x.kind; side = x.side; isin = x.isin; ours = x.ours; theirs = x.theirs; reference = x.reference; instruction = x.instruction; openedDay = x.day; state = BREAK_OPEN; resolvedDay = 0; correction = null; alerted = false });
        s.depotBreakCount += 1; s.depotBreaksOpen += 1;
      };
      case (#breakExplainedByFail(_)) s.explainedFails += 1;
      case (#depotBreakAged(x)) { switch (depotBreak(s, x.break_)) { case (?b) putDepotBreak(s, { b with alerted = true }); case null {} } };
      case (#depotBreakResolved(x)) { switch (depotBreak(s, x.break_)) { case (?b) { if (b.state == BREAK_OPEN) s.depotBreaksOpen -= 1; putDepotBreak(s, { b with state = BREAK_RESOLVED; resolvedDay = x.day; correction = x.correction }) }; case null {} } };
      case (#cashReconciliationIntended(x)) {
        let r : CashRow = switch (cashRow(s, x.currency)) { case (?r) r; case null { { currency = x.currency; ledger = null; ledgerBalance = 0; bookBalance = 0; difference = 0; day = 0; tipHeight = null; openBreak = 0; inFlight = false } } };
        putCash(s, { r with ledger = ?x.ledger; inFlight = true });
      };
      case (#cashReconciled(x)) {
        let r : CashRow = switch (cashRow(s, x.currency)) { case (?r) r; case null { { currency = x.currency; ledger = null; ledgerBalance = 0; bookBalance = 0; difference = 0; day = 0; tipHeight = null; openBreak = 0; inFlight = false } } };
        putCash(s, { r with ledger = ?x.ledger; ledgerBalance = x.ledgerBalance; bookBalance = x.bookBalance; difference = x.difference; day = x.day; tipHeight = x.tipHeight; inFlight = false });
        s.cashReconciliations += 1;
      };
      case (#cashReconciliationFailed(x)) { switch (cashRow(s, x.currency)) { case (?r) putCash(s, { r with inFlight = false }); case null {} } };
      case (#cashBreak(x)) {
        putCashBreak(s, { id = block; currency = x.currency; ledgerBalance = x.ledgerBalance; bookBalance = x.bookBalance; difference = x.difference; openedDay = x.day; state = BREAK_OPEN; resolvedDay = 0; correction = null; alerted = false });
        s.cashBreakCount += 1; s.cashBreaksOpen += 1;
        switch (cashRow(s, x.currency)) { case (?r) putCash(s, { r with openBreak = block }); case null {} };
      };
      case (#cashBreakAged(x)) { switch (cashBreak(s, x.break_)) { case (?b) putCashBreak(s, { b with alerted = true }); case null {} } };
      case (#cashBreakCleared(x)) { closeCashBreak(s, x.break_, BREAK_CLEARED, x.day, null) };
      case (#cashBreakResolved(x)) { closeCashBreak(s, x.break_, BREAK_RESOLVED, x.day, x.correction) };
    }
  };
  func closeCashBreak(s : State, id : Nat, state : Nat8, day : Nat, correction : ?Nat) {
    switch (cashBreak(s, id)) {
      case (?b) {
        if (b.state == BREAK_OPEN) s.cashBreaksOpen -= 1;
        putCashBreak(s, { b with state; resolvedDay = day; correction });
        switch (cashRow(s, b.currency)) { case (?r) { if (r.openBreak == id) putCash(s, { r with openBreak = 0 }) }; case null {} };
      };
      case null {};
    };
  };
  /// The entries a notification matched, keyed for the statement's pre-filter; folded beside the treasury event
  /// that marks the legs, by the caller that has both.
  func noteMatched(s : State, nostro : Text, matched : [(TT.StatementEntry, Nat)]) {
    for ((e, posting) in matched.vals()) ignore RI.put(s.notified, entryKey(nostro, e), R.key(posting, 8));
  };

  public func depotBreakView(b : DepotBreakRow, today : Nat) : RT.DepotBreakView {
    { id = b.id; depot = b.depot; kind = RT.breakKindText(b.kind); side = RT.breakSideText(b.side); isin = b.isin; ours = b.ours; theirs = b.theirs; reference = b.reference; instruction = b.instruction; statement = b.statement;
      openedDay = b.openedDay; ageDays = if (today > b.openedDay) today - b.openedDay else 0; state = breakStateText(b.state); resolvedDay = if (b.state == BREAK_OPEN) null else ?b.resolvedDay; correction = b.correction }
  };
  public func cashBreakView(b : CashBreakRow, today : Nat) : RT.CashBreakView {
    { id = b.id; currency = b.currency; ledgerBalance = b.ledgerBalance; bookBalance = b.bookBalance; difference = b.difference; openedDay = b.openedDay; ageDays = if (today > b.openedDay) today - b.openedDay else 0; state = breakStateText(b.state); resolvedDay = if (b.state == BREAK_OPEN) null else ?b.resolvedDay }
  };
  public func cashView(r : CashRow) : RT.CashView { { currency = r.currency; ledger = r.ledger; ledgerBalance = r.ledgerBalance; bookBalance = r.bookBalance; difference = r.difference; day = r.day; tipHeight = r.tipHeight; openBreak = if (r.openBreak == 0) null else ?r.openBreak } };
  public func status(s : State) : RT.Status {
    { notifications = s.notificationCount; depotStatements = s.depotStatements; depotBreaks = s.depotBreakCount; depotBreaksOpen = s.depotBreaksOpen; explainedFails = s.explainedFails; cashReconciliations = s.cashReconciliations; cashBreaks = s.cashBreakCount; cashBreaksOpen = s.cashBreaksOpen }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.policy) { case null w.byte(0); case (?p) { w.byte(1); for (c in p.cashAccounts.vals()) { w.text(c.currency); w.text(c.cash.account); switch (c.cash.sub) { case null w.byte(0); case (?t) { w.byte(1); w.text(t) } } }; w.nat(p.breakAgeAlertDays) } };
    w.nat(s.notificationCount); w.nat(s.depotStatements); w.nat(s.depotBreakCount); w.nat(s.depotBreaksOpen); w.nat(s.explainedFails); w.nat(s.cashReconciliations); w.nat(s.cashBreakCount); w.nat(s.cashBreaksOpen);
    for ((idx, width) in [(s.notifications, 32), (s.notified, 32), (s.statements, 32), (s.depotBreaks, 8), (s.cash, 8), (s.cashBreaks, 8)].vals()) {
      var n = 0;
      walk(idx, width, func(k, v) { w.blob(k); w.blob(v); n += 1 });
      w.nat(n);
    };
  };
}
