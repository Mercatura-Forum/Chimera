/// SettlementCore.mo: the settlement book folded from the desk log in stable memory: the venue and its ledgers,
/// the cycles, the instructions and their states, the mirror of Tachyon's audit log; and every decision the driver
/// takes, as pure functions over the rows and Tachyon's replies.
///
/// The driver in the actor makes the calls; this module says which call comes next for an instruction, whether a
/// trade read back from Tachyon is the instruction's trade, and whether a settlement event sits in the mirrored
/// audit log under the root Tachyon published. The mirror is Tachyon's own Merkle mountain range module, fed with
/// the leaves Tachyon enumerates, so the root the desk computes is byte for byte the root Tachyon computes.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Text "mo:core/Text";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import MMR "mo:tachyon/MerkleMMR";
import DT "mo:tachyon/DvpTypes";

import ST "SettlementTypes";

module {

  public let INSTRUCTION_ROW_BYTES : Nat = 182;
  public let CYCLE_ROW_BYTES : Nat = 74;
  public let LEDGER_ROW_BYTES : Nat = 31;
  let MAX_PAGE = 512;

  public type InstructionRow = {
    id : ST.InstructionId; family : ST.Family; deal : Nat; leg : Nat; cycle : Nat; role : ST.Role; counterparty : Principal;
    assetLedger : Principal; assetAmount : Nat; cashLedger : Principal; cashAmount : Nat; tradeId : Nat;   // 0: none
    state : ST.InstructionState; fails : Nat; escrowed : Bool; referenceHash : Nat; documentHash : Blob; lastBlock : Nat; matched : Bool;
  };
  public type CycleRow = { businessDate : Nat; market : Text; priceSource : Text; state : ST.CycleState; settled : Nat; failed : Nat; pending : Nat; instructions : Nat; lastBlock : Nat };

  // ─── rows ──────────────────────────────────────────────────────────────────

  func putPrincipal(b : R.Buf, p : Principal) { let bytes = Principal.toBlob(p); R.putNat(b, bytes.size(), 1); R.putText(b, "", 0); b.addBlob(bytes); var i = bytes.size(); while (i < 29) { R.putByte(b, 0); i += 1 } };
  func getPrincipal(a : [Nat8], off : Nat) : Principal { let n = R.getNat(a, off, 1); Principal.fromBlob(Blob.fromArray(Array.tabulate<Nat8>(n, func(i) { a[off + 1 + i] }))) };
  func stateCode(s : ST.InstructionState) : Nat8 { switch (s) { case (#instructed) 1; case (#opened) 2; case (#verified) 3; case (#funded) 4; case (#settled) 5; case (#failed) 6; case (#boughtIn) 7; case (#cancelled) 8; case (#matched) 9 } };
  func stateOf(b : Nat8) : ST.InstructionState { switch (b) { case 1 #instructed; case 2 #opened; case 3 #verified; case 4 #funded; case 5 #settled; case 6 #failed; case 7 #boughtIn; case 9 #matched; case _ #cancelled } };
  public func familyCode(f : ST.Family) : Nat8 { switch (f) { case (#treasury) 0; case (#repo) 1; case (#loan) 2; case (#collateral) 3 } };
  public func familyOf(c : Nat8) : ST.Family { switch (c) { case 1 #repo; case 2 #loan; case 3 #collateral; case _ #treasury } };
  public func hash8(t : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(t))), 0, 8) };

  func encodeInstruction(r : InstructionRow) : Blob {
    let b = R.buf();
    R.putNat(b, r.deal, 8); R.putNat(b, r.leg, 1); R.putNat(b, r.cycle, 4); R.putByte(b, switch (r.role) { case (#maker) 1; case (#taker) 2 });
    putPrincipal(b, r.counterparty); putPrincipal(b, r.assetLedger); R.putNat(b, r.assetAmount, 8); putPrincipal(b, r.cashLedger); R.putNat(b, r.cashAmount, 8);
    R.putNat(b, r.tradeId, 8); R.putByte(b, stateCode(r.state)); R.putNat(b, r.fails, 2); R.putBool(b, r.escrowed); R.putNat(b, r.referenceHash, 8); R.putBlob(b, r.documentHash, 32); R.putNat(b, r.lastBlock, 8);
    R.putByte(b, familyCode(r.family)); R.putBool(b, r.matched);
    R.done(b, INSTRUCTION_ROW_BYTES)
  };
  func decodeInstruction(id : Nat, v : Blob) : InstructionRow {
    let a = Blob.toArray(v);
    { id; family = familyOf(a[180]); deal = R.getNat(a, 0, 8); leg = R.getNat(a, 8, 1); cycle = R.getNat(a, 9, 4); role = if (a[13] == 1) #maker else #taker;
      counterparty = getPrincipal(a, 14); assetLedger = getPrincipal(a, 44); assetAmount = R.getNat(a, 74, 8); cashLedger = getPrincipal(a, 82); cashAmount = R.getNat(a, 112, 8);
      tradeId = R.getNat(a, 120, 8); state = stateOf(a[128]); fails = R.getNat(a, 129, 2); escrowed = R.getBool(a, 131); referenceHash = R.getNat(a, 132, 8); documentHash = R.getBlob(a, 140, 32); lastBlock = R.getNat(a, 172, 8); matched = R.getBool(a, 181) }
  };
  func encodeCycle(r : CycleRow) : Blob {
    let b = R.buf();
    R.putText(b, r.market, 16); R.putText(b, r.priceSource, 32); R.putByte(b, switch (r.state) { case (#open) 1; case (#closed) 2 });
    R.putNat(b, r.settled, 4); R.putNat(b, r.failed, 4); R.putNat(b, r.pending, 4); R.putNat(b, r.instructions, 4); R.putNat(b, r.lastBlock, 8); R.putNat(b, 0, 1);
    R.done(b, CYCLE_ROW_BYTES)
  };
  func decodeCycle(businessDate : Nat, v : Blob) : CycleRow {
    let a = Blob.toArray(v);
    { businessDate; market = R.getText(a, 0, 16); priceSource = R.getText(a, 16, 32); state = if (a[48] == 1) #open else #closed; settled = R.getNat(a, 49, 4); failed = R.getNat(a, 53, 4); pending = R.getNat(a, 57, 4); instructions = R.getNat(a, 61, 4); lastBlock = R.getNat(a, 65, 8) }
  };
  func ledgerKey(role : ST.LedgerRole) : Blob {
    let b = R.buf();
    switch (role) { case (#cash(x)) { R.putByte(b, 1); R.putText(b, x.currency, 12) }; case (#security(x)) { R.putByte(b, 2); R.putText(b, x.isin, 12) } };
    R.done(b, 13)
  };

  public type State = {
    instructions : RI.State;   // id(8) -> row
    byDeal : RI.State;         // deal(8) ‖ leg(1) -> id(8)
    byCycle : RI.State;        // cycle(4) ‖ id(8) -> 1
    byState : RI.State;        // state(1) ‖ id(8) -> 1
    cycles : RI.State;         // businessDate(4) -> row
    ledgers : RI.State;        // kind(1) ‖ text(12) -> principal(30) ‖ partial(1)
    mirror : MMR.State;        // Tachyon's audit log, leaf by leaf
    var venue : ?ST.Venue;
    var instructionCount : Nat;
    var cycleCount : Nat;
    var ledgerCount : Nat;
    var settledCount : Nat;
    var failedCount : Nat;
    var mirrorRoot : ?Blob;
  };

  public func newState(arena : RI.Arena) : State {
    {
      instructions = RI.newStateIn(arena, { keyBytes = 8; valBytes = INSTRUCTION_ROW_BYTES });
      byDeal = RI.newStateIn(arena, { keyBytes = 9; valBytes = 8 });
      byCycle = RI.newStateIn(arena, { keyBytes = 12; valBytes = 1 });
      byState = RI.newStateIn(arena, { keyBytes = 9; valBytes = 1 });
      cycles = RI.newStateIn(arena, { keyBytes = 4; valBytes = CYCLE_ROW_BYTES });
      ledgers = RI.newStateIn(arena, { keyBytes = 13; valBytes = LEDGER_ROW_BYTES });
      mirror = MMR.newState();
      var venue = null; var instructionCount = 0; var cycleCount = 0; var ledgerCount = 0; var settledCount = 0; var failedCount = 0; var mirrorRoot = null;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func venue(s : State) : ?ST.Venue { s.venue };
  public func instruction(s : State, id : Nat) : ?InstructionRow { switch (RI.get(s.instructions, R.key(id, 8))) { case (?v) ?decodeInstruction(id, v); case null null } };
  public func cycle(s : State, day : Nat) : ?CycleRow { switch (RI.get(s.cycles, R.key(day, 4))) { case (?v) ?decodeCycle(day, v); case null null } };
  public func ledger(s : State, role : ST.LedgerRole) : ?ST.LedgerDeclaration {
    switch (RI.get(s.ledgers, ledgerKey(role))) { case (?v) { let a = Blob.toArray(v); ?{ role; ledger = getPrincipal(a, 0); partial = R.getBool(a, 30) } }; case null null }
  };
  public func instructionOfDeal(s : State, deal : TT.DealId, leg : Nat) : ?InstructionRow {
    switch (RI.get(s.byDeal, R.key2(deal, 8, leg, 1))) { case (?v) instruction(s, R.getNat(Blob.toArray(v), 0, 8)); case null null }
  };
  /// Whether a deal's leg is in Tachyon's hands: an instruction that is neither settled nor closed by a decision.
  public func openInstructionOf(s : State, deal : TT.DealId, leg : Nat) : ?InstructionRow {
    switch (instructionOfDeal(s, deal, leg)) { case (?r) { switch (r.state) { case (#settled or #boughtIn or #cancelled) null; case (_) ?r } }; case null null }
  };
  func putInstruction(s : State, r : InstructionRow) { ignore RI.put(s.instructions, R.key(r.id, 8), encodeInstruction(r)) };
  func putCycle(s : State, r : CycleRow) { ignore RI.put(s.cycles, R.key(r.businessDate, 4), encodeCycle(r)) };
  func walk(idx : RI.State, lo : Blob, hi : Blob, f : (Blob, Blob) -> ()) {
    var cursor : ?Blob = null;
    label w loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) f(k, v);
      switch (page.cursor) { case null break w; case (?c) cursor := ?c };
    };
  };
  public func instructionsOfCycle(s : State, day : Nat) : [InstructionRow] {
    let (lo, hi) = R.prefixRange(day, 4, 8);
    let out = List.empty<InstructionRow>();
    walk(s.byCycle, lo, hi, func(k, _) { switch (instruction(s, R.getNat(Blob.toArray(k), 4, 8))) { case (?r) { if (r.cycle == day) List.add(out, r) }; case null {} } });
    List.toArray(out)
  };
  public func instructionsInState(s : State, st : ST.InstructionState) : [InstructionRow] {
    let (lo, hi) = R.prefixRange(Nat8.toNat(stateCode(st)), 1, 8);
    let out = List.empty<InstructionRow>();
    walk(s.byState, lo, hi, func(k, _) { switch (instruction(s, R.getNat(Blob.toArray(k), 1, 8))) { case (?r) { if (r.state == st) List.add(out, r) }; case null {} } });
    List.toArray(out)
  };
  public func ledgers(s : State) : [ST.LedgerDeclaration] {
    let out = List.empty<ST.LedgerDeclaration>();
    walk(s.ledgers, Blob.fromArray(Array.repeat<Nat8>(0, 13)), Blob.fromArray(Array.repeat<Nat8>(255, 13)), func(k, v) {
      let ka = Blob.toArray(k); let a = Blob.toArray(v);
      let role : ST.LedgerRole = if (ka[0] == 1) #cash({ currency = R.getText(ka, 1, 12) }) else #security({ isin = R.getText(ka, 1, 12) });
      List.add(out, { role; ledger = getPrincipal(a, 0); partial = R.getBool(a, 30) });
    });
    List.toArray(out)
  };

  // ─── planners ─────────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, ST.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidTerms({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planVenue(v : ST.Venue) : Res<ST.Event> {
    if (v.deadlineSecs == 0 or v.deadlineSecs > 7 * 86_400) return bad("the funding deadline is 1 second to 7 days");
    if (v.recycleLimit > 30) return bad("an instruction is recycled at most 30 times");
    if (bytesOf(v.claimsAccount) == 0) return bad("the claims account is named");
    #ok(#venueSet(v))
  };
  public func planLedger(d : ST.LedgerDeclaration) : Res<ST.Event> {
    switch (d.role) {
      case (#cash(x)) { if (bytesOf(x.currency) != 3) return bad("a currency code has three letters"); if (d.partial) return bad("partial delivery is a security ledger's policy") };
      case (#security(x)) { if (bytesOf(x.isin) != 12) return bad("an ISIN has twelve characters") };
    };
    #ok(#ledgerSet(d))
  };
  public func planOpenCycle(s : State, c : ST.Cycle, day : Nat) : Res<ST.Event> {
    if (s.venue == null) return #err(#NoVenue);
    if (cycle(s, c.businessDate) != null) return bad("cycle " # Nat.toText(c.businessDate) # " is already open");
    if (c.businessDate < day) return bad("a cycle opens on or after the day");
    if (bytesOf(c.market) == 0 or bytesOf(c.market) > 16) return bad("the market is named in 1..16 bytes");
    if (bytesOf(c.priceSource) == 0 or bytesOf(c.priceSource) > 32) return bad("the price source is named in 1..32 bytes");
    #ok(#cycleOpened({ cycle = c; day }))
  };
  /// An instruction for a security deal's delivery leg: the deal open and its first leg unsettled, the venue and
  /// both ledgers declared, the cycle open on the deal's settlement date, no other instruction open on the leg. The
  /// amounts are the deal's: the nominal on the instrument's ledger, the settlement amount on the cash ledger.
  /// The instrument's ledger is denominated in units of face: the market declared for the instrument names the
  /// unit, and an instrument without a market is denominated in its minor units. A nominal that is not a whole
  /// number of units cannot be delivered.
  public func ledgerUnits(nominal : Nat, unitNominal : Nat) : ?Nat { if (unitNominal == 0 or nominal % unitNominal != 0) null else ?(nominal / unitNominal) };
  public func planInstruct(s : State, treasury : TreasuryCore.State, r : TreasuryCore.DealRow, t : TT.SecurityTrade, settlementAmount : Nat, unitNominal : Nat, counterparty : Principal, tradeId : ?Nat, reference : Text, documentHash : Blob, day : Nat) : Res<ST.Event> {
    if (s.venue == null) return #err(#NoVenue);
    if (r.kind != 4) return #err(#DealNotSettleable({ deal = r.id; reason = "not a security" }));
    if (not TreasuryCore.isOpen(r)) return #err(#DealNotSettleable({ deal = r.id; reason = "the deal is " # TT.dealStateText(r.state) }));
    if (TreasuryCore.legSettled(r, 0)) return #err(#DealNotSettleable({ deal = r.id; reason = "the delivery leg has settled" }));
    switch (openInstructionOf(s, r.id, 0)) { case (?x) return #err(#AlreadyInstructed({ deal = r.id; instruction = x.id })); case null {} };
    let ?sec = TreasuryCore.security(treasury, t.isin) else return #err(#DealNotSettleable({ deal = r.id; reason = "unknown instrument" }));
    let ?assetL = ledger(s, #security({ isin = t.isin })) else return #err(#NoLedger({ role = #security({ isin = t.isin }) }));
    let ?cashL = ledger(s, #cash({ currency = sec.currency })) else return #err(#NoLedger({ role = #cash({ currency = sec.currency }) }));
    switch (cycle(s, t.settlement)) { case (?c) { if (c.state != #open) return #err(#NoCycle({ businessDate = t.settlement })) }; case null return #err(#NoCycle({ businessDate = t.settlement })) };
    if (t.settlement < day) return #err(#DealNotSettleable({ deal = r.id; reason = "the settlement date has passed; the deal settles by hand" }));
    if (Principal.isAnonymous(counterparty)) return bad("the counterparty is a principal");
    if (bytesOf(reference) == 0 or bytesOf(reference) > 35) return bad("the reference is 1..35 bytes");
    if (documentHash.size() != 32) return bad("the document hash is a sha256");
    let role : ST.Role = if (t.direction == #sell) #maker else #taker;
    switch (role, tradeId) {
      case (#maker, ?_) return bad("a sale's trade is opened by the desk; no trade id is given");
      case (#taker, null) return bad("a purchase names the trade the counterparty opened");
      case (_) {};
    };
    if (t.nominal == 0 or settlementAmount == 0) return bad("both legs are positive");
    let ?units = ledgerUnits(t.nominal, unitNominal) else return bad("the nominal is not a whole number of the ledger's units of " # Nat.toText(unitNominal));
    #ok(#instructed({ instruction = { family = #treasury; deal = r.id; leg = 0; cycle = t.settlement; role; counterparty; assetLedger = assetL.ledger; assetAmount = units; cashLedger = cashL.ledger; cashAmount = settlementAmount; tradeId; reference; documentHash; matched = false }; day }))
  };
  /// A fill of a market cycle instructed: the trade is the engine's, settled by its relayer between the desk and the
  /// participant; the amounts are the engine's (the units on the instrument's ledger, the price times the units on
  /// the cash ledger); the trade id is set when the engine's settlement is read back, and the desk observes it.
  public func planInstructMatched(s : State, r : TreasuryCore.DealRow, t : TT.SecurityTrade, assetLedger : Principal, cashLedger : Principal, units : Nat, cashAmount : Nat, counterparty : Principal, reference : Text, day : Nat) : Res<ST.Event> {
    if (s.venue == null) return #err(#NoVenue);
    if (r.kind != 4) return #err(#DealNotSettleable({ deal = r.id; reason = "not a security" }));
    if (not TreasuryCore.isOpen(r)) return #err(#DealNotSettleable({ deal = r.id; reason = "the deal is " # TT.dealStateText(r.state) }));
    if (TreasuryCore.legSettled(r, 0)) return #err(#DealNotSettleable({ deal = r.id; reason = "the delivery leg has settled" }));
    switch (openInstructionOf(s, r.id, 0)) { case (?x) return #err(#AlreadyInstructed({ deal = r.id; instruction = x.id })); case null {} };
    switch (cycle(s, t.settlement)) { case (?c) { if (c.state != #open) return #err(#NoCycle({ businessDate = t.settlement })) }; case null return #err(#NoCycle({ businessDate = t.settlement })) };
    if (Principal.isAnonymous(counterparty)) return bad("the counterparty is a principal");
    if (bytesOf(reference) == 0 or bytesOf(reference) > 35) return bad("the reference is 1..35 bytes");
    if (units == 0 or cashAmount == 0) return bad("both legs are positive");
    let role : ST.Role = if (t.direction == #sell) #maker else #taker;
    #ok(#instructed({ instruction = { family = #treasury; deal = r.id; leg = 0; cycle = t.settlement; role; counterparty; assetLedger; assetAmount = units; cashLedger; cashAmount; tradeId = null; reference; documentHash = Blob.fromArray(Array.repeat<Nat8>(0, 32)); matched = true }; day }))
  };
  /// A financing leg instructed: the repo's or the loan's start or close, with the amounts the caller computed
  /// from the row (the collateral or the lent nominal on the instrument's ledger, the cash on the cash ledger),
  /// the role the leg's direction gives the desk, and the cycle the leg's day.
  public func planInstructFinancing(s : State, family : ST.Family, id : Nat, leg : Nat, isin : Text, currency : Text, nominal : Nat, unitNominal : Nat, cashAmount : Nat, deskDelivers : Bool, cycleDay : Nat, counterparty : Principal, tradeId : ?Nat, reference : Text, day : Nat) : Res<ST.Event> {
    if (s.venue == null) return #err(#NoVenue);
    if (family == #treasury) return bad("a treasury deal is instructed by its own act");
    switch (openInstructionOf(s, id, leg)) { case (?x) return #err(#AlreadyInstructed({ deal = id; instruction = x.id })); case null {} };
    let ?assetL = ledger(s, #security({ isin })) else return #err(#NoLedger({ role = #security({ isin }) }));
    let ?cashL = ledger(s, #cash({ currency })) else return #err(#NoLedger({ role = #cash({ currency }) }));
    switch (cycle(s, cycleDay)) { case (?c) { if (c.state != #open) return #err(#NoCycle({ businessDate = cycleDay })) }; case null return #err(#NoCycle({ businessDate = cycleDay })) };
    if (cycleDay < day) return #err(#DealNotSettleable({ deal = id; reason = "the leg's day has passed; it settles by hand" }));
    if (Principal.isAnonymous(counterparty)) return bad("the counterparty is a principal");
    if (bytesOf(reference) == 0 or bytesOf(reference) > 35) return bad("the reference is 1..35 bytes");
    let role : ST.Role = if (deskDelivers) #maker else #taker;
    switch (role, tradeId) {
      case (#maker, ?_) return bad("a delivery's trade is opened by the desk; no trade id is given");
      case (#taker, null) return bad("a receipt names the trade the counterparty opened");
      case (_) {};
    };
    if (nominal == 0 or cashAmount == 0) return bad("both legs are positive");
    let ?units = ledgerUnits(nominal, unitNominal) else return bad("the nominal is not a whole number of the ledger's units of " # Nat.toText(unitNominal));
    #ok(#instructed({ instruction = { family; deal = id; leg; cycle = cycleDay; role; counterparty; assetLedger = assetL.ledger; assetAmount = units; cashLedger = cashL.ledger; cashAmount; tradeId; reference; documentHash = Blob.fromArray(Array.repeat<Nat8>(0, 32)); matched = false }; day }))
  };

  /// What the driver does next for an instruction, from its row alone.
  public type NextStep = {
    #openTrade;                                   // a sale: approve the security leg and open the trade
    #verifyTrade : { tradeId : Nat };             // a purchase: read the counterparty's trade and check it
    #fundMaker : { tradeId : Nat };               // a sale opened without its escrow: approve and fund
    #fundTaker : { tradeId : Nat };               // a purchase verified: approve the cash and fund
    #observe : { tradeId : Nat };                 // funded: read the trade; settled, aborted or waiting
    #resetTrade : { previous : Nat };             // failed and recycled: the previous trade must be aborted and reclaimed first
    #nothing : { reason : Text };
  };
  public func nextStep(r : InstructionRow) : NextStep {
    switch (r.state) {
      // a matched fill: the engine's trade is observed; with none set yet, the cycle's driver reads it from the engine
      case (#matched) { if (r.tradeId != 0) #observe({ tradeId = r.tradeId }) else #nothing({ reason = "awaiting the engine's trade" }) };
      case (#instructed) {
        switch (r.role) {
          case (#maker) { if (r.tradeId != 0) #resetTrade({ previous = r.tradeId }) else #openTrade };
          case (#taker) { if (r.escrowed) #resetTrade({ previous = r.tradeId }) else if (r.tradeId == 0) #nothing({ reason = "awaiting the counterparty's trade" }) else #verifyTrade({ tradeId = r.tradeId }) };
        }
      };
      case (#opened) { if (r.escrowed) #observe({ tradeId = r.tradeId }) else #fundMaker({ tradeId = r.tradeId }) };
      case (#verified) #fundTaker({ tradeId = r.tradeId });
      case (#funded) #observe({ tradeId = r.tradeId });
      case (#failed) { if (r.tradeId != 0) #observe({ tradeId = r.tradeId }) else #nothing({ reason = "failed; waiting for the next cycle or a decision" }) };
      case (_) #nothing({ reason = "the instruction is " # ST.stateText(r.state) });
    }
  };

  /// A purchase's trade read back from Tachyon, leg for leg against the instruction: the maker is the counterparty,
  /// the taker is the desk or open, the asset leg is the instrument's ledger and nominal, the cash leg the desk's
  /// cash ledger and amount, and the trade is open.
  public func checkTrade(r : InstructionRow, desk : Principal, t : DT.TradeView) : ?Text {
    if (not Principal.equal(t.maker, r.counterparty)) return ?"the maker is not the counterparty";
    switch (t.taker) { case (?x) { if (not Principal.equal(x, desk)) return ?"the trade is reserved for another taker" }; case null {} };
    if (not Principal.equal(t.legA.ledger, r.assetLedger)) return ?"the asset leg is on another ledger";
    if (not Principal.equal(t.legB.ledger, r.cashLedger)) return ?"the cash leg is on another ledger";
    switch (t.legA.kind) { case (#icrc1(x)) { if (x.amount != r.assetAmount) return ?("the asset leg is " # Nat.toText(x.amount) # ", the instruction " # Nat.toText(r.assetAmount)) }; case (_) return ?"the asset leg is not fungible" };
    switch (t.legB.kind) { case (#icrc1(x)) { if (x.amount != r.cashAmount) return ?("the cash leg is " # Nat.toText(x.amount) # ", the instruction " # Nat.toText(r.cashAmount)) }; case (_) return ?"the cash leg is not fungible" };
    switch (t.status) { case (#Open) null; case (#Funded) null; case (_) ?("the trade is " # tradeStatusText(t.status)) }
  };
  public func tradeStatusText(s : DT.TradeStatus) : Text { switch (s) { case (#Open) "Open"; case (#Funded) "Funded"; case (#Settled) "Settled"; case (#Aborted) "Aborted" } };

  // ─── the mirror of Tachyon's audit log ─────────────────────────────────────

  /// Tachyon's enumeration checked against the mirror: each leaf's hash must be the hash of its encoded text, the
  /// sequence must continue from zero, the leaves the mirror already holds must be the same leaves, and the root
  /// over the whole enumeration must be the root Tachyon published with it. Returns the leaves the mirror lacks,
  /// which the `#auditSynced` event carries and the fold appends; or why the enumeration is not believed.
  public func checkEnumeration(s : State, events : [DT.AuditEvent], publishedRoot : ?Blob) : Result.Result<{ from : Nat; leaves : [Blob]; root : Blob }, Text> {
    let have = s.mirror.leafCount;
    var i = 0;
    let fresh = List.empty<Blob>();
    for (e in events.vals()) {
      if (e.seq != i) return #err("audit sequence " # Nat.toText(e.seq) # " where " # Nat.toText(i) # " was expected");
      let leaf = MMR.hashLeaf(Text.encodeUtf8(e.encoded));
      if (not Text.equal(hex(leaf), e.leafHex)) return #err("audit leaf " # Nat.toText(e.seq) # " does not hash to its text");
      if (i < have) {
        switch (List.get(s.mirror.leafHashes, i)) { case (?l) { if (l != leaf) return #err("audit leaf " # Nat.toText(e.seq) # " differs from the mirrored leaf") }; case null return #err("mirror short") };
      } else List.add(fresh, leaf);
      i += 1;
    };
    if (events.size() < have) return #err("the enumeration is shorter than the mirror");
    let scratch = MMR.newState();
    for (e in events.vals()) ignore MMR.append(scratch, MMR.hashLeaf(Text.encodeUtf8(e.encoded)));
    switch (MMR.rootHash(scratch), publishedRoot) {
      case (?a, ?b) { if (a != b) return #err("the root over the enumeration is not the published root"); #ok({ from = have; leaves = List.toArray(fresh); root = b }) };
      case (_) #err("an empty audit log has no settlement to verify");
    }
  };
  public func mirrorRoot(s : State) : ?Blob { s.mirrorRoot };
  public func mirrorLeaves(s : State) : Nat { s.mirror.leafCount };

  /// The settlement receipt of a trade: the `SETTLED` event of that trade among the events enumerated, hashing to
  /// the leaf the mirror holds at its sequence, under the mirror's current root; the payout amounts read from it.
  public type Receipt = { seq : Nat; leaf : Blob; root : Blob; assetPaid : Nat; cashPaid : Nat };
  public func findReceipt(s : State, tradeId : Nat, events : [DT.AuditEvent]) : Result.Result<Receipt, Text> {
    let prefix = "SETTLED|id=" # Nat.toText(tradeId) # "|";
    for (e in events.vals()) {
      if (Text.startsWith(e.encoded, #text prefix)) {
        let leaf = MMR.hashLeaf(Text.encodeUtf8(e.encoded));
        switch (List.get(s.mirror.leafHashes, e.seq)) {
          case (?l) { if (l != leaf) return #err("the settlement event is not the leaf the mirror holds at " # Nat.toText(e.seq)) };
          case null return #err("the settlement event at " # Nat.toText(e.seq) # " is past the mirror");
        };
        let ?root = s.mirrorRoot else return #err("the mirror has no root");
        let ?(assetPaid, cashPaid) = parseSettled(e.encoded) else return #err("the settlement event does not carry both payouts");
        return #ok({ seq = e.seq; leaf; root; assetPaid; cashPaid });
      };
    };
    #err("no settlement event for trade " # Nat.toText(tradeId))
  };
  func parseSettled(t : Text) : ?(Nat, Nat) {
    var a : ?Nat = null; var b : ?Nat = null;
    for (part in Text.split(t, #char '|')) {
      if (Text.startsWith(part, #text "legA_to_taker=")) a := Nat.fromText(Text.trimStart(part, #text "legA_to_taker="));
      if (Text.startsWith(part, #text "legB_to_maker=")) b := Nat.fromText(Text.trimStart(part, #text "legB_to_maker="));
    };
    switch (a, b) { case (?x, ?y) ?(x, y); case (_) null }
  };
  public func hex(b : Blob) : Text {
    let digits = "0123456789abcdef";
    let chars = Text.toArray(digits);
    var out = "";
    for (x in b.vals()) { out #= Text.fromChar(chars[Nat8.toNat(x / 16)]) # Text.fromChar(chars[Nat8.toNat(x % 16)]) };
    out
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  func index(s : State, r : InstructionRow) {
    putInstruction(s, r);
    ignore RI.put(s.byState, R.key2(Nat8.toNat(stateCode(r.state)), 1, r.id, 8), Blob.fromArray([1]));
  };
  func withState(s : State, id : Nat, block : Nat, f : InstructionRow -> InstructionRow) {
    switch (instruction(s, id)) { case (?r) { let n = f({ r with lastBlock = block }); index(s, n) }; case null {} }
  };
  func bumpCycle(s : State, day : Nat, block : Nat, f : CycleRow -> CycleRow) { switch (cycle(s, day)) { case (?c) putCycle(s, f({ c with lastBlock = block })); case null {} } };

  public func fold(s : State, block : Nat, ev : ST.Event) {
    switch (ev) {
      case (#venueSet(v)) s.venue := ?v;
      case (#ledgerSet(d)) {
        let b = R.buf(); putPrincipal(b, d.ledger); R.putBool(b, d.partial);
        if (RI.put(s.ledgers, ledgerKey(d.role), R.done(b, LEDGER_ROW_BYTES)) == null) s.ledgerCount += 1;
      };
      case (#cycleOpened(x)) { putCycle(s, { businessDate = x.cycle.businessDate; market = x.cycle.market; priceSource = x.cycle.priceSource; state = #open; settled = 0; failed = 0; pending = 0; instructions = 0; lastBlock = block }); s.cycleCount += 1 };
      case (#cycleClosed(x)) bumpCycle(s, x.businessDate, block, func(c) { { c with state = #closed; settled = x.settled; failed = x.failed; pending = x.pending } });
      case (#instructed(x)) {
        let i = x.instruction;
        let r : InstructionRow = { id = block; family = i.family; deal = i.deal; leg = i.leg; cycle = i.cycle; role = i.role; counterparty = i.counterparty; assetLedger = i.assetLedger; assetAmount = i.assetAmount; cashLedger = i.cashLedger; cashAmount = i.cashAmount;
                                   tradeId = switch (i.tradeId) { case (?t) t; case null 0 }; state = if (i.matched) #matched else #instructed; fails = 0; escrowed = false; referenceHash = hash8(i.reference); documentHash = i.documentHash; lastBlock = block; matched = i.matched };
        index(s, r);
        ignore RI.put(s.byDeal, R.key2(i.deal, 8, i.leg, 1), R.key(block, 8));
        ignore RI.put(s.byCycle, R.key2(i.cycle, 4, block, 8), Blob.fromArray([1]));
        bumpCycle(s, i.cycle, block, func(c) { { c with instructions = c.instructions + 1; pending = c.pending + 1 } });
        s.instructionCount += 1;
      };
      case (#tradeOpened(x)) withState(s, x.instruction, block, func(r) { { r with state = #opened; tradeId = x.tradeId; escrowed = x.escrowed } });
      case (#tradeVerified(x)) withState(s, x.instruction, block, func(r) { { r with state = #verified; tradeId = x.tradeId } });
      case (#fundingRecorded(x)) withState(s, x.instruction, block, func(r) { { r with state = if (x.escrowed) #funded else r.state; escrowed = x.escrowed or r.escrowed } });
      case (#callRefused(x)) withState(s, x.instruction, block, func(r) { r });
      case (#auditSynced(x)) {
        if (x.from == s.mirror.leafCount) { for (l in x.leaves.vals()) ignore MMR.append(s.mirror, l); s.mirrorRoot := MMR.rootHash(s.mirror) };
      };
      case (#receiptVerified(x)) withState(s, x.instruction, block, func(r) { r });
      case (#settled(x)) {
        withState(s, x.instruction, block, func(r) { { r with state = #settled } });
        switch (instruction(s, x.instruction)) { case (?r) bumpCycle(s, r.cycle, block, func(c) { { c with settled = c.settled + 1; pending = if (c.pending > 0) c.pending - 1 else 0 } }); case null {} };
        s.settledCount += 1;
      };
      case (#failed(x)) {
        withState(s, x.instruction, block, func(r) { { r with state = #failed; fails = x.fails } });
        switch (instruction(s, x.instruction)) { case (?r) bumpCycle(s, r.cycle, block, func(c) { { c with failed = c.failed + 1; pending = if (c.pending > 0) c.pending - 1 else 0 } }); case null {} };
        s.failedCount += 1;
      };
      case (#recycled(x)) {
        withState(s, x.instruction, block, func(r) { { r with state = #instructed; cycle = x.cycle; fails = x.fails } });
        ignore RI.put(s.byCycle, R.key2(x.cycle, 4, x.instruction, 8), Blob.fromArray([1]));
        bumpCycle(s, x.cycle, block, func(c) { { c with instructions = c.instructions + 1; pending = c.pending + 1 } });
      };
      case (#reclaimed(x)) withState(s, x.instruction, block, func(r) { { r with escrowed = false } });
      // a matched fill whose engine trade will not settle becomes an ordinary instruction: the desk opens or funds the trade itself
      case (#tradeReset(x)) withState(s, x.instruction, block, func(r) { { r with tradeId = 0; escrowed = false; state = switch (r.state) { case (#opened or #verified or #funded or #matched) #instructed; case (st) st } } });
      case (#tradeAssigned(x)) withState(s, x.instruction, block, func(r) { { r with tradeId = x.tradeId; escrowed = false } });
      case (#boughtIn(x)) withState(s, x.instruction, block, func(r) { { r with state = #boughtIn } });
      case (#cancelled(x)) withState(s, x.instruction, block, func(r) { { r with state = #cancelled } });
      case (#statusReceived(x)) withState(s, x.instruction, block, func(r) { r });
      case (#split(_)) {};
    }
  };

  // ─── reads ────────────────────────────────────────────────────────────────

  public func view(r : InstructionRow, reference : Text) : ST.InstructionView {
    { id = r.id; family = ST.familyText(r.family); deal = r.deal; leg = r.leg; cycle = r.cycle; role = ST.roleText(r.role); counterparty = r.counterparty; assetLedger = r.assetLedger; assetAmount = r.assetAmount; cashLedger = r.cashLedger; cashAmount = r.cashAmount;
      tradeId = if (r.tradeId == 0) null else ?r.tradeId; state = ST.stateText(r.state); fails = r.fails; reference; lastBlock = r.lastBlock }
  };
  public func cycleView(c : CycleRow) : ST.CycleView {
    { businessDate = c.businessDate; market = c.market; priceSource = c.priceSource; state = switch (c.state) { case (#open) "open"; case (#closed) "closed" }; settled = c.settled; failed = c.failed; pending = c.pending; instructions = c.instructions }
  };
  public func status(s : State) : ST.Status {
    { instructions = s.instructionCount; cycles = s.cycleCount; ledgers = s.ledgerCount; mirroredLeaves = s.mirror.leafCount; settled = s.settledCount; failed = s.failedCount }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.venue) { case null w.byte(0); case (?v) { w.byte(1); w.principal(v.core); w.nat(v.deadlineSecs); w.nat(v.recycleLimit); w.text(v.claimsAccount) } };
    w.nat(s.instructionCount); w.nat(s.cycleCount); w.nat(s.ledgerCount); w.nat(s.settledCount); w.nat(s.failedCount);
    w.nat(s.mirror.leafCount); switch (s.mirrorRoot) { case (?r) w.blob(r); case null w.byte(0) };
    for ((idx, width) in [(s.instructions, 8), (s.byDeal, 9), (s.byCycle, 12), (s.byState, 9), (s.cycles, 4), (s.ledgers, 13)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      walk(idx, lo, hi, func(k, v) { w.blob(k); w.blob(v); n += 1 });
      w.nat(n);
    };
  };
}
