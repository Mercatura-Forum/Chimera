/// CustodyCore.mo: the custody book folded from the desk log in stable memory: the instrument extensions, the
/// depots, the holdings per lot and depot, the corporate actions and their entitlements.
///
/// The holdings are a fold over Manticore's treasury events as well as the desk's: a purchase lot settled lands in
/// the deal's depot; a sale consumes lots from the delivering depot; a redemption takes the lot out; a transfer
/// moves a holding between depots free of payment. The desk checks a sale's lots against the delivering depot
/// before Manticore's planner runs, so a fold never sees a holding go short. A corporate action's entitlement at the
/// record date is the lots on the recorded basis: the contractual settlement position (Manticore's open lots bought
/// with a settlement on or before the record date) or the actual settlement position (the depot holdings); its
/// payment moves Manticore's lot exactly as a sale or a redemption would, through Manticore's own events.
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
import Posting "mo:manticore/Posting";

import CT "CustodyTypes";

module {

  public let INSTRUMENT_ROW_BYTES : Nat = 55;
  public let DEPOT_ROW_BYTES : Nat = 80;
  public let ACTION_ROW_BYTES : Nat = 62;
  public let ENTITLEMENT_ROW_BYTES : Nat = 27;
  let MAX_PAGE = 512;

  public type InstrumentRow = { isin : Text; lei : Text; classification : CT.Classification; market : Text; settlementCycleDays : Nat; quotation : CT.Quotation; minDenomination : Nat; block : Nat };
  public type DepotRow = { id : Text; custodianHash : Nat; place : Text; safekeepingAccount : Text; block : Nat };
  public type ActionRow = { id : CT.ActionId; isin : Text; kind : Nat8; param : Nat; recordDate : Nat; exDate : Nat; paymentDate : Nat; state : CT.ActionState; lots : Nat; entitled : Nat; paid : Nat; lastBlock : Nat };
  public type EntitlementRow = { action : CT.ActionId; lot : TT.DealId; depotHash : Nat; nominal : Nat; amount : Nat; basis : CT.Basis; claimed : Bool; paid : Bool };

  func classCode(c : CT.Classification) : Nat8 { switch (c) { case (#sovereign) 1; case (#supranational) 2; case (#financial) 3; case (#corporate) 4 } };
  func classOf(b : Nat8) : CT.Classification { switch (b) { case 1 #sovereign; case 2 #supranational; case 3 #financial; case _ #corporate } };
  func kindCode(k : CT.Kind) : Nat8 { switch (k) { case (#coupon(_)) 1; case (#partialRedemption(_)) 2; case (#earlyRedemption(_)) 3; case (#cashDistribution(_)) 4 } };
  public func kindTextOf(b : Nat8) : Text { switch (b) { case 1 "coupon"; case 2 "partialRedemption"; case 3 "earlyRedemption"; case _ "cashDistribution" } };
  func paramOf(k : CT.Kind) : Nat { switch (k) { case (#coupon(x)) x.perHundredMicro; case (#partialRedemption(x)) x.ratioBps; case (#earlyRedemption(x)) x.priceMicro; case (#cashDistribution(x)) x.perHundredMicro } };
  func stateCode(s : CT.ActionState) : Nat8 { switch (s) { case (#announced) 1; case (#entitled) 2; case (#paid) 3; case (#cancelled) 4 } };
  func stateOf(b : Nat8) : CT.ActionState { switch (b) { case 1 #announced; case 2 #entitled; case 3 #paid; case _ #cancelled } };
  public func hash8(t : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(t))), 0, 8) };

  func encodeInstrument(r : InstrumentRow) : Blob {
    let b = R.buf();
    R.putText(b, r.lei, 20); R.putByte(b, classCode(r.classification)); R.putText(b, r.market, 16); R.putNat(b, r.settlementCycleDays, 1);
    R.putByte(b, switch (r.quotation) { case (#pricePer100) 0; case (#yield) 1 }); R.putNat(b, r.minDenomination, 8); R.putNat(b, r.block, 8);
    R.done(b, INSTRUMENT_ROW_BYTES)
  };
  func decodeInstrument(isin : Text, v : Blob) : InstrumentRow {
    let a = Blob.toArray(v);
    { isin; lei = R.getText(a, 0, 20); classification = classOf(a[20]); market = R.getText(a, 21, 16); settlementCycleDays = R.getNat(a, 37, 1); quotation = if (a[38] == 1) #yield else #pricePer100; minDenomination = R.getNat(a, 39, 8); block = R.getNat(a, 47, 8) }
  };
  func encodeDepot(r : DepotRow) : Blob { let b = R.buf(); R.putNat(b, r.custodianHash, 8); R.putText(b, r.place, 32); R.putText(b, r.safekeepingAccount, 32); R.putNat(b, r.block, 8); R.done(b, DEPOT_ROW_BYTES) };
  func decodeDepot(id : Text, v : Blob) : DepotRow { let a = Blob.toArray(v); { id; custodianHash = R.getNat(a, 0, 8); place = R.getText(a, 8, 32); safekeepingAccount = R.getText(a, 40, 32); block = R.getNat(a, 72, 8) } };
  func encodeAction(r : ActionRow) : Blob {
    let b = R.buf();
    R.putText(b, r.isin, 12); R.putByte(b, r.kind); R.putNat(b, r.param, 8); R.putNat(b, r.recordDate, 4); R.putNat(b, r.exDate, 4); R.putNat(b, r.paymentDate, 4); R.putByte(b, stateCode(r.state));
    R.putNat(b, r.lots, 4); R.putNat(b, r.entitled, 8); R.putNat(b, r.paid, 8); R.putNat(b, r.lastBlock, 8);
    R.done(b, ACTION_ROW_BYTES)
  };
  func decodeAction(id : Nat, v : Blob) : ActionRow {
    let a = Blob.toArray(v);
    { id; isin = R.getText(a, 0, 12); kind = a[12]; param = R.getNat(a, 13, 8); recordDate = R.getNat(a, 21, 4); exDate = R.getNat(a, 25, 4); paymentDate = R.getNat(a, 29, 4); state = stateOf(a[33]);
      lots = R.getNat(a, 34, 4); entitled = R.getNat(a, 38, 8); paid = R.getNat(a, 46, 8); lastBlock = R.getNat(a, 54, 8) }
  };
  func encodeEntitlement(r : EntitlementRow) : Blob { let b = R.buf(); R.putNat(b, r.depotHash, 8); R.putNat(b, r.nominal, 8); R.putNat(b, r.amount, 8); R.putByte(b, switch (r.basis) { case (#contractual) 0; case (#actual) 1 }); R.putBool(b, r.claimed); R.putBool(b, r.paid); R.done(b, ENTITLEMENT_ROW_BYTES) };
  func decodeEntitlement(action : Nat, lot : Nat, v : Blob) : EntitlementRow {
    let a = Blob.toArray(v);
    { action; lot; depotHash = R.getNat(a, 0, 8); nominal = R.getNat(a, 8, 8); amount = R.getNat(a, 16, 8); basis = if (a[24] == 1) #actual else #contractual; claimed = R.getBool(a, 25); paid = R.getBool(a, 26) }
  };

  public type State = {
    instruments : RI.State;    // isin(12) -> row
    depots : RI.State;         // id(32) -> row
    depotByHash : RI.State;    // hash(8) -> id(32)
    bookDepot : RI.State;      // book(32) -> depot(32)
    dealDepot : RI.State;      // deal(8) -> depot(32)
    holdings : RI.State;       // lot(8) ‖ depotHash(8) -> nominal(8)
    byDepotIsin : RI.State;    // depotHash(8) ‖ isin(12) ‖ lot(8) -> 1
    actions : RI.State;        // id(8) -> row
    actionsByIsin : RI.State;  // isin(12) ‖ id(8) -> 1
    actionsByState : RI.State; // state(1) ‖ id(8) -> 1
    entitlements : RI.State;   // action(8) ‖ lot(8) -> row
    encumbered : RI.State;     // lot(8) ‖ depotHash(8) -> nominal pledged or lent
    received : RI.State;       // isin(12) ‖ depotHash(8) -> collateral nominal held for a counterparty
    var policy : ?CT.Policy;
    var instrumentCount : Nat;
    var depotCount : Nat;
    var holdingCount : Nat;
    var actionCount : Nat;
    var entitlementCount : Nat;
    var transferCount : Nat;
  };

  public func newState(arena : RI.Arena) : State {
    {
      instruments = RI.newStateIn(arena, { keyBytes = 12; valBytes = INSTRUMENT_ROW_BYTES });
      depots = RI.newStateIn(arena, { keyBytes = 32; valBytes = DEPOT_ROW_BYTES });
      depotByHash = RI.newStateIn(arena, { keyBytes = 8; valBytes = 32 });
      bookDepot = RI.newStateIn(arena, { keyBytes = 32; valBytes = 32 });
      dealDepot = RI.newStateIn(arena, { keyBytes = 8; valBytes = 32 });
      holdings = RI.newStateIn(arena, { keyBytes = 16; valBytes = 8 });
      byDepotIsin = RI.newStateIn(arena, { keyBytes = 28; valBytes = 1 });
      actions = RI.newStateIn(arena, { keyBytes = 8; valBytes = ACTION_ROW_BYTES });
      actionsByIsin = RI.newStateIn(arena, { keyBytes = 20; valBytes = 1 });
      actionsByState = RI.newStateIn(arena, { keyBytes = 9; valBytes = 1 });
      entitlements = RI.newStateIn(arena, { keyBytes = 16; valBytes = ENTITLEMENT_ROW_BYTES });
      encumbered = RI.newStateIn(arena, { keyBytes = 16; valBytes = 8 });
      received = RI.newStateIn(arena, { keyBytes = 20; valBytes = 8 });
      var policy = null; var instrumentCount = 0; var depotCount = 0; var holdingCount = 0; var actionCount = 0; var entitlementCount = 0; var transferCount = 0;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func policy(s : State) : ?CT.Policy { s.policy };
  public func instrument(s : State, isin : Text) : ?InstrumentRow { switch (RI.get(s.instruments, R.textKey(isin, 12))) { case (?v) ?decodeInstrument(isin, v); case null null } };
  public func depot(s : State, id : Text) : ?DepotRow { switch (RI.get(s.depots, R.textKey(id, 32))) { case (?v) ?decodeDepot(id, v); case null null } };
  public func depotIdOfHash(s : State, h : Nat) : Text { switch (RI.get(s.depotByHash, R.key(h, 8))) { case (?v) R.getText(Blob.toArray(v), 0, 32); case null "" } };
  public func bookDepotOf(s : State, book : Text) : ?Text { switch (RI.get(s.bookDepot, R.textKey(book, 32))) { case (?v) ?R.getText(Blob.toArray(v), 0, 32); case null null } };
  public func dealDepotOf(s : State, deal : TT.DealId) : ?Text { switch (RI.get(s.dealDepot, R.key(deal, 8))) { case (?v) ?R.getText(Blob.toArray(v), 0, 32); case null null } };
  func holdingKey(lot : Nat, depotHash : Nat) : Blob { R.key2(lot, 8, depotHash, 8) };
  public func holding(s : State, lot : TT.DealId, depotId : Text) : Nat { switch (RI.get(s.holdings, holdingKey(lot, hash8(depotId)))) { case (?v) R.getNat(Blob.toArray(v), 0, 8); case null 0 } };
  public func encumbered(s : State, lot : TT.DealId, depotId : Text) : Nat { switch (RI.get(s.encumbered, holdingKey(lot, hash8(depotId)))) { case (?v) R.getNat(Blob.toArray(v), 0, 8); case null 0 } };
  /// What of a lot's holding in a depot a sale or a transfer may take: the holding less what is pledged or lent.
  public func available(s : State, lot : TT.DealId, depotId : Text) : Nat { let h = holding(s, lot, depotId); let e = encumbered(s, lot, depotId); if (e > h) 0 else h - e };
  func receivedKey(isin : Text, depotId : Text) : Blob { Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(isin, 12)), Blob.toArray(R.key(hash8(depotId), 8)))) };
  public func received(s : State, isin : Text, depotId : Text) : Nat { switch (RI.get(s.received, receivedKey(isin, depotId))) { case (?v) R.getNat(Blob.toArray(v), 0, 8); case null 0 } };
  /// The lots of an instrument available in a depot, first in first out, for a pledge or a loan.
  public func availableIn(s : State, depotId : Text, isin : Text) : [(TT.DealId, Nat)] {
    Array.filter<(TT.DealId, Nat)>(Array.map<(TT.DealId, Nat), (TT.DealId, Nat)>(holdingsIn(s, depotId, isin), func((lot, _)) { (lot, available(s, lot, depotId)) }), func((_, n)) { n > 0 })
  };
  public func availableView(s : State, depotId : Text, isin : Text) : CT.AvailableView {
    var held = 0; var enc = 0;
    for ((lot, n) in holdingsIn(s, depotId, isin).vals()) { held += n; enc += Nat.min(n, encumbered(s, lot, depotId)) };
    { depot = depotId; isin; held; encumbered = enc; available = held - enc; received = received(s, isin, depotId) }
  };
  public func action(s : State, id : CT.ActionId) : ?ActionRow { switch (RI.get(s.actions, R.key(id, 8))) { case (?v) ?decodeAction(id, v); case null null } };
  public func entitlement(s : State, id : CT.ActionId, lot : TT.DealId) : ?EntitlementRow { switch (RI.get(s.entitlements, R.key2(id, 8, lot, 8))) { case (?v) ?decodeEntitlement(id, lot, v); case null null } };
  func putAction(s : State, r : ActionRow) { ignore RI.put(s.actions, R.key(r.id, 8), encodeAction(r)) };
  func putEntitlement(s : State, r : EntitlementRow) { ignore RI.put(s.entitlements, R.key2(r.action, 8, r.lot, 8), encodeEntitlement(r)) };

  func walk(idx : RI.State, lo : Blob, hi : Blob, f : (Blob, Blob) -> ()) {
    var cursor : ?Blob = null;
    label w loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) f(k, v);
      switch (page.cursor) { case null break w; case (?c) cursor := ?c };
    };
  };
  func prefixText(t : Text, width : Nat, rest : Nat) : (Blob, Blob) {
    let p = Blob.toArray(R.textKey(t, width));
    (Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(0, rest))), Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(255, rest))))
  };
  /// A lot's holdings across every depot, in depot-hash order.
  public func holdingsOfLot(s : State, lot : TT.DealId) : [(Nat, Nat)] {
    let (lo, hi) = R.prefixRange(lot, 8, 8);
    let out = List.empty<(Nat, Nat)>();
    walk(s.holdings, lo, hi, func(k, v) { let n = R.getNat(Blob.toArray(v), 0, 8); if (n > 0) List.add(out, (R.getNat(Blob.toArray(k), 8, 8), n)) });
    List.toArray(out)
  };
  public func heldOfLot(s : State, lot : TT.DealId) : Nat { var n = 0; for ((_, h) in holdingsOfLot(s, lot).vals()) n += h; n };
  /// The holdings of a depot in an ISIN, by lot.
  public func holdingsIn(s : State, depotId : Text, isin : Text) : [(TT.DealId, Nat)] {
    let h = hash8(depotId);
    let prefix = Array.concat<Nat8>(Blob.toArray(R.key(h, 8)), Blob.toArray(R.textKey(isin, 12)));
    let lo = Blob.fromArray(Array.concat<Nat8>(prefix, Array.repeat<Nat8>(0, 8)));
    let hi = Blob.fromArray(Array.concat<Nat8>(prefix, Array.repeat<Nat8>(255, 8)));
    let out = List.empty<(TT.DealId, Nat)>();
    walk(s.byDepotIsin, lo, hi, func(k, _) { let lot = R.getNat(Blob.toArray(k), 20, 8); let n = holding(s, lot, depotId); if (n > 0) List.add(out, (lot, n)) });
    List.toArray(out)
  };
  /// The settled position of a depot in an ISIN.
  public func position(s : State, depotId : Text, isin : Text) : CT.PositionView {
    var nominal = 0; var lots = 0;
    for ((_, n) in holdingsIn(s, depotId, isin).vals()) { nominal += n; lots += 1 };
    { depot = depotId; isin; nominal; lots }
  };
  /// Every holding of a depot, by ISIN and lot.
  public func holdingsOf(s : State, depotId : Text, isinOf : TT.DealId -> (Text, Text)) : [CT.HoldingView] {
    let (lo, hi) = R.prefixRange(hash8(depotId), 8, 20);
    let out = List.empty<CT.HoldingView>();
    walk(s.byDepotIsin, lo, hi, func(k, _) {
      let a = Blob.toArray(k); let lot = R.getNat(a, 20, 8); let n = holding(s, lot, depotId);
      if (n > 0) { let (isin, book) = isinOf(lot); List.add(out, { lot; depot = depotId; isin; book; nominal = n }) };
    });
    List.toArray(out)
  };
  public func actionsOf(s : State, isin : Text) : [ActionRow] {
    let (lo, hi) = prefixText(isin, 12, 8);
    let out = List.empty<ActionRow>();
    walk(s.actionsByIsin, lo, hi, func(k, _) { switch (action(s, R.getNat(Blob.toArray(k), 12, 8))) { case (?r) List.add(out, r); case null {} } });
    List.toArray(out)
  };
  public func actionsInState(s : State, st : CT.ActionState) : [ActionRow] {
    let (lo, hi) = R.prefixRange(Nat8.toNat(stateCode(st)), 1, 8);
    let out = List.empty<ActionRow>();
    walk(s.actionsByState, lo, hi, func(k, _) { switch (action(s, R.getNat(Blob.toArray(k), 1, 8))) { case (?r) { if (r.state == st) List.add(out, r) }; case null {} } });
    List.toArray(out)
  };
  public func entitlementsOf(s : State, id : CT.ActionId) : [EntitlementRow] {
    let (lo, hi) = R.prefixRange(id, 8, 8);
    let out = List.empty<EntitlementRow>();
    walk(s.entitlements, lo, hi, func(k, v) { List.add(out, decodeEntitlement(id, R.getNat(Blob.toArray(k), 8, 8), v)) });
    List.toArray(out)
  };
  /// Whether an announced coupon covers the instrument's own coupon date: one whose record date is on or before the
  /// day and whose payment date is on or after it. Manticore's coupon for that date is then not paid twice.
  public func announcedCouponCovers(s : State, isin : Text, day : Nat) : Bool {
    for (r in actionsOf(s, isin).vals()) { if (r.kind == 1 and r.state != #cancelled and r.recordDate <= day and day <= r.paymentDate) return true };
    false
  };

  // ─── planners ─────────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, CT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidTerms({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planPolicy(p : CT.Policy) : Res<CT.Event> { #ok(#policySet(p)) };
  public func planExtend(s : State, treasury : TreasuryCore.State, x : CT.Extension, day : Nat) : Res<CT.Event> {
    if (TreasuryCore.security(treasury, x.isin) == null) return #err(#UnknownInstrument({ isin = x.isin }));
    if (bytesOf(x.lei) != 0 and bytesOf(x.lei) != 20) return bad("the LEI has 20 characters, or is absent");
    if (bytesOf(x.market) == 0 or bytesOf(x.market) > 16) return bad("the market is named in 1..16 bytes");
    if (x.settlementCycleDays > 10) return bad("the settlement cycle is at most 10 days");
    if (x.minDenomination == 0) return bad("the minimum denomination is positive");
    #ok(#instrumentExtended({ extension = x; day }))
  };
  public func planOpenDepot(s : State, d : CT.Depot, day : Nat) : Res<CT.Event> {
    if (bytesOf(d.id) == 0 or bytesOf(d.id) > 32) return bad("a depot id is 1..32 bytes");
    if (depot(s, d.id) != null) return bad("depot " # d.id # " is already open");
    if (bytesOf(d.custodian.name) == 0 or bytesOf(d.custodian.name) > 64) return bad("the custodian is named in 1..64 bytes");
    if (bytesOf(d.custodian.bic) != 8 and bytesOf(d.custodian.bic) != 11) return bad("the custodian's BIC has 8 or 11 characters");
    if (bytesOf(d.place) == 0 or bytesOf(d.place) > 32) return bad("the place of safekeeping is named in 1..32 bytes");
    if (bytesOf(d.safekeepingAccount) == 0 or bytesOf(d.safekeepingAccount) > 32) return bad("the safekeeping account is 1..32 bytes");
    #ok(#depotOpened({ depot = d; day }))
  };
  public func planSetBookDepot(s : State, book : Text, depotId : Text, day : Nat) : Res<CT.Event> {
    if (depot(s, depotId) == null) return #err(#UnknownDepot({ depot = depotId }));
    #ok(#bookDepotSet({ book; depot = depotId; day }))
  };
  public func planAssignDealDepot(s : State, treasury : TreasuryCore.State, deal : TT.DealId, depotId : Text, day : Nat) : Res<CT.Event> {
    if (depot(s, depotId) == null) return #err(#UnknownDepot({ depot = depotId }));
    let ?r = TreasuryCore.row(treasury, deal) else return bad("deal " # Nat.toText(deal) # " is unknown");
    if (r.kind != 4) return bad("deal " # Nat.toText(deal) # " is not a security");
    if (TreasuryCore.legSettled(r, 0)) return #err(#LotNotIn({ lot = deal; state = "settled" }));
    if (not TreasuryCore.isOpen(r)) return #err(#LotNotIn({ lot = deal; state = TT.dealStateText(r.state) }));
    #ok(#dealDepotAssigned({ deal; depot = depotId; day }))
  };
  public func planTransfer(s : State, lot : TT.DealId, from : Text, to : Text, nominal : Nat, reference : Text, day : Nat) : Res<CT.Event> {
    if (depot(s, from) == null) return #err(#UnknownDepot({ depot = from }));
    if (depot(s, to) == null) return #err(#UnknownDepot({ depot = to }));
    if (Text.equal(from, to)) return bad("a transfer moves between two depots");
    if (nominal == 0) return bad("a transfer moves a positive nominal");
    if (bytesOf(reference) > 64) return bad("the reference is at most 64 bytes");
    let held = available(s, lot, from);
    if (held < nominal) return #err(#DepotShort({ depot = from; isin = ""; held; wanted = nominal }));
    #ok(#transferred({ lot; from; to; nominal; reference; day }))
  };
  public func planAnnounce(s : State, treasury : TreasuryCore.State, a : CT.Announcement, day : Nat) : Res<CT.Event> {
    if (s.policy == null) return #err(#NoPolicy);
    if (TreasuryCore.security(treasury, a.isin) == null) return #err(#UnknownInstrument({ isin = a.isin }));
    if (a.source.size() != 32) return bad("the source is a sha256");
    if (a.exDate > a.recordDate or a.recordDate > a.paymentDate) return bad("ex date, record date and payment date are in order");
    if (a.recordDate < day) return bad("the record date is not in the past");
    switch (a.kind) {
      case (#coupon(x)) { if (x.perHundredMicro == 0) return bad("a coupon pays a positive amount per 100") };
      case (#partialRedemption(x)) { if (x.ratioBps == 0 or x.ratioBps >= 10_000) return bad("a partial redemption returns a ratio in 1..9999 basis points") };
      case (#earlyRedemption(x)) { if (x.priceMicro == 0) return bad("an early redemption names a positive price") };
      case (#cashDistribution(x)) { if (x.perHundredMicro == 0) return bad("a distribution pays a positive amount per 100") };
    };
    #ok(#announced({ announcement = a; day }))
  };
  public func planCancel(s : State, id : CT.ActionId, reason : Text, day : Nat) : Res<CT.Event> {
    let ?r = action(s, id) else return #err(#UnknownAction({ action = id }));
    if (r.state != #announced) return #err(#ActionNotIn({ action = id; state = CT.actionStateText(r.state); wanted = "announced" }));
    if (bytesOf(reason) == 0 or bytesOf(reason) > 128) return bad("a cancellation states its reason in 1..128 bytes");
    #ok(#cancelled({ action = id; reason; day }))
  };

  /// A sale's lots against the delivering depot: Manticore allocates the lots first-in-first-out or pro rata by
  /// the book's method; each consumed part must be held in the depot the sale delivers from. A sale with no depot
  /// (a book without one) is not checked, because nothing tracks it.
  public func checkSaleDepot(s : State, treasury : TreasuryCore.State, sale : TreasuryCore.DealRow, t : TT.SecurityTrade, method : TT.LotMethod) : ?CT.Error {
    let ?depotId = dealDepotOf(s, sale.id) else return null;
    for ((lot, q) in TreasuryCore.allocateSale(TreasuryCore.lotsOf(treasury, sale.book, t.isin), t.nominal, method).vals()) {
      let held = available(s, lot.id, depotId);
      if (held < q) return ?#DepotShort({ depot = depotId; isin = t.isin; held; wanted = q });
    };
    null
  };

  // ─── entitlements and their payment ────────────────────────────────────────

  func amountFor(kind : Nat8, param : Nat, nominal : Nat) : Nat {
    switch (kind) {
      case 1 M.roundNat(M.q(nominal * param, 100 * 1_000_000));       // a coupon per 100, micro
      case 2 M.roundNat(M.q(nominal * param, 10_000));                // a partial redemption at par, by the ratio
      case 3 M.roundNat(M.q(nominal * param, 100 * 1_000_000));       // an early redemption at a price per 100
      case _ M.roundNat(M.q(nominal * param, 100 * 1_000_000));       // a distribution per 100, micro
    }
  };
  /// The entitlement events of an action at its record date, on the recorded basis. `lotsOfIsin` gives every
  /// open purchase lot of the instrument across the books with its settlement day (the contractual basis).
  public func planEntitle(s : State, r : ActionRow, lotsOfIsin : () -> [(TreasuryCore.DealRow, ?Text)], day : Nat) : Res<[CT.Event]> {
    let ?p = s.policy else return #err(#NoPolicy);
    if (r.state != #announced) return #err(#ActionNotIn({ action = r.id; state = CT.actionStateText(r.state); wanted = "announced" }));
    if (day < r.recordDate) return #err(#NotDue({ action = r.id; due = r.recordDate; day }));
    let out = List.empty<CT.Event>();
    var total = 0; var lots = 0;
    switch (p.entitlementBasis) {
      case (#contractual) {
        for ((lot, depotId) in lotsOfIsin().vals()) {
          if (lot.start <= r.recordDate and lot.nominalLeft > 0) {
            let amount = amountFor(r.kind, r.param, lot.nominalLeft);
            List.add(out, #entitlementRecorded({ action = r.id; lot = lot.id; depot = switch (depotId) { case (?d) d; case null "" }; nominal = lot.nominalLeft; amount; basis = #contractual; day }));
            total += amount; lots += 1;
          };
        };
      };
      case (#actual) {
        // the lot's settled holdings across every depot it sits in, recorded against the lot's own depot
        for ((lot, depotId) in lotsOfIsin().vals()) {
          let held = heldOfLot(s, lot.id);
          if (held > 0) {
            let amount = amountFor(r.kind, r.param, held);
            List.add(out, #entitlementRecorded({ action = r.id; lot = lot.id; depot = switch (depotId) { case (?d) d; case null "" }; nominal = held; amount; basis = #actual; day }));
            total += amount; lots += 1;
          };
        };
      };
    };
    List.add(out, #entitled({ action = r.id; lots; total; day }));
    #ok(List.toArray(out))
  };

  public type Payment = { ev : CT.Event; legs : [JT.Leg]; treasury : ?TT.TreasuryEvent };

  /// The receivable a claimed coupon entitlement sits in until its payment date: the coupon receivable under the
  /// action's and the lot's own sub-ledger.
  public func claimSub(action : CT.ActionId, lot : TT.DealId) : JT.SubledgerKey { Posting.subledgerOf("corporate-action/" # Nat.toText(action) # "/" # Nat.toText(lot)) };
  /// The day a coupon entitlement is claimed: the instrument's own coupon date the announcement covers (the first
  /// on or after the record date and on or before the payment date), or the payment date when the grid has none
  /// there. On that day the treasury job has accrued the lot to the coupon date and left the grid coupon to the
  /// announcement.
  public func claimDay(sec : TreasuryCore.SecurityRow, r : ActionRow) : Nat {
    var day = r.paymentDate;
    for (pd in TreasuryCore.couponPeriodsOf(sec, 100_00).vals()) { if (pd.end >= r.recordDate and pd.end < day) day := pd.end };
    day
  };

  func addLeg(ls : List.List<JT.Leg>, account : Text, sub : ?JT.SubledgerKey, side : JT.Side, ccy : Text, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, sub, side, ccy, amount)) };
  func signed(ls : List.List<JT.Leg>, account : Text, sub : ?JT.SubledgerKey, ccy : Text, v : Int, up : JT.Side) {
    if (v > 0) addLeg(ls, account, sub, up, ccy, Int.abs(v)) else if (v < 0) addLeg(ls, account, sub, if (up == #debit) #credit else #debit, ccy, Int.abs(v));
  };
  func securitiesAccount(p : TT.Policy, flags : Nat8) : Text { if ((flags & TreasuryCore.F_FVOCI) != 0) p.securitiesFvoci else if ((flags & TreasuryCore.F_FVTPL) != 0) p.securitiesFvtpl else p.securitiesAmortisedCost };
  func fvContra(p : TT.Policy, flags : Nat8, gain : Bool) : Text { if ((flags & TreasuryCore.F_FVOCI) != 0) p.fvociReserve else (if (gain) p.unrealisedTradingGain else p.unrealisedTradingLoss) };

  /// A coupon entitlement claimed on its claim day: the announced amount into the receivable under the action,
  /// the lot's accrued coupon cleared against it and the difference to income, the shape of Manticore's own coupon
  /// with the receivable in place of the cash; Manticore's `#couponPaid` resets the lot's accrual.
  public func planClaim(p : TT.Policy, r : ActionRow, e : EntitlementRow, lot : TreasuryCore.DealRow, currency : Text, day : Nat) : Res<Payment> {
    if (r.state != #entitled) return #err(#ActionNotIn({ action = r.id; state = CT.actionStateText(r.state); wanted = "entitled" }));
    if (r.kind != 1) return bad("only a coupon is claimed");
    if (e.claimed) return #err(#ActionNotIn({ action = r.id; state = "claimed"; wanted = "entitled" }));
    let ls = List.empty<JT.Leg>();
    addLeg(ls, p.couponReceivable, ?claimSub(r.id, lot.id), #debit, currency, e.amount);
    signed(ls, p.couponReceivable, ?TreasuryCore.dealSub(lot.id), currency, lot.accruedPosted, #credit);
    signed(ls, p.couponIncome, null, currency, (e.amount : Int) - lot.accruedPosted, #credit);
    #ok({ ev = #entitlementClaimed({ action = r.id; lot = lot.id; amount = e.amount; accrued = lot.accruedPosted; day }); legs = List.toArray(ls); treasury = ?#couponPaid({ deal = lot.id; amount = e.amount; day }) })
  };

  /// One lot's entitlement paid on the payment date: the legs to post and the treasury event that moves the lot.
  /// A claimed coupon is cash against the claim's receivable; a distribution goes to income; a partial or early
  /// redemption consumes the lot pro rata to what is booked, as Manticore's sale consumes it, the proceeds against
  /// the book value and the difference realised.
  /// Where the cash of a payment on a lent lot goes instead: the part out on loan is a manufactured payment, a
  /// receivable from the borrower in the account given, pro rata to the nominal lent.
  public type Manufactured = { account : Text; sub : JT.SubledgerKey; lent : Nat };
  public func manufacturedPart(amount : Nat, nominal : Nat, lent : Nat) : Nat { if (nominal == 0 or lent == 0) 0 else M.roundNat(M.q(amount * Nat.min(lent, nominal), nominal)) };
  public func planPay(s : State, p : TT.Policy, r : ActionRow, e : EntitlementRow, lot : TreasuryCore.DealRow, cash : TT.CashAccount, currency : Text, day : Nat) : Res<Payment> { planPayWith(s, p, r, e, lot, cash, currency, day, null) };
  public func planPayWith(s : State, p : TT.Policy, r : ActionRow, e : EntitlementRow, lot : TreasuryCore.DealRow, cash : TT.CashAccount, currency : Text, day : Nat, manufactured : ?Manufactured) : Res<Payment> {
    if (r.state != #entitled) return #err(#ActionNotIn({ action = r.id; state = CT.actionStateText(r.state); wanted = "entitled" }));
    if (day < r.paymentDate) return #err(#NotDue({ action = r.id; due = r.paymentDate; day }));
    if (e.paid) return #err(#ActionNotIn({ action = r.id; state = "paid"; wanted = "entitled" }));
    let ls = List.empty<JT.Leg>();
    let sub = ?TreasuryCore.dealSub(lot.id);
    let cs = TreasuryCore.cashSub(cash);
    func cashIn(amount : Nat) {
      let borrowed = switch (manufactured) { case (?m) manufacturedPart(amount, e.nominal, m.lent); case null 0 };
      addLeg(ls, cash.account, cs, #debit, currency, amount - borrowed);
      switch (manufactured) { case (?m) addLeg(ls, m.account, ?m.sub, #debit, currency, borrowed); case null {} };
    };
    switch (r.kind) {
      case 1 {
        if (not e.claimed) return #err(#ActionNotIn({ action = r.id; state = "entitled"; wanted = "claimed" }));
        cashIn(e.amount);
        addLeg(ls, p.couponReceivable, ?claimSub(r.id, lot.id), #credit, currency, e.amount);
        #ok({ ev = #entitlementPaid({ action = r.id; lot = lot.id; amount = e.amount; nominal = 0; realised = 0; day }); legs = List.toArray(ls); treasury = null })
      };
      case 4 {
        cashIn(e.amount);
        addLeg(ls, p.couponIncome, null, #credit, currency, e.amount);
        #ok({ ev = #entitlementPaid({ action = r.id; lot = lot.id; amount = e.amount; nominal = 0; realised = 0; day }); legs = List.toArray(ls); treasury = null })
      };
      case _ {
        // the nominal leaving: the entitled nominal for a partial redemption, everything for an early one, bounded by what is left
        let q = Nat.min(if (r.kind == 2) M.roundNat(M.q(e.nominal * r.param, 10_000)) else lot.nominalLeft, lot.nominalLeft);
        if (q == 0) return bad("nothing left to redeem on lot " # Nat.toText(lot.id));
        func part(x : Int) : Int { if (q >= lot.nominalLeft) x else M.roundHalfEven(M.q(x * q, lot.nominalLeft)) };
        let cost = Int.abs(part(lot.costLeft)); let amort = part(lot.amortisedPosted); let fv = part(lot.fvPosted); let accrual = part(lot.accruedPosted);
        let proceeds = if (r.kind == 2) q else M.roundNat(M.q(q * r.param, 100 * 1_000_000));
        let book : Int = (cost : Int) + amort;
        addLeg(ls, cash.account, cs, #debit, currency, proceeds + Int.abs(accrual));
        signed(ls, securitiesAccount(p, lot.flags), sub, currency, book, #credit);
        // the fair-value adjustment reversed to its contra
        if (fv > 0) { addLeg(ls, fvContra(p, lot.flags, true), null, #debit, currency, Int.abs(fv)); addLeg(ls, securitiesAccount(p, lot.flags), sub, #credit, currency, Int.abs(fv)) }
        else if (fv < 0) { addLeg(ls, securitiesAccount(p, lot.flags), sub, #debit, currency, Int.abs(fv)); addLeg(ls, fvContra(p, lot.flags, false), null, #credit, currency, Int.abs(fv)) };
        signed(ls, p.couponReceivable, sub, currency, accrual, #credit);
        let realised : Int = (proceeds : Int) - book;
        if (realised > 0) addLeg(ls, p.realisedTradingGain, null, #credit, currency, Int.abs(realised)) else if (realised < 0) addLeg(ls, p.realisedTradingLoss, null, #debit, currency, Int.abs(realised));
        #ok({
          ev = #entitlementPaid({ action = r.id; lot = lot.id; amount = proceeds; nominal = q; realised; day }); legs = List.toArray(ls);
          treasury = ?#lotConsumed({ lot = lot.id; by = r.id; nominal = q; cost; amortisation = -amort; fv = -fv; accrual = -accrual; day });
        })
      };
    }
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  func indexAction(s : State, r : ActionRow) {
    ignore RI.put(s.actionsByIsin, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.isin, 12)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1]));
    ignore RI.put(s.actionsByState, R.key2(Nat8.toNat(stateCode(r.state)), 1, r.id, 8), Blob.fromArray([1]));
  };
  func setHolding(s : State, lot : Nat, depotId : Text, isin : Text, nominal : Nat) {
    let h = hash8(depotId);
    if (RI.put(s.holdings, holdingKey(lot, h), R.key(nominal, 8)) == null) s.holdingCount += 1;
    ignore RI.put(s.byDepotIsin, Blob.fromArray(Array.concat<Nat8>(Array.concat<Nat8>(Blob.toArray(R.key(h, 8)), Blob.toArray(R.textKey(isin, 12))), Blob.toArray(R.key(lot, 8)))), Blob.fromArray([1]));
  };
  func reduceHolding(s : State, lot : Nat, depotId : Text, isin : Text, by : Nat) {
    let held = holding(s, lot, depotId);
    setHolding(s, lot, depotId, isin, if (by > held) 0 else held - by);
  };
  /// A nominal leaving a lot wherever it is held: the lot's own depot first, then the others in index order.
  func reduceLotHolding(s : State, lot : Nat, isin : Text, by : Nat) {
    var left = by;
    switch (dealDepotOf(s, lot)) {
      case (?d) { let h = holding(s, lot, d); let q = Nat.min(h, left); if (q > 0) { reduceHolding(s, lot, d, isin, q); left -= q } };
      case null {};
    };
    for ((dh, h) in holdingsOfLot(s, lot).vals()) {
      if (left == 0) return;
      let d = depotIdOfHash(s, dh);
      let q = Nat.min(h, left);
      if (q > 0) { reduceHolding(s, lot, d, isin, q); left -= q };
    };
  };

  public func fold(s : State, block : Nat, ev : CT.Event, isinOf : TT.DealId -> Text) {
    switch (ev) {
      case (#policySet(p)) s.policy := ?p;
      case (#instrumentExtended(x)) {
        let e = x.extension;
        if (RI.put(s.instruments, R.textKey(e.isin, 12), encodeInstrument({ isin = e.isin; lei = e.lei; classification = e.classification; market = e.market; settlementCycleDays = e.settlementCycleDays; quotation = e.quotation; minDenomination = e.minDenomination; block })) == null) s.instrumentCount += 1;
      };
      case (#depotOpened(x)) {
        let d = x.depot;
        ignore RI.put(s.depots, R.textKey(d.id, 32), encodeDepot({ id = d.id; custodianHash = hash8(d.custodian.name); place = d.place; safekeepingAccount = d.safekeepingAccount; block }));
        ignore RI.put(s.depotByHash, R.key(hash8(d.id), 8), R.textKey(d.id, 32));
        s.depotCount += 1;
      };
      case (#bookDepotSet(x)) ignore RI.put(s.bookDepot, R.textKey(x.book, 32), R.textKey(x.depot, 32));
      case (#dealDepotAssigned(x)) ignore RI.put(s.dealDepot, R.key(x.deal, 8), R.textKey(x.depot, 32));
      case (#transferred(x)) {
        let isin = isinOf(x.lot);
        reduceHolding(s, x.lot, x.from, isin, x.nominal);
        setHolding(s, x.lot, x.to, isin, holding(s, x.lot, x.to) + x.nominal);
        s.transferCount += 1;
      };
      case (#announced(x)) {
        let a = x.announcement;
        let r : ActionRow = { id = block; isin = a.isin; kind = kindCode(a.kind); param = paramOf(a.kind); recordDate = a.recordDate; exDate = a.exDate; paymentDate = a.paymentDate; state = #announced; lots = 0; entitled = 0; paid = 0; lastBlock = block };
        putAction(s, r); indexAction(s, r); s.actionCount += 1;
      };
      case (#cancelled(x)) { switch (action(s, x.action)) { case (?r) { let n = { r with state = #cancelled; lastBlock = block }; putAction(s, n); indexAction(s, n) }; case null {} } };
      case (#entitlementRecorded(x)) {
        putEntitlement(s, { action = x.action; lot = x.lot; depotHash = hash8(x.depot); nominal = x.nominal; amount = x.amount; basis = x.basis; claimed = false; paid = false });
        s.entitlementCount += 1;
      };
      case (#entitled(x)) { switch (action(s, x.action)) { case (?r) { let n = { r with state = #entitled; lots = x.lots; entitled = x.total; lastBlock = block }; putAction(s, n); indexAction(s, n) }; case null {} } };
      case (#entitlementClaimed(x)) {
        switch (entitlement(s, x.action, x.lot)) { case (?e) putEntitlement(s, { e with claimed = true }); case null {} };
        switch (action(s, x.action)) { case (?r) putAction(s, { r with lastBlock = block }); case null {} };
      };
      case (#entitlementPaid(x)) {
        switch (entitlement(s, x.action, x.lot)) { case (?e) putEntitlement(s, { e with paid = true }); case null {} };
        switch (action(s, x.action)) { case (?r) putAction(s, { r with paid = r.paid + x.amount; lastBlock = block }); case null {} };
        // a redemption's nominal leaves the lot's holdings, its own depot first
        if (x.nominal > 0) reduceLotHolding(s, x.lot, isinOf(x.lot), x.nominal);
      };
      case (#paid(x)) { switch (action(s, x.action)) { case (?r) { let n = { r with state = #paid; lastBlock = block }; putAction(s, n); indexAction(s, n) }; case null {} } };
      case (#pledged(x)) encumber(s, x.lot, x.depot, x.nominal, true);
      case (#lent(x)) encumber(s, x.lot, x.depot, x.nominal, true);
      case (#released(x)) encumber(s, x.lot, x.depot, x.nominal, false);
      case (#lentReturned(x)) encumber(s, x.lot, x.depot, x.nominal, false);
      case (#collateralReceived(x)) { ignore RI.put(s.received, receivedKey(x.isin, x.depot), R.key(received(s, x.isin, x.depot) + x.nominal, 8)) };
      case (#collateralReturned(x)) { let have = received(s, x.isin, x.depot); ignore RI.put(s.received, receivedKey(x.isin, x.depot), R.key(if (x.nominal > have) 0 else have - x.nominal, 8)) };
    }
  };
  func encumber(s : State, lot : Nat, depotId : Text, by : Nat, more : Bool) {
    let have = encumbered(s, lot, depotId);
    let next = if (more) have + by else (if (by > have) 0 else have - by);
    ignore RI.put(s.encumbered, holdingKey(lot, hash8(depotId)), R.key(next, 8));
  };

  /// What the custody fold reads out of Manticore's treasury events: a purchase lot settled lands in its depot, a
  /// sale consumes the lots from the sale's delivering depot (the lot's own when the sale has none), a redemption
  /// takes the lot out. `rowOf` reads the treasury row as it stood when the event was folded.
  public func observeTreasury(s : State, ev : TT.TreasuryEvent, rowOf : TT.DealId -> ?TreasuryCore.DealRow) {
    switch (ev) {
      case (#legSettled(x)) {
        switch (rowOf(x.deal)) {
          case (?r) {
            if (r.kind == 4 and (r.flags & TreasuryCore.F_BUY) != 0) {
              switch (dealDepotOf(s, x.deal)) {
                case (?d) { if (x.leg == 0) setHolding(s, x.deal, d, r.isin, r.notional) else if (x.nominal > 0) reduceHolding(s, x.deal, d, r.isin, x.nominal) };
                case null {};
              };
            };
          };
          case null {};
        };
      };
      case (#lotConsumed(x)) {
        // a sale's consumption names the sale in `by`; a redemption paid by an action names the action, whose depot
        // is the lot's own; a consumption by an action is folded from the action's own event
        switch (rowOf(x.by)) {
          case (?sale) {
            let depotId = switch (dealDepotOf(s, x.by)) { case (?d) ?d; case null dealDepotOf(s, x.lot) };
            switch (depotId, rowOf(x.lot)) { case (?d, ?l) reduceHolding(s, x.lot, d, l.isin, x.nominal); case (_) {} };
            ignore sale;
          };
          case null {};
        };
      };
      case (_) {};
    }
  };

  // ─── reads ────────────────────────────────────────────────────────────────

  public func actionView(r : ActionRow) : CT.ActionView {
    { id = r.id; isin = r.isin; kind = kindTextOf(r.kind); recordDate = r.recordDate; exDate = r.exDate; paymentDate = r.paymentDate; state = CT.actionStateText(r.state); lots = r.lots; entitled = r.entitled; paid = r.paid; lastBlock = r.lastBlock }
  };
  public func entitlementView(s : State, e : EntitlementRow) : CT.EntitlementView {
    { action = e.action; lot = e.lot; depot = depotIdOfHash(s, e.depotHash); nominal = e.nominal; amount = e.amount; basis = CT.basisText(e.basis); claimed = e.claimed; paid = e.paid }
  };
  public func status(s : State) : CT.Status {
    { instruments = s.instrumentCount; depots = s.depotCount; holdings = s.holdingCount; actions = s.actionCount; entitlements = s.entitlementCount; transfers = s.transferCount }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.policy) { case null w.byte(0); case (?p) { w.byte(1); w.text(CT.basisText(p.entitlementBasis)) } };
    w.nat(s.instrumentCount); w.nat(s.depotCount); w.nat(s.holdingCount); w.nat(s.actionCount); w.nat(s.entitlementCount); w.nat(s.transferCount);
    for ((idx, width) in [(s.instruments, 12), (s.depots, 32), (s.depotByHash, 8), (s.bookDepot, 32), (s.dealDepot, 8), (s.holdings, 16), (s.byDepotIsin, 28), (s.actions, 8), (s.actionsByIsin, 20), (s.actionsByState, 9), (s.entitlements, 16), (s.encumbered, 16), (s.received, 20)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      walk(idx, lo, hi, func(k, v) { w.blob(k); w.blob(v); n += 1 });
      w.nat(n);
    };
  };
}
