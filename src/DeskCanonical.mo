/// DeskCanonical.mo: the bytes of the desk's commands and of the blocks its log records.
///
/// Two encoders, both under the kernel's freeze. The command encoder is version 1 of a registry: a checker
/// approves a command's hash, execution re-derives it under the version the proposal recorded, and a family's
/// bytes change only by a new version with the old encoder kept. The event codec writes every block's event as a
/// length-prefixed body so the frame around it (the kernel's `DomainLog`) can find the stored hash without knowing
/// the vocabulary, and inside the body a tag byte selects the family.
///
/// The treasury command bodies (tags 0x20 to 0x2C, Manticore's own second bytes) and the treasury event (tag 0x55,
/// Manticore's own) are written by `TreasuryCanonical` and are therefore byte for byte what Manticore's bank log
/// carries, which is what lets the independent verifier decode them with Manticore's decoder unchanged. The close
/// events are `CloseCanonical`'s; the five maker-checker blocks are the kernel's `Command` shapes.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Int "mo:core/Int";
import VarArray "mo:core/VarArray";
import List "mo:core/List";
import Blob "mo:core/Blob";

import JC "mo:journal/Canonical";
import KC "mo:kernel/codec/Canonical";
import AT "mo:kernel/auth/AuthTypes";
import Cmd "mo:kernel/domain/Command";
import Enc "mo:kernel/domain/Encoding";
import DL "mo:kernel/domain/DomainLog";
import Batch "mo:kernel/batch/Batch";

import TyCan "mo:manticore/TreasuryCanonical";
import CC "mo:manticore/CloseCanonical";
import MT "mo:manticore/MonitoringTypes";
import AlT "mo:manticore/AlertTypes";
import DC "mo:manticore/DayCount";
import PC "mo:manticore/ProductCanonical";

import CallT "CallTypes";
import CuT "CustodyTypes";
import ST "SettlementTypes";
import FT "FinancingTypes";
import VT "ValuationTypes";
import TT "mo:manticore/TreasuryTypes";

import T "DeskTypes";

module {

  public let BLOCK_DOMAIN : Text = "THEBES-DESK-BLOCK";
  public let BLOCK_VERSION : Nat8 = 1;
  public let COMMAND_DOMAIN_PREFIX : Text = "THEBES-DESK-COMMAND";
  public let COMMAND_ENCODING : Nat8 = 1;

  // ── helpers the journal's writer does not carry ──

  func wOptText(w : JC.Writer, t : ?Text) { switch (t) { case null w.byte(0); case (?x) { w.byte(1); w.text(x) } } };
  func rOptText(r : JC.Reader) : ??Text { switch (r.byte()) { case (?0) ?null; case (?1) { let ?x = r.text() else return null; ??x }; case (_) null } };
  func wTexts(w : JC.Writer, xs : [Text]) { w.len16(xs.size()); for (x in xs.vals()) w.text(x) };
  func rTexts(r : JC.Reader) : ?[Text] {
    let ?n = r.len16() else return null;
    let buf = VarArray.repeat<Text>("", n);
    var i = 0;
    while (i < n) { let ?x = r.text() else return null; buf[i] := x; i += 1 };
    ?Array.fromVarArray<Text>(buf)
  };
  func wNats(w : JC.Writer, xs : [Nat]) { w.len16(xs.size()); for (x in xs.vals()) w.nat(x) };
  func rNats(r : JC.Reader) : ?[Nat] {
    let ?n = r.len16() else return null;
    let buf = VarArray.repeat<Nat>(0, n);
    var i = 0;
    while (i < n) { let ?x = r.nat() else return null; buf[i] := x; i += 1 };
    ?Array.fromVarArray<Nat>(buf)
  };
  func wOptTexts(w : JC.Writer, xs : ?[Text]) { switch (xs) { case null w.byte(0); case (?v) { w.byte(1); wTexts(w, v) } } };
  func rOptTexts(r : JC.Reader) : ??[Text] { switch (r.byte()) { case (?0) ?null; case (?1) { let ?v = rTexts(r) else return null; ??v }; case (_) null } };
  func wMoneys(w : JC.Writer, xs : ?[AT.Money]) {
    switch (xs) { case null w.byte(0); case (?v) { w.byte(1); w.len16(v.size()); for (m in v.vals()) { w.text(m.currency); w.nat(m.amount) } } }
  };
  func rMoneys(r : JC.Reader) : ??[AT.Money] {
    switch (r.byte()) {
      case (?0) ?null;
      case (?1) {
        let ?n = r.len16() else return null;
        let buf = VarArray.repeat<AT.Money>({ currency = ""; amount = 0 }, n);
        var i = 0;
        while (i < n) { let ?currency = r.text() else return null; let ?amount = r.nat() else return null; buf[i] := { currency; amount }; i += 1 };
        ??Array.fromVarArray<AT.Money>(buf)
      };
      case (_) null;
    }
  };
  func wOptDay(w : JC.Writer, o : ?Nat) { switch (o) { case null w.byte(0); case (?x) { w.byte(1); w.nat(x) } } };
  func rOptDay(r : JC.Reader) : ??Nat { switch (r.byte()) { case (?0) ?null; case (?1) { let ?x = r.nat() else return null; ??x }; case (_) null } };

  func wInt(w : JC.Writer, i : Int) { w.byte(if (i < 0) 1 else 0); w.nat(Int.abs(i)) };
  func rInt(r : JC.Reader) : ?Int { let ?neg = r.byte() else return null; let ?m = r.nat() else return null; if (neg == 1) ?(-m) else ?m };
  func wCash(w : JC.Writer, c : { account : Text; sub : ?Text }) { w.text(c.account); wOptText(w, c.sub) };
  func rCash(r : JC.Reader) : ?{ account : Text; sub : ?Text } { let ?account = r.text() else return null; let ?sub = rOptText(r) else return null; ?{ account; sub } };
  public func wCallTerms(w : JC.Writer, t : CallT.Terms) {
    w.bool(t.placement); w.text(t.currency); w.nat(t.principal); w.nat(t.rateBps); PC.wConvention(w, t.dayCount); w.nat(t.noticeDays); w.nat(t.interestEveryDays); w.bool(t.capitalise); wCash(w, t.cash); w.nat(t.start);
  };
  public func rCallTerms(r : JC.Reader) : ?CallT.Terms {
    let ?placement = r.bool() else return null; let ?currency = r.text() else return null; let ?principal = r.nat() else return null; let ?rateBps = r.nat() else return null;
    let ?dayCount = PC.rConvention(r) else return null; let ?noticeDays = r.nat() else return null; let ?interestEveryDays = r.nat() else return null; let ?capitalise = r.bool() else return null;
    let ?cash = rCash(r) else return null; let ?start = r.nat() else return null;
    ?{ placement; currency; principal; rateBps; dayCount; noticeDays; interestEveryDays; capitalise; cash; start }
  };
  func wCallEvent(w : JC.Writer, e : CallT.Event) {
    switch (e) {
      case (#opened(x)) { w.byte(0x01); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wCallTerms(w, x.terms); w.text(x.reference); w.principal(x.trader); w.nat(x.day); w.bool(x.withinLimits); TyCan.writeOptPrincipal(w, x.approver) };
      case (#funded(x)) { w.byte(0x02); w.nat(x.call); w.nat(x.amount); w.nat(x.day) };
      case (#rateReset(x)) { w.byte(0x03); w.nat(x.call); w.nat(x.rateBps); w.nat(x.day); wInt(w, x.catchUp) };
      case (#balanceAdjusted(x)) { w.byte(0x04); w.nat(x.call); wInt(w, x.delta); w.nat(x.day); wInt(w, x.catchUp) };
      case (#noticeServed(x)) { w.byte(0x05); w.nat(x.call); w.nat(x.day); w.nat(x.repayDay) };
      case (#accrued(x)) { w.byte(0x06); w.nat(x.call); wInt(w, x.interest); w.nat(x.day) };
      case (#interestSettled(x)) { w.byte(0x07); w.nat(x.call); w.nat(x.amount); w.bool(x.capitalised); w.nat(x.day) };
      case (#repaid(x)) { w.byte(0x08); w.nat(x.call); w.nat(x.principal); w.nat(x.interest); w.nat(x.day) };
    }
  };
  func rCallEvent(r : JC.Reader) : ?CallT.Event {
    switch (r.byte()) {
      case (?0x01) {
        let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rCallTerms(r) else return null; let ?reference = r.text() else return null;
        let ?trader = r.principal() else return null; let ?day = r.nat() else return null; let ?withinLimits = r.bool() else return null; let ?approver = TyCan.readOptPrincipal(r) else return null;
        ?#opened({ book; counterparty; terms; reference; trader; day; withinLimits; approver })
      };
      case (?0x02) { let ?call = r.nat() else return null; let ?amount = r.nat() else return null; let ?day = r.nat() else return null; ?#funded({ call; amount; day }) };
      case (?0x03) { let ?call = r.nat() else return null; let ?rateBps = r.nat() else return null; let ?day = r.nat() else return null; let ?catchUp = rInt(r) else return null; ?#rateReset({ call; rateBps; day; catchUp }) };
      case (?0x04) { let ?call = r.nat() else return null; let ?delta = rInt(r) else return null; let ?day = r.nat() else return null; let ?catchUp = rInt(r) else return null; ?#balanceAdjusted({ call; delta; day; catchUp }) };
      case (?0x05) { let ?call = r.nat() else return null; let ?day = r.nat() else return null; let ?repayDay = r.nat() else return null; ?#noticeServed({ call; day; repayDay }) };
      case (?0x06) { let ?call = r.nat() else return null; let ?interest = rInt(r) else return null; let ?day = r.nat() else return null; ?#accrued({ call; interest; day }) };
      case (?0x07) { let ?call = r.nat() else return null; let ?amount = r.nat() else return null; let ?capitalised = r.bool() else return null; let ?day = r.nat() else return null; ?#interestSettled({ call; amount; capitalised; day }) };
      case (?0x08) { let ?call = r.nat() else return null; let ?principal = r.nat() else return null; let ?interest = r.nat() else return null; let ?day = r.nat() else return null; ?#repaid({ call; principal; interest; day }) };
      case (_) null;
    }
  };

  func wClass(w : JC.Writer, c : CuT.Classification) { w.byte(switch (c) { case (#sovereign) 1; case (#supranational) 2; case (#financial) 3; case (#corporate) 4 }) };
  func rClass(r : JC.Reader) : ?CuT.Classification { switch (r.byte()) { case (?1) ?#sovereign; case (?2) ?#supranational; case (?3) ?#financial; case (?4) ?#corporate; case (_) null } };
  func wExtension(w : JC.Writer, e : CuT.Extension) { w.text(e.isin); w.text(e.lei); wClass(w, e.classification); w.text(e.market); w.nat(e.settlementCycleDays); w.byte(switch (e.quotation) { case (#pricePer100) 0; case (#yield) 1 }); w.nat(e.minDenomination) };
  func rExtension(r : JC.Reader) : ?CuT.Extension {
    let ?isin = r.text() else return null; let ?lei = r.text() else return null; let ?classification = rClass(r) else return null; let ?market = r.text() else return null;
    let ?settlementCycleDays = r.nat() else return null; let quotation : CuT.Quotation = switch (r.byte()) { case (?0) #pricePer100; case (?1) #yield; case (_) return null }; let ?minDenomination = r.nat() else return null;
    ?{ isin; lei; classification; market; settlementCycleDays; quotation; minDenomination }
  };
  func wDepot(w : JC.Writer, d : CuT.Depot) { w.text(d.id); TyCan.writeCounterparty(w, d.custodian); w.text(d.place); w.text(d.safekeepingAccount) };
  func rDepot(r : JC.Reader) : ?CuT.Depot { let ?id = r.text() else return null; let ?custodian = TyCan.readCounterparty(r) else return null; let ?place = r.text() else return null; let ?safekeepingAccount = r.text() else return null; ?{ id; custodian; place; safekeepingAccount } };
  func wBasis(w : JC.Writer, b : CuT.Basis) { w.byte(switch (b) { case (#contractual) 0; case (#actual) 1 }) };
  func rBasis(r : JC.Reader) : ?CuT.Basis { switch (r.byte()) { case (?0) ?#contractual; case (?1) ?#actual; case (_) null } };
  func wKind(w : JC.Writer, k : CuT.Kind) {
    switch (k) {
      case (#coupon(x)) { w.byte(1); w.nat(x.perHundredMicro) }; case (#partialRedemption(x)) { w.byte(2); w.nat(x.ratioBps) };
      case (#earlyRedemption(x)) { w.byte(3); w.nat(x.priceMicro) }; case (#cashDistribution(x)) { w.byte(4); w.nat(x.perHundredMicro) };
    }
  };
  func rKind(r : JC.Reader) : ?CuT.Kind {
    let ?t = r.byte() else return null; let ?v = r.nat() else return null;
    switch (t) { case 1 ?#coupon({ perHundredMicro = v }); case 2 ?#partialRedemption({ ratioBps = v }); case 3 ?#earlyRedemption({ priceMicro = v }); case 4 ?#cashDistribution({ perHundredMicro = v }); case _ null }
  };
  func wAnnouncement(w : JC.Writer, a : CuT.Announcement) { w.text(a.isin); wKind(w, a.kind); w.nat(a.recordDate); w.nat(a.exDate); w.nat(a.paymentDate); w.blob(a.source) };
  func rAnnouncement(r : JC.Reader) : ?CuT.Announcement {
    let ?isin = r.text() else return null; let ?kind = rKind(r) else return null; let ?recordDate = r.nat() else return null; let ?exDate = r.nat() else return null; let ?paymentDate = r.nat() else return null; let ?source = r.blob() else return null;
    ?{ isin; kind; recordDate; exDate; paymentDate; source }
  };
  func wCustodyEvent(w : JC.Writer, e : CuT.Event) {
    switch (e) {
      case (#policySet(p)) { w.byte(0x01); wBasis(w, p.entitlementBasis) };
      case (#instrumentExtended(x)) { w.byte(0x02); wExtension(w, x.extension); w.nat(x.day) };
      case (#depotOpened(x)) { w.byte(0x03); wDepot(w, x.depot); w.nat(x.day) };
      case (#bookDepotSet(x)) { w.byte(0x04); w.text(x.book); w.text(x.depot); w.nat(x.day) };
      case (#dealDepotAssigned(x)) { w.byte(0x05); w.nat(x.deal); w.text(x.depot); w.nat(x.day) };
      case (#transferred(x)) { w.byte(0x06); w.nat(x.lot); w.text(x.from); w.text(x.to); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#announced(x)) { w.byte(0x07); wAnnouncement(w, x.announcement); w.nat(x.day) };
      case (#cancelled(x)) { w.byte(0x08); w.nat(x.action); w.text(x.reason); w.nat(x.day) };
      case (#entitlementRecorded(x)) { w.byte(0x09); w.nat(x.action); w.nat(x.lot); w.text(x.depot); w.nat(x.nominal); w.nat(x.amount); wBasis(w, x.basis); w.nat(x.day) };
      case (#entitled(x)) { w.byte(0x0A); w.nat(x.action); w.nat(x.lots); w.nat(x.total); w.nat(x.day) };
      case (#entitlementPaid(x)) { w.byte(0x0B); w.nat(x.action); w.nat(x.lot); w.nat(x.amount); w.nat(x.nominal); wInt(w, x.realised); w.nat(x.day) };
      case (#paid(x)) { w.byte(0x0C); w.nat(x.action); w.nat(x.lots); w.nat(x.total); w.nat(x.day) };
      case (#entitlementClaimed(x)) { w.byte(0x0D); w.nat(x.action); w.nat(x.lot); w.nat(x.amount); wInt(w, x.accrued); w.nat(x.day) };
      case (#pledged(x)) { w.byte(0x0E); w.nat(x.lot); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#released(x)) { w.byte(0x0F); w.nat(x.lot); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#lent(x)) { w.byte(0x10); w.nat(x.lot); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#lentReturned(x)) { w.byte(0x11); w.nat(x.lot); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#collateralReceived(x)) { w.byte(0x12); w.text(x.isin); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
      case (#collateralReturned(x)) { w.byte(0x13); w.text(x.isin); w.text(x.depot); w.nat(x.nominal); w.text(x.reference); w.nat(x.day) };
    }
  };
  func rCustodyEvent(r : JC.Reader) : ?CuT.Event {
    switch (r.byte()) {
      case (?0x01) { let ?entitlementBasis = rBasis(r) else return null; ?#policySet({ entitlementBasis }) };
      case (?0x02) { let ?extension = rExtension(r) else return null; let ?day = r.nat() else return null; ?#instrumentExtended({ extension; day }) };
      case (?0x03) { let ?depot = rDepot(r) else return null; let ?day = r.nat() else return null; ?#depotOpened({ depot; day }) };
      case (?0x04) { let ?book = r.text() else return null; let ?depot = r.text() else return null; let ?day = r.nat() else return null; ?#bookDepotSet({ book; depot; day }) };
      case (?0x05) { let ?deal = r.nat() else return null; let ?depot = r.text() else return null; let ?day = r.nat() else return null; ?#dealDepotAssigned({ deal; depot; day }) };
      case (?0x06) { let ?lot = r.nat() else return null; let ?from = r.text() else return null; let ?to = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#transferred({ lot; from; to; nominal; reference; day }) };
      case (?0x07) { let ?announcement = rAnnouncement(r) else return null; let ?day = r.nat() else return null; ?#announced({ announcement; day }) };
      case (?0x08) { let ?action = r.nat() else return null; let ?reason = r.text() else return null; let ?day = r.nat() else return null; ?#cancelled({ action; reason; day }) };
      case (?0x09) {
        let ?action = r.nat() else return null; let ?lot = r.nat() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?amount = r.nat() else return null;
        let ?basis = rBasis(r) else return null; let ?day = r.nat() else return null;
        ?#entitlementRecorded({ action; lot; depot; nominal; amount; basis; day })
      };
      case (?0x0A) { let ?action = r.nat() else return null; let ?lots = r.nat() else return null; let ?total = r.nat() else return null; let ?day = r.nat() else return null; ?#entitled({ action; lots; total; day }) };
      case (?0x0B) { let ?action = r.nat() else return null; let ?lot = r.nat() else return null; let ?amount = r.nat() else return null; let ?nominal = r.nat() else return null; let ?realised = rInt(r) else return null; let ?day = r.nat() else return null; ?#entitlementPaid({ action; lot; amount; nominal; realised; day }) };
      case (?0x0C) { let ?action = r.nat() else return null; let ?lots = r.nat() else return null; let ?total = r.nat() else return null; let ?day = r.nat() else return null; ?#paid({ action; lots; total; day }) };
      case (?0x0D) { let ?action = r.nat() else return null; let ?lot = r.nat() else return null; let ?amount = r.nat() else return null; let ?accrued = rInt(r) else return null; let ?day = r.nat() else return null; ?#entitlementClaimed({ action; lot; amount; accrued; day }) };
      case (?0x0E) { let ?lot = r.nat() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#pledged({ lot; depot; nominal; reference; day }) };
      case (?0x0F) { let ?lot = r.nat() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#released({ lot; depot; nominal; reference; day }) };
      case (?0x10) { let ?lot = r.nat() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#lent({ lot; depot; nominal; reference; day }) };
      case (?0x11) { let ?lot = r.nat() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#lentReturned({ lot; depot; nominal; reference; day }) };
      case (?0x12) { let ?isin = r.text() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#collateralReceived({ isin; depot; nominal; reference; day }) };
      case (?0x13) { let ?isin = r.text() else return null; let ?depot = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; let ?day = r.nat() else return null; ?#collateralReturned({ isin; depot; nominal; reference; day }) };
      case (_) null;
    }
  };

  // ─── settlement ───
  func wVenue(w : JC.Writer, v : ST.Venue) { w.principal(v.core); w.nat(v.deadlineSecs); w.nat(v.recycleLimit); w.text(v.claimsAccount) };
  func rVenue(r : JC.Reader) : ?ST.Venue { let ?core = r.principal() else return null; let ?deadlineSecs = r.nat() else return null; let ?recycleLimit = r.nat() else return null; let ?claimsAccount = r.text() else return null; ?{ core; deadlineSecs; recycleLimit; claimsAccount } };
  func wLedgerRole(w : JC.Writer, x : ST.LedgerRole) { switch (x) { case (#cash(c)) { w.byte(1); w.text(c.currency) }; case (#security(i)) { w.byte(2); w.text(i.isin) } } };
  func rLedgerRole(r : JC.Reader) : ?ST.LedgerRole { switch (r.byte()) { case (?1) { let ?currency = r.text() else return null; ?#cash({ currency }) }; case (?2) { let ?isin = r.text() else return null; ?#security({ isin }) }; case (_) null } };
  func wDeclaration(w : JC.Writer, d : ST.LedgerDeclaration) { wLedgerRole(w, d.role); w.principal(d.ledger); w.bool(d.partial) };
  func rDeclaration(r : JC.Reader) : ?ST.LedgerDeclaration { let ?role = rLedgerRole(r) else return null; let ?ledger = r.principal() else return null; let ?partial = r.bool() else return null; ?{ role; ledger; partial } };
  func wCycle(w : JC.Writer, c : ST.Cycle) { w.nat(c.businessDate); w.text(c.market); w.text(c.priceSource) };
  func rCycle(r : JC.Reader) : ?ST.Cycle { let ?businessDate = r.nat() else return null; let ?market = r.text() else return null; let ?priceSource = r.text() else return null; ?{ businessDate; market; priceSource } };
  func wRole(w : JC.Writer, x : ST.Role) { w.byte(switch (x) { case (#maker) 1; case (#taker) 2 }) };
  func rRole(r : JC.Reader) : ?ST.Role { switch (r.byte()) { case (?1) ?#maker; case (?2) ?#taker; case (_) null } };
  func wFamily(w : JC.Writer, f : ST.Family) { w.byte(switch (f) { case (#treasury) 0; case (#repo) 1; case (#loan) 2 }) };
  func rFamily(r : JC.Reader) : ?ST.Family { switch (r.byte()) { case (?0) ?#treasury; case (?1) ?#repo; case (?2) ?#loan; case (_) null } };
  func wInstruction(w : JC.Writer, i : ST.Instruction) {
    wFamily(w, i.family); w.nat(i.deal); w.nat(i.leg); w.nat(i.cycle); wRole(w, i.role); w.principal(i.counterparty); w.principal(i.assetLedger); w.nat(i.assetAmount); w.principal(i.cashLedger); w.nat(i.cashAmount);
    w.optNat(i.tradeId); w.text(i.reference); w.blob(i.documentHash);
  };
  func rInstruction(r : JC.Reader) : ?ST.Instruction {
    let ?family = rFamily(r) else return null; let ?deal = r.nat() else return null; let ?leg = r.nat() else return null; let ?cycle = r.nat() else return null; let ?role = rRole(r) else return null; let ?counterparty = r.principal() else return null;
    let ?assetLedger = r.principal() else return null; let ?assetAmount = r.nat() else return null; let ?cashLedger = r.principal() else return null; let ?cashAmount = r.nat() else return null;
    let ?tradeId = r.optNat() else return null; let ?reference = r.text() else return null; let ?documentHash = r.blob() else return null;
    ?{ family; deal; leg; cycle; role; counterparty; assetLedger; assetAmount; cashLedger; cashAmount; tradeId; reference; documentHash }
  };
  // ─── financing ───
  func wCollateral(w : JC.Writer, c : FT.Collateral) { w.text(c.isin); w.nat(c.nominal) };
  func rCollateral(r : JC.Reader) : ?FT.Collateral { let ?isin = r.text() else return null; let ?nominal = r.nat() else return null; ?{ isin; nominal } };
  func wOptCollateral(w : JC.Writer, c : ?FT.Collateral) { switch (c) { case null w.byte(0); case (?x) { w.byte(1); wCollateral(w, x) } } };
  func rOptCollateral(r : JC.Reader) : ??FT.Collateral { switch (r.byte()) { case (?0) ?null; case (?1) { let ?c = rCollateral(r) else return null; ??c }; case (_) null } };
  func wLots(w : JC.Writer, xs : [(Nat, Nat)]) { w.nat(xs.size()); for ((a, b) in xs.vals()) { w.nat(a); w.nat(b) } };
  func rLots(r : JC.Reader) : ?[(Nat, Nat)] { let ?n = r.nat() else return null; if (n > 10_000) return null; let out = List.empty<(Nat, Nat)>(); var i = 0; while (i < n) { let ?a = r.nat() else return null; let ?b = r.nat() else return null; List.add(out, (a, b)); i += 1 }; ?List.toArray(out) };
  func wFinancingPolicy(w : JC.Writer, p : FT.Policy) {
    for (a in [p.repoPayable, p.reverseRepoReceivable, p.repoInterestPayable, p.repoInterestReceivable, p.repoInterestExpense, p.repoInterestIncome, p.marginCashGiven, p.marginCashReceived, p.lendingFeeReceivable, p.lendingFeeIncome, p.cashCollateralPayable, p.rebateExpense, p.manufacturedPaymentReceivable].vals()) w.text(a);
    w.nat(p.marginGraceDays);
  };
  func rFinancingPolicy(r : JC.Reader) : ?FT.Policy {
    let ?repoPayable = r.text() else return null; let ?reverseRepoReceivable = r.text() else return null; let ?repoInterestPayable = r.text() else return null; let ?repoInterestReceivable = r.text() else return null;
    let ?repoInterestExpense = r.text() else return null; let ?repoInterestIncome = r.text() else return null; let ?marginCashGiven = r.text() else return null; let ?marginCashReceived = r.text() else return null;
    let ?lendingFeeReceivable = r.text() else return null; let ?lendingFeeIncome = r.text() else return null; let ?cashCollateralPayable = r.text() else return null; let ?rebateExpense = r.text() else return null;
    let ?manufacturedPaymentReceivable = r.text() else return null; let ?marginGraceDays = r.nat() else return null;
    ?{ repoPayable; reverseRepoReceivable; repoInterestPayable; repoInterestReceivable; repoInterestExpense; repoInterestIncome; marginCashGiven; marginCashReceived; lendingFeeReceivable; lendingFeeIncome; cashCollateralPayable; rebateExpense; manufacturedPaymentReceivable; marginGraceDays }
  };
  func wRepoTerms(w : JC.Writer, t : FT.RepoTerms) {
    w.bool(t.reverse); w.text(t.currency); w.nat(t.cash); w.nat(t.rateBps); PC.wConvention(w, t.dayCount); w.nat(t.start); w.optNat(t.maturity); wCollateral(w, t.collateral); w.nat(t.haircutBps); w.nat(t.thresholdBps); wCash(w, t.cashAccount); w.text(t.depot);
  };
  func rRepoTerms(r : JC.Reader) : ?FT.RepoTerms {
    let ?reverse = r.bool() else return null; let ?currency = r.text() else return null; let ?cash = r.nat() else return null; let ?rateBps = r.nat() else return null; let ?dayCount = PC.rConvention(r) else return null;
    let ?start = r.nat() else return null; let ?maturity = r.optNat() else return null; let ?collateral = rCollateral(r) else return null; let ?haircutBps = r.nat() else return null; let ?thresholdBps = r.nat() else return null;
    let ?cashAccount = rCash(r) else return null; let ?depot = r.text() else return null;
    ?{ reverse; currency; cash; rateBps; dayCount; start; maturity; collateral; haircutBps; thresholdBps; cashAccount; depot }
  };
  func wLoanTerms(w : JC.Writer, t : FT.LoanTerms) {
    w.text(t.isin); w.nat(t.nominal); w.text(t.currency); w.nat(t.valueMicro); w.nat(t.feeBps); PC.wConvention(w, t.dayCount);
    switch (t.collateral) { case (#cash(c)) { w.byte(1); w.nat(c.amount); w.nat(c.rebateBps) }; case (#securities(c)) { w.byte(2); wCollateral(w, c) } };
    w.nat(t.start); w.nat(t.noticeDays); wCash(w, t.cashAccount); w.text(t.depot);
  };
  func rLoanTerms(r : JC.Reader) : ?FT.LoanTerms {
    let ?isin = r.text() else return null; let ?nominal = r.nat() else return null; let ?currency = r.text() else return null; let ?valueMicro = r.nat() else return null; let ?feeBps = r.nat() else return null; let ?dayCount = PC.rConvention(r) else return null;
    let collateral : FT.LoanCollateral = switch (r.byte()) {
      case (?1) { let ?amount = r.nat() else return null; let ?rebateBps = r.nat() else return null; #cash({ amount; rebateBps }) };
      case (?2) { let ?c = rCollateral(r) else return null; #securities(c) };
      case (_) return null;
    };
    let ?start = r.nat() else return null; let ?noticeDays = r.nat() else return null; let ?cashAccount = rCash(r) else return null; let ?depot = r.text() else return null;
    ?{ isin; nominal; currency; valueMicro; feeBps; dayCount; collateral; start; noticeDays; cashAccount; depot }
  };
  // ─── valuation ───
  func wHedgeKind(w : JC.Writer, k : VT.HedgeKind) { switch (k) { case (#cashFlow(c)) { w.byte(1); w.nat(c.hedgedAmount) }; case (#fairValue) w.byte(2) } };
  func rHedgeKind(r : JC.Reader) : ?VT.HedgeKind { switch (r.byte()) { case (?1) { let ?hedgedAmount = r.nat() else return null; ?#cashFlow({ hedgedAmount }) }; case (?2) ?#fairValue; case (_) null } };
  func wOptIrs(w : JC.Writer, i : ?TT.Irs) { switch (i) { case null w.byte(0); case (?x) { w.byte(1); TyCan.writeKind(w, #irs(x)) } } };
  func rOptIrs(r : JC.Reader) : ??TT.Irs { switch (r.byte()) { case (?0) ?null; case (?1) { switch (TyCan.readKind(r)) { case (?#irs(x)) ??x; case (_) null } }; case (_) null } };
  func wValuationEvent(w : JC.Writer, e : VT.Event) {
    switch (e) {
      case (#policySet(p)) { w.byte(0x01); w.text(p.hedgeReserve) };
      case (#yieldQuoted(x)) { w.byte(0x02); w.text(x.isin); w.nat(x.day); w.nat(x.yieldBps); w.nat(x.priceMicro) };
      case (#thetaRecorded(x)) { w.byte(0x03); w.nat(x.deal); w.nat(x.day); wInt(w, x.theta) };
      case (#hedgeDesignated(x)) { w.byte(0x04); w.nat(x.hedging); w.nat(x.hedged); wHedgeKind(w, x.kind); wOptIrs(w, x.hypothetical); wInt(w, x.hedgingMark); wInt(w, x.hedgedValue); w.nat(x.day) };
      case (#hedgeAssessed(x)) { w.byte(0x05); w.nat(x.hedge); wInt(w, x.hedgingChange); wInt(w, x.hedgedChange); w.nat(x.effectivenessBps); wInt(w, x.effective); wInt(w, x.ineffective); w.nat(x.day) };
      case (#hedgeDedesignated(x)) { w.byte(0x06); w.nat(x.hedge); wInt(w, x.reclassified); w.nat(x.day) };
    }
  };
  func rValuationEvent(r : JC.Reader) : ?VT.Event {
    switch (r.byte()) {
      case (?0x01) { let ?hedgeReserve = r.text() else return null; ?#policySet({ hedgeReserve }) };
      case (?0x02) { let ?isin = r.text() else return null; let ?day = r.nat() else return null; let ?yieldBps = r.nat() else return null; let ?priceMicro = r.nat() else return null; ?#yieldQuoted({ isin; day; yieldBps; priceMicro }) };
      case (?0x03) { let ?deal = r.nat() else return null; let ?day = r.nat() else return null; let ?theta = rInt(r) else return null; ?#thetaRecorded({ deal; day; theta }) };
      case (?0x04) { let ?hedging = r.nat() else return null; let ?hedged = r.nat() else return null; let ?kind = rHedgeKind(r) else return null; let ?hypothetical = rOptIrs(r) else return null; let ?hedgingMark = rInt(r) else return null; let ?hedgedValue = rInt(r) else return null; let ?day = r.nat() else return null; ?#hedgeDesignated({ hedging; hedged; kind; hypothetical; hedgingMark; hedgedValue; day }) };
      case (?0x05) { let ?hedge = r.nat() else return null; let ?hedgingChange = rInt(r) else return null; let ?hedgedChange = rInt(r) else return null; let ?effectivenessBps = r.nat() else return null; let ?effective = rInt(r) else return null; let ?ineffective = rInt(r) else return null; let ?day = r.nat() else return null; ?#hedgeAssessed({ hedge; hedgingChange; hedgedChange; effectivenessBps; effective; ineffective; day }) };
      case (?0x06) { let ?hedge = r.nat() else return null; let ?reclassified = rInt(r) else return null; let ?day = r.nat() else return null; ?#hedgeDedesignated({ hedge; reclassified; day }) };
      case (_) null;
    }
  };
  func wPayer(w : JC.Writer, p : FT.MarginPayer) { w.byte(switch (p) { case (#desk) 1; case (#counterparty) 2 }) };
  func rPayer(r : JC.Reader) : ?FT.MarginPayer { switch (r.byte()) { case (?1) ?#desk; case (?2) ?#counterparty; case (_) null } };
  func wFinancingEvent(w : JC.Writer, e : FT.Event) {
    switch (e) {
      case (#policySet(p)) { w.byte(0x01); wFinancingPolicy(w, p) };
      case (#repoOpened(x)) { w.byte(0x02); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wRepoTerms(w, x.terms); w.text(x.reference); w.principal(x.trader); w.nat(x.day) };
      case (#repoStarted(x)) { w.byte(0x03); w.nat(x.repo); wLots(w, x.lots); w.nat(x.day) };
      case (#repoAccrued(x)) { w.byte(0x04); w.nat(x.repo); wInt(w, x.interest); w.nat(x.day) };
      case (#repoRateReset(x)) { w.byte(0x05); w.nat(x.repo); w.nat(x.rateBps); w.nat(x.day); wInt(w, x.catchUp) };
      case (#collateralMarked(x)) { w.byte(0x06); w.nat(x.repo); w.nat(x.value); w.nat(x.exposure); w.nat(x.priceMicro); w.nat(x.day) };
      case (#marginCallRaised(x)) { w.byte(0x07); w.nat(x.repo); w.nat(x.amount); wPayer(w, x.payer); w.nat(x.day); w.nat(x.due) };
      case (#marginMet(x)) { w.byte(0x08); w.nat(x.repo); w.nat(x.cash); wOptCollateral(w, x.collateral); wLots(w, x.lots); wPayer(w, x.payer); w.nat(x.day) };
      case (#collateralSubstituted(x)) { w.byte(0x09); w.nat(x.repo); wCollateral(w, x.out); wCollateral(w, x.in_); wLots(w, x.outLots); wLots(w, x.inLots); w.nat(x.day) };
      case (#repoClosed(x)) { w.byte(0x0A); w.nat(x.repo); w.nat(x.principal); w.nat(x.interest); wInt(w, x.marginReturned); w.nat(x.day) };
      case (#loanOpened(x)) { w.byte(0x0B); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wLoanTerms(w, x.terms); w.text(x.reference); w.principal(x.trader); w.nat(x.day) };
      case (#loanStarted(x)) { w.byte(0x0C); w.nat(x.loan); wLots(w, x.lots); w.nat(x.day) };
      case (#loanAccrued(x)) { w.byte(0x0D); w.nat(x.loan); wInt(w, x.fee); wInt(w, x.rebate); w.nat(x.day) };
      case (#loanRecalled(x)) { w.byte(0x0E); w.nat(x.loan); w.nat(x.day); w.nat(x.returnDay) };
      case (#loanReturned(x)) { w.byte(0x0F); w.nat(x.loan); w.nat(x.fee); w.nat(x.rebate); w.nat(x.day) };
      case (#manufacturedPayment(x)) { w.byte(0x10); w.nat(x.loan); w.nat(x.action); w.nat(x.lot); w.nat(x.amount); w.nat(x.day) };
    }
  };
  func rFinancingEvent(r : JC.Reader) : ?FT.Event {
    switch (r.byte()) {
      case (?0x01) { let ?p = rFinancingPolicy(r) else return null; ?#policySet(p) };
      case (?0x02) { let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rRepoTerms(r) else return null; let ?reference = r.text() else return null; let ?trader = r.principal() else return null; let ?day = r.nat() else return null; ?#repoOpened({ book; counterparty; terms; reference; trader; day }) };
      case (?0x03) { let ?repo = r.nat() else return null; let ?lots = rLots(r) else return null; let ?day = r.nat() else return null; ?#repoStarted({ repo; lots; day }) };
      case (?0x04) { let ?repo = r.nat() else return null; let ?interest = rInt(r) else return null; let ?day = r.nat() else return null; ?#repoAccrued({ repo; interest; day }) };
      case (?0x05) { let ?repo = r.nat() else return null; let ?rateBps = r.nat() else return null; let ?day = r.nat() else return null; let ?catchUp = rInt(r) else return null; ?#repoRateReset({ repo; rateBps; day; catchUp }) };
      case (?0x06) { let ?repo = r.nat() else return null; let ?value = r.nat() else return null; let ?exposure = r.nat() else return null; let ?priceMicro = r.nat() else return null; let ?day = r.nat() else return null; ?#collateralMarked({ repo; value; exposure; priceMicro; day }) };
      case (?0x07) { let ?repo = r.nat() else return null; let ?amount = r.nat() else return null; let ?payer = rPayer(r) else return null; let ?day = r.nat() else return null; let ?due = r.nat() else return null; ?#marginCallRaised({ repo; amount; payer; day; due }) };
      case (?0x08) { let ?repo = r.nat() else return null; let ?cash = r.nat() else return null; let ?collateral = rOptCollateral(r) else return null; let ?lots = rLots(r) else return null; let ?payer = rPayer(r) else return null; let ?day = r.nat() else return null; ?#marginMet({ repo; cash; collateral; lots; payer; day }) };
      case (?0x09) { let ?repo = r.nat() else return null; let ?out = rCollateral(r) else return null; let ?in_ = rCollateral(r) else return null; let ?outLots = rLots(r) else return null; let ?inLots = rLots(r) else return null; let ?day = r.nat() else return null; ?#collateralSubstituted({ repo; out; in_; outLots; inLots; day }) };
      case (?0x0A) { let ?repo = r.nat() else return null; let ?principal = r.nat() else return null; let ?interest = r.nat() else return null; let ?marginReturned = rInt(r) else return null; let ?day = r.nat() else return null; ?#repoClosed({ repo; principal; interest; marginReturned; day }) };
      case (?0x0B) { let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rLoanTerms(r) else return null; let ?reference = r.text() else return null; let ?trader = r.principal() else return null; let ?day = r.nat() else return null; ?#loanOpened({ book; counterparty; terms; reference; trader; day }) };
      case (?0x0C) { let ?loan = r.nat() else return null; let ?lots = rLots(r) else return null; let ?day = r.nat() else return null; ?#loanStarted({ loan; lots; day }) };
      case (?0x0D) { let ?loan = r.nat() else return null; let ?fee = rInt(r) else return null; let ?rebate = rInt(r) else return null; let ?day = r.nat() else return null; ?#loanAccrued({ loan; fee; rebate; day }) };
      case (?0x0E) { let ?loan = r.nat() else return null; let ?day = r.nat() else return null; let ?returnDay = r.nat() else return null; ?#loanRecalled({ loan; day; returnDay }) };
      case (?0x0F) { let ?loan = r.nat() else return null; let ?fee = r.nat() else return null; let ?rebate = r.nat() else return null; let ?day = r.nat() else return null; ?#loanReturned({ loan; fee; rebate; day }) };
      case (?0x10) { let ?loan = r.nat() else return null; let ?action = r.nat() else return null; let ?lot = r.nat() else return null; let ?amount = r.nat() else return null; let ?day = r.nat() else return null; ?#manufacturedPayment({ loan; action; lot; amount; day }) };
      case (_) null;
    }
  };
  func wBlobs(w : JC.Writer, xs : [Blob]) { w.nat(xs.size()); for (x in xs.vals()) w.blob(x) };
  func rBlobs(r : JC.Reader) : ?[Blob] { let ?n = r.nat() else return null; if (n > 100_000) return null; let out = List.empty<Blob>(); var i = 0; while (i < n) { let ?b = r.blob() else return null; List.add(out, b); i += 1 }; ?List.toArray(out) };
  func wSettlementEvent(w : JC.Writer, e : ST.Event) {
    switch (e) {
      case (#venueSet(v)) { w.byte(0x01); wVenue(w, v) };
      case (#ledgerSet(d)) { w.byte(0x02); wDeclaration(w, d) };
      case (#cycleOpened(x)) { w.byte(0x03); wCycle(w, x.cycle); w.nat(x.day) };
      case (#cycleClosed(x)) { w.byte(0x04); w.nat(x.businessDate); w.nat(x.settled); w.nat(x.failed); w.nat(x.pending); w.nat(x.day) };
      case (#instructed(x)) { w.byte(0x05); wInstruction(w, x.instruction); w.nat(x.day) };
      case (#tradeOpened(x)) { w.byte(0x06); w.nat(x.instruction); w.nat(x.tradeId); w.bool(x.escrowed); w.text(x.note); w.nat(x.day) };
      case (#tradeVerified(x)) { w.byte(0x07); w.nat(x.instruction); w.nat(x.tradeId); w.nat(x.day) };
      case (#fundingRecorded(x)) { w.byte(0x08); w.nat(x.instruction); w.nat(x.tradeId); w.bool(x.escrowed); w.bool(x.bothEscrowed); w.text(x.note); w.nat(x.day) };
      case (#callRefused(x)) { w.byte(0x09); w.nat(x.instruction); w.text(x.step); w.text(x.reason); w.nat(x.day) };
      case (#auditSynced(x)) { w.byte(0x0A); w.nat(x.from); wBlobs(w, x.leaves); w.blob(x.root); w.nat(x.day) };
      case (#receiptVerified(x)) { w.byte(0x0B); w.nat(x.instruction); w.nat(x.tradeId); w.nat(x.seq); w.blob(x.leaf); w.blob(x.root); w.nat(x.assetPaid); w.nat(x.cashPaid); w.nat(x.day) };
      case (#settled(x)) { w.byte(0x0C); w.nat(x.instruction); w.nat(x.tradeId); w.nat(x.day) };
      case (#failed(x)) { w.byte(0x0D); w.nat(x.instruction); w.text(x.cause); w.nat(x.fails); w.nat(x.day) };
      case (#recycled(x)) { w.byte(0x0E); w.nat(x.instruction); w.nat(x.cycle); w.nat(x.fails); w.nat(x.day) };
      case (#reclaimed(x)) { w.byte(0x0F); w.nat(x.instruction); w.nat(x.tradeId); w.text(x.note); w.nat(x.day) };
      case (#tradeReset(x)) { w.byte(0x10); w.nat(x.instruction); w.nat(x.previous); w.nat(x.day) };
      case (#boughtIn(x)) { w.byte(0x11); w.nat(x.instruction); w.nat(x.replacement); w.nat(x.claim); w.nat(x.day) };
      case (#cancelled(x)) { w.byte(0x12); w.nat(x.instruction); w.blob(x.ourConsent); w.blob(x.theirConsent); w.text(x.reason); w.nat(x.day) };
      case (#statusReceived(x)) { w.byte(0x13); w.nat(x.instruction); w.text(x.status); w.nat(x.quantity); w.nat(x.amount); w.bool(x.matched); w.blob(x.documentHash); w.nat(x.day) };
      case (#split(x)) { w.byte(0x14); w.nat(x.deal); wNats(w, x.parts); w.nat(x.day) };
      case (#tradeAssigned(x)) { w.byte(0x15); w.nat(x.instruction); w.nat(x.tradeId); w.nat(x.day) };
    }
  };
  func rSettlementEvent(r : JC.Reader) : ?ST.Event {
    switch (r.byte()) {
      case (?0x01) { let ?v = rVenue(r) else return null; ?#venueSet(v) };
      case (?0x02) { let ?d = rDeclaration(r) else return null; ?#ledgerSet(d) };
      case (?0x03) { let ?cycle = rCycle(r) else return null; let ?day = r.nat() else return null; ?#cycleOpened({ cycle; day }) };
      case (?0x04) { let ?businessDate = r.nat() else return null; let ?settled = r.nat() else return null; let ?failed = r.nat() else return null; let ?pending = r.nat() else return null; let ?day = r.nat() else return null; ?#cycleClosed({ businessDate; settled; failed; pending; day }) };
      case (?0x05) { let ?instruction = rInstruction(r) else return null; let ?day = r.nat() else return null; ?#instructed({ instruction; day }) };
      case (?0x06) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?escrowed = r.bool() else return null; let ?note = r.text() else return null; let ?day = r.nat() else return null; ?#tradeOpened({ instruction; tradeId; escrowed; note; day }) };
      case (?0x07) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?day = r.nat() else return null; ?#tradeVerified({ instruction; tradeId; day }) };
      case (?0x08) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?escrowed = r.bool() else return null; let ?bothEscrowed = r.bool() else return null; let ?note = r.text() else return null; let ?day = r.nat() else return null; ?#fundingRecorded({ instruction; tradeId; escrowed; bothEscrowed; note; day }) };
      case (?0x09) { let ?instruction = r.nat() else return null; let ?step = r.text() else return null; let ?reason = r.text() else return null; let ?day = r.nat() else return null; ?#callRefused({ instruction; step; reason; day }) };
      case (?0x0A) { let ?from = r.nat() else return null; let ?leaves = rBlobs(r) else return null; let ?root = r.blob() else return null; let ?day = r.nat() else return null; ?#auditSynced({ from; leaves; root; day }) };
      case (?0x0B) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?seq = r.nat() else return null; let ?leaf = r.blob() else return null; let ?root = r.blob() else return null; let ?assetPaid = r.nat() else return null; let ?cashPaid = r.nat() else return null; let ?day = r.nat() else return null; ?#receiptVerified({ instruction; tradeId; seq; leaf; root; assetPaid; cashPaid; day }) };
      case (?0x0C) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?day = r.nat() else return null; ?#settled({ instruction; tradeId; day }) };
      case (?0x0D) { let ?instruction = r.nat() else return null; let ?cause = r.text() else return null; let ?fails = r.nat() else return null; let ?day = r.nat() else return null; ?#failed({ instruction; cause; fails; day }) };
      case (?0x0E) { let ?instruction = r.nat() else return null; let ?cycle = r.nat() else return null; let ?fails = r.nat() else return null; let ?day = r.nat() else return null; ?#recycled({ instruction; cycle; fails; day }) };
      case (?0x0F) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?note = r.text() else return null; let ?day = r.nat() else return null; ?#reclaimed({ instruction; tradeId; note; day }) };
      case (?0x10) { let ?instruction = r.nat() else return null; let ?previous = r.nat() else return null; let ?day = r.nat() else return null; ?#tradeReset({ instruction; previous; day }) };
      case (?0x11) { let ?instruction = r.nat() else return null; let ?replacement = r.nat() else return null; let ?claim = r.nat() else return null; let ?day = r.nat() else return null; ?#boughtIn({ instruction; replacement; claim; day }) };
      case (?0x12) { let ?instruction = r.nat() else return null; let ?ourConsent = r.blob() else return null; let ?theirConsent = r.blob() else return null; let ?reason = r.text() else return null; let ?day = r.nat() else return null; ?#cancelled({ instruction; ourConsent; theirConsent; reason; day }) };
      case (?0x13) { let ?instruction = r.nat() else return null; let ?status = r.text() else return null; let ?quantity = r.nat() else return null; let ?amount = r.nat() else return null; let ?matched = r.bool() else return null; let ?documentHash = r.blob() else return null; let ?day = r.nat() else return null; ?#statusReceived({ instruction; status; quantity; amount; matched; documentHash; day }) };
      case (?0x14) { let ?deal = r.nat() else return null; let ?parts = rNats(r) else return null; let ?day = r.nat() else return null; ?#split({ deal; parts; day }) };
      case (?0x15) { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; let ?day = r.nat() else return null; ?#tradeAssigned({ instruction; tradeId; day }) };
      case (_) null;
    }
  };

  public func writeScope(w : JC.Writer, s : AT.Scope) { wOptTexts(w, s.partitions); wOptTexts(w, s.currencies); wMoneys(w, s.ceiling); wMoneys(w, s.dailyLimit) };
  public func readScope(r : JC.Reader) : ?AT.Scope {
    let ?partitions = rOptTexts(r) else return null; let ?currencies = rOptTexts(r) else return null;
    let ?ceiling = rMoneys(r) else return null; let ?dailyLimit = rMoneys(r) else return null;
    ?{ partitions; currencies; ceiling; dailyLimit }
  };
  func wPolicy(w : JC.Writer, p : AT.DualPolicy) { w.text(p.permission); w.nat(p.required); w.text(p.eligibleRole); w.nat(p.ttlSeconds) };
  func rPolicy(r : JC.Reader) : ?AT.DualPolicy {
    let ?permission = r.text() else return null; let ?required = r.nat() else return null; let ?eligibleRole = r.text() else return null; let ?ttlSeconds = r.nat() else return null;
    ?{ permission; required; eligibleRole; ttlSeconds }
  };
  func wIdentity(w : JC.Writer, i : T.Identity) { w.text(i.name); w.text(i.bic); w.text(i.lei) };
  func rIdentity(r : JC.Reader) : ?T.Identity { let ?name = r.text() else return null; let ?bic = r.text() else return null; let ?lei = r.text() else return null; ?{ name; bic; lei } };
  func wDates(w : JC.Writer, postingDate : Nat, valueDate : Nat, period : Text, narration : Text) { w.nat(postingDate); w.nat(valueDate); w.text(period); w.text(narration) };
  func rDates(r : JC.Reader) : ?(Nat, Nat, Text, Text) {
    let ?a = r.nat() else return null; let ?b = r.nat() else return null; let ?c = r.text() else return null; let ?d = r.text() else return null; ?(a, b, c, d)
  };

  // ═══════════════════════════════════════════════════════
  //  COMMANDS
  // ═══════════════════════════════════════════════════════

  /// Version 1. The treasury tags are Manticore's second bytes (0x20 to 0x2C) so the bodies decode with its decoder.
  public func writeCommand(w : JC.Writer, c : T.Command) {
    switch (c) {
      case (#openBook(x)) { w.byte(0x01); w.text(x.id); w.text(x.name); wOptText(w, x.parent); w.bool(x.sharia) };
      case (#closeBook(x)) { w.byte(0x02); w.text(x.id) };
      case (#defineRole(x)) { w.byte(0x03); w.text(x.id); w.text(x.name); wTexts(w, x.permissions) };
      case (#grantRole(x)) { w.byte(0x04); w.principal(x.subject); w.text(x.role); writeScope(w, x.scope) };
      case (#revokeRole(x)) { w.byte(0x05); w.principal(x.subject); w.text(x.role) };
      case (#setDualPolicy(p)) { w.byte(0x06); wPolicy(w, p) };
      case (#clearDualPolicy(x)) { w.byte(0x07); w.text(x.permission) };
      case (#setFeatureActivation(x)) { w.byte(0x08); w.text(x.feature); w.nat64(x.height) };
      case (#setDeskIdentity(i)) { w.byte(0x09); wIdentity(w, i) };
      case (#journalRegisterCurrency(x)) { w.byte(0x10); w.text(x.code); w.nat8(x.minorUnits) };
      case (#journalOpenAccount(x)) { w.byte(0x11); w.text(x.code); w.text(x.name); w.side(x.normalSide); w.category(x.category); w.constraint(x.constraint) };
      case (#journalCloseAccount(x)) { w.byte(0x12); w.text(x.code) };
      case (#journalOpenPeriod(x)) { w.byte(0x13); w.text(x.id); w.nat(x.start); w.nat(x.end) };
      case (#journalClosePeriod(x)) { w.byte(0x14); w.text(x.id) };
      case (#journalSetCalendar(x)) { w.byte(0x15); w.calendar(x.calendar) };
      case (#journalSetCalendarAuthority(x)) { w.byte(0x16); w.calendarAuthority(x.authority); w.nat(x.maxRollDays); wOptDay(w, x.businessDate) };
      case (#journalRollBusinessDate(x)) { w.byte(0x17); w.nat(x.day) };
      case (#journalSetActivationHeight(x)) { w.byte(0x18); w.nat64(x.height) };
      case (#setFunctionalCurrency(x)) { w.byte(0x30); w.text(x.currency) };
      case (#setFxPair(x)) { w.byte(0x31); CC.wPair(w, x.pair) };
      case (#setFxRate(x)) { w.byte(0x32); CC.wRate(w, x.rate) };
      case (#recordRateFixing(x)) { w.byte(0x33); w.text(x.index); w.nat(x.day); w.nat(x.rateBps) };
      case (#openEndOfDay(x)) { w.byte(0x34); w.text(x.book); w.nat(x.businessDate); w.nat(x.shardSize) };
      case (#setRetryPolicy(x)) { w.byte(0x35); w.text(x.book); w.nat(x.limit) };
      case (#resolveEndOfDayFailure(x)) { w.byte(0x36); w.text(x.book); w.nat(x.businessDate); w.nat(x.item); w.nat(x.entity); w.text(x.reason) };
      case (#clearAlert(x)) { w.byte(0x37); w.nat(x.alert); w.text(x.reason) };
      case (#revaluePositions(x)) { w.byte(0x38); w.text(x.period); w.nat(x.postingDate); w.nat(x.valueDate); w.text(x.narration) };
      case (#openCall(x)) { w.byte(0x40); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wCallTerms(w, x.terms); w.text(x.reference); TyCan.writeOptPrincipal(w, x.approver) };
      case (#resetCallRate(x)) { w.byte(0x41); w.nat(x.call); w.nat(x.rateBps); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#adjustCallBalance(x)) { w.byte(0x42); w.nat(x.call); wInt(w, x.delta); TyCan.writeOptPrincipal(w, x.approver); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#serveCallNotice(x)) { w.byte(0x43); w.nat(x.call) };
      case (#settleCall(x)) { w.byte(0x44); w.nat(x.call); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#setCustodyPolicy(p)) { w.byte(0x50); wBasis(w, p.entitlementBasis) };
      case (#extendInstrument(x)) { w.byte(0x51); wExtension(w, x.extension) };
      case (#openDepot(x)) { w.byte(0x52); wDepot(w, x.depot) };
      case (#setBookDepot(x)) { w.byte(0x53); w.text(x.book); w.text(x.depot) };
      case (#assignDealDepot(x)) { w.byte(0x54); w.nat(x.deal); w.text(x.depot) };
      case (#transferDepot(x)) { w.byte(0x55); w.nat(x.lot); w.text(x.from); w.text(x.to); w.nat(x.nominal); w.text(x.reference) };
      case (#announceCorporateAction(x)) { w.byte(0x56); wAnnouncement(w, x.announcement) };
      case (#cancelCorporateAction(x)) { w.byte(0x57); w.nat(x.action); w.text(x.reason) };
      case (#processCorporateAction(x)) { w.byte(0x58); w.nat(x.action); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#setSettlementVenue(x)) { w.byte(0x60); wVenue(w, x.venue) };
      case (#setSettlementLedger(x)) { w.byte(0x61); wDeclaration(w, x.declaration) };
      case (#openSettlementCycle(x)) { w.byte(0x62); wCycle(w, x.cycle) };
      case (#instructSettlement(x)) { w.byte(0x63); w.nat(x.deal); w.principal(x.counterparty); w.optNat(x.tradeId); w.text(x.reference) };
      case (#setInstructionTrade(x)) { w.byte(0x64); w.nat(x.instruction); w.nat(x.tradeId) };
      case (#recycleSettlement(x)) { w.byte(0x65); w.nat(x.instruction); w.nat(x.cycle) };
      case (#recordSettlementStatus(x)) { w.byte(0x66); w.nat(x.instruction); w.blob(x.document) };
      case (#buyIn(x)) { w.byte(0x67); w.nat(x.instruction); TyCan.writeCounterparty(w, x.counterparty); w.nat(x.priceMicro); w.nat(x.settlement); w.text(x.reference); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#cancelSettlement(x)) { w.byte(0x68); w.nat(x.instruction); w.blob(x.ourConsent); w.blob(x.theirConsent); w.text(x.reason) };
      case (#splitDeal(x)) { w.byte(0x69); w.nat(x.deal); wNats(w, x.parts) };
      case (#setFinancingPolicy(p)) { w.byte(0x70); wFinancingPolicy(w, p) };
      case (#openRepo(x)) { w.byte(0x71); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wRepoTerms(w, x.terms); w.text(x.reference) };
      case (#settleRepoLeg(x)) { w.byte(0x72); w.nat(x.repo); w.nat(x.leg); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#resetRepoRate(x)) { w.byte(0x73); w.nat(x.repo); w.nat(x.rateBps); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#meetMarginCall(x)) { w.byte(0x74); w.nat(x.repo); w.nat(x.cash); wOptCollateral(w, x.collateral); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#substituteCollateral(x)) { w.byte(0x75); w.nat(x.repo); wCollateral(w, x.out); wCollateral(w, x.in_) };
      case (#openLoan(x)) { w.byte(0x76); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); wLoanTerms(w, x.terms); w.text(x.reference) };
      case (#settleLoanLeg(x)) { w.byte(0x77); w.nat(x.loan); w.nat(x.leg); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#recallLoan(x)) { w.byte(0x78); w.nat(x.loan) };
      case (#instructFinancing(x)) { w.byte(0x79); wFamily(w, x.family); w.nat(x.id); w.nat(x.leg); w.principal(x.counterparty); w.optNat(x.tradeId); w.text(x.reference) };
      case (#setValuationPolicy(p)) { w.byte(0x80); w.text(p.hedgeReserve) };
      case (#quoteBondYield(x)) { w.byte(0x81); w.text(x.isin); w.nat(x.day); w.nat(x.yieldBps); w.blob(x.source) };
      case (#designateHedge(x)) { w.byte(0x82); w.nat(x.hedging); w.nat(x.hedged); wHedgeKind(w, x.kind) };
      case (#assessHedge(x)) { w.byte(0x83); w.nat(x.hedge); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#dedesignateHedge(x)) { w.byte(0x84); w.nat(x.hedge); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#setTreasuryPolicy(p)) { w.byte(0x20); TyCan.writePolicy(w, p) };
      case (#registerSecurity(x)) { w.byte(0x21); TyCan.writeSecurityTerms(w, x.terms) };
      case (#publishCurve(x)) { w.byte(0x22); TyCan.writeCurve(w, x.curve) };
      case (#setTreasuryLimit(x)) { w.byte(0x23); TyCan.writeLimit(w, x.limit) };
      case (#registerNostro(x)) { w.byte(0x24); TyCan.writeNostro(w, x.nostro) };
      case (#captureDeal(x)) { w.byte(0x25); w.text(x.book); TyCan.writeCounterparty(w, x.counterparty); TyCan.writeKind(w, x.kind); w.text(x.reference); TyCan.writeOptPrincipal(w, x.approver) };
      case (#confirmDeal(x)) { w.byte(0x26); w.nat(x.deal); w.blob(x.confirmation); TyCan.writeOptFields(w, x.fields); w.optBlob(x.document) };
      case (#amendDeal(x)) { w.byte(0x27); w.nat(x.deal); TyCan.writeKind(w, x.kind); w.text(x.reason) };
      case (#cancelDeal(x)) { w.byte(0x28); w.nat(x.deal); w.text(x.reason) };
      case (#settleDealLeg(x)) { w.byte(0x29); w.nat(x.deal); w.nat(x.leg); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#markDeal(x)) { w.byte(0x2A); w.nat(x.deal); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
      case (#recordNostroStatement(x)) { w.byte(0x2B); w.text(x.nostro); w.blob(x.statement); w.nat(x.from); w.nat(x.to); TyCan.writeEntries(w, x.entries); w.optBlob(x.document) };
      case (#resolveNostroBreak(x)) { w.byte(0x2C); w.nat(x.breakId); w.text(x.resolution); TyCan.writeOptCorrection(w, x.correction); wDates(w, x.postingDate, x.valueDate, x.period, x.narration) };
    }
  };

  public func readCommand(r : JC.Reader) : ?T.Command {
    let ?tag = r.byte() else return null;
    switch (tag) {
      case 0x01 { let ?id = r.text() else return null; let ?name = r.text() else return null; let ?parent = rOptText(r) else return null; let ?sharia = r.bool() else return null; ?#openBook({ id; name; parent; sharia }) };
      case 0x02 { let ?id = r.text() else return null; ?#closeBook({ id }) };
      case 0x03 { let ?id = r.text() else return null; let ?name = r.text() else return null; let ?permissions = rTexts(r) else return null; ?#defineRole({ id; name; permissions }) };
      case 0x04 { let ?subject = r.principal() else return null; let ?role = r.text() else return null; let ?scope = readScope(r) else return null; ?#grantRole({ subject; role; scope }) };
      case 0x05 { let ?subject = r.principal() else return null; let ?role = r.text() else return null; ?#revokeRole({ subject; role }) };
      case 0x06 { let ?p = rPolicy(r) else return null; ?#setDualPolicy(p) };
      case 0x07 { let ?permission = r.text() else return null; ?#clearDualPolicy({ permission }) };
      case 0x08 { let ?feature = r.text() else return null; let ?height = r.nat64() else return null; ?#setFeatureActivation({ feature; height }) };
      case 0x09 { let ?i = rIdentity(r) else return null; ?#setDeskIdentity(i) };
      case 0x10 { let ?code = r.text() else return null; let ?minorUnits = r.nat8() else return null; ?#journalRegisterCurrency({ code; minorUnits }) };
      case 0x11 {
        let ?code = r.text() else return null; let ?name = r.text() else return null; let ?normalSide = r.side() else return null;
        let ?category = r.category() else return null; let ?constraint = r.constraint() else return null;
        ?#journalOpenAccount({ code; name; normalSide; category; constraint })
      };
      case 0x12 { let ?code = r.text() else return null; ?#journalCloseAccount({ code }) };
      case 0x13 { let ?id = r.text() else return null; let ?start = r.nat() else return null; let ?end = r.nat() else return null; ?#journalOpenPeriod({ id; start; end }) };
      case 0x14 { let ?id = r.text() else return null; ?#journalClosePeriod({ id }) };
      case 0x15 { let ?calendar = r.calendar() else return null; ?#journalSetCalendar({ calendar }) };
      case 0x16 { let ?authority = r.calendarAuthority() else return null; let ?maxRollDays = r.nat() else return null; let ?businessDate = rOptDay(r) else return null; ?#journalSetCalendarAuthority({ authority; maxRollDays; businessDate }) };
      case 0x17 { let ?day = r.nat() else return null; ?#journalRollBusinessDate({ day }) };
      case 0x18 { let ?height = r.nat64() else return null; ?#journalSetActivationHeight({ height }) };
      case 0x30 { let ?currency = r.text() else return null; ?#setFunctionalCurrency({ currency }) };
      case 0x31 { let ?pair = CC.rPair(r) else return null; ?#setFxPair({ pair }) };
      case 0x32 { let ?rate = CC.rRate(r) else return null; ?#setFxRate({ rate }) };
      case 0x33 { let ?index = r.text() else return null; let ?day = r.nat() else return null; let ?rateBps = r.nat() else return null; ?#recordRateFixing({ index; day; rateBps }) };
      case 0x34 { let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?shardSize = r.nat() else return null; ?#openEndOfDay({ book; businessDate; shardSize }) };
      case 0x35 { let ?book = r.text() else return null; let ?limit = r.nat() else return null; ?#setRetryPolicy({ book; limit }) };
      case 0x36 {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?item = r.nat() else return null; let ?entity = r.nat() else return null; let ?reason = r.text() else return null;
        ?#resolveEndOfDayFailure({ book; businessDate; item; entity; reason })
      };
      case 0x37 { let ?alert = r.nat() else return null; let ?reason = r.text() else return null; ?#clearAlert({ alert; reason }) };
      case 0x38 { let ?period = r.text() else return null; let ?postingDate = r.nat() else return null; let ?valueDate = r.nat() else return null; let ?narration = r.text() else return null; ?#revaluePositions({ period; postingDate; valueDate; narration }) };
      case 0x40 {
        let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rCallTerms(r) else return null;
        let ?reference = r.text() else return null; let ?approver = TyCan.readOptPrincipal(r) else return null;
        ?#openCall({ book; counterparty; terms; reference; approver })
      };
      case 0x41 { let ?call = r.nat() else return null; let ?rateBps = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#resetCallRate({ call; rateBps; postingDate; valueDate; period; narration }) };
      case 0x42 { let ?call = r.nat() else return null; let ?delta = rInt(r) else return null; let ?approver = TyCan.readOptPrincipal(r) else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#adjustCallBalance({ call; delta; approver; postingDate; valueDate; period; narration }) };
      case 0x43 { let ?call = r.nat() else return null; ?#serveCallNotice({ call }) };
      case 0x44 { let ?call = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#settleCall({ call; postingDate; valueDate; period; narration }) };
      case 0x50 { let ?entitlementBasis = rBasis(r) else return null; ?#setCustodyPolicy({ entitlementBasis }) };
      case 0x51 { let ?extension = rExtension(r) else return null; ?#extendInstrument({ extension }) };
      case 0x52 { let ?depot = rDepot(r) else return null; ?#openDepot({ depot }) };
      case 0x53 { let ?book = r.text() else return null; let ?depot = r.text() else return null; ?#setBookDepot({ book; depot }) };
      case 0x54 { let ?deal = r.nat() else return null; let ?depot = r.text() else return null; ?#assignDealDepot({ deal; depot }) };
      case 0x55 { let ?lot = r.nat() else return null; let ?from = r.text() else return null; let ?to = r.text() else return null; let ?nominal = r.nat() else return null; let ?reference = r.text() else return null; ?#transferDepot({ lot; from; to; nominal; reference }) };
      case 0x56 { let ?announcement = rAnnouncement(r) else return null; ?#announceCorporateAction({ announcement }) };
      case 0x57 { let ?action = r.nat() else return null; let ?reason = r.text() else return null; ?#cancelCorporateAction({ action; reason }) };
      case 0x58 { let ?action = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#processCorporateAction({ action; postingDate; valueDate; period; narration }) };
      case 0x60 { let ?venue = rVenue(r) else return null; ?#setSettlementVenue({ venue }) };
      case 0x61 { let ?declaration = rDeclaration(r) else return null; ?#setSettlementLedger({ declaration }) };
      case 0x62 { let ?cycle = rCycle(r) else return null; ?#openSettlementCycle({ cycle }) };
      case 0x63 { let ?deal = r.nat() else return null; let ?counterparty = r.principal() else return null; let ?tradeId = r.optNat() else return null; let ?reference = r.text() else return null; ?#instructSettlement({ deal; counterparty; tradeId; reference }) };
      case 0x64 { let ?instruction = r.nat() else return null; let ?tradeId = r.nat() else return null; ?#setInstructionTrade({ instruction; tradeId }) };
      case 0x65 { let ?instruction = r.nat() else return null; let ?cycle = r.nat() else return null; ?#recycleSettlement({ instruction; cycle }) };
      case 0x66 { let ?instruction = r.nat() else return null; let ?document = r.blob() else return null; ?#recordSettlementStatus({ instruction; document }) };
      case 0x67 { let ?instruction = r.nat() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?priceMicro = r.nat() else return null; let ?settlement = r.nat() else return null; let ?reference = r.text() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#buyIn({ instruction; counterparty; priceMicro; settlement; reference; postingDate; valueDate; period; narration }) };
      case 0x68 { let ?instruction = r.nat() else return null; let ?ourConsent = r.blob() else return null; let ?theirConsent = r.blob() else return null; let ?reason = r.text() else return null; ?#cancelSettlement({ instruction; ourConsent; theirConsent; reason }) };
      case 0x69 { let ?deal = r.nat() else return null; let ?parts = rNats(r) else return null; ?#splitDeal({ deal; parts }) };
      case 0x70 { let ?p = rFinancingPolicy(r) else return null; ?#setFinancingPolicy(p) };
      case 0x71 { let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rRepoTerms(r) else return null; let ?reference = r.text() else return null; ?#openRepo({ book; counterparty; terms; reference }) };
      case 0x72 { let ?repo = r.nat() else return null; let ?leg = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#settleRepoLeg({ repo; leg; postingDate; valueDate; period; narration }) };
      case 0x73 { let ?repo = r.nat() else return null; let ?rateBps = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#resetRepoRate({ repo; rateBps; postingDate; valueDate; period; narration }) };
      case 0x74 { let ?repo = r.nat() else return null; let ?cash = r.nat() else return null; let ?collateral = rOptCollateral(r) else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#meetMarginCall({ repo; cash; collateral; postingDate; valueDate; period; narration }) };
      case 0x75 { let ?repo = r.nat() else return null; let ?out = rCollateral(r) else return null; let ?in_ = rCollateral(r) else return null; ?#substituteCollateral({ repo; out; in_ }) };
      case 0x76 { let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?terms = rLoanTerms(r) else return null; let ?reference = r.text() else return null; ?#openLoan({ book; counterparty; terms; reference }) };
      case 0x77 { let ?loan = r.nat() else return null; let ?leg = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#settleLoanLeg({ loan; leg; postingDate; valueDate; period; narration }) };
      case 0x78 { let ?loan = r.nat() else return null; ?#recallLoan({ loan }) };
      case 0x80 { let ?hedgeReserve = r.text() else return null; ?#setValuationPolicy({ hedgeReserve }) };
      case 0x81 { let ?isin = r.text() else return null; let ?day = r.nat() else return null; let ?yieldBps = r.nat() else return null; let ?source = r.blob() else return null; ?#quoteBondYield({ isin; day; yieldBps; source }) };
      case 0x82 { let ?hedging = r.nat() else return null; let ?hedged = r.nat() else return null; let ?kind = rHedgeKind(r) else return null; ?#designateHedge({ hedging; hedged; kind }) };
      case 0x83 { let ?hedge = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#assessHedge({ hedge; postingDate; valueDate; period; narration }) };
      case 0x84 { let ?hedge = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#dedesignateHedge({ hedge; postingDate; valueDate; period; narration }) };
      case 0x79 { let ?family = rFamily(r) else return null; let ?id = r.nat() else return null; let ?leg = r.nat() else return null; let ?counterparty = r.principal() else return null; let ?tradeId = r.optNat() else return null; let ?reference = r.text() else return null; ?#instructFinancing({ family; id; leg; counterparty; tradeId; reference }) };
      case 0x20 { let ?p = TyCan.readPolicy(r) else return null; ?#setTreasuryPolicy(p) };
      case 0x21 { let ?terms = TyCan.readSecurityTerms(r) else return null; ?#registerSecurity({ terms }) };
      case 0x22 { let ?curve = TyCan.readCurve(r) else return null; ?#publishCurve({ curve }) };
      case 0x23 { let ?limit = TyCan.readLimit(r) else return null; ?#setTreasuryLimit({ limit }) };
      case 0x24 { let ?nostro = TyCan.readNostro(r) else return null; ?#registerNostro({ nostro }) };
      case 0x25 {
        let ?book = r.text() else return null; let ?counterparty = TyCan.readCounterparty(r) else return null; let ?kind = TyCan.readKind(r) else return null;
        let ?reference = r.text() else return null; let ?approver = TyCan.readOptPrincipal(r) else return null;
        ?#captureDeal({ book; counterparty; kind; reference; approver })
      };
      case 0x26 {
        let ?deal = r.nat() else return null; let ?confirmation = r.blob() else return null; let ?fields = TyCan.readOptFields(r) else return null; let ?document = r.optBlob() else return null;
        ?#confirmDeal({ deal; confirmation; fields; document })
      };
      case 0x27 { let ?deal = r.nat() else return null; let ?kind = TyCan.readKind(r) else return null; let ?reason = r.text() else return null; ?#amendDeal({ deal; kind; reason }) };
      case 0x28 { let ?deal = r.nat() else return null; let ?reason = r.text() else return null; ?#cancelDeal({ deal; reason }) };
      case 0x29 { let ?deal = r.nat() else return null; let ?leg = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#settleDealLeg({ deal; leg; postingDate; valueDate; period; narration }) };
      case 0x2A { let ?deal = r.nat() else return null; let ?(postingDate, valueDate, period, narration) = rDates(r) else return null; ?#markDeal({ deal; postingDate; valueDate; period; narration }) };
      case 0x2B {
        let ?nostro = r.text() else return null; let ?statement = r.blob() else return null; let ?from = r.nat() else return null; let ?to = r.nat() else return null;
        let ?entries = TyCan.readEntries(r) else return null; let ?document = r.optBlob() else return null;
        ?#recordNostroStatement({ nostro; statement; from; to; entries; document })
      };
      case 0x2C {
        let ?breakId = r.nat() else return null; let ?resolution = r.text() else return null; let ?correction = TyCan.readOptCorrection(r) else return null;
        let ?(postingDate, valueDate, period, narration) = rDates(r) else return null;
        ?#resolveNostroBreak({ breakId; resolution; correction; postingDate; valueDate; period; narration })
      };
      case _ null;
    }
  };

  public func commandBytes(c : T.Command) : Blob { let w = JC.Writer(); writeCommand(w, c); w.toBlob() };

  /// The registry the kernel's `Command` binds and re-derives with. Version 1 represents every family.
  public func registry() : Enc.Registry<T.Command> {
    {
      domainPrefix = COMMAND_DOMAIN_PREFIX;
      current = COMMAND_ENCODING;
      encoders = [{
        version = COMMAND_ENCODING;
        write = func(w : KC.Writer, c : T.Command) : Bool { let jw = JC.Writer(); writeCommand(jw, c); w.bytes(jw.toArray()); true };
        read = func(r : KC.Reader) : ?T.Command {
          let ?bytes = r.take(r.remaining()) else return null;
          let jr = JC.Reader(bytes);
          let ?c = readCommand(jr) else return null;
          if (jr.remaining() != 0) return null;
          ?c
        };
      }];
    }
  };

  public func commandHash(c : T.Command) : Blob {
    switch (Enc.hashAt(registry(), COMMAND_ENCODING, c)) { case (?h) h; case null Blob.fromArray([]) }
  };

  // ═══════════════════════════════════════════════════════
  //  EVENTS
  // ═══════════════════════════════════════════════════════

  func wRefusal(w : JC.Writer, r : AT.RefusalReason) {
    w.byte(switch (r) {
      case (#noGrant) 0; case (#outsidePartition) 1; case (#outsideCurrency) 2; case (#overCeiling) 3; case (#overDailyLimit) 4; case (#notEligibleChecker) 5;
      case (#selfApproval) 6; case (#commandHashMismatch) 7; case (#proposalExpired) 8; case (#noPolicy) 9; case (#noWitness) 10; case (#unknown) 11;
    })
  };
  func rRefusal(r : JC.Reader) : ?AT.RefusalReason {
    switch (r.byte()) {
      case (?0) ?#noGrant; case (?1) ?#outsidePartition; case (?2) ?#outsideCurrency; case (?3) ?#overCeiling; case (?4) ?#overDailyLimit; case (?5) ?#notEligibleChecker;
      case (?6) ?#selfApproval; case (?7) ?#commandHashMismatch; case (?8) ?#proposalExpired; case (?9) ?#noPolicy; case (?10) ?#noWitness; case (?11) ?#unknown; case (_) null;
    }
  };
  public func writeFinding(w : JC.Writer, f : MT.Finding) { w.text(f.rule); w.nat(f.version); w.nat(f.account); w.nat(f.day); wNats(w, f.postings); w.text(f.detail) };
  public func readFinding(r : JC.Reader) : ?MT.Finding {
    let ?rule = r.text() else return null; let ?version = r.nat() else return null; let ?account = r.nat() else return null;
    let ?day = r.nat() else return null; let ?postings = rNats(r) else return null; let ?detail = r.text() else return null;
    ?{ rule; version; account; day; postings; detail }
  };
  func wAlert(w : JC.Writer, e : AlT.AlertEvent) {
    switch (e) {
      case (#alertOpened(x)) { w.byte(0x01); writeFinding(w, x.finding); w.byte(switch (x.source) { case (#posting) 0; case (#endOfDay) 1 }) };
      case (#alertCleared(x)) { w.byte(0x02); w.nat(x.alert); w.text(x.reason) };
      case (#alertEscalated(x)) { w.byte(0x03); w.nat(x.alert); w.text(x.reportRef) };
    }
  };
  func rAlert(r : JC.Reader) : ?AlT.AlertEvent {
    switch (r.byte()) {
      case (?0x01) {
        let ?finding = readFinding(r) else return null;
        let source : AlT.Source = switch (r.byte()) { case (?0) #posting; case (?1) #endOfDay; case (_) return null };
        ?#alertOpened({ finding; source })
      };
      case (?0x02) { let ?alert = r.nat() else return null; let ?reason = r.text() else return null; ?#alertCleared({ alert; reason }) };
      case (?0x03) { let ?alert = r.nat() else return null; let ?reportRef = r.text() else return null; ?#alertEscalated({ alert; reportRef }) };
      case (_) null;
    }
  };
  func wFailures(w : JC.Writer, fs : [Batch.Failure]) {
    w.len16(fs.size());
    for (f in fs.vals()) { w.nat(f.seq); w.text(f.job); w.text(f.scope); w.nat(f.entity); w.text(f.reason); w.nat(f.attempts) };
  };
  func rFailures(r : JC.Reader) : ?[Batch.Failure] {
    let ?n = r.len16() else return null;
    let buf = VarArray.repeat<Batch.Failure>({ seq = 0; job = ""; scope = ""; entity = 0; reason = ""; attempts = 0 }, n);
    var i = 0;
    while (i < n) {
      let ?seq = r.nat() else return null; let ?job = r.text() else return null; let ?scope = r.text() else return null;
      let ?entity = r.nat() else return null; let ?reason = r.text() else return null; let ?attempts = r.nat() else return null;
      buf[i] := { seq; job; scope; entity; reason; attempts }; i += 1;
    };
    ?Array.fromVarArray<Batch.Failure>(buf)
  };
  func wEod(w : JC.Writer, e : T.EodEvent) {
    switch (e) {
      case (#opened(x)) { w.byte(0x01); w.text(x.book); w.nat(x.businessDate); w.nat(x.shardSize); w.nat(x.openedAtHeight); w.nat(x.maxDeal); w.blob(x.planHash); w.nat(x.items); w.nat(x.entities) };
      case (#chunk(x)) { w.byte(0x02); w.text(x.book); w.nat(x.businessDate); w.nat(x.cursorFrom); w.nat(x.cursorTo); w.nat(x.posted); w.nat(x.examined); w.nat(x.zeroMovement); wFailures(w, x.failures) };
      case (#retry(x)) {
        w.byte(0x03); w.text(x.book); w.nat(x.businessDate);
        w.len16(x.resolved.size()); for (z in x.resolved.vals()) { w.nat(z.item); w.nat(z.entity) };
        wFailures(w, x.failures); w.nat(x.posted);
      };
      case (#completed(x)) { w.byte(0x04); w.text(x.book); w.nat(x.businessDate); w.nat(x.posted); w.nat(x.examined); w.nat(x.zeroMovement); w.nat(x.failures) };
      case (#retryPolicySet(x)) { w.byte(0x05); w.text(x.book); w.nat(x.limit) };
      case (#failureResolved(x)) { w.byte(0x06); w.text(x.book); w.nat(x.businessDate); w.nat(x.item); w.nat(x.entity); w.text(x.reason) };
    }
  };
  func rEod(r : JC.Reader) : ?T.EodEvent {
    switch (r.byte()) {
      case (?0x01) {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?shardSize = r.nat() else return null; let ?openedAtHeight = r.nat() else return null;
        let ?maxDeal = r.nat() else return null; let ?planHash = r.blob() else return null; let ?items = r.nat() else return null; let ?entities = r.nat() else return null;
        ?#opened({ book; businessDate; shardSize; openedAtHeight; maxDeal; planHash; items; entities })
      };
      case (?0x02) {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?cursorFrom = r.nat() else return null; let ?cursorTo = r.nat() else return null;
        let ?posted = r.nat() else return null; let ?examined = r.nat() else return null; let ?zeroMovement = r.nat() else return null; let ?failures = rFailures(r) else return null;
        ?#chunk({ book; businessDate; cursorFrom; cursorTo; posted; examined; zeroMovement; failures })
      };
      case (?0x03) {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null;
        let ?n = r.len16() else return null;
        let buf = VarArray.repeat<{ item : Nat; entity : Nat }>({ item = 0; entity = 0 }, n);
        var i = 0;
        while (i < n) { let ?item = r.nat() else return null; let ?entity = r.nat() else return null; buf[i] := { item; entity }; i += 1 };
        let ?failures = rFailures(r) else return null; let ?posted = r.nat() else return null;
        ?#retry({ book; businessDate; resolved = Array.fromVarArray(buf); failures; posted })
      };
      case (?0x04) {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?posted = r.nat() else return null; let ?examined = r.nat() else return null;
        let ?zeroMovement = r.nat() else return null; let ?failures = r.nat() else return null;
        ?#completed({ book; businessDate; posted; examined; zeroMovement; failures })
      };
      case (?0x05) { let ?book = r.text() else return null; let ?limit = r.nat() else return null; ?#retryPolicySet({ book; limit }) };
      case (?0x06) {
        let ?book = r.text() else return null; let ?businessDate = r.nat() else return null; let ?item = r.nat() else return null; let ?entity = r.nat() else return null; let ?reason = r.text() else return null;
        ?#failureResolved({ book; businessDate; item; entity; reason })
      };
      case (_) null;
    }
  };

  /// A kernel-shaped body: written with the kernel's writer, carried as bytes.
  func kernelBytes(write : KC.Writer -> ()) : [Nat8] { let kw = KC.Writer(); write(kw); kw.toArray() };

  /// The event's body: a tag byte, then the family's fields.
  public func writeEventBody(w : JC.Writer, e : T.Event) {
    switch (e) {
      case (#deskInstalled(x)) { w.byte(0x01); w.principal(x.installer) };
      case (#bookOpened(x)) { w.byte(0x02); w.text(x.id); w.text(x.name); wOptText(w, x.parent); w.bool(x.sharia) };
      case (#bookClosed(x)) { w.byte(0x03); w.text(x.id) };
      case (#roleDefined(x)) { w.byte(0x04); w.text(x.id); w.text(x.name); wTexts(w, x.permissions) };
      case (#roleGranted(x)) { w.byte(0x05); w.principal(x.subject); w.text(x.role); writeScope(w, x.scope) };
      case (#roleRevoked(x)) { w.byte(0x06); w.principal(x.subject); w.text(x.role) };
      case (#dualPolicySet(p)) { w.byte(0x07); wPolicy(w, p) };
      case (#dualPolicyCleared(x)) { w.byte(0x08); w.text(x.permission) };
      case (#featureActivationSet(x)) { w.byte(0x09); w.text(x.feature); w.nat64(x.height) };
      case (#identitySet(i)) { w.byte(0x0A); wIdentity(w, i) };
      case (#commandProposed(p)) { w.byte(0x10); w.bytes(kernelBytes(func(kw) { Cmd.writeProposed(kw, p) })) };
      case (#commandApproved(a)) { w.byte(0x11); w.bytes(kernelBytes(func(kw) { Cmd.writeApproved(kw, a) })) };
      case (#commandExecuted(x)) { w.byte(0x12); w.bytes(kernelBytes(func(kw) { Cmd.writeExecuted(kw, x) })) };
      case (#commandRejected(x)) { w.byte(0x13); w.bytes(kernelBytes(func(kw) { Cmd.writeRejected(kw, x) })) };
      case (#commandExpired(x)) { w.byte(0x14); w.bytes(kernelBytes(func(kw) { Cmd.writeExpired(kw, x) })) };
      case (#operationRefused(x)) { w.byte(0x15); w.principal(x.subject); w.text(x.permission); wRefusal(w, x.reason); w.text(x.detail) };
      case (#emergencyOverride(x)) { w.byte(0x16); w.blob(x.commandHash); w.byte(x.commandEncoding); w.principal(x.actor_); w.principal(x.witness); w.text(x.justification) };
      case (#overrideReviewed(x)) { w.byte(0x17); w.nat(x.override_); w.principal(x.reviewer); w.text(x.disposition) };
      case (#dailyConsumed(x)) { w.byte(0x18); w.principal(x.subject); w.text(x.currency); w.nat(x.day); w.nat(x.amount) };
      case (#close(ce)) { w.byte(0x20); CC.writeEvent(w, ce) };
      case (#fixingRecorded(x)) { w.byte(0x21); w.text(x.index); w.nat(x.day); w.nat(x.rateBps) };
      case (#journalMark(x)) { w.byte(0x22); w.nat(x.height) };
      case (#eod(x)) { w.byte(0x30); wEod(w, x) };
      case (#alert(a)) { w.byte(0x48); wAlert(w, a) };
      case (#treasury(te)) { w.byte(0x55); TyCan.writeEvent(w, te) };
      case (#call(ce)) { w.byte(0x60); wCallEvent(w, ce) };
      case (#custody(ce)) { w.byte(0x61); wCustodyEvent(w, ce) };
      case (#settlement(se)) { w.byte(0x62); wSettlementEvent(w, se) };
      case (#financing(fe)) { w.byte(0x63); wFinancingEvent(w, fe) };
      case (#valuation(ve)) { w.byte(0x64); wValuationEvent(w, ve) };
    }
  };

  public func readEventBody(bytes : [Nat8]) : ?T.Event {
    if (bytes.size() == 0) return null;
    let rest = Array.tabulate<Nat8>(bytes.size() - 1, func(i) { bytes[i + 1] });
    func kernel<X>(read : KC.Reader -> ?X) : ?X {
      let kr = KC.Reader(rest);
      let ?x = read(kr) else return null;
      if (not kr.atEnd()) return null;
      ?x
    };
    let r = JC.Reader(rest);
    let out : ?T.Event = switch (bytes[0]) {
      case 0x01 { let ?installer = r.principal() else return null; ?#deskInstalled({ installer }) };
      case 0x02 { let ?id = r.text() else return null; let ?name = r.text() else return null; let ?parent = rOptText(r) else return null; let ?sharia = r.bool() else return null; ?#bookOpened({ id; name; parent; sharia }) };
      case 0x03 { let ?id = r.text() else return null; ?#bookClosed({ id }) };
      case 0x04 { let ?id = r.text() else return null; let ?name = r.text() else return null; let ?permissions = rTexts(r) else return null; ?#roleDefined({ id; name; permissions }) };
      case 0x05 { let ?subject = r.principal() else return null; let ?role = r.text() else return null; let ?scope = readScope(r) else return null; ?#roleGranted({ subject; role; scope }) };
      case 0x06 { let ?subject = r.principal() else return null; let ?role = r.text() else return null; ?#roleRevoked({ subject; role }) };
      case 0x07 { let ?p = rPolicy(r) else return null; ?#dualPolicySet(p) };
      case 0x08 { let ?permission = r.text() else return null; ?#dualPolicyCleared({ permission }) };
      case 0x09 { let ?feature = r.text() else return null; let ?height = r.nat64() else return null; ?#featureActivationSet({ feature; height }) };
      case 0x0A { let ?i = rIdentity(r) else return null; ?#identitySet(i) };
      case 0x10 { switch (kernel<Cmd.Proposed>(Cmd.readProposed)) { case (?p) ?#commandProposed(p); case null null } };
      case 0x11 { switch (kernel<Cmd.Approved>(Cmd.readApproved)) { case (?a) ?#commandApproved(a); case null null } };
      case 0x12 { switch (kernel<Cmd.Executed>(Cmd.readExecuted)) { case (?x) ?#commandExecuted(x); case null null } };
      case 0x13 { switch (kernel<Cmd.Rejected>(Cmd.readRejected)) { case (?x) ?#commandRejected(x); case null null } };
      case 0x14 { switch (kernel<Cmd.Expired>(Cmd.readExpired)) { case (?x) ?#commandExpired(x); case null null } };
      case 0x15 { let ?subject = r.principal() else return null; let ?permission = r.text() else return null; let ?reason = rRefusal(r) else return null; let ?detail = r.text() else return null; ?#operationRefused({ subject; permission; reason; detail }) };
      case 0x16 {
        let ?commandHash = r.blob() else return null; let ?commandEncoding = r.byte() else return null; let ?actor_ = r.principal() else return null;
        let ?witness = r.principal() else return null; let ?justification = r.text() else return null;
        ?#emergencyOverride({ commandHash; commandEncoding; actor_; witness; justification })
      };
      case 0x17 { let ?override_ = r.nat() else return null; let ?reviewer = r.principal() else return null; let ?disposition = r.text() else return null; ?#overrideReviewed({ override_; reviewer; disposition }) };
      case 0x18 { let ?subject = r.principal() else return null; let ?currency = r.text() else return null; let ?day = r.nat() else return null; let ?amount = r.nat() else return null; ?#dailyConsumed({ subject; currency; day; amount }) };
      case 0x20 { let ?ce = CC.readEvent(r) else return null; ?#close(ce) };
      case 0x21 { let ?index = r.text() else return null; let ?day = r.nat() else return null; let ?rateBps = r.nat() else return null; ?#fixingRecorded({ index; day; rateBps }) };
      case 0x22 { let ?height = r.nat() else return null; ?#journalMark({ height }) };
      case 0x30 { let ?x = rEod(r) else return null; ?#eod(x) };
      case 0x48 { let ?a = rAlert(r) else return null; ?#alert(a) };
      case 0x55 { let ?te = TyCan.readEvent(r) else return null; ?#treasury(te) };
      case 0x60 { let ?ce = rCallEvent(r) else return null; ?#call(ce) };
      case 0x61 { let ?ce = rCustodyEvent(r) else return null; ?#custody(ce) };
      case 0x62 { let ?se = rSettlementEvent(r) else return null; ?#settlement(se) };
      case 0x63 { let ?fe = rFinancingEvent(r) else return null; ?#financing(fe) };
      case 0x64 { let ?ve = rValuationEvent(r) else return null; ?#valuation(ve) };
      case _ null;
    };
    switch (out) {
      case null null;
      case (?e) {
        // a kernel-shaped body was read to its end by `kernel`; every other body must have consumed its bytes too
        switch (bytes[0]) { case (0x10 or 0x11 or 0x12 or 0x13 or 0x14) ?e; case _ { if (r.remaining() != 0) null else ?e } }
      };
    }
  };

  public func eventBytes(e : T.Event) : Blob { let w = JC.Writer(); writeEventBody(w, e); w.toBlob() };

  /// The block codec the kernel's log uses: version 1, the desk's domain, the body length-prefixed.
  public func codec() : DL.Codec<T.Event> {
    {
      version = BLOCK_VERSION;
      supports = func(v : Nat8) : Bool { v == BLOCK_VERSION };
      domain = BLOCK_DOMAIN;
      write = func(w : KC.Writer, e : T.Event) { let body = eventBytes(e); w.nat(body.size()); w.blobRaw(body) };
      read = func(r : KC.Reader) : ?T.Event { let ?n = r.nat() else return null; let ?bytes = r.take(n) else return null; readEventBody(bytes) };
    }
  };

}
