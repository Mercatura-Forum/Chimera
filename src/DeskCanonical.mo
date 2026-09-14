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
