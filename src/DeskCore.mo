/// DeskCore.mo: the desk's state machine: admission, the fold, the end of day, the views.
///
/// Every state change is one block applied by `apply`, which is the only place state changes; every `plan` and
/// `prepare` function is pure and returns either a typed error or the events to commit; `replay` rebuilds the
/// state from the log and `fingerprint` digests it, so "the state is the fold of the log" is a property a battery
/// asserts. The treasury domain is Manticore's, folded here unchanged: its planners are called with the desk's
/// valuation context, its events are the desk's `#treasury` blocks, its rows live in the desk's arena.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Nat64 "mo:core/Nat64";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import JT "mo:journal/JournalTypes";
import JCore "mo:journal/JournalCore";
import JC "mo:journal/Canonical";
import KC "mo:kernel/codec/Canonical";
import AT "mo:kernel/auth/AuthTypes";
import MC "mo:kernel/auth/MakerChecker";
import E "mo:kernel/auth/Entitlements";
import Cmd "mo:kernel/domain/Command";
import Batch "mo:kernel/batch/Batch";
import RI "mo:kernel/index/RegionIndex";

import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import TreasuryMath "mo:manticore/TreasuryMath";
import TreasuryMessages "mo:manticore/TreasuryMessages";
import CloseCore "mo:manticore/CloseCore";
import Fx "mo:manticore/Fx";
import AlertCore "mo:manticore/AlertCore";
import AlT "mo:manticore/AlertTypes";
import MT "mo:manticore/MonitoringTypes";
import Posting "mo:manticore/Posting";

import T "DeskTypes";
import Cat "Catalogue";
import Can "DeskCanonical";
import Auth "Authority";
import Fixings "Fixings";
import Eod "EndOfDay";
import CallT "CallTypes";
import CallCore "CallCore";
import CuT "CustodyTypes";
import CustodyCore "CustodyCore";
import ST "SettlementTypes";
import SettlementCore "SettlementCore";
import SettlementMessages "SettlementMessages";
import FT "FinancingTypes";
import FinancingCore "FinancingCore";
import VT "ValuationTypes";
import Valuation "Valuation";
import HedgeCore "HedgeCore";
import Attribution "Attribution";
import CoT "CollateralTypes";
import CollateralCore "CollateralCore";
import LT "LimitTypes";
import LimitCore "LimitCore";
import RT "ReconciliationTypes";
import ReconciliationCore "ReconciliationCore";
import ReconciliationMessages "ReconciliationMessages";
import Shard "mo:kernel/sweep/Shard";
import R "mo:kernel/rows/StableRows";
import Map "mo:core/Map";
import DT "mo:tachyon/DvpTypes";
import Sha256 "mo:sha2/Sha256";
import Calendar "mo:journal/Calendar";

module {

  public type Block = Auth.Block;
  public type Blocks = Auth.Blocks;

  public type State = {
    arena : RI.Arena;
    var height : Nat;
    authority : Auth.State;
    close : CloseCore.State;
    fixings : Fixings.State;
    alerts : AlertCore.State;
    eod : Eod.State;
    treasury : TreasuryCore.State;
    calls : CallCore.State;
    custody : CustodyCore.State;
    settlement : SettlementCore.State;
    financing : FinancingCore.State;
    hedges : HedgeCore.State;
    attribution : Attribution.State;
    collateral : CollateralCore.State;
    limits : LimitCore.State;
    reconciliation : ReconciliationCore.State;
  };

  public func newState(installer : Principal) : State { newStateIn(installer, RI.newArena()) };
  /// A state whose indexes live in a given arena: the live state's own, or the replay's, whose pages are reused.
  public func newStateIn(installer : Principal, arena : RI.Arena) : State {
    {
      arena; var height = 0;
      authority = Auth.newState(arena, installer);
      close = CloseCore.newState();
      fixings = Fixings.newState(arena);
      alerts = AlertCore.newState(arena);
      eod = Eod.newState();
      treasury = TreasuryCore.newState(arena);
      calls = CallCore.newState(arena);
      custody = CustodyCore.newState(arena);
      settlement = SettlementCore.newState(arena);
      financing = FinancingCore.newState(arena);
      hedges = HedgeCore.newState(arena);
      attribution = Attribution.newState(arena);
      collateral = CollateralCore.newState(arena);
      limits = LimitCore.newState(arena);
      reconciliation = ReconciliationCore.newState(arena);
    }
  };

  public func height(s : State) : Nat { s.height };

  // ═══════════════════════════════════════════════════════
  //  WHAT A COMMAND DOES
  // ═══════════════════════════════════════════════════════

  public type JournalStep = { #event : JT.Event; #existing : Nat };
  /// The desk event a command records, the events that follow it in the same act (one block each, in order), and
  /// its journal steps. Committed in that order: event, extras, journal.
  public type Plan = { event : ?T.Event; extra : [T.Event]; journal : [JournalStep] };
  type Res<X> = Result.Result<X, T.Error>;

  func only(ev : T.Event) : Res<Plan> { #ok({ event = ?ev; extra = []; journal = [] }) };
  func nothing() : Res<Plan> { #ok({ event = null; extra = []; journal = [] }) };
  func journalConfig(r : Result.Result<JT.Event, JT.ConfigError>) : Res<Plan> {
    switch (r) { case (#err(e)) #err(#JournalConfigError({ error = e })); case (#ok(e)) #ok({ event = null; extra = []; journal = [#event(e)] }) }
  };
  func planned(r : Auth.Planned) : Res<Plan> { switch (r) { case (#err(e)) #err(e); case (#ok(null)) nothing(); case (#ok(?ev)) only(ev) } };

  func requirePostableAccount(js : JCore.State, role : Text, code : Text) : ?T.Error {
    let ?a = JCore.getAccount(js, code) else return ?#UnknownAccount({ role; account = code });
    if (a.status == #closed) return ?#UnknownAccount({ role = role # " (closed)"; account = code });
    if (a.attributes.usage == #header) return ?#InvalidPair({ reason = "account " # code # " is a header account and cannot carry postings" });
    null
  };

  /// The day the daily limits of a command are measured on: its posting date where it has one, else today.
  public func commandDay(c : T.Command) : ?Nat {
    switch (c) {
      case (#settleDealLeg(x)) ?x.postingDate;
      case (#markDeal(x)) ?x.postingDate;
      case (#resolveNostroBreak(x)) ?x.postingDate;
      case (#openEndOfDay(x)) ?x.businessDate;
      case (#revaluePositions(x)) ?x.postingDate;
      case (#resetCallRate(x)) ?x.postingDate;
      case (#adjustCallBalance(x)) ?x.postingDate;
      case (#settleCall(x)) ?x.postingDate;
      case (#processCorporateAction(x)) ?x.postingDate;
      case (#buyIn(x)) ?x.postingDate;
      case (#settleRepoLeg(x)) ?x.postingDate;
      case (#resetRepoRate(x)) ?x.postingDate;
      case (#meetMarginCall(x)) ?x.postingDate;
      case (#settleLoanLeg(x)) ?x.postingDate;
      case (#assessHedge(x)) ?x.postingDate;
      case (#dedesignateHedge(x)) ?x.postingDate;
      case (_) null;
    }
  };
  /// The book an operation acts in, read out of the operation's own data.
  public func commandBookOf(s : State, c : T.Command) : ?T.BookId {
    switch (c) {
      case (#captureDeal(x)) ?x.book;
      case (#setTreasuryLimit(x)) ?x.limit.book;
      case (#openEndOfDay(x)) ?x.book;
      case (#setRetryPolicy(x)) ?x.book;
      case (#resolveEndOfDayFailure(x)) ?x.book;
      case (#confirmDeal(x)) bookOfDeal(s, x.deal);
      case (#amendDeal(x)) bookOfDeal(s, x.deal);
      case (#cancelDeal(x)) bookOfDeal(s, x.deal);
      case (#settleDealLeg(x)) bookOfDeal(s, x.deal);
      case (#markDeal(x)) bookOfDeal(s, x.deal);
      case (#openCall(x)) ?x.book;
      case (#resetCallRate(x)) bookOfCall(s, x.call);
      case (#adjustCallBalance(x)) bookOfCall(s, x.call);
      case (#serveCallNotice(x)) bookOfCall(s, x.call);
      case (#settleCall(x)) bookOfCall(s, x.call);
      case (#setBookDepot(x)) ?x.book;
      case (#assignDealDepot(x)) bookOfDeal(s, x.deal);
      case (#transferDepot(x)) bookOfDeal(s, x.lot);
      case (#instructSettlement(x)) bookOfDeal(s, x.deal);
      case (#splitDeal(x)) bookOfDeal(s, x.deal);
      case (#buyIn(x)) bookOfInstruction(s, x.instruction);
      case (#cancelSettlement(x)) bookOfInstruction(s, x.instruction);
      case (#openRepo(x)) ?x.book;
      case (#openLoan(x)) ?x.book;
      case (#settleRepoLeg(x)) bookOfRepo(s, x.repo);
      case (#resetRepoRate(x)) bookOfRepo(s, x.repo);
      case (#meetMarginCall(x)) bookOfRepo(s, x.repo);
      case (#substituteCollateral(x)) bookOfRepo(s, x.repo);
      case (#settleLoanLeg(x)) bookOfLoan(s, x.loan);
      case (#recallLoan(x)) bookOfLoan(s, x.loan);
      case (#instructFinancing(x)) { switch (x.family) { case (#repo) bookOfRepo(s, x.id); case (#loan) bookOfLoan(s, x.id); case (#treasury) bookOfDeal(s, x.id); case (#collateral) bookOfSubstitution(s, x.id) } };
      case (#pledgeCollateral(x)) bookOfDeal(s, x.lot);
      case (#openCollateralSubstitution(x)) bookOfDeal(s, x.lot);
      case (#settleCollateralSubstitution(x)) bookOfSubstitution(s, x.substitution);
      case (#designateHedge(x)) bookOfDeal(s, x.hedging);
      case (#assessHedge(x)) { switch (HedgeCore.hedge(s.hedges, x.hedge)) { case (?h) bookOfDeal(s, h.hedging); case null null } };
      case (#dedesignateHedge(x)) { switch (HedgeCore.hedge(s.hedges, x.hedge)) { case (?h) bookOfDeal(s, h.hedging); case null null } };
      case (_) null;
    }
  };
  func bookOfRepo(s : State, id : Nat) : ?T.BookId { switch (FinancingCore.repo(s.financing, id)) { case (?r) ?r.book; case null null } };
  func bookOfLoan(s : State, id : Nat) : ?T.BookId { switch (FinancingCore.loan(s.financing, id)) { case (?r) ?r.book; case null null } };
  func bookOfInstruction(s : State, id : Nat) : ?T.BookId { switch (SettlementCore.instruction(s.settlement, id)) { case (?i) bookOfDeal(s, i.deal); case null null } };
  func bookOfCall(s : State, call : Nat) : ?T.BookId { switch (CallCore.row(s.calls, call)) { case (?r) ?r.book; case null null } };
  func balanceOfCall(s : State, call : Nat) : [(Text, Nat)] { switch (CallCore.row(s.calls, call)) { case (?r) [(r.currency, r.balance)]; case null [] } };
  func bookOfDeal(s : State, deal : Nat) : ?T.BookId { switch (TreasuryCore.row(s.treasury, deal)) { case (?r) ?r.book; case null null } };
  func bookOfSubstitution(s : State, id : Nat) : ?T.BookId { switch (CollateralCore.securitiesRow(s.collateral, id)) { case (?r) { switch (r.lot) { case (?l) bookOfDeal(s, l); case null null } }; case null null } };
  func notionalOfDeal(s : State, deal : Nat) : [(Text, Nat)] { switch (TreasuryCore.row(s.treasury, deal)) { case (?r) [(treasuryRowCurrency(s, r), r.notional)]; case null [] } };
  /// The currency a deal's result is in: the quote currency of a forward, a swap or an option, the instrument's
  /// currency of a lot, the deal's own otherwise.
  public func resultCurrency(s : State, bb : Blocks, deal : Nat) : Text {
    switch (TreasuryCore.row(s.treasury, deal)) {
      case (?r) {
        switch (treasuryKindOf(bb, r)) {
          case (?#fxForward(f)) f.quote;
          case (?#fxSwap(x)) x.near.quote;
          case (?#fxOption(o)) o.quote;
          case (_) treasuryRowCurrency(s, r);
        }
      };
      case null "";
    }
  };
  func treasuryRowCurrency(s : State, r : TreasuryCore.DealRow) : Text {
    if (r.kind == 4) { switch (TreasuryCore.security(s.treasury, r.isin)) { case (?x) x.currency; case null "" } } else r.currency
  };
  func treasuryKindTotals(s : State, kind : TT.DealKind) : [(Text, Nat)] {
    switch (kind) {
      case (#moneyMarket(m)) [(m.currency, m.principal)];
      case (#fxForward(f)) [(f.base, f.baseAmount)];
      case (#fxSwap(x)) [(x.near.base, x.near.baseAmount + x.far.baseAmount)];
      case (#security(t)) { switch (TreasuryCore.security(s.treasury, t.isin)) { case (?x) [(x.currency, t.nominal)]; case null [] } };
      case (#irs(i)) [(i.currency, i.notional)];
      case (#fxOption(o)) [(o.quote, o.premium)];
    }
  };
  /// Per-currency totals of the money a command moves or commits: what the ceilings and daily limits measure.
  public func commandTotals(s : State, c : T.Command) : [(Text, Nat)] {
    switch (c) {
      case (#captureDeal(x)) treasuryKindTotals(s, x.kind);
      case (#amendDeal(x)) treasuryKindTotals(s, x.kind);
      case (#cancelDeal(x)) notionalOfDeal(s, x.deal);
      case (#settleDealLeg(x)) notionalOfDeal(s, x.deal);
      case (#markDeal(x)) notionalOfDeal(s, x.deal);
      case (#resolveNostroBreak(x)) { switch (x.correction) { case (?cr) [(cr.currency, cr.amount)]; case null [] } };
      case (#openCall(x)) [(x.terms.currency, x.terms.principal)];
      case (#adjustCallBalance(x)) { switch (CallCore.row(s.calls, x.call)) { case (?r) [(r.currency, Int.abs(x.delta))]; case null [] } };
      case (#resetCallRate(x)) balanceOfCall(s, x.call);
      case (#settleCall(x)) balanceOfCall(s, x.call);
      case (#transferDepot(x)) { switch (TreasuryCore.row(s.treasury, x.lot)) { case (?r) [(treasuryRowCurrency(s, r), x.nominal)]; case null [] } };
      case (#instructSettlement(x)) notionalOfDeal(s, x.deal);
      case (#splitDeal(x)) notionalOfDeal(s, x.deal);
      case (#buyIn(x)) { switch (SettlementCore.instruction(s.settlement, x.instruction)) { case (?i) notionalOfDeal(s, i.deal); case null [] } };
      case (#cancelSettlement(x)) { switch (SettlementCore.instruction(s.settlement, x.instruction)) { case (?i) notionalOfDeal(s, i.deal); case null [] } };
      case (#openRepo(x)) [(x.terms.currency, x.terms.cash)];
      case (#settleRepoLeg(x)) { switch (FinancingCore.repo(s.financing, x.repo)) { case (?r) [(r.currency, r.cash)]; case null [] } };
      case (#resetRepoRate(x)) { switch (FinancingCore.repo(s.financing, x.repo)) { case (?r) [(r.currency, r.cash)]; case null [] } };
      case (#meetMarginCall(x)) { switch (FinancingCore.repo(s.financing, x.repo)) { case (?r) [(r.currency, x.cash)]; case null [] } };
      case (#openLoan(x)) [(x.terms.currency, FinancingCore.loanValue({ id = 0; state = #open; book = ""; cpHash = 0; currency = x.terms.currency; isin = x.terms.isin; nominal = x.terms.nominal; valueMicro = x.terms.valueMicro; feeBps = 0; dayCount = 0; cashCollateral = 0; rebateBps = 0; collateralIsin = ""; collateralNominal = 0; start = 0; noticeDays = 0; returnDay = 0; accrualFrom = 0; feeBefore = 0; feePosted = 0; rebateBefore = 0; rebatePosted = 0; manufactured = 0; depot = ""; lastBlock = 0; refHash = 0 }))];
      case (#settleLoanLeg(x)) { switch (FinancingCore.loan(s.financing, x.loan)) { case (?r) [(r.currency, FinancingCore.loanValue(r))]; case null [] } };
      case (#instructFinancing(x)) { switch (x.family, FinancingCore.repo(s.financing, x.id), FinancingCore.loan(s.financing, x.id)) { case (#repo, ?r, _) [(r.currency, r.cash)]; case (#loan, _, ?l) [(l.currency, FinancingCore.loanValue(l))]; case (#collateral, _, _) { switch (CollateralCore.securitiesRow(s.collateral, x.id)) { case (?r) [(r.currency, r.cashReturned)]; case null [] } }; case (_) [] } };
      case (#postCollateralCash(x)) [(x.currency, x.amount)];
      case (#openCollateralSubstitution(x)) [(x.currency, x.cashReturned)];
      case (#settleCollateralSubstitution(x)) { switch (CollateralCore.securitiesRow(s.collateral, x.substitution)) { case (?r) [(r.currency, r.cashReturned)]; case null [] } };
      case (#settleCollateralInterest(x)) [(x.currency, Int.abs(CollateralCore.cashRow(s.collateral, x.agreement, x.currency).interestAccrued))];
      case (#assessHedge(x)) { switch (HedgeCore.hedge(s.hedges, x.hedge)) { case (?h) notionalOfDeal(s, h.hedging); case null [] } };
      case (#dedesignateHedge(x)) { switch (HedgeCore.hedge(s.hedges, x.hedge)) { case (?h) notionalOfDeal(s, h.hedging); case null [] } };
      case (_) [];
    }
  };
  public func operationOf(s : State, c : T.Command) : E.Operation {
    { permission = Auth.commandPermission(c); partition = commandBookOf(s, c); totals = commandTotals(s, c) }
  };

  // ─── the valuation context and the terms of a deal ─────────────────────────

  /// The terms of a deal: the kind recorded by the block that captured it, or by the last amendment.
  public func treasuryTerms(bb : Blocks) : Nat -> ?TT.DealKind {
    func(block : Nat) : ?TT.DealKind {
      switch (bb.get(block)) {
        case (?b) { switch (b.event) { case (#treasury(#dealCaptured(x))) ?x.kind; case (#treasury(#dealAmended(x))) ?x.kind; case (_) null } };
        case null null;
      }
    }
  };
  public func treasuryKindOf(bb : Blocks, r : TreasuryCore.DealRow) : ?TT.DealKind { treasuryTerms(bb)(r.termsBlock) };
  public func treasuryCaptureOf(bb : Blocks, id : Nat) : (Text, Text) {
    switch (bb.get(id)) { case (?b) { switch (b.event) { case (#treasury(#dealCaptured(x))) (x.counterparty.name, x.reference); case (_) ("", "") } }; case null ("", "") }
  };
  public func treasuryCounterpartyOf(bb : Blocks, id : Nat) : TT.Counterparty {
    let none : TT.Counterparty = { party = null; name = ""; bic = ""; lei = "" };
    switch (bb.get(id)) { case (?b) { switch (b.event) { case (#treasury(#dealCaptured(x))) x.counterparty; case (_) none } }; case null none }
  };
  /// The counterparty and the reference of a call, from its opening block.
  public func callOpeningOf(bb : Blocks, id : Nat) : (Text, Text, ?TT.CashAccount) {
    switch (bb.get(id)) { case (?b) { switch (b.event) { case (#call(#opened(x))) (x.counterparty.name, x.reference, ?x.terms.cash); case (_) ("", "", null) } }; case null ("", "", null) }
  };
  func callErr<X>(e : CallT.Error) : Res<X> { #err(#CallError({ error = e })) };
  func policyOf(s : State) : Res<TT.Policy> { switch (TreasuryCore.policy(s.treasury)) { case (?p) #ok(p); case null #err(#TreasuryError({ error = #NoPolicy })) } };
  /// The desk's calendar: the journal's, with the shift to the next business day the call money's notice uses.
  func nextBusinessDay(js : JCore.State) : Nat -> Nat {
    switch (JCore.calendar(js)) {
      case (?c) { let cal : Calendar.Calendar = { restDays = c.restDays; holidays = c.holidays }; func(d : Nat) : Nat { if (Calendar.isBusinessDay(cal, d)) d else Calendar.nextBusinessDay(cal, d) } };
      case null func(d : Nat) : Nat { d };
    }
  };

  // ─── limits across both families ───────────────────────────────────────────

  /// A counterparty's exposure in a currency across a book: Manticore's treasury deals as its own limit measures
  /// them, plus the desk's open call balances.
  public func treasuryExposure(s : State, book : Text, cpName : Text, currency : Text) : Nat {
    let h = TreasuryCore.hash8(cpName);
    var total = 0;
    for (r in TreasuryCore.openInBook(s.treasury, book).vals()) { if (r.cpHash == h and Text.equal(treasuryRowCurrency(s, r), currency)) total += (if (r.kind == 4) r.nominalLeft else r.notional) };
    total
  };
  public func counterpartyExposure(s : State, book : Text, cpName : Text, currency : Text) : Nat {
    treasuryExposure(s, book, cpName, currency) + CallCore.exposureOf(s.calls, book, cpName, currency)
  };
  /// The counterparty limit measured with the call balances counted, for a new act of `amount`: a breach with no
  /// approver is a refusal; with one, the breach is recorded after the act unless Manticore's own measure records
  /// it already (a treasury capture whose treasury-only exposure breaches records its own block).
  func combinedLimitEvents(s : State, book : Text, cp : TT.Counterparty, currency : Text, amount : Nat, treasuryOnlyBreaches : Bool, id : Nat, approver : ?Principal, day : Nat) : Res<[T.Event]> {
    switch (TreasuryCore.limitOf(s.treasury, book, #counterpartyExposure, currency, cp.name)) {
      case null #ok([]);
      case (?limit) {
        let measured = counterpartyExposure(s, book, cp.name, currency) + amount;
        if (measured <= limit) return #ok([]);
        switch (approver) {
          case null #err(#TreasuryError({ error = #LimitBreached({ kind = "counterpartyExposure"; subject = cp.name; limit; measured }) }));
          case (?a) { if (treasuryOnlyBreaches) #ok([]) else #ok([#treasury(#limitBreached({ limit = { book; kind = #counterpartyExposure; currency; subject = cp.name; value = limit }; measured; deal = id; approver = a; day }))]) };
        }
      };
    }
  };

  // ─── the limit tree ─────────────────────────────────────────────────────────

  func limitErr<X>(e : LT.Error) : Res<X> { #err(#LimitError({ error = e })) };
  func collateralErr<X>(e : CoT.Error) : Res<X> { #err(#CollateralError({ error = e })) };
  /// Whether a book is the desk named or lies under it in the book tree.
  func isUnderBook(s : State) : (Text, Text) -> Bool {
    func(book : Text, desk : Text) : Bool {
      var cur : ?Text = ?book;
      var depth = 0;
      label up loop {
        switch (cur) {
          case (?b) { if (Text.equal(b, desk)) return true; cur := switch (Auth.getBook(s.authority, b)) { case (?bk) bk.parent; case null null } };
          case null break up;
        };
        depth += 1;
        if (depth > T.MAX_BOOK_DEPTH) break up;
      };
      false
    }
  };
  func bookExists(s : State) : Text -> Bool { func(b : Text) : Bool { switch (Auth.getBook(s.authority, b)) { case (?bk) bk.open; case null false } } };
  func daysLeft(maturity : Nat, day : Nat) : Nat { if (maturity > day) maturity - day else 0 };
  /// What the tree measures of a treasury deal at capture: the principal, the base amount, the nominal or the
  /// notional in the deal's currency; the instrument and its issuer for a purchase; the days left of a term.
  func treasuryCaptureFacts(s : State, id : Nat, book : Text, cpHash : Nat, kind : TT.DealKind, day : Nat) : LimitCore.RowFacts {
    let none : LimitCore.RowFacts = { family = #treasury; id; book; cpHash; currency = ""; amount = 0; isin = ""; issuerHash = 0; classification = null; remainingDays = null };
    switch (kind) {
      case (#moneyMarket(m)) { { none with currency = m.currency; amount = m.principal; remainingDays = ?daysLeft(m.maturity, day) } };
      case (#fxForward(f)) { { none with currency = f.base; amount = f.baseAmount } };
      case (#fxSwap(x)) { { none with currency = x.near.base; amount = x.near.baseAmount } };
      case (#security(t)) {
        let (currency, issuerHash, maturity) = switch (TreasuryCore.security(s.treasury, t.isin)) { case (?x) (x.currency, x.issuerHash, x.maturity); case null ("", 0, 0) };
        let buy = t.direction == #buy;
        let classification = if (buy) { switch (CustodyCore.instrument(s.custody, t.isin)) { case (?i) ?i.classification; case null null } } else null;
        { none with currency; amount = t.nominal; isin = if (buy) t.isin else ""; issuerHash = if (buy) issuerHash else 0; classification; remainingDays = ?daysLeft(maturity, day) }
      };
      case (#irs(i)) { { none with currency = i.currency; amount = i.notional; remainingDays = ?daysLeft(i.maturity, day) } };
      case (#fxOption(o)) { { none with currency = o.base; amount = o.baseAmount } };
    }
  };
  /// The same facts read from a treasury row, as the sweep reads them: the nominal left of a security, the
  /// notional of everything else.
  func treasuryRowFacts(s : State, r : TreasuryCore.DealRow, day : Nat) : LimitCore.RowFacts {
    let buy = (r.flags & TreasuryCore.F_BUY) != 0;
    let (currency, issuerHash, classification) : (Text, Nat, ?CuT.Classification) = if (r.kind == 4) {
      let sec = TreasuryCore.security(s.treasury, r.isin);
      (switch (sec) { case (?x) x.currency; case null "" }, if (buy) { switch (sec) { case (?x) x.issuerHash; case null 0 } } else 0, if (buy) { switch (CustodyCore.instrument(s.custody, r.isin)) { case (?i) ?i.classification; case null null } } else null)
    } else (r.currency, 0, null);
    { family = #treasury; id = r.id; book = r.book; cpHash = r.cpHash; currency; amount = if (r.kind == 4) r.nominalLeft else r.notional; isin = if (buy) r.isin else ""; issuerHash; classification;
      remainingDays = if (r.kind == 1 or r.kind == 4 or r.kind == 5) ?daysLeft(r.maturity, day) else null }
  };
  func callFacts(id : Nat, book : Text, cpHash : Nat, currency : Text, amount : Nat) : LimitCore.RowFacts {
    { family = #call; id; book; cpHash; currency; amount; isin = ""; issuerHash = 0; classification = null; remainingDays = null }
  };
  func repoFacts(id : Nat, book : Text, cpHash : Nat, currency : Text, cash : Nat, maturity : Nat, day : Nat) : LimitCore.RowFacts {
    { family = #repo; id; book; cpHash; currency; amount = cash; isin = ""; issuerHash = 0; classification = null; remainingDays = if (maturity == 0) null else ?daysLeft(maturity, day) }
  };
  func loanFacts(id : Nat, book : Text, cpHash : Nat, currency : Text, value : Nat) : LimitCore.RowFacts {
    { family = #loan; id; book; cpHash; currency; amount = value; isin = ""; issuerHash = 0; classification = null; remainingDays = null }
  };
  /// The tree's verdict on a new row: the utilisation it records when the row falls under any node, and the
  /// breaches it makes. A breach without an approver is a refusal; with one, every breach is recorded after the
  /// row.
  func treeEvents(s : State, f : LimitCore.RowFacts, approver : ?Principal, day : Nat) : Res<[T.Event]> {
    let nodes = LimitCore.nodesFor(s.limits, LimitCore.prepare(s.limits), f, isUnderBook(s));
    if (nodes.size() == 0) return #ok([]);
    let breaches = LimitCore.breachesOf(s.limits, nodes, f.amount);
    let out = List.empty<T.Event>();
    List.add(out, #limits(#utilised({ family = f.family; id = f.id; currency = f.currency; amount = f.amount; nodes; day })));
    if (breaches.size() > 0) {
      switch (approver) {
        case null { let (node, measured, limit) = breaches[0]; return limitErr(#Breached({ node; measured; limit })) };
        case (?ap) { for ((node, measured, limit) in breaches.vals()) List.add(out, #limits(#breached({ node; family = f.family; id = f.id; measured; limit; approver = ap; day }))) };
      };
    };
    #ok(List.toArray(out))
  };
  /// The facts of a command that adds a row to the tree, with the approver it names; nothing for any other.
  func treeFactsOf(s : State, command : T.Command, day : Nat) : ?(LimitCore.RowFacts, ?Principal) {
    switch (command) {
      case (#captureDeal(x)) ?(treasuryCaptureFacts(s, s.height, x.book, TreasuryCore.hash8(x.counterparty.name), x.kind, day), x.approver);
      case (#openCall(x)) ?(callFacts(s.height, x.book, TreasuryCore.hash8(x.counterparty.name), x.terms.currency, x.terms.principal), x.approver);
      case (#adjustCallBalance(x)) {
        if (x.delta <= 0) return null;
        switch (CallCore.row(s.calls, x.call)) { case (?r) ?(callFacts(r.id, r.book, r.cpHash, r.currency, Int.abs(x.delta)), x.approver); case null null }
      };
      case (#openRepo(x)) ?(repoFacts(s.height, x.book, TreasuryCore.hash8(x.counterparty.name), x.terms.currency, x.terms.cash, switch (x.terms.maturity) { case (?m) m; case null 0 }, day), null);
      case (#openLoan(x)) ?(loanFacts(s.height, x.book, TreasuryCore.hash8(x.counterparty.name), x.terms.currency, TreasuryMath.cleanCost(x.terms.nominal, x.terms.valueMicro)), null);
      case (_) null;
    }
  };
  func treeEventsOf(s : State, command : T.Command, day : Nat) : Res<[T.Event]> {
    switch (treeFactsOf(s, command, day)) { case (?(f, approver)) treeEvents(s, f, approver, day); case null #ok([]) }
  };
  /// The refusal blocks a tree breach leaves: one per breached node, so the desk proves what it prevented.
  public func refusalTrail(s : State, js : JCore.State, now : Nat64, subject : Principal, command : T.Command, e : T.Error) : [T.Event] {
    switch (e) { case (#LimitError({ error = #Breached(_) })) {}; case (_) return [] };
    let day = JCore.effectiveToday(js, now);
    let ?(f, _) = treeFactsOf(s, command, day) else return [];
    let nodes = LimitCore.nodesFor(s.limits, LimitCore.prepare(s.limits), f, isUnderBook(s));
    Array.map<(Text, Nat, Nat), T.Event>(LimitCore.breachesOf(s.limits, nodes, f.amount), func((node, measured, limit)) { #limits(#breachRefused({ node; family = f.family; subject; amount = f.amount; measured; limit; day })) })
  };

  // ─── collateral ─────────────────────────────────────────────────────────────

  func collateralPolicyOf(s : State) : Res<CoT.Policy> { switch (CollateralCore.policy(s.collateral)) { case (?p) #ok(p); case null collateralErr(#NoPolicy) } };
  func agreementRow(s : State, id : Text) : Res<CollateralCore.AgreementRow> { switch (CollateralCore.requireAgreement(s.collateral, id)) { case (#ok(r)) #ok(r); case (#err(e)) collateralErr(e) } };
  /// An amount in one currency stated in another at the day's recorded rates, through the functional currency.
  func convertOn(s : State, amount : Nat, from : Text, to : Text, day : Nat) : Res<Nat> {
    if (Text.equal(from, to)) return #ok(amount);
    let ?functional = CloseCore.functional(s.close) else return #err(#NoFunctionalCurrency);
    var q = TreasuryMath.ofNat(amount);
    if (not Text.equal(from, functional)) { let ?r = CloseCore.rateOn(s.close, from, day) else return #err(#MissingRate({ currency = from; asOf = day })); q := TreasuryMath.mul(q, TreasuryMath.q(r.numerator, r.denominator)) };
    if (not Text.equal(to, functional)) { let ?r = CloseCore.rateOn(s.close, to, day) else return #err(#MissingRate({ currency = to; asOf = day })); q := TreasuryMath.mul(q, TreasuryMath.q(r.denominator, r.numerator)) };
    #ok(TreasuryMath.roundNat(q))
  };
  func convertSigned(s : State, amount : Int, from : Text, to : Text, day : Nat) : Res<Int> {
    switch (convertOn(s, Int.abs(amount), from, to, day)) { case (#err(e)) #err(e); case (#ok(v)) #ok(if (amount < 0) -v else v) }
  };
  /// A security's value in an agreement's pool on a day: the day's price, the schedule's haircut for the
  /// instrument's class and residual maturity, the mismatch add-on, in the agreement's currency.
  func poolSecuritiesValue(s : State, a : CollateralCore.AgreementRow, isin : Text, nominal : Nat, day : Nat) : Res<Nat> {
    let ?sec = TreasuryCore.security(s.treasury, isin) else return treasuryErr(#UnknownSecurity({ isin }));
    let ?inst = CustodyCore.instrument(s.custody, isin) else return custodyErr(#UnknownInstrument({ isin }));
    let ?price = priceOf(s, isin, day) else return collateralErr(#NoPrice({ isin; day }));
    let ?cut = CollateralCore.haircutBps(s.collateral, a, inst.classification, daysLeft(sec.maturity, day)) else return collateralErr(#NoHaircut({ isin }));
    convertOn(s, CollateralCore.securitiesValue(nominal, price, cut, not Text.equal(sec.currency, a.currency)), sec.currency, a.currency, day)
  };
  func poolCashValue(s : State, a : CollateralCore.AgreementRow, amount : Nat, currency : Text, day : Nat) : Res<Nat> {
    convertOn(s, CollateralCore.cashValue(amount, not Text.equal(currency, a.currency)), currency, a.currency, day)
  };
  /// The credit support balance of an agreement on a day: what the desk holds less what it posted, cash and
  /// securities, after haircuts, in the agreement's currency.
  func poolBalance(s : State, a : CollateralCore.AgreementRow, day : Nat) : Res<Int> {
    var bal : Int = 0;
    for (c in CollateralCore.cashOf(s.collateral, a.id).vals()) {
      let net : Int = (c.received : Int) - c.given;
      if (net != 0) { let v = switch (poolCashValue(s, a, Int.abs(net), c.currency, day)) { case (#err(e)) return #err(e); case (#ok(v)) v }; bal += (if (net > 0) v else -v) };
    };
    for (r in CollateralCore.securitiesOf(s.collateral, a.id).vals()) {
      if (r.state != #live) continue;
      let v = switch (poolSecuritiesValue(s, a, r.isin, r.nominal, day)) { case (#err(e)) return #err(e); case (#ok(v)) v };
      bal += (if (r.given) -v else v);
    };
    #ok(bal)
  };
  /// What a movement credits to the agreement's open call: its value when the movement runs the call's way.
  func callCreditFor(s : State, a : CollateralCore.AgreementRow, deskDelivers : Bool, value : Nat) : Nat {
    if (a.openCall == 0) return 0;
    switch (CollateralCore.call(s.collateral, a.openCall)) { case (?c) { if (c.state == CollateralCore.CALL_OPEN and c.deliver == deskDelivers) value else 0 }; case null 0 }
  };
  func callMetAfter(s : State, a : CollateralCore.AgreementRow, credit : Nat, day : Nat) : [T.Event] {
    if (a.openCall == 0 or credit == 0) return [];
    switch (CollateralCore.call(s.collateral, a.openCall)) { case (?c) { if (c.state == CollateralCore.CALL_OPEN and c.outstanding <= credit) [#collateral(#callMet({ agreement = a.id; call = c.id; day }))] else [] }; case null [] }
  };
  func collateralPost(s : State, js : JCore.State, journalCaller : Principal, now : Nat64, p : CoT.Policy, a : CollateralCore.AgreementRow, ev : CoT.Event, purpose : Text, parts : [Text], postingDate : Nat, valueDate : Nat, period : Text, narration : Text, extras : [T.Event]) : Res<Plan> {
    let legs = CollateralCore.legsOf(p, a, ev);
    if (legs.size() == 0) return #ok({ event = ?#collateral(ev); extra = extras; journal = [] });
    switch (postLegs(js, journalCaller, now, purpose, parts, legs, postingDate, valueDate, period, narration)) {
      case (#err(e)) #err(e);
      case (#ok(plan)) #ok({ event = ?#collateral(ev); extra = extras; journal = plan.journal });
    }
  };
  /// A lot of the desk's own that can be pledged: a settled purchase, held in a depot with enough available.
  func pledgeableLot(s : State, lot : Nat, nominal : Nat) : Res<(TreasuryCore.DealRow, Text)> {
    let r = switch (treasuryRow(s, lot)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    if (r.kind != 4 or (r.flags & TreasuryCore.F_BUY) == 0) return custodyErr(#LotNotIn({ lot; state = "not a purchase" }));
    if (nominal == 0) return collateralErr(#InvalidMove({ reason = "a pledge is a positive nominal" }));
    var best : ?(Text, Nat) = null;
    for ((h, _) in CustodyCore.holdingsOfLot(s.custody, lot).vals()) {
      let d = CustodyCore.depotIdOfHash(s.custody, h);
      let av = CustodyCore.available(s.custody, lot, d);
      if (av >= nominal) return #ok((r, d));
      switch (best) { case (?(_, b)) { if (av > b) best := ?(d, av) }; case null best := ?(d, av) };
    };
    switch (best) { case (?(d, av)) custodyErr(#DepotShort({ depot = d; isin = r.isin; held = av; wanted = nominal })); case null custodyErr(#LotNotIn({ lot; state = "not held in a depot" })) }
  };
  /// The settlement of a substitution: the securities live in the pool, the cash comes back, the call credited
  /// by the net the desk delivered.
  public func planSubstitutionSettlement(s : State, js : JCore.State, journalCaller : Principal, now : Nat64, agreement : Text, id : Nat, authId : Text, postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let p = switch (collateralPolicyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
    let a = switch (agreementRow(s, agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    let r = switch (CollateralCore.requireSecurities(s.collateral, agreement, id, #pledged)) { case (#err(e)) return collateralErr(e); case (#ok(r)) r };
    let v = switch (poolSecuritiesValue(s, a, r.isin, r.nominal, valueDate)) { case (#err(e)) return #err(e); case (#ok(v)) v };
    let back = switch (poolCashValue(s, a, r.cashReturned, r.currency, valueDate)) { case (#err(e)) return #err(e); case (#ok(v)) v };
    let credit = if (v >= back) callCreditFor(s, a, true, v - back) else callCreditFor(s, a, false, back - v);
    let c = CollateralCore.cashRow(s.collateral, agreement, r.currency);
    if (valueDate < c.accrualFrom) return collateralErr(#InvalidMove({ reason = "a settlement is not dated before the last accrual" }));
    let catchUp = if (r.cashReturned == 0) 0 else CollateralCore.interestTarget(a, c, valueDate) - c.interestAccrued;
    let ev : CoT.Event = #substitutionSettled({ agreement; substitution = id; callCredit = credit; interestCatchUp = catchUp; day = valueDate });
    let legs = CollateralCore.substitutionLegs(p, a, r, catchUp);
    let extras = callMetAfter(s, a, credit, valueDate);
    if (legs.size() == 0) return #ok({ event = ?#collateral(ev); extra = extras; journal = [] });
    switch (postLegs(js, journalCaller, now, "collateral-substitution", [authId, agreement, Nat.toText(id), Nat.toText(valueDate)], legs, postingDate, valueDate, period, narration)) {
      case (#err(e)) #err(e);
      case (#ok(plan)) #ok({ event = ?#collateral(ev); extra = extras; journal = plan.journal });
    }
  };

  // ─── the exposure of a counterparty's rows ──────────────────────────────────

  /// What a treasury row contributes to its counterparty's exposure on a day, in the agreement's currency: a
  /// funded placement's principal and accrued (a taking's the same, owed the other way), a derivative's mark;
  /// nothing for a security, whose settlement is delivery versus payment.
  func treasuryContribution(s : State, a : CollateralCore.AgreementRow, r : TreasuryCore.DealRow, day : Nat) : Res<?Int> {
    if (not CollateralCore.covers(a, #treasury)) return #ok(null);
    let placement = (r.flags & TreasuryCore.F_BUY) != 0;
    switch (r.kind) {
      case 1 {
        if (not TreasuryCore.legSettled(r, 0) or TreasuryCore.legSettled(r, 1)) return #ok(?0);
        let e : Int = (r.notional : Int) + r.accruedPosted;
        some(convertSigned(s, if (placement) e else -e, r.currency, a.currency, day))
      };
      case (2 or 3 or 6) some(convertSigned(s, r.markPosted, r.secondCurrency, a.currency, day));
      case 5 some(convertSigned(s, r.markPosted + r.accruedPosted, r.currency, a.currency, day));
      case (_) #ok(?0);
    }
  };
  func some(r : Res<Int>) : Res<?Int> { switch (r) { case (#ok(v)) #ok(?v); case (#err(e)) #err(e) } };
  func callContribution(s : State, a : CollateralCore.AgreementRow, r : CallCore.Row, day : Nat) : Res<?Int> {
    if (not CollateralCore.covers(a, #calls)) return #ok(null);
    if ((r.flags & CallCore.F_FUNDED) == 0) return #ok(?0);
    let e : Int = (r.balance : Int) + r.accruedPosted;
    some(convertSigned(s, if ((r.flags & CallCore.F_PLACEMENT) != 0) e else -e, r.currency, a.currency, day))
  };
  /// A started repo: the cash lender's exposure less the collateral's value after the repo's own haircut, owed to
  /// the desk on a reverse and by it on a repo.
  func repoContribution(s : State, a : CollateralCore.AgreementRow, r : FinancingCore.RepoRow, day : Nat) : Res<?Int> {
    if (not CollateralCore.covers(a, #repos)) return #ok(null);
    if (r.state != #started) return #ok(?0);
    let ?price = priceOf(s, r.isin, day) else return financingErr(#NoPrice({ isin = r.isin; day }));
    let e : Int = (FinancingCore.exposure(r, day) : Int) - FinancingCore.collateralValue(r.nominal, price, r.haircutBps);
    some(convertSigned(s, if (r.reverse) e else -e, r.currency, a.currency, day))
  };
  /// A started loan: the lent value and the fee due less the rebate and the collateral the desk holds.
  func loanContribution(s : State, a : CollateralCore.AgreementRow, l : FinancingCore.LoanRow, day : Nat) : Res<?Int> {
    if (not CollateralCore.covers(a, #loans)) return #ok(null);
    if (l.state != #started and l.state != #recalled) return #ok(?0);
    var e : Int = (FinancingCore.loanValue(l) : Int) + l.feePosted - l.rebatePosted - l.cashCollateral;
    if (l.collateralNominal > 0) {
      let ?price = priceOf(s, l.collateralIsin, day) else return financingErr(#NoPrice({ isin = l.collateralIsin; day }));
      e -= TreasuryMath.cleanCost(l.collateralNominal, price);
    };
    some(convertSigned(s, e, l.currency, a.currency, day))
  };
  func contributionUnder(a : CollateralCore.AgreementRow, c : Int) : Int { if (a.netting or c > 0) c else 0 };

  // ─── the risk sweep ─────────────────────────────────────────────────────────

  public type SweepAdvance = { day : Nat; family : Text; visited : Nat; familyDone : Bool; published : Bool; blocks : [Nat]; postings : [Nat] };

  /// One slice of the open sweep: the next bounded run of rows of the family in hand, each row's measure added
  /// to the nodes it falls under and its contribution to its counterparty's agreement; the slice recorded with its
  /// cursor so the next message resumes from the log. Past the last family, the publication: the utilisation per
  /// node, then per agreement the exposure, the pool valued, the interest accrued, the standing call superseded
  /// (an unmet one alerted first) and the day's call raised.
  public func runRiskSweepSlice(s : State, _bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, recorder : Recorder) : Res<SweepAdvance> {
    let ?w = s.limits.sweep else return limitErr(#NoSweep);
    let acc = newAcc(recorder);
    if (w.family > 3) return publishSweep(s, js, jb, journalCaller, now, acc, w);
    let prepared = LimitCore.prepare(s.limits);
    let under = isUnderBook(s);
    let nodeSums = Map.empty<Text, Nat>();
    let agSums = Map.empty<Text, (Int, Nat)>();
    var firstError : ?T.Error = null;
    func take(f : LimitCore.RowFacts, contribution : (CollateralCore.AgreementRow) -> Res<?Int>) {
      for (n in LimitCore.nodesFor(s.limits, prepared, f, under).vals()) {
        let cur = switch (Map.get(nodeSums, Text.compare, n)) { case (?v) v; case null 0 };
        Map.add(nodeSums, Text.compare, n, cur + f.amount);
      };
      switch (CollateralCore.agreementOfCounterpartyHash(s.collateral, f.cpHash)) {
        case (?a) {
          switch (contribution(a)) {
            case (#ok(?c)) { let (cs, cn) = switch (Map.get(agSums, Text.compare, a.id)) { case (?v) v; case null (0, 0) }; Map.add(agSums, Text.compare, a.id, (cs + contributionUnder(a, c), cn + 1)) };
            case (#ok(null)) {};
            case (#err(e)) { if (firstError == null) firstError := ?e };
          };
        };
        case null {};
      };
    };
    let family = LimitCore.familyOf(w.family);
    let (lo, hi) = R.fullRange(8);
    func idOf(k : Blob) : Nat { R.getNat(Blob.toArray(k), 0, 8) };
    let step = switch (family) {
      case (#treasury) Shard.stepRows(s.treasury.deals, lo, hi, w.cursor, w.sliceSize, func(k, _) {
        let id = idOf(k); if (id >= w.bound) return;
        switch (TreasuryCore.row(s.treasury, id)) { case (?r) { if (TreasuryCore.isOpen(r)) take(treasuryRowFacts(s, r, w.day), func(a) { treasuryContribution(s, a, r, w.day) }) }; case null {} };
      });
      case (#call) Shard.stepRows(s.calls.rows, lo, hi, w.cursor, w.sliceSize, func(k, _) {
        let id = idOf(k); if (id >= w.bound) return;
        switch (CallCore.row(s.calls, id)) { case (?r) { if (CallCore.isOpen(r)) take(callFacts(r.id, r.book, r.cpHash, r.currency, r.balance), func(a) { callContribution(s, a, r, w.day) }) }; case null {} };
      });
      case (#repo) Shard.stepRows(s.financing.repos, lo, hi, w.cursor, w.sliceSize, func(k, _) {
        let id = idOf(k); if (id >= w.bound) return;
        switch (FinancingCore.repo(s.financing, id)) { case (?r) { if (r.state != #closed) take(repoFacts(r.id, r.book, r.cpHash, r.currency, r.cash, r.maturity, w.day), func(a) { repoContribution(s, a, r, w.day) }) }; case null {} };
      });
      case (#loan) Shard.stepRows(s.financing.loans, lo, hi, w.cursor, w.sliceSize, func(k, _) {
        let id = idOf(k); if (id >= w.bound) return;
        switch (FinancingCore.loan(s.financing, id)) { case (?l) { if (l.state != #returned) take(loanFacts(l.id, l.book, l.cpHash, l.currency, FinancingCore.loanValue(l)), func(a) { loanContribution(s, a, l, w.day) }) }; case null {} };
      });
    };
    switch (firstError) { case (?e) return #err(e); case null {} };
    let nodes = Array.map<(Text, Nat), (Text, Nat)>(Map.toArray(nodeSums), func(x) { x });
    let agreements = Array.map<(Text, (Int, Nat)), (Text, Int, Nat)>(Map.toArray(agSums), func((a, (c, n))) { (a, c, n) });
    record(acc, #limits(#sweepSliced({ day = w.day; slice = w.slices; family; visited = step.visited; nextCursor = step.nextCursor; familyDone = step.completed; nodes; agreements })));
    #ok({ day = w.day; family = LT.familyText(family); visited = step.visited; familyDone = step.completed; published = false; blocks = List.toArray(acc.blocks); postings = [] })
  };
  /// The publication: every figure is computed before any block is recorded, so a missing price or rate refuses
  /// the publication whole and the sweep stays open to be published once the data is there.
  func publishSweep(s : State, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, w : LimitCore.Sweep) : Res<SweepAdvance> {
    let day = w.day;
    let nodes = Array.map<LimitCore.NodeRow, (Text, Nat)>(LimitCore.liveNodes(s.limits), func(n) { (n.id, n.pending) });
    let due = nextBusinessDay(js)(day + 1);
    let period = periodForDay(js, day);
    type Planned = { a : CollateralCore.AgreementRow; balance : Int; interest : [(CoT.Event, JT.PostingInput)] };
    let planned = List.empty<Planned>();
    for (a in CollateralCore.agreements(s.collateral).vals()) {
      let balance = switch (poolBalance(s, a, day)) { case (#err(e)) return #err(e); case (#ok(b)) b };
      let interest = List.empty<(CoT.Event, JT.PostingInput)>();
      for (c in CollateralCore.cashOf(s.collateral, a.id).vals()) {
        let delta = CollateralCore.interestTarget(a, c, day) - c.interestAccrued;
        if (delta != 0) {
          let ?p = CollateralCore.policy(s.collateral) else return collateralErr(#NoPolicy);
          let ?per = period else return #err(#JournalError({ error = #UnknownPeriod({ period = "open period containing day " # Nat.toText(day) }) }));
          let ev : CoT.Event = #interestAccrued({ agreement = a.id; currency = c.currency; interest = delta; day });
          let legs = CollateralCore.legsOf(p, a, ev);
          if (not Posting.balances(legs)) return limitErr(#InvalidSweep({ reason = "collateral interest " # a.id # " " # c.currency # ": the legs do not balance" }));
          List.add(interest, (ev, { idempotencyKey = Posting.key("collateral-interest", [a.id, c.currency, Nat.toText(day)]); postingDate = day; valueDate = day; period = per; legs; sourceRef = { kind = "collateral-interest"; id = a.id # "/" # c.currency # "/" # Nat.toText(day) }; narration = "collateral interest " # a.id # " " # c.currency; correctionOf = null }));
        };
      };
      List.add(planned, { a; balance; interest = List.toArray(interest) });
    };
    record(acc, #limits(#sweepPublished({ day; slices = w.slices; rows = w.rows; nodes })));
    for (pl in List.values(planned)) {
      let a0 = pl.a;
      record(acc, #collateral(#exposureRecorded({ agreement = a0.id; day; exposure = a0.pendingExposure; balance = pl.balance; rows = a0.pendingRows })));
      for ((ev, input) in pl.interest.vals()) {
        switch (batchPost(js, jb, journalCaller, now, acc, input)) { case (?why) return limitErr(#InvalidSweep({ reason = "collateral interest " # a0.id # ": " # why })); case null {} };
        record(acc, #collateral(ev));
      };
      // the standing call: alerted when unmet past its grace, then superseded by the day's figure
      let a = switch (CollateralCore.agreement(s.collateral, a0.id)) { case (?x) x; case null a0 };
      if (a.openCall != 0) {
        switch (CollateralCore.call(s.collateral, a.openCall)) {
          case (?c) {
            if (c.state == CollateralCore.CALL_OPEN) {
              if (day >= c.due + a.graceDays) {
                switch (alertFor(s, { rule = "collateral.call.unmet"; version = 1; account = c.id; day; postings = []; detail = "agreement " # a.id # ": call " # Nat.toText(c.id) # " of " # Nat.toText(c.amount) # " due " # Nat.toText(c.due) # " unmet, " # Nat.toText(c.outstanding) # " outstanding" }, #endOfDay)) { case (?al) record(acc, al); case null {} };
              };
              record(acc, #collateral(#callSuperseded({ agreement = a.id; call = c.id; outstanding = c.outstanding; day })));
            };
          };
          case null {};
        };
      };
      switch (CollateralCore.callFor(a, a0.pendingExposure, pl.balance)) {
        case (?(amount, deliver)) record(acc, #collateral(#callRaised({ agreement = a.id; id = s.height; amount; deliver; day; due })));
        case null {};
      };
    };
    #ok({ day; family = "done"; visited = 0; familyDone = true; published = true; blocks = List.toArray(acc.blocks); postings = List.toArray(acc.postings) })
  };

  // ─── reconciliation ─────────────────────────────────────────────────────────

  func reconErr<X>(e : RT.Error) : Res<X> { #err(#ReconciliationError({ error = e })) };
  /// The depot's positions by instrument: the desk's own lots and the collateral held for counterparties, which
  /// the custodian holds in the same safekeeping account.
  public func depotPositions(s : State, depot : Text) : [(Text, Nat)] {
    let sums = Map.empty<Text, Nat>();
    for (h in CustodyCore.holdingsOf(s.custody, depot, func(lot : Nat) : (Text, Text) { switch (TreasuryCore.row(s.treasury, lot)) { case (?r) (r.isin, r.book); case null ("", "") } }).vals()) {
      let cur = switch (Map.get(sums, Text.compare, h.isin)) { case (?v) v; case null 0 };
      Map.add(sums, Text.compare, h.isin, cur + h.nominal);
    };
    let h = CustodyCore.hash8(depot);
    let (lo, hi) = R.fullRange(20);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.custody.received, lo, hi, cursor, 512);
      for ((k, v) in page.entries.vals()) {
        let a = Blob.toArray(k);
        if (R.getNat(a, 12, 8) == h) { let isin = R.getText(a, 0, 12); let n = R.getNat(Blob.toArray(v), 0, 8); let cur = switch (Map.get(sums, Text.compare, isin)) { case (?x) x; case null 0 }; Map.add(sums, Text.compare, isin, cur + n) };
      };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    Map.toArray(sums)
  };
  /// The depot and the instrument of an instruction, by its family.
  func instructionDepotIsin(s : State, i : SettlementCore.InstructionRow) : ?(Text, Text) {
    switch (i.family) {
      case (#treasury) {
        let ?r = TreasuryCore.row(s.treasury, i.deal) else return null;
        let depot = switch (CustodyCore.dealDepotOf(s.custody, i.deal)) { case (?d) d; case null { switch (CustodyCore.bookDepotOf(s.custody, r.book)) { case (?d) d; case null return null } } };
        ?(depot, r.isin)
      };
      case (#repo) { switch (FinancingCore.repo(s.financing, i.deal)) { case (?r) ?(r.depot, r.isin); case null null } };
      case (#loan) { switch (FinancingCore.loan(s.financing, i.deal)) { case (?l) ?(l.depot, l.isin); case null null } };
      case (#collateral) { switch (CollateralCore.securitiesRow(s.collateral, i.deal)) { case (?r) ?(r.depot, r.isin); case null null } };
    }
  };
  /// The desk's instructions of a depot with a cycle in a window, as the custodian's transaction report is
  /// matched against them; with them, the instructions that failed in the window and were recycled into the
  /// business day after it, which the custodian did not post and the fail explains.
  public func depotInstructions(s : State, js : JCore.State, depot : Text, from : Nat, to : Nat) : [ReconciliationCore.DeskInstruction] {
    let out = List.empty<ReconciliationCore.DeskInstruction>();
    func take(i : SettlementCore.InstructionRow, failedOnly : Bool) {
      let failed = i.state == #failed or (i.fails > 0 and i.state != #settled);
      if (failedOnly and not failed) return;
      switch (instructionDepotIsin(s, i)) {
        case (?(dep, isin)) { if (Text.equal(dep, depot)) List.add(out, { id = i.id; referenceHash = i.referenceHash; isin; nominal = i.assetAmount; delivered = i.role == #maker; settled = i.state == #settled; failed; day = i.cycle }) };
        case null {};
      };
    };
    var d = from;
    while (d <= to) { for (i in SettlementCore.instructionsOfCycle(s.settlement, d).vals()) take(i, false); d += 1 };
    for (i in SettlementCore.instructionsOfCycle(s.settlement, nextBusinessDay(js)(to + 1)).vals()) take(i, true);
    List.toArray(out)
  };
  public func bookCashBalance(js : JCore.State, cash : TT.CashAccount, currency : Text, day : Nat) : Int {
    let v = JCore.valueDatedBalance(js, cash.account, TreasuryCore.cashSub(cash), currency, day);
    (v.debits : Int) - v.credits
  };
  /// What a cash reconciliation needs before its call: the policy's account for the currency, the ledger the
  /// venue settles the currency on, and no reconciliation of the currency in flight.
  public func prepareCashReconciliation(s : State, js : JCore.State, now : Nat64, currency : Text) : Res<{ ledger : Principal; cash : TT.CashAccount; event : T.Event }> {
    let ?p = ReconciliationCore.policy(s.reconciliation) else return reconErr(#NoPolicy);
    let ?cash = ReconciliationCore.cashAccountOf(p, currency) else return reconErr(#NoCashAccount({ currency }));
    let ?decl = SettlementCore.ledger(s.settlement, #cash({ currency })) else return reconErr(#NoLedger({ currency }));
    switch (ReconciliationCore.cashRow(s.reconciliation, currency)) { case (?r) { if (r.inFlight) return reconErr(#ReconciliationInFlight({ currency })) }; case null {} };
    let day = JCore.effectiveToday(js, now);
    #ok({ ledger = decl.ledger; cash; event = #reconciliation(#cashReconciliationIntended({ currency; ledger = decl.ledger; day })) })
  };
  /// The ledger's reply folded against the book: the reconciliation recorded with the log height beside the balance; a
  /// difference opens a break when none stands, an agreement clears the standing one.
  public func completeCashReconciliation(s : State, js : JCore.State, now : Nat64, currency : Text, ledger : Principal, cash : TT.CashAccount, ledgerBalance : Nat, tipHeight : ?Nat) : [T.Event] {
    let day = JCore.effectiveToday(js, now);
    let book = bookCashBalance(js, cash, currency, day);
    let difference : Int = (ledgerBalance : Int) - book;
    let out = List.empty<T.Event>();
    List.add(out, #reconciliation(#cashReconciled({ currency; ledger; ledgerBalance; bookBalance = book; difference; tipHeight; day })));
    let standing = switch (ReconciliationCore.cashRow(s.reconciliation, currency)) { case (?r) r.openBreak; case null 0 };
    if (difference != 0 and standing == 0) List.add(out, #reconciliation(#cashBreak({ currency; ledger; ledgerBalance; bookBalance = book; difference; day })))
    else if (difference == 0 and standing != 0) List.add(out, #reconciliation(#cashBreakCleared({ break_ = standing; day })));
    List.toArray(out)
  };
  /// The reconciliation report of a day: the nostro, depot and cash breaks open by age band, the statements and
  /// notifications recorded, the last cash reconciliation per currency; canonical text, hashed, at the height.
  public func reconciliationReport(s : State, day : Nat) : { height : Nat; day : Nat; json : Text; hash : Blob } {
    func band(age : Nat) : Text { if (age <= 2) "0-2" else if (age <= 5) "3-5" else "6+" };
    func ageOf(opened : Nat) : Nat { if (day > opened) day - opened else 0 };
    func bands(ages : [Nat]) : Text {
      var a = 0; var b = 0; var c = 0;
      for (x in ages.vals()) { switch (band(x)) { case "0-2" a += 1; case "3-5" b += 1; case (_) c += 1 } };
      "{\"0-2\":" # Nat.toText(a) # ",\"3-5\":" # Nat.toText(b) # ",\"6+\":" # Nat.toText(c) # ",\"open\":" # Nat.toText(ages.size()) # "}"
    };
    func q(t : Text) : Text { "\"" # t # "\"" };
    func int(i : Int) : Text { (if (i < 0) "-" else "") # Nat.toText(Int.abs(i)) };
    let nostroAges = Array.map<TreasuryCore.BreakRow, Nat>(TreasuryCore.openBreaks(s.treasury), func(b) { ageOf(b.openedDay) });
    let depotAges = Array.map<ReconciliationCore.DepotBreakRow, Nat>(ReconciliationCore.openDepotBreaks(s.reconciliation), func(b) { ageOf(b.openedDay) });
    let cashAges = Array.map<ReconciliationCore.CashBreakRow, Nat>(ReconciliationCore.openCashBreaks(s.reconciliation), func(b) { ageOf(b.openedDay) });
    let ts = TreasuryCore.status(s.treasury);
    let rs = ReconciliationCore.status(s.reconciliation);
    var depots = "";
    for (st in ReconciliationCore.statements(s.reconciliation).vals()) {
      depots #= (if (depots.size() > 0) "," else "") # "{\"depot\":" # q(st.depot) # ",\"kind\":" # q(RT.statementKindText(st.kind)) # ",\"date\":" # Nat.toText(st.statementDate) # ",\"reported\":" # Nat.toText(st.reported) # ",\"matched\":" # Nat.toText(st.matched) # ",\"explained\":" # Nat.toText(st.explained) # ",\"breaks\":" # Nat.toText(st.breaks) # "}";
    };
    var cash = "";
    for (c in ReconciliationCore.cashRows(s.reconciliation).vals()) {
      cash #= (if (cash.size() > 0) "," else "") # "{\"currency\":" # q(c.currency) # ",\"day\":" # Nat.toText(c.day) # ",\"ledger\":" # Nat.toText(c.ledgerBalance) # ",\"book\":" # int(c.bookBalance) # ",\"difference\":" # int(c.difference) # ",\"openBreak\":" # Nat.toText(c.openBreak) # "}";
    };
    let json = "{\"report\":\"reconciliation\",\"version\":1,\"day\":" # Nat.toText(day) # ",\"height\":" # Nat.toText(s.height)
      # ",\"nostro\":{\"statements\":" # Nat.toText(ts.statements) # ",\"notifications\":" # Nat.toText(rs.notifications) # ",\"breaksTotal\":" # Nat.toText(ts.breaksTotal) # ",\"breaks\":" # bands(nostroAges) # "}"
      # ",\"depot\":{\"statements\":[" # depots # "],\"explainedFails\":" # Nat.toText(rs.explainedFails) # ",\"breaksTotal\":" # Nat.toText(rs.depotBreaks) # ",\"breaks\":" # bands(depotAges) # "}"
      # ",\"cash\":{\"reconciliations\":" # Nat.toText(rs.cashReconciliations) # ",\"accounts\":[" # cash # "],\"breaksTotal\":" # Nat.toText(rs.cashBreaks) # ",\"breaks\":" # bands(cashAges) # "}}";
    { height = s.height; day; json; hash = Sha256.fromBlob(#sha256, Text.encodeUtf8(json)) }
  };

  /// The functional currency, the recorded spot rates and the position pairs of the close, the recorded fixings,
  /// the Sharia-flagged books.
  public func treasuryCtx(s : State) : Res<TreasuryCore.Ctx> {
    let ?functional = CloseCore.functional(s.close) else return #err(#NoFunctionalCurrency);
    #ok({
      functional;
      spot = func(ccy : Text, day : Nat) : ?Fx.Rate { CloseCore.rateOn(s.close, ccy, day) };
      pair = func(ccy : Text) : ?Fx.PositionPair { CloseCore.getPair(s.close, ccy) };
      fixing = func(index : Text, day : Nat) : ?Nat { Fixings.fixingOn(s.fixings, index, day) };
      isShariaBook = func(book : Text) : Bool { Auth.isShariaBook(s.authority, book) };
    })
  };
  func treasuryErr<X>(e : TT.TreasuryError) : Res<X> { #err(#TreasuryError({ error = e })) };
  func treasuryPlan(r : Result.Result<TT.TreasuryEvent, TT.TreasuryError>) : Res<Plan> {
    switch (r) { case (#err(e)) treasuryErr(e); case (#ok(ev)) only(#treasury(ev)) }
  };
  func treasuryRow(s : State, id : TT.DealId) : Res<TreasuryCore.DealRow> {
    switch (TreasuryCore.row(s.treasury, id)) { case (?r) #ok(r); case null treasuryErr(#UnknownDeal({ deal = id })) }
  };
  func treasuryKind(bb : Blocks, r : TreasuryCore.DealRow) : Res<TT.DealKind> {
    switch (treasuryKindOf(bb, r)) { case (?k) #ok(k); case null treasuryErr(#UnknownDeal({ deal = r.id })) }
  };
  func postOne(js : JCore.State, journalCaller : Principal, now : Nat64, input : JT.PostingInput) : Res<Plan> {
    switch (JCore.preparePost(js, journalCaller, now, input)) {
      case (#err(e)) #err(#JournalError({ error = e }));
      case (#ok(#event(e))) #ok({ event = null; extra = []; journal = [#event(e)] });
      case (#ok(#duplicate(idx))) #ok({ event = null; extra = []; journal = [#existing(idx)] });
    }
  };
  /// A posting of the legs an act built, asserted to balance here so a builder fault is named by the builder.
  func postLegs(js : JCore.State, journalCaller : Principal, now : Nat64, purpose : Text, parts : [Text], legs : [JT.Leg], postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    if (not Posting.balances(legs)) return treasuryErr(#InvalidTerms({ reason = purpose # ": the generated legs do not balance" }));
    postOne(js, journalCaller, now, { idempotencyKey = Posting.key(purpose, parts); postingDate; valueDate; period; legs; sourceRef = { kind = purpose; id = Text.join(parts.vals(), "/") }; narration; correctionOf = null })
  };
  func treasuryPost(js : JCore.State, journalCaller : Principal, now : Nat64, purpose : Text, parts : [Text], act : TreasuryCore.Act, postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let extras = Array.map<TT.TreasuryEvent, T.Event>(act.extras, func(e) { #treasury(e) });
    if (act.legs.size() == 0) return #ok({ event = ?#treasury(act.ev); extra = extras; journal = [] });
    switch (postLegs(js, journalCaller, now, purpose, parts, act.legs, postingDate, valueDate, period, narration)) {
      case (#err(e)) #err(e);
      case (#ok(plan)) #ok({ event = ?#treasury(act.ev); extra = extras; journal = plan.journal });
    }
  };
  func minorUnitsOf(js : JCore.State, ccy : Text) : ?Nat8 { JCore.currencyMinorUnits(js, ccy) };
  func callPart(ev : CallT.Event) : Text {
    switch (ev) { case (#funded(_)) "f"; case (#accrued(_)) "a"; case (#interestSettled(_)) "i"; case (#repaid(_)) "r"; case (#rateReset(_)) "reset"; case (#balanceAdjusted(_)) "adjust"; case (_) "" }
  };
  func callPost(js : JCore.State, journalCaller : Principal, now : Nat64, p : TT.Policy, r : CallCore.Row, cash : TT.CashAccount, ev : CallT.Event, purpose : Text, parts : [Text], postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let legs = CallCore.legsOf(p, r, cash, ev);
    if (legs.size() == 0) return only(#call(ev));
    switch (postLegs(js, journalCaller, now, purpose, parts, legs, postingDate, valueDate, period, narration)) {
      case (#err(e)) #err(e);
      case (#ok(plan)) #ok({ event = ?#call(ev); extra = []; journal = plan.journal });
    }
  };

  func settlementErr<X>(e : ST.Error) : Res<X> { #err(#SettlementError({ error = e })) };
  func settlementPlan(r : Result.Result<ST.Event, ST.Error>) : Res<Plan> { switch (r) { case (#err(e)) settlementErr(e); case (#ok(ev)) only(#settlement(ev)) } };
  func instructionRow(s : State, id : Nat) : Res<SettlementCore.InstructionRow> { switch (SettlementCore.instruction(s.settlement, id)) { case (?i) #ok(i); case null settlementErr(#UnknownInstruction({ instruction = id })) } };
  /// The settlement amount of a security deal's delivery leg and its sese.023, as the desk renders it: the clean
  /// cost and the coupon accrued to the settlement date, the safekeeping account of the deal's depot.
  public func instructionDocument(s : State, js : JCore.State, bb : Blocks, r : TreasuryCore.DealRow, t : TT.SecurityTrade) : Res<(Nat, Text)> {
    let ?sec = TreasuryCore.security(s.treasury, t.isin) else return treasuryErr(#UnknownSecurity({ isin = t.isin }));
    let ?minor = minorUnitsOf(js, sec.currency) else return #err(#MissingRate({ currency = sec.currency; asOf = t.settlement }));
    let terms : TT.SecurityTerms = { isin = sec.isin; issuer = sec.issuer; currency = sec.currency; couponBps = sec.couponBps; couponsPerYear = sec.couponsPerYear; dayCount = TreasuryCore.conventionOf(sec); issue = sec.issue; maturity = sec.maturity };
    let accrued = TreasuryMath.accruedCoupon(t.nominal, sec.couponBps, terms.dayCount, TreasuryCore.couponPeriodsOf(sec, t.nominal), t.settlement);
    let amount = r.secondAmount + Int.abs(accrued);
    let (_, reference) = treasuryCaptureOf(bb, r.id);
    #ok((amount, TreasuryMessages.sese023Xml(reference, t, terms, amount, minor, safekeepingAccountOf(s, r.id), r.day)))
  };
  public func safekeepingAccountOf(s : State, deal : Nat) : Text {
    switch (CustodyCore.dealDepotOf(s.custody, deal)) { case (?d) { switch (CustodyCore.depot(s.custody, d)) { case (?row) row.safekeepingAccount; case null d } }; case null "" }
  };
  /// The settlement of an instructed leg from Tachyon's verified receipt: the receipt and the settlement recorded,
  /// then Manticore's leg settlement with its postings, in one act.
  public func planReceiptSettlement(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, now : Nat64, i : SettlementCore.InstructionRow, receipt : SettlementCore.Receipt, day : Nat) : Res<Plan> {
    let head : [T.Event] = [
      #settlement(#receiptVerified({ instruction = i.id; tradeId = i.tradeId; seq = receipt.seq; leaf = receipt.leaf; root = receipt.root; assetPaid = receipt.assetPaid; cashPaid = receipt.cashPaid; day })),
      #settlement(#settled({ instruction = i.id; tradeId = i.tradeId; day })),
    ];
    if (i.family != #treasury) {
      let ?period = periodForDay(js, day) else return #err(#JournalError({ error = #UnknownPeriod({ period = "open period containing day " # Nat.toText(day) }) }));
      let plan = switch (i.family) {
        case (#repo) planRepoLeg(s, bb, js, journalCaller, now, i.deal, i.leg, "tachyon", day, day, period, "settled through Tachyon, trade " # Nat.toText(i.tradeId));
        case (#loan) planLoanLeg(s, bb, js, journalCaller, now, i.deal, i.leg, "tachyon", day, day, period, "settled through Tachyon, trade " # Nat.toText(i.tradeId));
        case (#collateral) {
          let ?r = CollateralCore.securitiesRow(s.collateral, i.deal) else return collateralErr(#UnknownPledge({ agreement = ""; id = i.deal }));
          planSubstitutionSettlement(s, js, journalCaller, now, r.agreement, i.deal, "tachyon", day, day, period, "settled through Tachyon, trade " # Nat.toText(i.tradeId))
        };
        case (#treasury) return financingErr(#InvalidTerms({ reason = "unreachable" }));
      };
      return switch (plan) {
        case (#err(e)) #err(e);
        case (#ok(p)) { let ev = switch (p.event) { case (?e) [e]; case null [] }; #ok({ event = ?head[0]; extra = Array.concat<T.Event>(Array.concat<T.Event>([head[1]], ev), p.extra); journal = p.journal }) };
      };
    };
    let r = switch (treasuryRow(s, i.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    switch (requireNoOpenRun(s, r.book, day)) { case (?e) return #err(e); case null {} };
    let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
    let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
    let ?period = periodForDay(js, day) else return #err(#JournalError({ error = #UnknownPeriod({ period = "open period containing day " # Nat.toText(day) }) }));
    switch (saleDepotCheck(s, r, kind, i.leg)) { case (?e) return #err(e); case null {} };
    let act = switch (TreasuryCore.planSettleLeg(s.treasury, r, kind, i.leg, day, ctx)) { case (#err(e)) return treasuryErr(e); case (#ok(a)) a };
    switch (treasuryPost(js, journalCaller, now, "treasury-settle", ["tachyon", Nat.toText(i.id), Nat.toText(i.leg)], act, day, day, period, "settled through Tachyon, trade " # Nat.toText(i.tradeId))) {
      case (#err(e)) #err(e);
      case (#ok(plan)) {
        let ev = switch (plan.event) { case (?e) [e]; case null [] };
        #ok({ event = ?head[0]; extra = Array.concat<T.Event>(Array.concat<T.Event>([head[1]], ev), plan.extra); journal = plan.journal })
      };
    }
  };
  func financingErr<X>(e : FT.Error) : Res<X> { #err(#FinancingError({ error = e })) };
  func financingPlan(r : Result.Result<FT.Event, FT.Error>) : Res<Plan> { switch (r) { case (#err(e)) financingErr(e); case (#ok(ev)) only(#financing(ev)) } };
  func repoRow(s : State, id : Nat) : Res<FinancingCore.RepoRow> { switch (FinancingCore.repo(s.financing, id)) { case (?r) #ok(r); case null financingErr(#UnknownRepo({ repo = id })) } };
  func loanRow(s : State, id : Nat) : Res<FinancingCore.LoanRow> { switch (FinancingCore.loan(s.financing, id)) { case (?r) #ok(r); case null financingErr(#UnknownLoan({ loan = id })) } };
  /// The opening block of a repo or a loan: the counterparty, the reference and the cash account.
  public func repoOpeningOf(bb : Blocks, id : Nat) : (Text, Text, ?TT.CashAccount) {
    switch (bb.get(id)) { case (?b) { switch (b.event) { case (#financing(#repoOpened(x))) (x.counterparty.name, x.reference, ?x.terms.cashAccount); case (_) ("", "", null) } }; case null ("", "", null) }
  };
  public func loanOpeningOf(bb : Blocks, id : Nat) : (Text, Text, ?TT.CashAccount) {
    switch (bb.get(id)) { case (?b) { switch (b.event) { case (#financing(#loanOpened(x))) (x.counterparty.name, x.reference, ?x.terms.cashAccount); case (_) ("", "", null) } }; case null ("", "", null) }
  };
  /// The day's price of an instrument per 100, from its price curve.
  func priceOf(s : State, isin : Text, day : Nat) : ?Nat {
    switch (TreasuryCore.curveOn(s.treasury, isin, day)) { case (?c) { if (c.kind == #securityPrice and c.points.size() == 1) ?Int.abs(c.points[0].1) else null }; case null null }
  };
  /// A financing event with its postings and the custody movements that go with it: the pledge or the receipt of
  /// collateral at a repo's start, its release or return at the close, the lots lent at a loan's start and back
  /// at its return. `reference` names the repo or the loan on the custody events.
  func financingAct(s : State, js : JCore.State, journalCaller : Principal, now : Nat64, ev : FT.Event, r : ?FinancingCore.RepoRow, l : ?FinancingCore.LoanRow, cash : TT.CashAccount, custodyEvents : [T.Event], purpose : Text, parts : [Text], postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let ?p = FinancingCore.policy(s.financing) else return financingErr(#NoPolicy);
    let legs = FinancingCore.legsOf(p, cash, r, l, ev);
    if (legs.size() == 0) return #ok({ event = ?#financing(ev); extra = custodyEvents; journal = [] });
    switch (postLegs(js, journalCaller, now, purpose, parts, legs, postingDate, valueDate, period, narration)) {
      case (#err(e)) #err(e);
      case (#ok(plan)) #ok({ event = ?#financing(ev); extra = custodyEvents; journal = plan.journal });
    }
  };
  /// The custody side of a repo's start: the desk's collateral pledged lot by lot from the depot, or the
  /// counterparty's collateral received into it.
  func repoStartCustody(s : State, r : FinancingCore.RepoRow, day : Nat) : Res<([(TT.DealId, Nat)], [T.Event])> {
    let reference = "repo/" # Nat.toText(r.id);
    if (r.reverse) return #ok(([], [#custody(#collateralReceived({ isin = r.isin; depot = r.depot; nominal = r.nominal; reference; day }))]));
    let lots = switch (FinancingCore.allocate(CustodyCore.availableIn(s.custody, r.depot, r.isin), r.nominal, r.isin)) { case (#err(e)) return financingErr(e); case (#ok(x)) x };
    #ok((lots, Array.map<(TT.DealId, Nat), T.Event>(lots, func((lot, n)) { #custody(#pledged({ lot; depot = r.depot; nominal = n; reference; day })) })))
  };
  func repoCloseCustody(s : State, r : FinancingCore.RepoRow, day : Nat) : [T.Event] {
    let reference = "repo/" # Nat.toText(r.id);
    if (r.reverse) return [#custody(#collateralReturned({ isin = r.isin; depot = r.depot; nominal = r.nominal; reference; day }))];
    // every lot pledged to the repo is released: the pledges are the custody's encumbrances under the repo's reference
    Array.map<(TT.DealId, Nat), T.Event>(pledgedLots(s, r), func((lot, n)) { #custody(#released({ lot; depot = r.depot; nominal = n; reference; day })) })
  };
  /// The lots a repo holds pledged, read back from the desk log's own events for the repo.
  public func pledgedLots(s : State, r : FinancingCore.RepoRow) : [(TT.DealId, Nat)] { FinancingCore.lotsOfRepo(s.financing, r.id) };
  /// The plan of a repo leg: the start (the cash and the collateral) or the close (the repurchase price and the
  /// collateral back), by hand or from Tachyon's receipt.
  public func planRepoLeg(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, now : Nat64, id : Nat, leg : Nat, authId : Text, postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let r = switch (repoRow(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    switch (requireNoOpenRun(s, r.book, valueDate)) { case (?e) return #err(e); case null {} };
    let (_, _, cashOpt) = repoOpeningOf(bb, id);
    let ?cash = cashOpt else return financingErr(#UnknownRepo({ repo = id }));
    switch (leg) {
      case 0 {
        if (r.state != #open) return financingErr(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "open" }));
        if (valueDate < r.start) return financingErr(#NotDue({ id; due = r.start; day = valueDate }));
        let (lots, custody) = switch (repoStartCustody(s, r, valueDate)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        financingAct(s, js, journalCaller, now, #repoStarted({ repo = id; lots; day = valueDate }), ?r, null, cash, custody, "repo-start", [authId, Nat.toText(id)], postingDate, valueDate, period, narration)
      };
      case 1 {
        let ev = switch (FinancingCore.planClose(s.financing, id, valueDate)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
        financingAct(s, js, journalCaller, now, ev, ?r, null, cash, repoCloseCustody(s, r, valueDate), "repo-close", [authId, Nat.toText(id)], postingDate, valueDate, period, narration)
      };
      case (_) financingErr(#InvalidTerms({ reason = "a repo has a start leg and a close leg" }));
    }
  };
  public func planLoanLeg(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, now : Nat64, id : Nat, leg : Nat, authId : Text, postingDate : Nat, valueDate : Nat, period : Text, narration : Text) : Res<Plan> {
    let l = switch (loanRow(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    switch (requireNoOpenRun(s, l.book, valueDate)) { case (?e) return #err(e); case null {} };
    let (_, _, cashOpt) = loanOpeningOf(bb, id);
    let ?cash = cashOpt else return financingErr(#UnknownLoan({ loan = id }));
    let reference = "loan/" # Nat.toText(id);
    switch (leg) {
      case 0 {
        if (l.state != #open) return financingErr(#LoanNotIn({ loan = id; state = FT.loanStateText(l.state); wanted = "open" }));
        if (valueDate < l.start) return financingErr(#NotDue({ id; due = l.start; day = valueDate }));
        let lots = switch (FinancingCore.allocate(CustodyCore.availableIn(s.custody, l.depot, l.isin), l.nominal, l.isin)) { case (#err(e)) return financingErr(e); case (#ok(x)) x };
        let custody = List.empty<T.Event>();
        for ((lot, n) in lots.vals()) List.add(custody, #custody(#lent({ lot; depot = l.depot; nominal = n; reference; day = valueDate })));
        if (l.collateralNominal > 0) List.add(custody, #custody(#collateralReceived({ isin = l.collateralIsin; depot = l.depot; nominal = l.collateralNominal; reference; day = valueDate })));
        financingAct(s, js, journalCaller, now, #loanStarted({ loan = id; lots; day = valueDate }), null, ?l, cash, List.toArray(custody), "loan-start", [authId, Nat.toText(id)], postingDate, valueDate, period, narration)
      };
      case 1 {
        let ev = switch (FinancingCore.planReturn(s.financing, id, valueDate)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
        let custody = List.empty<T.Event>();
        for ((lot, n) in FinancingCore.lotsOfLoan(s.financing, id).vals()) List.add(custody, #custody(#lentReturned({ lot; depot = l.depot; nominal = n; reference; day = valueDate })));
        if (l.collateralNominal > 0) List.add(custody, #custody(#collateralReturned({ isin = l.collateralIsin; depot = l.depot; nominal = l.collateralNominal; reference; day = valueDate })));
        financingAct(s, js, journalCaller, now, ev, null, ?l, cash, List.toArray(custody), "loan-return", [authId, Nat.toText(id)], postingDate, valueDate, period, narration)
      };
      case (_) financingErr(#InvalidTerms({ reason = "a loan has a start leg and a return leg" }));
    }
  };
  /// What a financing leg moves on the ledgers, for a settlement instruction: the collateral or the lent nominal
  /// against the cash of the leg, and whether the desk delivers the security.
  public func financingLegAmounts(s : State, family : ST.Family, id : Nat, leg : Nat, day : Nat) : Res<{ isin : Text; currency : Text; nominal : Nat; cash : Nat; deskDelivers : Bool; cycleDay : Nat }> {
    switch (family) {
      case (#repo) {
        let r = switch (repoRow(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (leg) {
          case 0 { if (r.state != #open) return financingErr(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "open" })); #ok({ isin = r.isin; currency = r.currency; nominal = r.nominal; cash = r.cash; deskDelivers = not r.reverse; cycleDay = r.start }) };
          case 1 {
            if (r.state != #started) return financingErr(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "started" }));
            let closeDay = if (r.maturity == 0) day else r.maturity;
            let interest = FinancingCore.repoInterestTarget(r, closeDay);
            #ok({ isin = r.isin; currency = r.currency; nominal = r.nominal; cash = r.cash + Int.abs(interest); deskDelivers = r.reverse; cycleDay = closeDay })
          };
          case (_) financingErr(#InvalidTerms({ reason = "a repo has a start leg and a close leg" }));
        }
      };
      case (#loan) {
        let l = switch (loanRow(s, id)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        if (l.cashCollateral == 0) return financingErr(#InvalidTerms({ reason = "a loan against securities collateral settles by hand; the venue trades a security against cash" }));
        switch (leg) {
          case 0 { if (l.state != #open) return financingErr(#LoanNotIn({ loan = id; state = FT.loanStateText(l.state); wanted = "open" })); #ok({ isin = l.isin; currency = l.currency; nominal = l.nominal; cash = l.cashCollateral; deskDelivers = true; cycleDay = l.start }) };
          case 1 {
            if (l.state != #recalled) return financingErr(#LoanNotIn({ loan = id; state = FT.loanStateText(l.state); wanted = "recalled" }));
            let fee = Int.abs(FinancingCore.loanFeeTarget(l, l.returnDay)); let rebate = Int.abs(FinancingCore.loanRebateTarget(l, l.returnDay));
            // the collateral and the rebate go back, the fee comes in: the cash leg is the net the desk pays
            let net = l.cashCollateral + rebate;
            #ok({ isin = l.isin; currency = l.currency; nominal = l.nominal; cash = if (net > fee) net - fee else 1; deskDelivers = false; cycleDay = l.returnDay })
          };
          case (_) financingErr(#InvalidTerms({ reason = "a loan has a start leg and a return leg" }));
        }
      };
      case (#collateral) {
        // a substitution: the desk delivers the securities against the cash it takes back, on the day's cycle
        let ?r = CollateralCore.securitiesRow(s.collateral, id) else return collateralErr(#UnknownPledge({ agreement = ""; id }));
        if (r.state != #pledged) return collateralErr(#PledgeNotIn({ id; state = CoT.securitiesStateText(r.state); wanted = "pledged" }));
        if (leg != 0) return financingErr(#InvalidTerms({ reason = "a substitution has one leg" }));
        #ok({ isin = r.isin; currency = r.currency; nominal = r.nominal; cash = r.cashReturned; deskDelivers = true; cycleDay = day })
      };
      case (#treasury) financingErr(#InvalidTerms({ reason = "a treasury deal is instructed by its own act" }));
    }
  };
  func valuationErr<X>(e : VT.Error) : Res<X> { #err(#ValuationError({ error = e })) };
  /// The market data recorded for a day, for the desk's own valuation.
  public func dataOn(s : State, day : Nat) : Valuation.Data {
    Valuation.dataOn(s.treasury, day, func(ccy : Text, d : Nat) : ?Fx.Rate { CloseCore.rateOn(s.close, ccy, d) }, func(index : Text, d : Nat) : ?Nat { Fixings.fixingOn(s.fixings, index, d) })
  };
  /// The mark of a deal at a day from the data of another day: the day's own for the mark itself, the previous
  /// day's for the theta.
  public func markOf(s : State, bb : Blocks, deal : Nat, day : Nat, dataDay : Nat) : Res<?Int> {
    let r = switch (treasuryRow(s, deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
    let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
    switch (Valuation.markWith(s.treasury, r, kind, day, dataOn(s, dataDay))) {
      case (#ok(v)) #ok(v);
      case (#err(#NoCurve(x))) treasuryErr(#UnknownCurve({ curve = x.curve; day = dataDay }));
      case (#err(#NoSpot(x))) treasuryErr(#NoRate({ currency = x.currency; day = dataDay }));
      case (#err(#UnknownSecurity(x))) treasuryErr(#UnknownSecurity({ isin = x.isin }));
      case (#err(#NotMarked)) #ok(null);
    }
  };
  /// The hedged item's value for a hedge on a day: the hypothetical derivative's mark for a cash-flow hedge, the
  /// lot's clean value at the day's price for a fair-value hedge.
  func hedgedValueOf(s : State, bb : Blocks, h : HedgeCore.Row, hypothetical : ?TT.Irs, day : Nat) : Res<Int> {
    if (h.cashFlow) {
      let ?hyp = hypothetical else return valuationErr(#InvalidTerms({ reason = "a cash-flow hedge names its hypothetical derivative" }));
      let r = switch (treasuryRow(s, h.hedging)) { case (#err(e)) return #err(e); case (#ok(r)) r };
      switch (Valuation.markWith(s.treasury, r, #irs(hyp), day, dataOn(s, day))) { case (#ok(?v)) #ok(v); case (#ok(null)) #ok(0); case (#err(_)) treasuryErr(#UnknownCurve({ curve = hyp.discountCurve; day })) }
    } else {
      let r = switch (treasuryRow(s, h.hedged)) { case (#err(e)) return #err(e); case (#ok(r)) r };
      let ?price = priceOf(s, r.isin, day) else return valuationErr(#NoPrice({ isin = r.isin; day }));
      #ok(TreasuryMath.cleanCost(r.nominalLeft, price))
    }
  };
  func hypotheticalOf(bb : Blocks, hedge : Nat) : ?TT.Irs { switch (bb.get(hedge)) { case (?b) { switch (b.event) { case (#valuation(#hedgeDesignated(x))) x.hypothetical; case (_) null } }; case null null } };
  func lotAccountOf(p : TT.Policy, flags : Nat8) : Text { if ((flags & TreasuryCore.F_FVOCI) != 0) p.securitiesFvoci else if ((flags & TreasuryCore.F_FVTPL) != 0) p.securitiesFvtpl else p.securitiesAmortisedCost };
  func custodyErr<X>(e : CuT.Error) : Res<X> { #err(#CustodyError({ error = e })) };
  func custodyPlan(r : Result.Result<CuT.Event, CuT.Error>) : Res<Plan> { switch (r) { case (#err(e)) custodyErr(e); case (#ok(ev)) only(#custody(ev)) } };
  func isinOfLot(s : State) : TT.DealId -> Text { func(lot : Nat) : Text { switch (TreasuryCore.row(s.treasury, lot)) { case (?r) r.isin; case null "" } } };
  /// The terms' cash account of a security lot, from its capture or last amendment.
  func lotCash(bb : Blocks, r : TreasuryCore.DealRow) : ?TT.CashAccount { switch (treasuryKindOf(bb, r)) { case (?#security(t)) ?t.cash; case (_) null } };
  /// Every open purchase lot of an instrument across the books, with its depot: what an entitlement is computed over.
  func lotsOfIsin(s : State, isin : Text) : [(TreasuryCore.DealRow, ?Text)] {
    let out = List.empty<(TreasuryCore.DealRow, ?Text)>();
    for (b in Auth.listBooks(s.authority).vals()) {
      for (r in TreasuryCore.dealsOfBook(s.treasury, b.id).vals()) {
        if (r.kind == 4 and (r.flags & TreasuryCore.F_BUY) != 0 and TreasuryCore.isOpen(r) and Text.equal(r.isin, isin)) List.add(out, (r, CustodyCore.dealDepotOf(s.custody, r.id)));
      };
    };
    List.toArray(out)
  };
  /// The sale's lots against its delivering depot, before Manticore's planner: a lot the depot does not hold cannot
  /// be delivered from it.
  func saleDepotCheck(s : State, r : TreasuryCore.DealRow, kind : TT.DealKind, leg : Nat) : ?T.Error {
    switch (kind) {
      case (#security(t)) {
        if (t.direction != #sell or leg != 0) return null;
        let ?p = TreasuryCore.policy(s.treasury) else return null;
        switch (CustodyCore.checkSaleDepot(s.custody, s.treasury, r, t, p.lotMethod)) { case (?e) ?#CustodyError({ error = e }); case null null }
      };
      case (_) null;
    }
  };
  /// The coupon claim due on a lot: an entitled coupon action of its instrument whose claim day has come and whose
  /// entitlement on the lot is not yet claimed.
  func claimDueFor(s : State, r : TreasuryCore.DealRow, day : Nat) : ?(CustodyCore.ActionRow, CustodyCore.EntitlementRow) {
    let ?sec = TreasuryCore.security(s.treasury, r.isin) else return null;
    for (ar in CustodyCore.actionsOf(s.custody, r.isin).vals()) {
      if (ar.kind == 1 and ar.state == #entitled and day >= CustodyCore.claimDay(sec, ar)) {
        switch (CustodyCore.entitlement(s.custody, ar.id, r.id)) { case (?e) { if (not e.claimed) return ?(ar, e) }; case null {} };
      };
    };
    null
  };
  /// A corporate action's acts due on a day, one at a time: the entitlement at the record date, then each lot's
  /// payment at the payment date, then the action closed as paid.
  public type ActionAct = { events : [T.Event]; legs : [JT.Leg]; lot : ?TT.DealId; step : Text };
  func nextActionAct(s : State, bb : Blocks, id : CuT.ActionId, day : Nat) : Res<?ActionAct> {
    let ?r = CustodyCore.action(s.custody, id) else return custodyErr(#UnknownAction({ action = id }));
    switch (r.state) {
      case (#announced) {
        if (day < r.recordDate) return #ok(null);
        switch (CustodyCore.planEntitle(s.custody, r, func() { lotsOfIsin(s, r.isin) }, day)) {
          case (#err(e)) custodyErr(e);
          case (#ok(evs)) #ok(?{ events = Array.map<CuT.Event, T.Event>(evs, func(e) { #custody(e) }); legs = []; lot = null; step = "entitle" });
        }
      };
      case (#entitled) {
        let ?p = TreasuryCore.policy(s.treasury) else return #err(#TreasuryError({ error = #NoPolicy }));
        let ?sec = TreasuryCore.security(s.treasury, r.isin) else return custodyErr(#UnknownInstrument({ isin = r.isin }));
        let entitlements = CustodyCore.entitlementsOf(s.custody, id);
        // a coupon is claimed on its claim day, lot by lot, before anything is paid
        if (r.kind == 1 and day >= CustodyCore.claimDay(sec, r)) {
          for (e in entitlements.vals()) {
            if (not e.claimed) {
              let ?lot = TreasuryCore.row(s.treasury, e.lot) else return custodyErr(#LotNotIn({ lot = e.lot; state = "unknown" }));
              switch (CustodyCore.planClaim(p, r, e, lot, sec.currency, day)) {
                case (#err(err)) return custodyErr(err);
                case (#ok(claim)) {
                  let evs = switch (claim.treasury) { case (?te) [#treasury(te), #custody(claim.ev)]; case null [#custody(claim.ev)] };
                  return #ok(?{ events = evs; legs = claim.legs; lot = ?e.lot; step = "claim" });
                };
              };
            };
          };
        };
        if (day < r.paymentDate) return #ok(null);
        var total = 0; var lots = 0;
        for (e in entitlements.vals()) {
          lots += 1; total += e.amount;
          if (not e.paid) {
            let ?lot = TreasuryCore.row(s.treasury, e.lot) else return custodyErr(#LotNotIn({ lot = e.lot; state = "unknown" }));
            let ?cash = lotCash(bb, lot) else return custodyErr(#LotNotIn({ lot = e.lot; state = "terms not in the log" }));
            // a lot out on loan: the lent part of a coupon or a distribution is a manufactured payment, a receivable from the borrower
            let loans = FinancingCore.loansOfLot(s.financing, e.lot);
            var lent = 0; for ((_, n) in loans.vals()) lent += n;
            let manufactured : ?CustodyCore.Manufactured = if (lent == 0 or (r.kind != 1 and r.kind != 4)) null else {
              switch (FinancingCore.policy(s.financing), loans.size()) { case (?fp, _) ?{ account = fp.manufacturedPaymentReceivable; sub = FinancingCore.loanSub(loans[0].0); lent }; case (_) null }
            };
            switch (CustodyCore.planPayWith(s.custody, p, r, e, lot, cash, sec.currency, day, manufactured)) {
              case (#err(err)) return custodyErr(err);
              case (#ok(pay)) {
                var evs : [T.Event] = switch (pay.treasury) { case (?te) [#treasury(te), #custody(pay.ev)]; case null [#custody(pay.ev)] };
                switch (manufactured) { case (?m) { let part = CustodyCore.manufacturedPart(e.amount, e.nominal, m.lent); if (part > 0) evs := Array.concat<T.Event>(evs, [#financing(#manufacturedPayment({ loan = loans[0].0; action = id; lot = e.lot; amount = part; day }))]) }; case null {} };
                return #ok(?{ events = evs; legs = pay.legs; lot = ?e.lot; step = "pay" });
              };
            };
          };
        };
        #ok(?{ events = [#custody(#paid({ action = id; lots; total; day }))]; legs = []; lot = null; step = "close" })
      };
      case (_) #ok(null);
    }
  };

  /// An act dated into a day an end-of-day run is computing from is refused: the book for that day is closed to new
  /// history while the run is open.
  func requireNoOpenRun(s : State, book : T.BookId, valueDate : Nat) : ?T.Error {
    switch (Eod.openRunCovering(s.eod, book, valueDate)) {
      case (?r) ?#BatchError({ error = #RunExists({ book; businessDate = r.businessDate }) });
      case null null;
    }
  };

  // ═══════════════════════════════════════════════════════
  //  PLANNING
  // ═══════════════════════════════════════════════════════

  /// Validate a command against both states and say what it would do. `authority` is the principal whose act it
  /// is: the performer, the maker of an approved proposal, the actor of an override.
  public func planCommand(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, now : Nat64, authority : Principal, authId : Text, command : T.Command) : Res<Plan> {
    let a = s.authority;
    let today = JCore.effectiveToday(js, now);
    switch (command) {
      // ── authority ──
      case (#openBook(x)) planned(Auth.planOpenBook(a, x));
      case (#closeBook(x)) planned(Auth.planCloseBook(a, x.id));
      case (#defineRole(x)) planned(Auth.planDefineRole(a, x));
      case (#grantRole(x)) planned(Auth.planGrantRole(a, x));
      case (#revokeRole(x)) planned(Auth.planRevokeRole(a, x));
      case (#setDualPolicy(p)) planned(Auth.planSetDualPolicy(a, p));
      case (#clearDualPolicy(x)) planned(Auth.planClearDualPolicy(a, x.permission));
      case (#setFeatureActivation(x)) planned(Auth.planSetFeatureActivation(x, JCore.isActive(js), s.height));
      case (#setDeskIdentity(i)) planned(Auth.planSetIdentity(i));
      // ── the journal's configuration, the journal's own validation ──
      case (#journalRegisterCurrency(x)) journalConfig(JCore.prepareRegisterCurrency(js, journalCaller, x.code, x.minorUnits));
      case (#journalOpenAccount(x)) journalConfig(JCore.prepareOpenAccount(js, journalCaller, x.code, x.name, x.normalSide, x.category, x.constraint));
      case (#journalCloseAccount(x)) journalConfig(JCore.prepareCloseAccount(js, journalCaller, x.code));
      case (#journalOpenPeriod(x)) journalConfig(JCore.prepareOpenPeriod(js, journalCaller, x.id, x.start, x.end));
      case (#journalClosePeriod(x)) {
        // a period with an unresolved end-of-day failure or an open run inside it does not close: loud, not silent
        switch (JCore.getPeriod(js, x.id)) {
          case (?p) {
            for (b in Auth.listBooks(a).vals()) {
              if (Eod.unresolvedFailures(s.eod, b.id, p.start, p.end) > 0) return #err(#BatchError({ error = #InvalidRetry({ reason = "period " # x.id # " has unresolved end-of-day failures in book " # b.id }) }));
              for (r in Eod.listRuns(s.eod).vals()) { if (Text.equal(r.book, b.id) and r.businessDate >= p.start and r.businessDate <= p.end and Eod.isOpen(r)) return #err(#BatchError({ error = #RunExists({ book = b.id; businessDate = r.businessDate }) })) };
            };
          };
          case null {};
        };
        journalConfig(JCore.prepareClosePeriod(js, journalCaller, x.id))
      };
      case (#journalSetCalendar(x)) journalConfig(JCore.prepareSetCalendar(js, journalCaller, x.calendar));
      case (#journalSetCalendarAuthority(x)) journalConfig(JCore.prepareSetCalendarAuthority(js, journalCaller, x.authority, x.maxRollDays, x.businessDate));
      case (#journalRollBusinessDate(x)) journalConfig(JCore.prepareRollBusinessDate(js, journalCaller, now, x.day));
      case (#journalSetActivationHeight(x)) journalConfig(JCore.prepareSetActivationHeight(js, journalCaller, x.height));
      // ── the close's market data ──
      case (#setFunctionalCurrency(x)) {
        if (JCore.currencyMinorUnits(js, x.currency) == null) return #err(#InvalidRate({ reason = "currency " # x.currency # " is not registered in the journal" }));
        switch (CloseCore.functional(s.close)) {
          case (?c) { if (Text.equal(c, x.currency)) nothing() else #err(#FunctionalCurrencyAlreadySet({ currency = c })) };
          case null only(#close(#functionalCurrencySet({ currency = x.currency })));
        }
      };
      case (#setFxPair(x)) {
        let ?functional = CloseCore.functional(s.close) else return #err(#NoFunctionalCurrency);
        if (Text.equal(x.pair.currency, functional)) return #err(#InvalidPair({ reason = "the functional currency has no position against itself" }));
        if (JCore.currencyMinorUnits(js, x.pair.currency) == null) return #err(#InvalidPair({ reason = "currency " # x.pair.currency # " is not registered in the journal" }));
        for (code in [x.pair.position, x.pair.equivalent, x.pair.unrealised, x.pair.realised].vals()) { switch (requirePostableAccount(js, "position pair", code)) { case (?e) return #err(e); case null {} } };
        if (Text.equal(x.pair.position, x.pair.equivalent)) return #err(#InvalidPair({ reason = "the position and its equivalent must be different accounts" }));
        if (Text.equal(x.pair.unrealised, x.pair.realised)) return #err(#InvalidPair({ reason = "unrealised and realised results must be different accounts" }));
        only(#close(#fxPairSet({ pair = x.pair })))
      };
      case (#setFxRate(x)) {
        let ?functional = CloseCore.functional(s.close) else return #err(#NoFunctionalCurrency);
        switch (Fx.validateRate(x.rate)) { case (?r) return #err(#InvalidRate({ reason = r })); case null {} };
        if (not Text.equal(x.rate.functional, functional)) return #err(#InvalidRate({ reason = "the rate is quoted into " # x.rate.functional # "; the functional currency is " # functional }));
        if (CloseCore.getPair(s.close, x.rate.currency) == null) return #err(#UnknownPair({ currency = x.rate.currency }));
        // a rate for a day already recorded is replaced only by an identical one
        switch (CloseCore.rateOn(s.close, x.rate.currency, x.rate.asOf)) {
          case (?existing) {
            if (existing.numerator == x.rate.numerator and existing.denominator == x.rate.denominator) return nothing();
            return #err(#InvalidRate({ reason = "a different rate is already recorded for " # x.rate.currency # " on day " # Nat.toText(x.rate.asOf) }));
          };
          case null {};
        };
        only(#close(#fxRateSet({ rate = x.rate })))
      };
      case (#recordRateFixing(x)) {
        switch (Fixings.plan(s.fixings, x.index, x.day, x.rateBps)) {
          case (#err(reason)) #err(#InvalidFixing({ reason }));
          case (#ok(null)) nothing();
          case (#ok(?f)) only(#fixingRecorded(f));
        }
      };
      // ── the end of day ──
      case (#openEndOfDay(x)) {
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        switch (Eod.getRun(s.eod, x.book, x.businessDate)) { case (?_) return #err(#BatchError({ error = #RunExists({ book = x.book; businessDate = x.businessDate }) })); case null {} };
        // a run is for the business date the journal is on
        let ?bd = JCore.businessDate(js) else return #err(#BatchError({ error = #BusinessDateMismatch({ businessDate = x.businessDate; requested = x.businessDate }) }));
        if (bd != x.businessDate) return #err(#BatchError({ error = #BusinessDateMismatch({ businessDate = bd; requested = x.businessDate }) }));
        let shardSize = if (x.shardSize == 0) Batch.DEFAULT_SHARD_SIZE else x.shardSize;
        switch (Auth.requireFeature(a, T.FEATURE_END_OF_DAY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Batch.plan(Eod.planInput(x.book, shardSize))) {
          case (#err(#invalidShardSize(d))) #err(#BatchError({ error = #InvalidShardSize({ shardSize = d.shardSize }) }));
          case (#err(#planTooLarge(d))) #err(#BatchError({ error = #PlanTooLarge({ items = d.items }) }));
          case (#err(_)) #err(#BatchError({ error = #PlanTooLarge({ items = 0 }) }));
          case (#ok(items)) {
            if (items.size() == 0) return #err(#BatchError({ error = #NothingToAdvance({ book = x.book; businessDate = x.businessDate; cursor = 0 }) }));
            only(#eod(#opened({ book = x.book; businessDate = x.businessDate; shardSize; openedAtHeight = JCore.height(js); maxDeal = s.height; planHash = Batch.planHash(items); items = items.size(); entities = Batch.entityCount(items) })))
          };
        }
      };
      case (#setRetryPolicy(x)) {
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        switch (Batch.validateRetry({ scope = x.book; limit = x.limit })) { case (?r) return #err(#BatchError({ error = #InvalidRetry({ reason = r }) })); case null {} };
        only(#eod(#retryPolicySet({ book = x.book; limit = x.limit })))
      };
      case (#resolveEndOfDayFailure(x)) {
        let ?r = Eod.getRun(s.eod, x.book, x.businessDate) else return #err(#BatchError({ error = #UnknownRun({ book = x.book; businessDate = x.businessDate }) }));
        if (not Eod.hasFailure(r, x.item, x.entity)) return #err(#BatchError({ error = #UnknownFailure({ book = x.book; businessDate = x.businessDate; item = x.item; entity = x.entity }) }));
        if (Text.encodeUtf8(x.reason).size() == 0) return #err(#BatchError({ error = #InvalidRetry({ reason = "a resolution states its reason" }) }));
        only(#eod(#failureResolved({ book = x.book; businessDate = x.businessDate; item = x.item; entity = x.entity; reason = x.reason })))
      };
      // ── alerts ──
      case (#clearAlert(x)) {
        switch (AlertCore.planClear(s.alerts, x.alert, x.reason)) { case (#err(e)) #err(#AlertError({ error = e })); case (#ok(ev)) only(#alert(ev)) }
      };
      // ── the revaluation: every monetary position restated at the value date's rate, one posting per currency ──
      case (#revaluePositions(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let ?functional = CloseCore.functional(s.close) else return #err(#NoFunctionalCurrency);
        let events = List.empty<T.Event>();
        let steps = List.empty<JournalStep>();
        for (pair in CloseCore.listPairs(s.close).vals()) {
          if (pair.monetary) {
            let ?rate = CloseCore.rateOn(s.close, pair.currency, x.valueDate) else return #err(#MissingRate({ currency = pair.currency; asOf = x.valueDate }));
            let pos = JCore.balance(js, pair.position, null, pair.currency);
            let eq = JCore.balance(js, pair.equivalent, null, functional);
            let position = if (pos.debitsPosted >= pos.creditsPosted) pos.debitsPosted - pos.creditsPosted else pos.creditsPosted - pos.debitsPosted;
            let positionSide : JT.Side = if (pos.debitsPosted >= pos.creditsPosted) #debit else #credit;
            let equivalent = if (eq.debitsPosted >= eq.creditsPosted) eq.debitsPosted - eq.creditsPosted else eq.creditsPosted - eq.debitsPosted;
            switch (Fx.revalue(pair, position, positionSide, equivalent, rate, #halfEven)) {
              case (#err(_)) return #err(#InvalidPair({ reason = "the pair for " # pair.currency # " is not monetary" }));
              case (#ok(rev)) {
                switch (Fx.revaluationLegs(pair, functional, rev)) {
                  case null {};
                  case (?legs) {
                    switch (postLegs(js, journalCaller, now, "fx-revaluation", [x.period, pair.currency, Nat.toText(x.valueDate)], legs, x.postingDate, x.valueDate, x.period, x.narration)) {
                      case (#err(e)) return #err(e);
                      case (#ok(plan)) { for (st in plan.journal.vals()) List.add(steps, st) };
                    };
                    List.add(events, #close(#fxRevalued({ currency = pair.currency; position; equivalent; revalued = rev.revalued; movement = rev.movement; direction = rev.direction; rateNumerator = rate.numerator; rateDenominator = rate.denominator; rateAsOf = rate.asOf; day = x.valueDate })));
                  };
                };
              };
            };
          };
        };
        let evs = List.toArray(events);
        if (evs.size() == 0) return nothing();
        #ok({ event = ?evs[0]; extra = Array.tabulate<T.Event>(evs.size() - 1, func(i) { evs[i + 1] }); journal = List.toArray(steps) })
      };
      // ── call and notice money ──
      case (#openCall(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        if (TreasuryCore.policy(s.treasury) == null) return callErr(#NoPolicy);
        if (Auth.isShariaBook(a, x.book)) return callErr(#ShariaBook({ book = x.book }));
        if (Text.encodeUtf8(x.counterparty.name).size() == 0 or Text.encodeUtf8(x.counterparty.name).size() > 64) return callErr(#InvalidTerms({ reason = "the counterparty is named in 1..64 bytes" }));
        if (Text.encodeUtf8(x.reference).size() > 64) return callErr(#InvalidTerms({ reason = "the reference is at most 64 bytes" }));
        switch (CallCore.validateTerms(x.terms, today)) { case (?r) return callErr(#InvalidTerms({ reason = r })); case null {} };
        switch (x.terms.cash.sub) {
          case (?sub) { if (TreasuryCore.nostroOfAccount(s.treasury, TreasuryCore.nostroAccountHash(x.terms.cash.account, ?Posting.subledgerOf(sub), x.terms.currency)) == null) return callErr(#InvalidTerms({ reason = "the settlement account " # x.terms.cash.account # "/" # sub # " in " # x.terms.currency # " is not a registered nostro" })) };
          case null { switch (JCore.getAccount(js, x.terms.cash.account)) { case null return #err(#UnknownAccount({ role = "call settlement"; account = x.terms.cash.account })); case (?_) {} } };
        };
        let extras = switch (combinedLimitEvents(s, x.book, x.counterparty, x.terms.currency, x.terms.principal, false, s.height, x.approver, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        let tree = switch (treeEventsOf(s, command, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        #ok({ event = ?#call(#opened({ book = x.book; counterparty = x.counterparty; terms = x.terms; reference = x.reference; trader = authority; day = today; withinLimits = extras.size() == 0; approver = x.approver })); extra = Array.concat<T.Event>(extras, tree); journal = [] })
      };
      case (#resetCallRate(x)) {
        let p = switch (policyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
        let r = switch (CallCore.rowOpen(s.calls, x.call)) { case (#err(e)) return callErr(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let (_, _, cash) = callOpeningOf(bb, x.call);
        let ?cs = cash else return callErr(#UnknownCall({ call = x.call }));
        switch (CallCore.planReset(s.calls, x.call, x.rateBps, x.valueDate)) {
          case (#err(e)) callErr(e);
          case (#ok(ev)) callPost(js, journalCaller, now, p, r, cs, ev, "call-reset", [authId, Nat.toText(x.call), Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration);
        }
      };
      case (#adjustCallBalance(x)) {
        let p = switch (policyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
        let r = switch (CallCore.rowOpen(s.calls, x.call)) { case (#err(e)) return callErr(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let (cpName, _, cash) = callOpeningOf(bb, x.call);
        let ?cs = cash else return callErr(#UnknownCall({ call = x.call }));
        // an addition counts towards the counterparty limit like an opening; a draw never breaches
        let extras = if (x.delta > 0) { switch (combinedLimitEvents(s, r.book, { party = null; name = cpName; bic = ""; lei = "" }, r.currency, Int.abs(x.delta), false, x.call, x.approver, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs } } else [];
        let tree = switch (treeEventsOf(s, command, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        switch (CallCore.planAdjust(s.calls, x.call, x.delta, x.valueDate)) {
          case (#err(e)) callErr(e);
          case (#ok(ev)) {
            switch (callPost(js, journalCaller, now, p, r, cs, ev, "call-adjust", [authId, Nat.toText(x.call), Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration)) {
              case (#err(e)) #err(e);
              case (#ok(plan)) #ok({ plan with extra = Array.concat<T.Event>(Array.concat<T.Event>(plan.extra, extras), tree) });
            }
          };
        }
      };
      case (#serveCallNotice(x)) {
        switch (CallCore.planNotice(s.calls, x.call, today, nextBusinessDay(js))) { case (#err(e)) callErr(e); case (#ok(ev)) only(#call(ev)) }
      };
      case (#settleCall(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let p = switch (policyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
        let r = switch (CallCore.rowOpen(s.calls, x.call)) { case (#err(e)) return callErr(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let (_, _, cash) = callOpeningOf(bb, x.call);
        let ?cs = cash else return callErr(#UnknownCall({ call = x.call }));
        switch (CallCore.planDue(s.calls, x.call, x.valueDate)) {
          case (#err(e)) callErr(e);
          case (#ok(null)) nothing();
          case (#ok(?ev)) callPost(js, journalCaller, now, p, r, cs, ev, "call-settle", [authId, Nat.toText(x.call), Nat.toText(x.valueDate), callPart(ev)], x.postingDate, x.valueDate, x.period, x.narration);
        }
      };
      // ── securities services ──
      case (#setCustodyPolicy(p)) custodyPlan(CustodyCore.planPolicy(p));
      case (#extendInstrument(x)) custodyPlan(CustodyCore.planExtend(s.custody, s.treasury, x.extension, today));
      case (#openDepot(x)) custodyPlan(CustodyCore.planOpenDepot(s.custody, x.depot, today));
      case (#setBookDepot(x)) {
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        custodyPlan(CustodyCore.planSetBookDepot(s.custody, x.book, x.depot, today))
      };
      case (#assignDealDepot(x)) custodyPlan(CustodyCore.planAssignDealDepot(s.custody, s.treasury, x.deal, x.depot, today));
      case (#transferDepot(x)) custodyPlan(CustodyCore.planTransfer(s.custody, x.lot, x.from, x.to, x.nominal, x.reference, today));
      case (#announceCorporateAction(x)) custodyPlan(CustodyCore.planAnnounce(s.custody, s.treasury, x.announcement, today));
      case (#cancelCorporateAction(x)) custodyPlan(CustodyCore.planCancel(s.custody, x.action, x.reason, today));
      // ── settlement through Tachyon ──
      case (#setSettlementVenue(x)) {
        switch (JCore.getAccount(js, x.venue.claimsAccount)) { case null return #err(#UnknownAccount({ role = "settlement claims"; account = x.venue.claimsAccount })); case (?_) {} };
        settlementPlan(SettlementCore.planVenue(x.venue))
      };
      case (#setSettlementLedger(x)) settlementPlan(SettlementCore.planLedger(x.declaration));
      case (#openSettlementCycle(x)) settlementPlan(SettlementCore.planOpenCycle(s.settlement, x.cycle, today));
      case (#instructSettlement(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let r = switch (treasuryRow(s, x.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        let #security(t) = kind else return settlementErr(#DealNotSettleable({ deal = x.deal; reason = "not a security" }));
        let (amount, document) = switch (instructionDocument(s, js, bb, r, t)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        settlementPlan(SettlementCore.planInstruct(s.settlement, s.treasury, r, t, amount, x.counterparty, x.tradeId, x.reference, Sha256.fromBlob(#sha256, Text.encodeUtf8(document)), today))
      };
      case (#setInstructionTrade(x)) {
        let i = switch (instructionRow(s, x.instruction)) { case (#err(e)) return #err(e); case (#ok(i)) i };
        if (i.role != #taker) return settlementErr(#InvalidTerms({ reason = "a sale's trade is the desk's own opening" }));
        if (i.state != #instructed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = ST.stateText(i.state); wanted = "instructed" }));
        if (i.escrowed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = "escrowed on trade " # Nat.toText(i.tradeId); wanted = "reclaimed" }));
        if (x.tradeId == 0) return settlementErr(#InvalidTerms({ reason = "a trade id is positive" }));
        only(#settlement(#tradeAssigned({ instruction = i.id; tradeId = x.tradeId; day = today })))
      };
      case (#recycleSettlement(x)) {
        let i = switch (instructionRow(s, x.instruction)) { case (#err(e)) return #err(e); case (#ok(i)) i };
        if (i.state != #failed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = ST.stateText(i.state); wanted = "failed" }));
        if (x.cycle < today) return settlementErr(#InvalidTerms({ reason = "an instruction is recycled into a cycle on or after the day" }));
        switch (SettlementCore.cycle(s.settlement, x.cycle)) { case (?c) { if (c.state != #open) return settlementErr(#NoCycle({ businessDate = x.cycle })) }; case null return settlementErr(#NoCycle({ businessDate = x.cycle })) };
        only(#settlement(#recycled({ instruction = i.id; cycle = x.cycle; fails = i.fails; day = today })))
      };
      case (#recordSettlementStatus(x)) {
        let i = switch (instructionRow(s, x.instruction)) { case (#err(e)) return #err(e); case (#ok(i)) i };
        let r = switch (treasuryRow(s, i.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let ?sec = TreasuryCore.security(s.treasury, r.isin) else return treasuryErr(#UnknownSecurity({ isin = r.isin }));
        let ?minor = minorUnitsOf(js, sec.currency) else return #err(#MissingRate({ currency = sec.currency; asOf = today }));
        let (_, reference) = treasuryCaptureOf(bb, i.deal);
        switch (SettlementMessages.parseSese024(x.document, minor)) {
          case (#err(reason)) settlementErr(#InvalidTerms({ reason = "sese.024: " # reason }));
          case (#ok(inc)) {
            let matched = Text.equal(inc.reference, reference) and inc.quantity == i.assetAmount and (inc.amount == 0 or inc.amount == i.cashAmount) and (inc.isin.size() == 0 or Text.equal(inc.isin, r.isin))
              and (switch (inc.tradeId) { case (?t) t == i.tradeId; case null true });
            only(#settlement(#statusReceived({ instruction = i.id; status = inc.status; quantity = inc.quantity; amount = inc.amount; matched; documentHash = Sha256.fromBlob(#sha256, x.document); day = today })))
          };
        }
      };
      case (#cancelSettlement(x)) {
        let i = switch (instructionRow(s, x.instruction)) { case (#err(e)) return #err(e); case (#ok(i)) i };
        switch (i.state) { case (#settled or #boughtIn or #cancelled) return settlementErr(#InstructionNotIn({ instruction = i.id; state = ST.stateText(i.state); wanted = "open" })); case (_) {} };
        if (i.escrowed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = "escrowed on trade " # Nat.toText(i.tradeId); wanted = "reclaimed" }));
        if (x.ourConsent.size() != 32 or x.theirConsent.size() != 32) return settlementErr(#InvalidTerms({ reason = "each party's consent is a sha256" }));
        // the deal itself is cancelled with the settlement: both parties agreed not to settle it
        switch (TreasuryCore.planCancel(s.treasury, i.deal, "settlement cancelled by consent: " # x.reason, today)) {
          case (#err(e)) treasuryErr(e);
          case (#ok(te)) #ok({ event = ?#settlement(#cancelled({ instruction = i.id; ourConsent = x.ourConsent; theirConsent = x.theirConsent; reason = x.reason; day = today })); extra = [#treasury(te)]; journal = [] });
        }
      };
      case (#buyIn(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let ?venue = SettlementCore.venue(s.settlement) else return settlementErr(#NoVenue);
        let i = switch (instructionRow(s, x.instruction)) { case (#err(e)) return #err(e); case (#ok(i)) i };
        if (i.state != #failed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = ST.stateText(i.state); wanted = "failed" }));
        if (i.role != #taker) return settlementErr(#InvalidTerms({ reason = "a buy-in replaces a purchase the counterparty failed to deliver" }));
        if (i.escrowed) return settlementErr(#InstructionNotIn({ instruction = i.id; state = "escrowed on trade " # Nat.toText(i.tradeId); wanted = "reclaimed" }));
        let r = switch (treasuryRow(s, i.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        let #security(t) = kind else return settlementErr(#DealNotSettleable({ deal = i.deal; reason = "not a security" }));
        let ?sec = TreasuryCore.security(s.treasury, t.isin) else return treasuryErr(#UnknownSecurity({ isin = t.isin }));
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        let te = switch (TreasuryCore.planCancel(s.treasury, i.deal, "bought in after a settlement fail", today)) { case (#err(e)) return treasuryErr(e); case (#ok(e)) e };
        // the replacement purchase, at the new price and date, with the original's trader; captured at the block after the cancellation
        let replacementId = s.height + 2;
        let trader = switch (bb.get(i.deal)) { case (?b) { switch (b.event) { case (#treasury(#dealCaptured(c))) c.trader; case (_) authority } }; case null authority };
        let terms : TT.SecurityTrade = { t with priceMicro = x.priceMicro; settlement = x.settlement };
        let cap = switch (TreasuryCore.planCapture(s.treasury, replacementId, r.book, x.counterparty, #security(terms), x.reference, trader, today, ?authority, ctx, treasuryTerms(bb))) { case (#err(e)) return treasuryErr(e); case (#ok(c)) c };
        // the claim on the failing counterparty: what the replacement costs beyond the original, clean
        let original = TreasuryMath.cleanCost(t.nominal, t.priceMicro);
        let replacement = TreasuryMath.cleanCost(t.nominal, x.priceMicro);
        let claim = if (replacement > original) replacement - original else 0;
        let depotAssigned : [T.Event] = switch (CustodyCore.bookDepotOf(s.custody, r.book)) { case (?d) [#custody(#dealDepotAssigned({ deal = replacementId; depot = d; day = today }))]; case null [] };
        let extras = Array.concat<T.Event>(Array.concat<T.Event>([#treasury(te), #treasury(cap.ev)], Array.map<TT.TreasuryEvent, T.Event>(cap.extras, func(e) { #treasury(e) })), depotAssigned);
        let ev : T.Event = #settlement(#boughtIn({ instruction = i.id; replacement = replacementId; claim; day = today }));
        if (claim == 0) return #ok({ event = ?ev; extra = extras; journal = [] });
        let ?p = TreasuryCore.policy(s.treasury) else return treasuryErr(#NoPolicy);
        let legs = [Posting.leg(venue.claimsAccount, ?Posting.subledgerOf("settlement/" # Nat.toText(i.id)), #debit, sec.currency, claim), Posting.leg(p.realisedTradingGain, null, #credit, sec.currency, claim)];
        switch (postLegs(js, journalCaller, now, "settlement-claim", [authId, Nat.toText(i.id)], legs, x.postingDate, x.valueDate, x.period, x.narration)) {
          case (#err(e)) #err(e);
          case (#ok(plan)) #ok({ event = ?ev; extra = extras; journal = plan.journal });
        }
      };
      // ── financing ──
      case (#setFinancingPolicy(pol)) {
        for (code in FinancingCore.accountsOf(pol).vals()) { switch (JCore.getAccount(js, code)) { case null return #err(#UnknownAccount({ role = "financing policy"; account = code })); case (?_) {} } };
        financingPlan(FinancingCore.planPolicy(pol))
      };
      case (#openRepo(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        if (FinancingCore.policy(s.financing) == null) return financingErr(#NoPolicy);
        if (Auth.isShariaBook(a, x.book)) return callErr(#ShariaBook({ book = x.book }));
        if (Text.encodeUtf8(x.counterparty.name).size() == 0 or Text.encodeUtf8(x.counterparty.name).size() > 64) return financingErr(#InvalidTerms({ reason = "the counterparty is named in 1..64 bytes" }));
        if (Text.encodeUtf8(x.reference).size() > 64) return financingErr(#InvalidTerms({ reason = "the reference is at most 64 bytes" }));
        switch (FinancingCore.validateRepo(x.terms, today)) { case (?e) return financingErr(e); case null {} };
        if (TreasuryCore.security(s.treasury, x.terms.collateral.isin) == null) return treasuryErr(#UnknownSecurity({ isin = x.terms.collateral.isin }));
        if (CustodyCore.depot(s.custody, x.terms.depot) == null) return custodyErr(#UnknownDepot({ depot = x.terms.depot }));
        switch (JCore.getAccount(js, x.terms.cashAccount.account)) { case null return #err(#UnknownAccount({ role = "repo cash"; account = x.terms.cashAccount.account })); case (?_) {} };
        let tree = switch (treeEventsOf(s, command, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        #ok({ event = ?#financing(#repoOpened({ book = x.book; counterparty = x.counterparty; terms = x.terms; reference = x.reference; trader = authority; day = today })); extra = tree; journal = [] })
      };
      case (#settleRepoLeg(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (SettlementCore.openInstructionOf(s.settlement, x.repo, x.leg)) { case (?i) return settlementErr(#AlreadyInstructed({ deal = x.repo; instruction = i.id })); case null {} };
        planRepoLeg(s, bb, js, journalCaller, now, x.repo, x.leg, authId, x.postingDate, x.valueDate, x.period, x.narration)
      };
      case (#resetRepoRate(x)) {
        let r = switch (repoRow(s, x.repo)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let (_, _, cashOpt) = repoOpeningOf(bb, x.repo);
        let ?cash = cashOpt else return financingErr(#UnknownRepo({ repo = x.repo }));
        let ev = switch (FinancingCore.planRateReset(s.financing, x.repo, x.rateBps, x.valueDate)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
        financingAct(s, js, journalCaller, now, ev, ?r, null, cash, [], "repo-reset", [authId, Nat.toText(x.repo), Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration)
      };
      case (#meetMarginCall(x)) {
        let r = switch (repoRow(s, x.repo)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let (_, _, cashOpt) = repoOpeningOf(bb, x.repo);
        let ?cash = cashOpt else return financingErr(#UnknownRepo({ repo = x.repo }));
        let ?price = priceOf(s, r.isin, x.valueDate) else return financingErr(#NoPrice({ isin = r.isin; day = x.valueDate }));
        let reference = "repo/" # Nat.toText(x.repo);
        let (lots, custody) : ([(TT.DealId, Nat)], [T.Event]) = switch (x.collateral) {
          case (?c) {
            if (r.reverse) ([], [#custody(#collateralReceived({ isin = c.isin; depot = r.depot; nominal = c.nominal; reference; day = x.valueDate }))])
            else {
              let lots = switch (FinancingCore.allocate(CustodyCore.availableIn(s.custody, r.depot, c.isin), c.nominal, c.isin)) { case (#err(e)) return financingErr(e); case (#ok(x)) x };
              (lots, Array.map<(TT.DealId, Nat), T.Event>(lots, func((lot, n)) { #custody(#pledged({ lot; depot = r.depot; nominal = n; reference; day = x.valueDate })) }))
            }
          };
          case null ([], []);
        };
        let ev = switch (FinancingCore.planMeetMargin(s.financing, x.repo, x.cash, x.collateral, price, lots, x.valueDate)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
        financingAct(s, js, journalCaller, now, ev, ?r, null, cash, custody, "repo-margin", [authId, Nat.toText(x.repo), Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration)
      };
      case (#substituteCollateral(x)) {
        let r = switch (repoRow(s, x.repo)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let ?price = priceOf(s, r.isin, today) else return financingErr(#NoPrice({ isin = r.isin; day = today }));
        let reference = "repo/" # Nat.toText(x.repo);
        if (r.reverse) {
          let ev = switch (FinancingCore.planSubstitute(s.financing, x.repo, x.out, x.in_, price, price, [], [], today)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
          return #ok({ event = ?#financing(ev); extra = [#custody(#collateralReturned({ isin = x.out.isin; depot = r.depot; nominal = x.out.nominal; reference; day = today })), #custody(#collateralReceived({ isin = x.in_.isin; depot = r.depot; nominal = x.in_.nominal; reference; day = today }))]; journal = [] });
        };
        // the pledged lots released for what goes out, first in first out over the repo's pledges; the new pledged from what is available
        var left = x.out.nominal;
        let outLots = List.empty<(TT.DealId, Nat)>();
        for ((lot, n) in pledgedLots(s, r).vals()) { if (left > 0) { let q = Nat.min(left, n); List.add(outLots, (lot, q)); left -= q } };
        if (left > 0) return financingErr(#InsufficientCollateral({ isin = x.out.isin; available = x.out.nominal - left; wanted = x.out.nominal }));
        let inLots = switch (FinancingCore.allocate(CustodyCore.availableIn(s.custody, r.depot, x.in_.isin), x.in_.nominal, x.in_.isin)) { case (#err(e)) return financingErr(e); case (#ok(v)) v };
        let ev = switch (FinancingCore.planSubstitute(s.financing, x.repo, x.out, x.in_, price, price, List.toArray(outLots), inLots, today)) { case (#err(e)) return financingErr(e); case (#ok(e)) e };
        let custody = List.empty<T.Event>();
        for ((lot, n) in List.values(outLots)) List.add(custody, #custody(#released({ lot; depot = r.depot; nominal = n; reference; day = today })));
        for ((lot, n) in inLots.vals()) List.add(custody, #custody(#pledged({ lot; depot = r.depot; nominal = n; reference; day = today })));
        #ok({ event = ?#financing(ev); extra = List.toArray(custody); journal = [] })
      };
      case (#openLoan(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        if (FinancingCore.policy(s.financing) == null) return financingErr(#NoPolicy);
        if (Text.encodeUtf8(x.counterparty.name).size() == 0 or Text.encodeUtf8(x.counterparty.name).size() > 64) return financingErr(#InvalidTerms({ reason = "the counterparty is named in 1..64 bytes" }));
        if (Text.encodeUtf8(x.reference).size() > 64) return financingErr(#InvalidTerms({ reason = "the reference is at most 64 bytes" }));
        switch (FinancingCore.validateLoan(x.terms, today)) { case (?e) return financingErr(e); case null {} };
        if (TreasuryCore.security(s.treasury, x.terms.isin) == null) return treasuryErr(#UnknownSecurity({ isin = x.terms.isin }));
        if (CustodyCore.depot(s.custody, x.terms.depot) == null) return custodyErr(#UnknownDepot({ depot = x.terms.depot }));
        switch (JCore.getAccount(js, x.terms.cashAccount.account)) { case null return #err(#UnknownAccount({ role = "loan cash"; account = x.terms.cashAccount.account })); case (?_) {} };
        let tree = switch (treeEventsOf(s, command, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        #ok({ event = ?#financing(#loanOpened({ book = x.book; counterparty = x.counterparty; terms = x.terms; reference = x.reference; trader = authority; day = today })); extra = tree; journal = [] })
      };
      case (#settleLoanLeg(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (SettlementCore.openInstructionOf(s.settlement, x.loan, x.leg)) { case (?i) return settlementErr(#AlreadyInstructed({ deal = x.loan; instruction = i.id })); case null {} };
        planLoanLeg(s, bb, js, journalCaller, now, x.loan, x.leg, authId, x.postingDate, x.valueDate, x.period, x.narration)
      };
      case (#recallLoan(x)) {
        let l = switch (loanRow(s, x.loan)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let returnDay = nextBusinessDay(js)(today + l.noticeDays);
        financingPlan(FinancingCore.planRecall(s.financing, x.loan, today, returnDay))
      };
      case (#instructFinancing(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let amounts = switch (financingLegAmounts(s, x.family, x.id, x.leg, today)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        settlementPlan(SettlementCore.planInstructFinancing(s.settlement, x.family, x.id, x.leg, amounts.isin, amounts.currency, amounts.nominal, amounts.cash, amounts.deskDelivers, amounts.cycleDay, x.counterparty, x.tradeId, x.reference, today))
      };
      // ── valuation ──
      case (#setValuationPolicy(pol)) {
        switch (JCore.getAccount(js, pol.hedgeReserve)) { case null return #err(#UnknownAccount({ role = "hedge reserve"; account = pol.hedgeReserve })); case (?_) {} };
        switch (HedgeCore.planPolicy(pol)) { case (#err(e)) valuationErr(e); case (#ok(ev)) only(#valuation(ev)) }
      };
      case (#quoteBondYield(x)) {
        let ?sec = TreasuryCore.security(s.treasury, x.isin) else return treasuryErr(#UnknownSecurity({ isin = x.isin }));
        if (x.day < today) return valuationErr(#InvalidTerms({ reason = "a quote is for the day or ahead" }));
        let ?price = Valuation.priceOfYield(sec, x.yieldBps, x.day) else return valuationErr(#InvalidTerms({ reason = "the yield prices the bond at nothing, or the bond has matured" }));
        switch (TreasuryCore.planPublishCurve(s.treasury, { id = x.isin; kind = #securityPrice; currency = sec.currency; day = x.day; points = [(0, price)]; source = x.source })) {
          case (#err(e)) treasuryErr(e);
          case (#ok(null)) only(#valuation(#yieldQuoted({ isin = x.isin; day = x.day; yieldBps = x.yieldBps; priceMicro = price })));
          case (#ok(?te)) #ok({ event = ?#valuation(#yieldQuoted({ isin = x.isin; day = x.day; yieldBps = x.yieldBps; priceMicro = price })); extra = [#treasury(te)]; journal = [] });
        }
      };
      case (#designateHedge(x)) {
        if (HedgeCore.policy(s.hedges) == null) return valuationErr(#NoPolicy);
        let hr = switch (treasuryRow(s, x.hedging)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        if (not TreasuryCore.isOpen(hr)) return valuationErr(#NotMarkable({ deal = x.hedging; reason = "the hedging instrument is " # TT.dealStateText(hr.state) }));
        let hk = switch (treasuryKind(bb, hr)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        switch (hk) { case (#irs(_) or #fxForward(_)) {}; case (_) return valuationErr(#NotMarkable({ deal = x.hedging; reason = "a hedging instrument is a swap or a forward" })) };
        let hedgingMark = switch (markOf(s, bb, x.hedging, today, today)) { case (#err(e)) return #err(e); case (#ok(?v)) v; case (#ok(null)) return valuationErr(#NotMarkable({ deal = x.hedging; reason = "the hedging instrument carries no mark today" })) };
        switch (x.kind) {
          case (#cashFlow(c)) {
            let #irs(i) = hk else return valuationErr(#InvalidTerms({ reason = "a cash-flow hedge is by a swap" }));
            if (c.hedgedAmount == 0) return valuationErr(#InvalidTerms({ reason = "the hedged amount is positive" }));
            switch (treasuryRow(s, x.hedged)) { case (#err(_)) { if (CallCore.row(s.calls, x.hedged) == null) return valuationErr(#NotMarkable({ deal = x.hedged; reason = "the hedged item is a deal or a call" })) }; case (#ok(_)) {} };
            let hyp : TT.Irs = { i with notional = c.hedgedAmount };
            let hedgedValue = switch (Valuation.markWith(s.treasury, hr, #irs(hyp), today, dataOn(s, today))) { case (#ok(?v)) v; case (#ok(null)) 0; case (#err(_)) return treasuryErr(#UnknownCurve({ curve = i.discountCurve; day = today })) };
            only(#valuation(#hedgeDesignated({ hedging = x.hedging; hedged = x.hedged; kind = x.kind; hypothetical = ?hyp; hedgingMark; hedgedValue; day = today })))
          };
          case (#fairValue) {
            let lot = switch (treasuryRow(s, x.hedged)) { case (#err(e)) return #err(e); case (#ok(r)) r };
            if (lot.kind != 4 or (lot.flags & TreasuryCore.F_BUY) == 0 or not TreasuryCore.isOpen(lot)) return valuationErr(#NotMarkable({ deal = x.hedged; reason = "a fair-value hedge is of a purchase lot" }));
            let ?price = priceOf(s, lot.isin, today) else return valuationErr(#NoPrice({ isin = lot.isin; day = today }));
            only(#valuation(#hedgeDesignated({ hedging = x.hedging; hedged = x.hedged; kind = x.kind; hypothetical = null; hedgingMark; hedgedValue = TreasuryMath.cleanCost(lot.nominalLeft, price); day = today })))
          };
        }
      };
      case (#assessHedge(x)) {
        let ?vp = HedgeCore.policy(s.hedges) else return valuationErr(#NoPolicy);
        let ?tp = TreasuryCore.policy(s.treasury) else return treasuryErr(#NoPolicy);
        let ?h = HedgeCore.hedge(s.hedges, x.hedge) else return valuationErr(#UnknownHedge({ hedge = x.hedge }));
        let hr = switch (treasuryRow(s, h.hedging)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, hr.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let hedgingMark = switch (markOf(s, bb, h.hedging, x.valueDate, x.valueDate)) { case (#err(e)) return #err(e); case (#ok(?v)) v; case (#ok(null)) 0 };
        let hedgedValue = switch (hedgedValueOf(s, bb, h, hypotheticalOf(bb, x.hedge), x.valueDate)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let ev = switch (HedgeCore.planAssess(s.hedges, x.hedge, hedgingMark, hedgedValue, x.valueDate)) { case (#err(e)) return valuationErr(e); case (#ok(e)) e };
        let (currency, lotAccount) = if (h.cashFlow) (hr.currency, "") else { switch (treasuryRow(s, h.hedged)) { case (#err(e)) return #err(e); case (#ok(l)) (treasuryRowCurrency(s, l), lotAccountOf(tp, l.flags)) } };
        let legs = HedgeCore.legsOf(vp, tp, h, currency, lotAccount, ev);
        if (legs.size() == 0) return only(#valuation(ev));
        switch (postLegs(js, journalCaller, now, "hedge-assessment", [authId, Nat.toText(x.hedge), Nat.toText(x.valueDate)], legs, x.postingDate, x.valueDate, x.period, x.narration)) {
          case (#err(e)) #err(e);
          case (#ok(plan)) #ok({ event = ?#valuation(ev); extra = []; journal = plan.journal });
        }
      };
      case (#dedesignateHedge(x)) {
        let ?vp = HedgeCore.policy(s.hedges) else return valuationErr(#NoPolicy);
        let ?tp = TreasuryCore.policy(s.treasury) else return treasuryErr(#NoPolicy);
        let ?h = HedgeCore.hedge(s.hedges, x.hedge) else return valuationErr(#UnknownHedge({ hedge = x.hedge }));
        let hr = switch (treasuryRow(s, h.hedging)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let ev = switch (HedgeCore.planDedesignate(s.hedges, x.hedge, x.valueDate)) { case (#err(e)) return valuationErr(e); case (#ok(e)) e };
        let legs = HedgeCore.legsOf(vp, tp, h, hr.currency, "", ev);
        if (legs.size() == 0) return only(#valuation(ev));
        switch (postLegs(js, journalCaller, now, "hedge-dedesignation", [authId, Nat.toText(x.hedge)], legs, x.postingDate, x.valueDate, x.period, x.narration)) {
          case (#err(e)) #err(e);
          case (#ok(plan)) #ok({ event = ?#valuation(ev); extra = []; journal = plan.journal });
        }
      };
      case (#splitDeal(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let r = switch (treasuryRow(s, x.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        let #security(t) = kind else return settlementErr(#DealNotSettleable({ deal = x.deal; reason = "not a security" }));
        switch (SettlementCore.ledger(s.settlement, #security({ isin = t.isin }))) { case (?l) { if (not l.partial) return settlementErr(#PartialNotAllowed({ isin = t.isin })) }; case null return settlementErr(#NoLedger({ role = #security({ isin = t.isin }) })) };
        switch (SettlementCore.openInstructionOf(s.settlement, x.deal, 0)) { case (?i) return settlementErr(#AlreadyInstructed({ deal = x.deal; instruction = i.id })); case null {} };
        if (x.parts.size() < 2 or x.parts.size() > 16) return settlementErr(#InvalidTerms({ reason = "a deal splits into 2..16 parts" }));
        var sum = 0; for (q in x.parts.vals()) { if (q == 0) return settlementErr(#InvalidTerms({ reason = "every part is positive" })); sum += q };
        if (sum != t.nominal) return settlementErr(#InvalidTerms({ reason = "the parts sum to the deal's nominal" }));
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        let te = switch (TreasuryCore.planCancel(s.treasury, x.deal, "split for partial delivery", today)) { case (#err(e)) return treasuryErr(e); case (#ok(e)) e };
        let (cp, trader, reference) = switch (bb.get(x.deal)) { case (?b) { switch (b.event) { case (#treasury(#dealCaptured(c))) (c.counterparty, c.trader, c.reference); case (_) return treasuryErr(#UnknownDeal({ deal = x.deal })) } }; case null return treasuryErr(#UnknownDeal({ deal = x.deal })) };
        let depot = CustodyCore.bookDepotOf(s.custody, r.book);
        let extras = List.empty<T.Event>();
        List.add(extras, #treasury(te));
        var next = s.height + 2;   // the split at s.height, the cancellation after it, then the parts
        var partNo = 1;
        for (q in x.parts.vals()) {
          let terms : TT.SecurityTrade = { t with nominal = q };
          // the parts are captured with the split's approver: the exposure they add is the original's, cancelled in the same act
          let cap = switch (TreasuryCore.planCapture(s.treasury, next, r.book, cp, #security(terms), reference # "/" # Nat.toText(partNo), trader, today, ?authority, ctx, treasuryTerms(bb))) { case (#err(e)) return treasuryErr(e); case (#ok(c)) c };
          List.add(extras, #treasury(cap.ev));
          var used = 1;
          for (e in cap.extras.vals()) { List.add(extras, #treasury(e)); used += 1 };
          switch (depot) { case (?d) { List.add(extras, #custody(#dealDepotAssigned({ deal = next; depot = d; day = today }))); used += 1 }; case null {} };
          next += used; partNo += 1;
        };
        #ok({ event = ?#settlement(#split({ deal = x.deal; parts = x.parts; day = today })); extra = List.toArray(extras); journal = [] })
      };
      case (#processCorporateAction(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (nextActionAct(s, bb, x.action, x.valueDate)) {
          case (#err(e)) #err(e);
          case (#ok(null)) {
            // nothing due by hand is a typed refusal: the day it falls due, or the state it is in
            let ?r = CustodyCore.action(s.custody, x.action) else return custodyErr(#UnknownAction({ action = x.action }));
            switch (r.state) {
              case (#announced) custodyErr(#NotDue({ action = x.action; due = r.recordDate; day = x.valueDate }));
              case (#entitled) custodyErr(#NotDue({ action = x.action; due = r.paymentDate; day = x.valueDate }));
              case (_) custodyErr(#ActionNotIn({ action = x.action; state = CuT.actionStateText(r.state); wanted = "announced or entitled" }));
            }
          };
          case (#ok(?act)) {
            let first = act.events[0];
            let rest = Array.tabulate<T.Event>(act.events.size() - 1, func(i) { act.events[i + 1] });
            if (act.legs.size() == 0) return #ok({ event = ?first; extra = rest; journal = [] });
            let lotText = switch (act.lot) { case (?l) Nat.toText(l); case null "" };
            switch (postLegs(js, journalCaller, now, "corporate-action", [authId, Nat.toText(x.action), act.step, lotText, Nat.toText(x.valueDate)], act.legs, x.postingDate, x.valueDate, x.period, x.narration)) {
              case (#err(e)) #err(e);
              case (#ok(plan)) #ok({ event = ?first; extra = rest; journal = plan.journal });
            }
          };
        }
      };
      // ── collateral ──
      case (#setCollateralPolicy(pol)) {
        for (acct in CollateralCore.accountsOf(pol).vals()) { switch (requirePostableAccount(js, "collateral", acct)) { case (?e) return #err(e); case null {} } };
        switch (CollateralCore.planPolicy(pol)) { case (#err(e)) collateralErr(e); case (#ok(ev)) only(#collateral(ev)) }
      };
      case (#setCollateralAgreement(x)) {
        let ag = x.agreement;
        if (JCore.currencyMinorUnits(js, ag.currency) == null) return collateralErr(#InvalidAgreement({ reason = "currency " # ag.currency # " is not registered in the journal" }));
        switch (requirePostableAccount(js, "collateral cash", ag.cash.account)) { case (?e) return #err(e); case null {} };
        switch (CollateralCore.planSetAgreement(s.collateral, ag, today)) { case (#err(e)) collateralErr(e); case (#ok(ev)) only(#collateral(ev)) }
      };
      case (#postCollateralCash(x)) {
        let p = switch (collateralPolicyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        // cash the counterparty posts, or takes back of the desk's, meets a call on the counterparty; the reverse meets a call on the desk
        let deskDelivers = switch (x.move) { case (#given or #receivedReturned) true; case (_) false };
        let value = switch (poolCashValue(s, ag, x.amount, x.currency, x.valueDate)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let credit = callCreditFor(s, ag, deskDelivers, value);
        let ev = switch (CollateralCore.planCashMove(s.collateral, x.agreement, x.move, x.amount, x.currency, credit, x.valueDate)) { case (#err(e)) return collateralErr(e); case (#ok(e)) e };
        collateralPost(s, js, journalCaller, now, p, ag, ev, "collateral-cash", [authId, x.agreement, CoT.cashMoveText(x.move), Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration, callMetAfter(s, ag, credit, x.valueDate))
      };
      case (#pledgeCollateral(x)) {
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let (lot, depot) = switch (pledgeableLot(s, x.lot, x.nominal)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let value = switch (poolSecuritiesValue(s, ag, lot.isin, x.nominal, today)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let credit = callCreditFor(s, ag, true, value);
        let custody : [T.Event] = [#custody(#pledged({ lot = x.lot; depot; nominal = x.nominal; reference = "collateral/" # x.agreement; day = today }))];
        #ok({ event = ?#collateral(#securitiesPledged({ agreement = x.agreement; id = s.height; lot = x.lot; isin = lot.isin; depot; nominal = x.nominal; callCredit = credit; day = today })); extra = Array.concat<T.Event>(custody, callMetAfter(s, ag, credit, today)); journal = [] })
      };
      case (#releaseCollateral(x)) {
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let r = switch (CollateralCore.requireSecurities(s.collateral, x.agreement, x.pledge, #live)) { case (#err(e)) return collateralErr(e); case (#ok(r)) r };
        let ?lot = r.lot else return collateralErr(#PledgeNotIn({ id = x.pledge; state = "received"; wanted = "pledged by the desk" }));
        let value = switch (poolSecuritiesValue(s, ag, r.isin, r.nominal, today)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let credit = callCreditFor(s, ag, false, value);
        let custody : [T.Event] = [#custody(#released({ lot; depot = r.depot; nominal = r.nominal; reference = "collateral/" # x.agreement; day = today }))];
        #ok({ event = ?#collateral(#securitiesReleased({ agreement = x.agreement; pledge = x.pledge; callCredit = credit; day = today })); extra = Array.concat<T.Event>(custody, callMetAfter(s, ag, credit, today)); journal = [] })
      };
      case (#receiveCollateral(x)) {
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        if (x.nominal == 0) return collateralErr(#InvalidMove({ reason = "a receipt is a positive nominal" }));
        if (CustodyCore.depot(s.custody, x.depot) == null) return custodyErr(#UnknownDepot({ depot = x.depot }));
        let value = switch (poolSecuritiesValue(s, ag, x.isin, x.nominal, today)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let credit = callCreditFor(s, ag, false, value);
        let custody : [T.Event] = [#custody(#collateralReceived({ isin = x.isin; depot = x.depot; nominal = x.nominal; reference = "collateral/" # x.agreement; day = today }))];
        #ok({ event = ?#collateral(#securitiesReceived({ agreement = x.agreement; id = s.height; isin = x.isin; depot = x.depot; nominal = x.nominal; callCredit = credit; day = today })); extra = Array.concat<T.Event>(custody, callMetAfter(s, ag, credit, today)); journal = [] })
      };
      case (#returnCollateral(x)) {
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let r = switch (CollateralCore.requireSecurities(s.collateral, x.agreement, x.receipt, #live)) { case (#err(e)) return collateralErr(e); case (#ok(r)) r };
        if (r.given) return collateralErr(#PledgeNotIn({ id = x.receipt; state = "pledged by the desk"; wanted = "received" }));
        let value = switch (poolSecuritiesValue(s, ag, r.isin, r.nominal, today)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        let credit = callCreditFor(s, ag, true, value);
        let custody : [T.Event] = [#custody(#collateralReturned({ isin = r.isin; depot = r.depot; nominal = r.nominal; reference = "collateral/" # x.agreement; day = today }))];
        #ok({ event = ?#collateral(#securitiesReturned({ agreement = x.agreement; receipt = x.receipt; callCredit = credit; day = today })); extra = Array.concat<T.Event>(custody, callMetAfter(s, ag, credit, today)); journal = [] })
      };
      case (#openCollateralSubstitution(x)) {
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        if (x.cashReturned == 0) return collateralErr(#InvalidMove({ reason = "a substitution returns cash; a delivery against nothing is a pledge" }));
        if (Text.encodeUtf8(x.currency).size() != 3) return collateralErr(#InvalidMove({ reason = "a currency code has three letters" }));
        let c = CollateralCore.cashRow(s.collateral, x.agreement, x.currency);
        if (c.given < x.cashReturned) return collateralErr(#PoolShort({ agreement = x.agreement; currency = x.currency; held = c.given; wanted = x.cashReturned }));
        let (lot, depot) = switch (pledgeableLot(s, x.lot, x.nominal)) { case (#err(e)) return #err(e); case (#ok(v)) v };
        switch (poolSecuritiesValue(s, ag, lot.isin, x.nominal, today)) { case (#err(e)) return #err(e); case (#ok(_)) {} };
        let custody : [T.Event] = [#custody(#pledged({ lot = x.lot; depot; nominal = x.nominal; reference = "collateral/" # x.agreement; day = today }))];
        #ok({ event = ?#collateral(#substitutionOpened({ agreement = x.agreement; id = s.height; lot = x.lot; isin = lot.isin; depot; nominal = x.nominal; cashReturned = x.cashReturned; currency = x.currency; day = today })); extra = custody; journal = [] })
      };
      case (#settleCollateralSubstitution(x)) {
        switch (SettlementCore.openInstructionOf(s.settlement, x.substitution, 0)) { case (?i) return settlementErr(#AlreadyInstructed({ deal = x.substitution; instruction = i.id })); case null {} };
        planSubstitutionSettlement(s, js, journalCaller, now, x.agreement, x.substitution, authId, x.postingDate, x.valueDate, x.period, x.narration)
      };
      case (#settleCollateralInterest(x)) {
        let p = switch (collateralPolicyOf(s)) { case (#err(e)) return #err(e); case (#ok(p)) p };
        let ag = switch (agreementRow(s, x.agreement)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let ev = switch (CollateralCore.planSettleInterest(s.collateral, x.agreement, x.currency, x.valueDate)) { case (#err(e)) return collateralErr(e); case (#ok(e)) e };
        collateralPost(s, js, journalCaller, now, p, ag, ev, "collateral-interest-settled", [authId, x.agreement, x.currency, Nat.toText(x.valueDate)], x.postingDate, x.valueDate, x.period, x.narration, [])
      };
      // ── limits ──
      case (#setLimitNode(x)) {
        if (JCore.currencyMinorUnits(js, x.node.currency) == null) return limitErr(#InvalidNode({ reason = "currency " # x.node.currency # " is not registered in the journal" }));
        switch (LimitCore.planSetNode(s.limits, x.node, isUnderBook(s), bookExists(s), today)) { case (#err(e)) limitErr(e); case (#ok(ev)) only(#limits(ev)) }
      };
      case (#removeLimitNode(x)) { switch (LimitCore.planRemoveNode(s.limits, x.node, today)) { case (#err(e)) limitErr(e); case (#ok(ev)) only(#limits(ev)) } };
      case (#amendCounterparty(x)) { switch (LimitCore.planAmendCounterparty(s.limits, x.counterparty, today)) { case (#err(e)) limitErr(e); case (#ok(ev)) only(#limits(ev)) } };
      case (#openRiskSweep(x)) {
        for (r in Eod.listRuns(s.eod).vals()) { if (Eod.isOpen(r)) return limitErr(#RunOpen({ book = r.book; businessDate = r.businessDate })) };
        switch (LimitCore.planOpenSweep(s.limits, today, s.height, x.sliceSize)) { case (#err(e)) limitErr(e); case (#ok(ev)) only(#limits(ev)) }
      };
      // ── reconciliation ──
      case (#setReconciliationPolicy(pol)) {
        for (c in pol.cashAccounts.vals()) { switch (requirePostableAccount(js, "settlement cash", c.cash.account)) { case (?e) return #err(e); case null {} } };
        switch (ReconciliationCore.planPolicy(pol)) { case (#err(e)) reconErr(e); case (#ok(ev)) only(#reconciliation(ev)) }
      };
      case (#recordNostroNotification(x)) {
        let ?nr = TreasuryCore.nostro(s.treasury, x.nostro) else return treasuryErr(#UnknownNostro({ nostro = x.nostro }));
        let mu : Nat8 = switch (minorUnitsOf(js, nr.currency)) { case (?m) m; case null 2 };
        let parsed = switch (ReconciliationMessages.parseCamt054(x.document, nr.currency, mu)) { case (#ok(p)) p; case (#err(reason)) return reconErr(#BadDocument({ reason })) };
        let hash = Sha256.fromBlob(#sha256, x.document);
        if (TreasuryCore.statementKnown(s.treasury, hash) or ReconciliationCore.notification(s.reconciliation, hash) != null) return reconErr(#StatementKnown({ statement = hash }));
        if (parsed.entries.size() == 0) return reconErr(#BadDocument({ reason = "a notification carries at least one entry" }));
        if (parsed.entries.size() > ReconciliationCore.MAX_ENTRIES) return reconErr(#BadDocument({ reason = "at most " # Nat.toText(ReconciliationCore.MAX_ENTRIES) # " entries" }));
        var lo = parsed.entries[0].valueDay; var hi = lo;
        for (e in parsed.entries.vals()) { if (e.amount == 0) return reconErr(#BadDocument({ reason = "an entry's amount is positive" })); if (e.valueDay > today) return reconErr(#BadDocument({ reason = "an entry's value day is not after today" })); if (e.valueDay < lo) lo := e.valueDay; if (e.valueDay > hi) hi := e.valueDay };
        let legs = TreasuryCore.nostroLegsIn(s.treasury, nr.accountHash, if (lo > nr.tolerance) lo - nr.tolerance else 0, hi + nr.tolerance);
        let m = ReconciliationCore.matchNotification(parsed.entries, legs, nr.tolerance);
        let matched = Array.map<(Nat, Nat), (TT.StatementEntry, Nat)>(m.matches, func((e, posting)) { (parsed.entries[e], posting) });
        let extras = List.empty<T.Event>();
        List.add(extras, #reconciliation(#notificationRecorded({ nostro = x.nostro; notification = hash; entries = parsed.entries.size(); matched; unmatched = m.unmatched.size(); day = today })));
        // the matched legs are marked through Manticore's own statement event, with no breaks: the statement of the day decides the rest
        if (m.matches.size() > 0) List.add(extras, #treasury(#statementRecorded({ nostro = x.nostro; statement = hash; from = lo; to = hi; entries = parsed.entries.size(); matches = Array.map<(Nat, Nat), Nat>(m.matches, func((_, p)) { p }); breaks = 0; day = today })));
        #ok({ event = ?#journalMark({ height = JCore.height(js) }); extra = List.toArray(extras); journal = [] })
      };
      case (#recordDepotStatement(x)) {
        let ?depot = CustodyCore.depot(s.custody, x.depot) else return custodyErr(#UnknownDepot({ depot = x.depot }));
        let hash = Sha256.fromBlob(#sha256, x.document);
        if (ReconciliationCore.statement(s.reconciliation, hash) != null) return reconErr(#StatementKnown({ statement = hash }));
        let ?kind = ReconciliationMessages.depotDocumentKind(x.document) else return reconErr(#BadDocument({ reason = "the document is a semt.002 holdings report or a semt.017 transaction posting report" }));
        let mu : Nat8 = 2;
        let extras = List.empty<T.Event>();
        switch (kind) {
          case (#holdings) {
            let parsed = switch (ReconciliationMessages.parseSemt002(x.document, mu)) { case (#ok(p)) p; case (#err(reason)) return reconErr(#BadDocument({ reason })) };
            if (not Text.equal(parsed.account, depot.safekeepingAccount)) return reconErr(#WrongAccount({ depot = x.depot; expected = depot.safekeepingAccount; got = parsed.account }));
            if (parsed.holdings.size() > ReconciliationCore.MAX_REPORTED) return reconErr(#BadDocument({ reason = "at most " # Nat.toText(ReconciliationCore.MAX_REPORTED) # " balances" }));
            if (parsed.statementDate > today) return reconErr(#BadDocument({ reason = "the statement date is not after today" }));
            let m = ReconciliationCore.matchHoldings(parsed.holdings, depotPositions(s, x.depot));
            List.add(extras, #reconciliation(#depotStatementRecorded({ depot = x.depot; statement = hash; kind = #holdings; statementDate = parsed.statementDate; from = parsed.statementDate; to = parsed.statementDate; reported = parsed.holdings.size(); matched = m.matched; explained = 0; breaks = m.breaks.size(); day = today })));
            for ((isin, ours, theirs, side) in m.breaks.vals()) List.add(extras, #reconciliation(#depotBreak({ depot = x.depot; statement = hash; kind = #position; side; isin; ours; theirs; reference = ""; instruction = null; day = today })));
          };
          case (#transactions) {
            let parsed = switch (ReconciliationMessages.parseSemt017(x.document, mu)) { case (#ok(p)) p; case (#err(reason)) return reconErr(#BadDocument({ reason })) };
            if (not Text.equal(parsed.account, depot.safekeepingAccount)) return reconErr(#WrongAccount({ depot = x.depot; expected = depot.safekeepingAccount; got = parsed.account }));
            if (parsed.transactions.size() > ReconciliationCore.MAX_REPORTED) return reconErr(#BadDocument({ reason = "at most " # Nat.toText(ReconciliationCore.MAX_REPORTED) # " postings" }));
            if (parsed.to > today) return reconErr(#BadDocument({ reason = "the period ends on or before today" }));
            if (parsed.to > parsed.from + 31) return reconErr(#BadDocument({ reason = "the period spans at most 31 days" }));
            let m = ReconciliationCore.matchTransactions(parsed.transactions, depotInstructions(s, js, x.depot, parsed.from, parsed.to));
            List.add(extras, #reconciliation(#depotStatementRecorded({ depot = x.depot; statement = hash; kind = #transactions; statementDate = parsed.to; from = parsed.from; to = parsed.to; reported = parsed.transactions.size(); matched = m.matched.size(); explained = m.explained.size(); breaks = m.breaks.size(); day = today })));
            for (iid in m.explained.vals()) {
              switch (SettlementCore.instruction(s.settlement, iid)) {
                case (?i) { let (_, isin) = switch (instructionDepotIsin(s, i)) { case (?v) v; case null ("", "") }; List.add(extras, #reconciliation(#breakExplainedByFail({ depot = x.depot; statement = hash; isin; reference = ""; instruction = iid; nominal = i.assetAmount; day = today }))) };
                case null {};
              };
            };
            for ((side, isin, ours, theirs, reference, instruction) in m.breaks.vals()) List.add(extras, #reconciliation(#depotBreak({ depot = x.depot; statement = hash; kind = #transaction; side; isin; ours; theirs; reference; instruction; day = today })));
          };
        };
        let all = List.toArray(extras);
        #ok({ event = ?all[0]; extra = Array.sliceToArray<T.Event>(all, 1, all.size()); journal = [] })
      };
      case (#resolveDepotBreak(x)) {
        switch (x.correction) { case (?c) { if (c >= s.height) return reconErr(#BadDocument({ reason = "the correction names a recorded block" })) }; case null {} };
        switch (ReconciliationCore.planResolveDepotBreak(s.reconciliation, x.break_, x.resolution, x.correction, today)) { case (#err(e)) reconErr(e); case (#ok(ev)) only(#reconciliation(ev)) }
      };
      case (#resolveCashBreak(x)) {
        switch (x.correction) { case (?c) { if (c >= s.height) return reconErr(#BadDocument({ reason = "the correction names a recorded block" })) }; case null {} };
        switch (ReconciliationCore.planResolveCashBreak(s.reconciliation, x.break_, x.resolution, x.correction, today)) { case (#err(e)) reconErr(e); case (#ok(ev)) only(#reconciliation(ev)) }
      };
      // ── treasury: Manticore's planners with the desk's context ──
      case (#setTreasuryPolicy(pol)) {
        for (code in TreasuryCore.accountsOf(pol).vals()) { switch (JCore.getAccount(js, code)) { case null return #err(#UnknownAccount({ role = "treasury policy"; account = code })); case (?_) {} } };
        treasuryPlan(TreasuryCore.planPolicy(pol))
      };
      case (#registerSecurity(x)) treasuryPlan(TreasuryCore.planRegisterSecurity(s.treasury, x.terms, today));
      case (#publishCurve(x)) {
        switch (TreasuryCore.planPublishCurve(s.treasury, x.curve)) { case (#err(e)) treasuryErr(e); case (#ok(null)) nothing(); case (#ok(?ev)) only(#treasury(ev)) }
      };
      case (#setTreasuryLimit(x)) {
        switch (Auth.requireOpenBook(a, x.limit.book)) { case (?e) return #err(e); case null {} };
        treasuryPlan(TreasuryCore.planSetLimit(x.limit, today))
      };
      case (#registerNostro(x)) {
        switch (JCore.getAccount(js, x.nostro.account)) { case null return #err(#UnknownAccount({ role = "nostro"; account = x.nostro.account })); case (?_) {} };
        switch (TreasuryCore.planRegisterNostro(s.treasury, x.nostro, today)) {
          case (#err(e)) treasuryErr(e);
          // the journal mark precedes the registration in the log, so a fresh fold indexes the journal to that height
          // before the nostro exists, exactly as the live contract did
          case (#ok(ev)) #ok({ event = ?#journalMark({ height = JCore.height(js) }); extra = [#treasury(ev)]; journal = [] });
        }
      };
      case (#captureDeal(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        switch (Auth.requireOpenBook(a, x.book)) { case (?e) return #err(e); case null {} };
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        // the counterparty limit with the call balances counted, before Manticore's own measure over its deals
        let (dealCcy, dealAmount) : (Text, Nat) = switch (treasuryKindTotals(s, x.kind)) { case (xs) { if (xs.size() > 0) xs[0] else ("", 0) } };
        let treasuryOnly = switch (TreasuryCore.limitOf(s.treasury, x.book, #counterpartyExposure, dealCcy, x.counterparty.name)) { case (?l) treasuryExposure(s, x.book, x.counterparty.name, dealCcy) + dealAmount > l; case null false };
        let combined = if (dealAmount == 0) [] else { switch (combinedLimitEvents(s, x.book, x.counterparty, dealCcy, dealAmount, treasuryOnly, s.height, x.approver, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs } };
        // the tree: what the deal adds under every node it falls under, and the breaches an approver accepted
        let tree = switch (treeEventsOf(s, command, today)) { case (#err(e)) return #err(e); case (#ok(xs)) xs };
        // the trader is whose act it is; the approver on the command is the desk head who accepted a breach
        // a security deal is held in the book's depot when the book declares one
        let depotAssigned : [T.Event] = switch (x.kind, CustodyCore.bookDepotOf(s.custody, x.book)) { case (#security(_), ?d) [#custody(#dealDepotAssigned({ deal = s.height; depot = d; day = today }))]; case (_) [] };
        switch (TreasuryCore.planCapture(s.treasury, s.height, x.book, x.counterparty, x.kind, x.reference, authority, today, x.approver, ctx, treasuryTerms(bb))) {
          case (#err(e)) treasuryErr(e);
          case (#ok(r)) #ok({ event = ?#treasury(r.ev); extra = Array.concat<T.Event>(Array.concat<T.Event>(Array.concat<T.Event>(Array.map<TT.TreasuryEvent, T.Event>(r.extras, func(e) { #treasury(e) }), combined), depotAssigned), tree); journal = [] });
        }
      };
      case (#confirmDeal(x)) {
        let r = switch (treasuryRow(s, x.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        let cp = treasuryCounterpartyOf(bb, x.deal);
        let fields : TT.ConfirmationFields = switch (x.fields, x.document) {
          case (?f, null) f;
          case (null, ?doc) {
            if (r.kind != 2 and r.kind != 3) return treasuryErr(#BadDocument({ reason = "an fxtr.014 confirms an FX deal; other kinds are confirmed by their fields" }));
            switch (TreasuryMessages.parseFxtr014(doc, r.currency, func(c : Text) : ?Nat8 { minorUnitsOf(js, c) })) { case (#ok(f)) f; case (#err(reason)) return treasuryErr(#BadDocument({ reason })) }
          };
          case (null, null) return treasuryErr(#BadDocument({ reason = "a confirmation carries its fields or the document" }));
          case (?_, ?_) return treasuryErr(#BadDocument({ reason = "a confirmation carries its fields or the document, not both" }));
        };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        treasuryPlan(TreasuryCore.planConfirm(s.treasury, x.deal, x.confirmation, fields, cp, kind, today))
      };
      case (#amendDeal(x)) {
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        treasuryPlan(TreasuryCore.planAmend(s.treasury, x.deal, x.kind, x.reason, today, ctx))
      };
      case (#cancelDeal(x)) treasuryPlan(TreasuryCore.planCancel(s.treasury, x.deal, x.reason, today));
      case (#settleDealLeg(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let r = switch (treasuryRow(s, x.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        switch (saleDepotCheck(s, r, kind, x.leg)) { case (?e) return #err(e); case null {} };
        // a leg in Tachyon's hands settles from Tachyon's receipt, never by hand
        switch (SettlementCore.openInstructionOf(s.settlement, x.deal, x.leg)) { case (?i) return settlementErr(#AlreadyInstructed({ deal = x.deal; instruction = i.id })); case null {} };
        switch (TreasuryCore.planSettleLeg(s.treasury, r, kind, x.leg, x.valueDate, ctx)) {
          case (#err(e)) treasuryErr(e);
          case (#ok(act)) treasuryPost(js, journalCaller, now, "treasury-settle", [authId, Nat.toText(x.deal), Nat.toText(x.leg)], act, x.postingDate, x.valueDate, x.period, x.narration);
        }
      };
      case (#markDeal(x)) {
        switch (Auth.requireFeature(a, T.FEATURE_TREASURY, s.height)) { case (?e) return #err(e); case null {} };
        let r = switch (treasuryRow(s, x.deal)) { case (#err(e)) return #err(e); case (#ok(r)) r };
        switch (requireNoOpenRun(s, r.book, x.valueDate)) { case (?e) return #err(e); case null {} };
        let kind = switch (treasuryKind(bb, r)) { case (#err(e)) return #err(e); case (#ok(k)) k };
        let ctx = switch (treasuryCtx(s)) { case (#err(e)) return #err(e); case (#ok(c)) c };
        switch (TreasuryCore.planMark(s.treasury, r, kind, x.valueDate, ctx)) {
          case (#err(e)) treasuryErr(e);
          case (#ok(null)) nothing();
          case (#ok(?act)) treasuryPost(js, journalCaller, now, "treasury-mark", [authId, Nat.toText(x.deal), Nat.toText(x.valueDate)], act, x.postingDate, x.valueDate, x.period, x.narration);
        }
      };
      case (#recordNostroStatement(x)) {
        let entries = switch (x.document) {
          case (?doc) {
            if (x.entries.size() > 0) return treasuryErr(#BadDocument({ reason = "the entries come from the document or from the command, not both" }));
            let ?nr = TreasuryCore.nostro(s.treasury, x.nostro) else return treasuryErr(#UnknownNostro({ nostro = x.nostro }));
            let mu : Nat8 = switch (minorUnitsOf(js, nr.currency)) { case (?m) m; case null 2 };
            switch (TreasuryMessages.parseCamt053(doc, nr.currency, mu)) { case (#ok(parsed)) parsed.entries; case (#err(reason)) return treasuryErr(#BadDocument({ reason })) }
          };
          case null x.entries;
        };
        // the entries a notification already matched are the notification's: dropped here, so the statement finds
        // those legs marked and reconciles the rest
        let fresh = Array.filter<TT.StatementEntry>(entries, func(e) { ReconciliationCore.notifiedPosting(s.reconciliation, x.nostro, e) == null });
        let notified : [T.Event] = if (fresh.size() == entries.size()) [] else [#reconciliation(#statementEntriesNotified({ nostro = x.nostro; statement = x.statement; entries = entries.size() - fresh.size(); day = today }))];
        switch (TreasuryCore.planRecordStatement(s.treasury, x.nostro, x.statement, x.from, x.to, fresh, today)) {
          case (#err(e)) treasuryErr(e);
          // the mark first: the matches and the breaks name postings the fold must have indexed before it applies them
          case (#ok(r)) #ok({ event = ?#journalMark({ height = JCore.height(js) }); extra = Array.concat<T.Event>(Array.concat<T.Event>(notified, [#treasury(r.ev)]), Array.map<TT.TreasuryEvent, T.Event>(r.breaks, func(e) { #treasury(e) })); journal = [] });
        }
      };
      case (#resolveNostroBreak(x)) {
        switch (x.correction) { case (?c) { switch (JCore.getAccount(js, c.account)) { case null return #err(#UnknownAccount({ role = "nostro correction"; account = c.account })); case (?_) {} } }; case null {} };
        switch (TreasuryCore.planResolveBreak(s.treasury, x.breakId, x.resolution, x.correction, today)) {
          case (#err(e)) treasuryErr(e);
          case (#ok(act)) treasuryPost(js, journalCaller, now, "nostro-resolve", [authId, Nat.toText(x.breakId)], act, x.postingDate, x.valueDate, x.period, x.narration);
        }
      };
    }
  };

  // ═══════════════════════════════════════════════════════
  //  THE MAKER-CHECKER PATHS
  // ═══════════════════════════════════════════════════════

  type Refusal = { error : T.Error; record : Bool };
  public type ProposeOutcome = { event : T.Event; trailer : Blob; permission : T.PermissionId };

  func dayOf(js : JCore.State, now : Nat64, c : T.Command) : Nat { switch (commandDay(c)) { case (?d) d; case null JCore.effectiveToday(js, now) } };

  /// A maker proposes: the maker holds the command's own permission, the command is dual-authorised, and it
  /// validates now against both states. The body travels in the trailer under the recorded encoding.
  public func prepareProposal(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, caller : Principal, now : Nat64, command : T.Command, justification : Text) : Result.Result<ProposeOutcome, Refusal> {
    let perm = Auth.commandPermissionRecord(command);
    if (not MC.textFits(justification, AT.MAX_JUSTIFICATION_BYTES)) return #err({ error = #InvalidPolicy({ reason = "justification exceeds the bound" }); record = false });
    switch (Auth.authorise(s.authority, caller, operationOf(s, command), dayOf(js, now, command))) {
      case (#err(e)) return #err({ error = e; record = Auth.recordableRefusal(s.authority, caller) });
      case (#ok(_)) {};
    };
    let ?policy = Auth.policyFor(s.authority, perm.id) else return #err({ error = #InvalidPolicy({ reason = "permission " # perm.id # " is single-authority; use perform" }); record = false });
    switch (planCommand(s, bb, js, journalCaller, now, caller, Nat.toText(s.height), command)) { case (#err(e)) return #err({ error = e; record = false }); case (#ok(_)) {} };
    let ?bound = Cmd.bind(Can.registry(), command) else return #err({ error = #UnrepresentableCommand({ family = Cat.commandName(command); version = Can.COMMAND_ENCODING }); record = false });
    let ?trailer = Cmd.trailerWithBody(Can.registry(), bound.commandEncoding, command) else return #err({ error = #UnrepresentableCommand({ family = Cat.commandName(command); version = Can.COMMAND_ENCODING }); record = false });
    let proposed : Cmd.Proposed = {
      permission = perm.id; partition = commandBookOf(s, command); maker = caller; required = policy.required; eligibleRole = policy.eligibleRole;
      expiresAt = now + Nat64.fromNat(policy.ttlSeconds) * 1_000_000_000; justification; commandHash = bound.commandHash; commandEncoding = bound.commandEncoding;
    };
    #ok({ event = #commandProposed(proposed); trailer; permission = perm.id })
  };

  public type ApproveOutcome = { approval : T.Event; execute : ?{ command : T.Command; commandHash : Blob; permission : T.PermissionId; maker : Principal; proposal : Nat } };

  /// A checker approves: the hash the checker sees is re-derived from the body under the recorded version; when
  /// the approval completes the policy the maker's authority is checked again and the command re-validated.
  public func prepareApprove(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, caller : Principal, now : Nat64, index : Nat) : Result.Result<ApproveOutcome, Refusal> {
    let ?e = Auth.proposalEntry(s.authority, bb, index) else return #err({ error = #UnknownProposal({ index }); record = false });
    switch (MC.checkApprover(e, caller, Auth.holdsRole(s.authority, caller, e.eligibleRole), now)) {
      case (?err) return #err({ error = T.ofAuth(err); record = Auth.recordableRefusal(s.authority, caller) });
      case null {};
    };
    let ?command = Auth.bodyOf(bb, index) else return #err({ error = #CommandHashMismatch({ recorded = e.commandHash; recomputed = Blob.fromArray([]) }); record = true });
    let ?recomputed = Cmd.rederive(Can.registry(), { permission = e.permission; partition = e.partition; maker = e.maker; required = e.required; eligibleRole = e.eligibleRole; expiresAt = e.expiresAt; justification = e.justification; commandHash = e.commandHash; commandEncoding = e.commandEncoding }, command) else return #err({ error = #CommandHashMismatch({ recorded = e.commandHash; recomputed = Blob.fromArray([]) }); record = true });
    if (not MC.hashesAgree(e.commandHash, recomputed)) return #err({ error = #CommandHashMismatch({ recorded = e.commandHash; recomputed }); record = true });
    let approval : T.Event = #commandApproved({ proposal = index; checker = caller; commandHash = recomputed });
    if (not MC.completesWith(e)) return #ok({ approval; execute = null });
    switch (Auth.authorise(s.authority, e.maker, operationOf(s, command), dayOf(js, now, command))) {
      case (#err(err)) return #err({ error = err; record = true });
      case (#ok(_)) {};
    };
    switch (planCommand(s, bb, js, journalCaller, now, e.maker, Nat.toText(index), command)) { case (#err(err)) return #err({ error = err; record = false }); case (#ok(_)) {} };
    #ok({ approval; execute = ?{ command; commandHash = recomputed; permission = e.permission; maker = e.maker; proposal = index } })
  };

  public func prepareReject(s : State, bb : Blocks, caller : Principal, now : Nat64, index : Nat, reason : Text) : Result.Result<T.Event, Refusal> {
    let ?e = Auth.proposalEntry(s.authority, bb, index) else return #err({ error = #UnknownProposal({ index }); record = false });
    if (not MC.isAwaiting(e)) return #err({ error = #ProposalNotAwaiting({ index }); record = false });
    if (not MC.textFits(reason, AT.MAX_JUSTIFICATION_BYTES)) return #err({ error = #InvalidPolicy({ reason = "reason exceeds the bound" }); record = false });
    if (not Auth.holdsRole(s.authority, caller, e.eligibleRole)) return #err({ error = #NotEligibleChecker({ checker = caller; eligibleRole = e.eligibleRole }); record = Auth.recordableRefusal(s.authority, caller) });
    if (MC.isExpired(e, now)) return #err({ error = #ProposalExpired({ index; expiresAt = e.expiresAt }); record = true });
    #ok(#commandRejected({ proposal = index; checker = caller; reason }))
  };

  /// A single-authority command: the caller holds the permission and the permission has no dual policy.
  public func preparePerform(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, caller : Principal, now : Nat64, command : T.Command) : Result.Result<{ permission : T.PermissionId }, Refusal> {
    let perm = Auth.commandPermissionRecord(command);
    switch (Auth.requireUsable(s.authority, perm)) { case (?e) return #err({ error = e; record = false }); case null {} };
    switch (Auth.policyFor(s.authority, perm.id)) { case (?p) return #err({ error = #RequiresDualAuthorisation({ permission = perm.id; required = p.required }); record = false }); case null {} };
    switch (Auth.authorise(s.authority, caller, operationOf(s, command), dayOf(js, now, command))) {
      case (#err(e)) return #err({ error = e; record = Auth.recordableRefusal(s.authority, caller) });
      case (#ok(_)) {};
    };
    switch (planCommand(s, bb, js, journalCaller, now, caller, Nat.toText(s.height), command)) { case (#err(e)) return #err({ error = e; record = false }); case (#ok(_)) {} };
    #ok({ permission = perm.id })
  };

  /// The emergency path: a distinct permission no operational role carries, a witness who could have approved,
  /// and a review that stays open until a disposition is recorded.
  public func prepareOverride(s : State, bb : Blocks, js : JCore.State, journalCaller : Principal, caller : Principal, now : Nat64, command : T.Command, witness : Principal, justification : Text) : Result.Result<{ event : T.Event; trailer : Blob; permission : T.PermissionId }, Refusal> {
    let perm = Auth.commandPermissionRecord(command);
    if (not MC.textFits(justification, AT.MAX_JUSTIFICATION_BYTES)) return #err({ error = #InvalidPolicy({ reason = "justification exceeds the bound" }); record = false });
    if (Text.encodeUtf8(justification).size() == 0) return #err({ error = #InvalidPolicy({ reason = "an override requires a justification" }); record = false });
    let eligible = switch (Auth.policyFor(s.authority, perm.id)) {
      case (?p) Auth.holdsRole(s.authority, witness, p.eligibleRole);
      case null return #err({ error = #InvalidPolicy({ reason = "permission " # perm.id # " has no dual policy; it needs no override" }); record = false });
    };
    switch (MC.checkOverride(caller, if (Principal.isAnonymous(witness)) null else ?witness, eligible)) {
      case (?e) return #err({ error = T.ofAuth(e); record = Auth.recordableRefusal(s.authority, caller) });
      case null {};
    };
    switch (Auth.authorise(s.authority, caller, { permission = "command.breakGlass"; partition = null; totals = [] }, 0)) {
      case (#err(e)) return #err({ error = e; record = Auth.recordableRefusal(s.authority, caller) });
      case (#ok(_)) {};
    };
    switch (Auth.authorise(s.authority, caller, operationOf(s, command), dayOf(js, now, command))) {
      case (#err(e)) return #err({ error = e; record = Auth.recordableRefusal(s.authority, caller) });
      case (#ok(_)) {};
    };
    switch (planCommand(s, bb, js, journalCaller, now, caller, Nat.toText(s.height), command)) { case (#err(e)) return #err({ error = e; record = false }); case (#ok(_)) {} };
    let ?bound = Cmd.bind(Can.registry(), command) else return #err({ error = #UnrepresentableCommand({ family = Cat.commandName(command); version = Can.COMMAND_ENCODING }); record = false });
    let ?trailer = Cmd.trailerWithBody(Can.registry(), bound.commandEncoding, command) else return #err({ error = #UnrepresentableCommand({ family = Cat.commandName(command); version = Can.COMMAND_ENCODING }); record = false });
    #ok({ event = #emergencyOverride({ commandHash = bound.commandHash; commandEncoding = bound.commandEncoding; actor_ = caller; witness; justification }); trailer; permission = perm.id })
  };

  public func prepareReviewOverride(s : State, bb : Blocks, caller : Principal, index : Nat, disposition : Text) : Res<T.Event> {
    let ?o = Auth.overrideView(s.authority, bb, index) else return #err(#UnknownOverride({ index }));
    if (o.reviewedBy != null) return #err(#OverrideAlreadyReviewed({ index }));
    if (Principal.equal(o.actor_, caller)) return #err(#SelfApproval({ maker = o.actor_ }));
    if (not MC.textFits(disposition, AT.MAX_JUSTIFICATION_BYTES)) return #err(#InvalidPolicy({ reason = "disposition exceeds the bound" }));
    if (Text.encodeUtf8(disposition).size() == 0) return #err(#InvalidPolicy({ reason = "a review requires a disposition" }));
    #ok(#overrideReviewed({ override_ = index; reviewer = caller; disposition }))
  };

  /// What an execution consumed of its authority's daily limits: one event per currency, recorded after the act.
  public func consumptionEvents(s : State, js : JCore.State, now : Nat64, authority : Principal, command : T.Command) : [T.Event] {
    let day = dayOf(js, now, command);
    Array.map<(Text, Nat), T.Event>(commandTotals(s, command), func((currency, amount)) { #dailyConsumed({ subject = authority; currency; day; amount }) })
  };

  // ═══════════════════════════════════════════════════════
  //  THE END OF DAY
  // ═══════════════════════════════════════════════════════

  public type Recorder = { desk : T.Event -> Nat; journal : JT.Event -> Nat };
  type ChunkAcc = { recorder : Recorder; blocks : List.List<Nat>; postings : List.List<Nat>; var posted : Nat; var examined : Nat; var zeroMovement : Nat; failures : List.List<Batch.Failure> };
  func newAcc(recorder : Recorder) : ChunkAcc { { recorder; blocks = List.empty<Nat>(); postings = List.empty<Nat>(); var posted = 0; var examined = 0; var zeroMovement = 0; failures = List.empty<Batch.Failure>() } };
  func record(acc : ChunkAcc, ev : T.Event) { List.add(acc.blocks, acc.recorder.desk(ev)) };
  func fail(acc : ChunkAcc, item : Nat, job : Batch.Job, book : Text, entity : Nat, reason : Text) {
    if (List.size(acc.failures) < Batch.MAX_FAILURES) List.add(acc.failures, { seq = item; job = job.name; scope = book; entity; reason; attempts = 1 });
  };
  func legsEqual(a : [JT.Leg], b : [JT.Leg]) : Bool {
    if (a.size() != b.size()) return false;
    var i = 0;
    while (i < a.size()) {
      let x = a[i]; let y = b[i];
      if (not Text.equal(x.account, y.account) or x.subledger != y.subledger or x.side != y.side or not Text.equal(x.currency, y.currency) or x.amount != y.amount) return false;
      i += 1;
    };
    true
  };
  /// One posting on the batch's behalf. A `#duplicate` from the journal is not believed until the named block's
  /// legs are the legs this job intended.
  func batchPost(js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, input : JT.PostingInput) : ?Text {
    switch (JCore.preparePost(js, journalCaller, now, input)) {
      case (#err(e)) ?debug_show (e);
      case (#ok(#event(ev))) { List.add(acc.postings, acc.recorder.journal(ev)); acc.posted += 1; null };
      case (#ok(#duplicate(idx))) {
        switch (JCore.postingView(js, jb, idx)) {
          case null ?("the journal named block " # Nat.toText(idx) # " as a duplicate and it cannot be read");
          case (?v) { if (not legsEqual(v.record.legs, input.legs)) ?("the duplicate at block " # Nat.toText(idx) # " does not carry the legs this job intended") else { List.add(acc.postings, idx); null } };
        }
      };
    }
  };
  public func alertFor(s : State, finding : MT.Finding, source : AlT.Source) : ?T.Event {
    switch (AlertCore.known(s.alerts, finding)) { case (?_) null; case null ?#alert(#alertOpened({ finding; source })) }
  };
  func periodForDay(js : JCore.State, day : Nat) : ?Text {
    for (p in JCore.listPeriods(js).vals()) { if (p.status == #open and day >= p.start and day <= p.end) return ?p.id };
    null
  };

  /// The treasury job (Manticore's end-of-day treasury job, unchanged in order): for every open deal of the book, the coupon
  /// falling due, the day's accrual, the mark against the day's curves and spot, then every leg due on or before
  /// the day; then the breaks aged past the policy's threshold and the confirmations overdue, each an alert. A
  /// missing rate or curve fails the deal's item and nothing else.
  func jobTreasury(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, item : Batch.PlanItem, index : Nat, day : Nat, period : Text, book : Text, onlyDeal : ?Nat) {
    if (TreasuryCore.policy(s.treasury) == null) { fail(acc, index, item.job, book, 0, "no treasury policy"); return };
    let ctx = switch (treasuryCtx(s)) { case (#err(e)) { fail(acc, index, item.job, book, 0, debug_show e); return }; case (#ok(c)) c };
    func post(kind : Text, id : Nat, part : Text, act : TreasuryCore.Act, narration : Text) : Bool {
      if (act.legs.size() > 0) {
        if (not Posting.balances(act.legs)) { fail(acc, index, item.job, book, id, kind # ": the legs do not balance"); return false };
        let input : JT.PostingInput = { idempotencyKey = Posting.key(kind, [Nat.toText(id), part, Nat.toText(day)]); postingDate = day; valueDate = day; period; legs = act.legs; sourceRef = { kind; id = Nat.toText(id) # "/" # part # "/" # Nat.toText(day) }; narration; correctionOf = null };
        switch (batchPost(js, jb, journalCaller, now, acc, input)) { case (?why) { fail(acc, index, item.job, book, id, why); return false }; case null {} };
      };
      record(acc, #treasury(act.ev));
      for (e in act.extras.vals()) record(acc, #treasury(e));
      true
    };
    for (r0 in TreasuryCore.openInBook(s.treasury, book).vals()) {
      let mine = switch (onlyDeal) { case null true; case (?e) e == r0.id };
      if (not mine) continue;
      acc.examined += 1;
      let ?kind = treasuryKindOf(bb, r0) else { fail(acc, index, item.job, book, r0.id, "the deal's terms are not in the log"); continue };
      func current() : ?TreasuryCore.DealRow { TreasuryCore.row(s.treasury, r0.id) };
      // the instrument's own coupon, unless an announced coupon covers the date: then the announced coupon's claim
      // stands in the coupon's own slot, before the accrual, so the lot's accrual is cleared into the claim and
      // never reversed
      switch (current()) {
        case (?r) {
          if (r.kind == 4 and CustodyCore.announcedCouponCovers(s.custody, r.isin, day)) {
            switch (claimDueFor(s, r, day), TreasuryCore.policy(s.treasury), TreasuryCore.security(s.treasury, r.isin)) {
              case (?(ar, e), ?p, ?sec) {
                switch (CustodyCore.planClaim(p, ar, e, r, sec.currency, day)) {
                  case (#ok(claim)) {
                    let te = switch (claim.treasury) { case (?te) te; case null #couponPaid({ deal = r.id; amount = e.amount; day }) };
                    let a : TreasuryCore.Act = { ev = te; legs = claim.legs; extras = [] };
                    if (post("corporate-action", ar.id, "claim/" # Nat.toText(r.id), a, "corporate action " # Nat.toText(ar.id) # " claim")) record(acc, #custody(claim.ev));
                  };
                  case (#err(err)) fail(acc, index, item.job, book, r.id, debug_show err);
                };
              };
              case (_) {};
            };
          } else {
            switch (TreasuryCore.planCoupon(s.treasury, r, kind, day)) { case (#ok(?a)) ignore post("treasury-coupon", r.id, "c", a, "coupon"); case (#ok(null)) {}; case (#err(e)) fail(acc, index, item.job, book, r.id, debug_show e) };
          };
        };
        case null {};
      };
      switch (current()) {
        case (?r) { switch (TreasuryCore.planAccrue(s.treasury, r, kind, day)) { case (#ok(?a)) ignore post("treasury-accrual", r.id, "a", a, "accrual to day " # Nat.toText(day)); case (#ok(null)) {}; case (#err(e)) fail(acc, index, item.job, book, r.id, debug_show e) } };
        case null {};
      };
      switch (current()) {
        case (?r) {
          // the theta first, from the previous day's data at the day against the mark as it stands, then the mark
          switch (Valuation.markWith(s.treasury, r, kind, day, dataOn(s, day - 1))) {
            case (#ok(?withPrevious)) { let standing = if (r.kind == 4) r.fvPosted else r.markPosted; record(acc, #valuation(#thetaRecorded({ deal = r.id; day; theta = withPrevious - standing }))) };
            case (_) {};
          };
          switch (TreasuryCore.planMark(s.treasury, r, kind, day, ctx)) { case (#ok(?a)) ignore post("treasury-mark", r.id, "m", a, "valuation at day " # Nat.toText(day)); case (#ok(null)) {}; case (#err(e)) fail(acc, index, item.job, book, r.id, debug_show e) };
        };
        case null {};
      };
      var leg = 0;
      label legs while (leg < r0.legs) {
        let ?r = current() else break legs;
        if (not TreasuryCore.isOpen(r)) break legs;
        if (TreasuryCore.legSettled(r, leg)) { leg += 1; continue legs };
        let secMaturity = switch (TreasuryCore.security(s.treasury, r.isin)) { case (?x) x.maturity; case null 0 };
        switch (TreasuryCore.legDue(kind, leg, secMaturity)) {
          case (?due) {
            if (due > day) break legs;
            // a leg in Tachyon's hands settles from Tachyon's receipt: the run leaves it and everything after it
            if (SettlementCore.openInstructionOf(s.settlement, r.id, leg) != null) break legs;
            switch (saleDepotCheck(s, r, kind, leg)) { case (?e) { fail(acc, index, item.job, book, r.id, debug_show e); break legs }; case null {} };
            switch (TreasuryCore.planSettleLeg(s.treasury, r, kind, leg, day, ctx)) {
              case (#ok(a)) { if (not post("treasury-settle", r.id, Nat.toText(leg), a, "leg " # Nat.toText(leg) # " settled")) break legs };
              case (#err(e)) { fail(acc, index, item.job, book, r.id, debug_show e); break legs };
            };
          };
          case null break legs;
        };
        leg += 1;
      };
    };
    if (onlyDeal == null) {
      for (ev in TreasuryCore.agedBreaks(s.treasury, day).vals()) {
        record(acc, #treasury(ev));
        switch (ev) {
          case (#breakAged(b)) { switch (alertFor(s, { rule = "nostro.break.aged"; version = 1; account = b.breakId; day; postings = []; detail = "nostro break " # Nat.toText(b.breakId) # " open for " # Nat.toText(b.ageDays) # " days" }, #endOfDay)) { case (?a) record(acc, a); case null {} } };
          case (_) {};
        };
      };
      for (ev in TreasuryCore.overdueConfirmations(s.treasury, day).vals()) {
        record(acc, #treasury(ev));
        switch (ev) {
          case (#confirmationOverdue(c)) { switch (alertFor(s, { rule = "treasury.confirmation.overdue"; version = 1; account = c.deal; day; postings = []; detail = "deal " # Nat.toText(c.deal) # " unconfirmed for " # Nat.toText(c.ageDays) # " days" }, #endOfDay)) { case (?a) record(acc, a); case null {} } };
          case (_) {};
        };
      };
    };
  };

  /// The call-money job: for every open call of the book, what falls due on the day, one act at a time and each
  /// folded before the next is asked for: the funding, the accrual to the day, the interest settlement, the
  /// repayment. A failure names the call and stops that call's day, nothing else.
  func jobCalls(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, item : Batch.PlanItem, index : Nat, day : Nat, period : Text, book : Text, onlyCall : ?Nat) {
    let ?p = TreasuryCore.policy(s.treasury) else { fail(acc, index, item.job, book, 0, "no treasury policy"); return };
    for (r0 in CallCore.openInBook(s.calls, book).vals()) {
      let mine = switch (onlyCall) { case null true; case (?e) e == r0.id };
      if (not mine) continue;
      acc.examined += 1;
      let (_, _, cash) = callOpeningOf(bb, r0.id);
      let ?cs = cash else { fail(acc, index, item.job, book, r0.id, "the call's terms are not in the log"); continue };
      var steps = 0;
      label acts while (steps < 8) {
        steps += 1;
        let ?r = CallCore.row(s.calls, r0.id) else break acts;
        switch (CallCore.planDue(s.calls, r0.id, day)) {
          case (#ok(null)) break acts;
          case (#err(e)) { fail(acc, index, item.job, book, r0.id, debug_show e); break acts };
          case (#ok(?ev)) {
            let legs = CallCore.legsOf(p, r, cs, ev);
            if (legs.size() > 0) {
              if (not Posting.balances(legs)) { fail(acc, index, item.job, book, r0.id, "call: the legs do not balance"); break acts };
              let kind = "call-" # callPart(ev);
              let input : JT.PostingInput = { idempotencyKey = Posting.key(kind, [Nat.toText(r0.id), Nat.toText(day)]); postingDate = day; valueDate = day; period; legs; sourceRef = { kind; id = Nat.toText(r0.id) # "/" # Nat.toText(day) }; narration = kind # " " # Nat.toText(day); correctionOf = null };
              switch (batchPost(js, jb, journalCaller, now, acc, input)) { case (?why) { fail(acc, index, item.job, book, r0.id, why); break acts }; case null {} };
            };
            record(acc, #call(ev));
          };
        };
      };
    };
  };

  /// The custody job: every corporate action due on the day, one act at a time, each folded before the next is
  /// asked for: the entitlement at the record date, a lot's payment at the payment date, the action closed as paid.
  func jobCustody(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, item : Batch.PlanItem, index : Nat, day : Nat, period : Text, book : Text, onlyAction : ?Nat) {
    let due = Array.concat<CustodyCore.ActionRow>(CustodyCore.actionsInState(s.custody, #announced), CustodyCore.actionsInState(s.custody, #entitled));
    for (r0 in due.vals()) {
      let mine = switch (onlyAction) { case null true; case (?e) e == r0.id };
      if (not mine) continue;
      acc.examined += 1;
      var steps = 0;
      label acts while (steps < 256) {
        steps += 1;
        switch (nextActionAct(s, bb, r0.id, day)) {
          case (#ok(null)) break acts;
          case (#err(e)) { fail(acc, index, item.job, book, r0.id, debug_show e); break acts };
          case (#ok(?act)) {
            if (act.legs.size() > 0) {
              if (not Posting.balances(act.legs)) { fail(acc, index, item.job, book, r0.id, "corporate action: the legs do not balance"); break acts };
              let lotText = switch (act.lot) { case (?l) Nat.toText(l); case null "" };
              let input : JT.PostingInput = { idempotencyKey = Posting.key("corporate-action", [Nat.toText(r0.id), act.step # "/" # lotText, Nat.toText(day)]); postingDate = day; valueDate = day; period; legs = act.legs; sourceRef = { kind = "corporate-action"; id = Nat.toText(r0.id) # "/" # act.step # "/" # lotText # "/" # Nat.toText(day) }; narration = "corporate action " # Nat.toText(r0.id) # " " # act.step; correctionOf = null };
              switch (batchPost(js, jb, journalCaller, now, acc, input)) { case (?why) { fail(acc, index, item.job, book, r0.id, why); break acts }; case null {} };
            };
            for (ev in act.events.vals()) record(acc, ev);
          };
        };
      };
    };
    if (onlyAction == null) ageReconciliationBreaks(s, acc, day);
  };

  /// The depot and cash breaks past the reconciliation policy's age, each alerted once by the custody job.
  func ageReconciliationBreaks(s : State, acc : ChunkAcc, day : Nat) {
    let ?p = ReconciliationCore.policy(s.reconciliation) else return;
    for (ev in ReconciliationCore.agedBreaks(s.reconciliation, day, p.breakAgeAlertDays).vals()) {
      record(acc, #reconciliation(ev));
      switch (ev) {
        case (#depotBreakAged(b)) { switch (alertFor(s, { rule = "depot.break.aged"; version = 1; account = b.break_; day; postings = []; detail = "depot break " # Nat.toText(b.break_) # " open for " # Nat.toText(b.ageDays) # " days" }, #endOfDay)) { case (?a) record(acc, a); case null {} } };
        case (_) {};
      };
    };
    for (b in ReconciliationCore.agedCashBreaks(s.reconciliation, day, p.breakAgeAlertDays).vals()) {
      record(acc, #reconciliation(#cashBreakAged({ break_ = b.id; ageDays = day - b.openedDay; day })));
      switch (alertFor(s, { rule = "cash.break.aged"; version = 1; account = b.id; day; postings = []; detail = "cash break " # Nat.toText(b.id) # " in " # b.currency # " open for " # Nat.toText(day - b.openedDay) # " days" }, #endOfDay)) { case (?a) record(acc, a); case null {} };
    };
  };

  /// The settlement job: the cycle of the day closed. Every instruction of the cycle still open is failed with the
  /// cause named and, within the venue's recycle limit, recycled into the next business day's cycle (opened by the
  /// recycling when it does not exist, with the closing cycle's market and price source); past the limit it waits
  /// for a decision. The desk's escrow on a failed trade is reclaimed by the driver once Tachyon aborts it.
  func jobSettlement(s : State, js : JCore.State, acc : ChunkAcc, item : Batch.PlanItem, index : Nat, day : Nat, book : Text, onlyInstruction : ?Nat) {
    let ?c = SettlementCore.cycle(s.settlement, day) else return;
    if (c.state != #open) return;
    let ?venue = SettlementCore.venue(s.settlement) else { fail(acc, index, item.job, book, 0, "no settlement venue"); return };
    let nextDay = nextBusinessDay(js)(day + 1);
    var settled = 0; var failed = 0; var pending = 0;
    for (i in SettlementCore.instructionsOfCycle(s.settlement, day).vals()) {
      let mine = switch (onlyInstruction) { case null true; case (?x) x == i.id };
      switch (i.state) {
        case (#settled) settled += 1;
        case (#boughtIn or #cancelled) {};
        case (#failed) { if (i.cycle == day) failed += 1 };
        case (_) {
          if (not mine) { pending += 1; continue };
          acc.examined += 1;
          let fails = i.fails + 1;
          record(acc, #settlement(#failed({ instruction = i.id; cause = "not settled by the close of cycle " # Nat.toText(day) # " (" # ST.stateText(i.state) # ")"; fails; day })));
          failed += 1;
          if (fails <= venue.recycleLimit) {
            if (SettlementCore.cycle(s.settlement, nextDay) == null) record(acc, #settlement(#cycleOpened({ cycle = { businessDate = nextDay; market = c.market; priceSource = c.priceSource }; day })));
            record(acc, #settlement(#recycled({ instruction = i.id; cycle = nextDay; fails; day })));
          };
        };
      };
    };
    if (onlyInstruction == null) record(acc, #settlement(#cycleClosed({ businessDate = day; settled; failed; pending; day })));
  };

  /// The financing job: for every open repo and loan of the book, the start on its day (when no instruction has
  /// it), the accrual to the day, the collateral marked at the day's price with the margin call it raises, the
  /// unmet call past the grace alerted, the term repo closed at maturity and the loan returned on its day.
  func jobFinancing(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, item : Batch.PlanItem, index : Nat, day : Nat, period : Text, book : Text, only : ?Nat) {
    let ?p = FinancingCore.policy(s.financing) else return;
    let nextDay = nextBusinessDay(js)(day + 1);
    func postPlan(kind : Text, id : Nat, plan : Plan) : Bool {
      switch (plan.event) { case (?ev) record(acc, ev); case null {} };
      for (ev in plan.extra.vals()) record(acc, ev);
      true
    };
    func postEvent(kind : Text, id : Nat, part : Text, ev : FT.Event, r : ?FinancingCore.RepoRow, l : ?FinancingCore.LoanRow, cash : TT.CashAccount, custody : [T.Event]) : Bool {
      let legs = FinancingCore.legsOf(p, cash, r, l, ev);
      if (legs.size() > 0) {
        if (not Posting.balances(legs)) { fail(acc, index, item.job, book, id, kind # ": the legs do not balance"); return false };
        let input : JT.PostingInput = { idempotencyKey = Posting.key(kind, [Nat.toText(id), part, Nat.toText(day)]); postingDate = day; valueDate = day; period; legs; sourceRef = { kind; id = Nat.toText(id) # "/" # part # "/" # Nat.toText(day) }; narration = kind # " " # Nat.toText(id); correctionOf = null };
        switch (batchPost(js, jb, journalCaller, now, acc, input)) { case (?why) { fail(acc, index, item.job, book, id, why); return false }; case null {} };
      };
      record(acc, #financing(ev));
      for (ev in custody.vals()) record(acc, ev);
      true
    };
    for (r0 in FinancingCore.openReposInBook(s.financing, book).vals()) {
      let mine = switch (only) { case null true; case (?e) e == r0.id };
      if (not mine) continue;
      acc.examined += 1;
      let (_, _, cashOpt) = repoOpeningOf(bb, r0.id);
      let ?cash = cashOpt else { fail(acc, index, item.job, book, r0.id, "the repo's terms are not in the log"); continue };
      // the start, by the run, when its day has come and Tachyon does not have it
      switch (FinancingCore.repo(s.financing, r0.id)) {
        case (?r) {
          if (r.state == #open and day >= r.start and SettlementCore.openInstructionOf(s.settlement, r.id, 0) == null) {
            switch (repoStartCustody(s, r, day)) {
              case (#err(e)) { fail(acc, index, item.job, book, r.id, debug_show e); continue };
              case (#ok((lots, custody))) { if (not postEvent("repo-start", r.id, "s", #repoStarted({ repo = r.id; lots; day }), ?r, null, cash, custody)) continue };
            };
          };
        };
        case null continue;
      };
      // the accrual
      switch (FinancingCore.planRepoDue(s.financing, r0.id, day)) {
        case (#ok(?ev)) { switch (FinancingCore.repo(s.financing, r0.id)) { case (?r) { if (not postEvent("repo-accrual", r.id, "a", ev, ?r, null, cash, [])) continue }; case null {} } };
        case (#ok(null)) {};
        case (#err(e)) { fail(acc, index, item.job, book, r0.id, debug_show e); continue };
      };
      // the mark, the margin call, the unmet call
      switch (FinancingCore.repo(s.financing, r0.id)) {
        case (?r) {
          // the mark, unless the repo closes today: the close returns every margin in full
          if (r.state == #started and not (r.maturity != 0 and day >= r.maturity)) {
            switch (priceOf(s, r.isin, day)) {
              case (?price) { for (ev in FinancingCore.planMark(s.financing, r, price, day, nextDay).vals()) record(acc, #financing(ev)) };
              case null { fail(acc, index, item.job, book, r.id, "no price for " # r.isin # " on day " # Nat.toText(day)); continue };
            };
            if (r.marginCalled > 0 and day >= r.marginDue + p.marginGraceDays) {
              switch (alertFor(s, { rule = "repo.margin.unmet"; version = 1; account = r.id; day; postings = []; detail = "repo " # Nat.toText(r.id) # ": margin call of " # Nat.toText(r.marginCalled) # " due " # Nat.toText(r.marginDue) # " unmet" }, #endOfDay)) { case (?a) record(acc, a); case null {} };
            };
          };
        };
        case null {};
      };
      // the close at maturity, by the run, when Tachyon does not have it
      switch (FinancingCore.repo(s.financing, r0.id)) {
        case (?r) {
          if (r.state == #started and r.maturity != 0 and day >= r.maturity and SettlementCore.openInstructionOf(s.settlement, r.id, 1) == null) {
            switch (FinancingCore.planClose(s.financing, r.id, day)) {
              case (#ok(ev)) ignore postEvent("repo-close", r.id, "c", ev, ?r, null, cash, repoCloseCustody(s, r, day));
              case (#err(e)) fail(acc, index, item.job, book, r.id, debug_show e);
            };
          };
        };
        case null {};
      };
    };
    for (l0 in FinancingCore.openLoansInBook(s.financing, book).vals()) {
      let mine = switch (only) { case null true; case (?e) e == l0.id };
      if (not mine) continue;
      acc.examined += 1;
      let (_, _, cashOpt) = loanOpeningOf(bb, l0.id);
      let ?cash = cashOpt else { fail(acc, index, item.job, book, l0.id, "the loan's terms are not in the log"); continue };
      let reference = "loan/" # Nat.toText(l0.id);
      switch (FinancingCore.loan(s.financing, l0.id)) {
        case (?l) {
          if (l.state == #open and day >= l.start and SettlementCore.openInstructionOf(s.settlement, l.id, 0) == null) {
            switch (FinancingCore.allocate(CustodyCore.availableIn(s.custody, l.depot, l.isin), l.nominal, l.isin)) {
              case (#err(e)) { fail(acc, index, item.job, book, l.id, debug_show e); continue };
              case (#ok(lots)) {
                let custody = List.empty<T.Event>();
                for ((lot, n) in lots.vals()) List.add(custody, #custody(#lent({ lot; depot = l.depot; nominal = n; reference; day })));
                if (l.collateralNominal > 0) List.add(custody, #custody(#collateralReceived({ isin = l.collateralIsin; depot = l.depot; nominal = l.collateralNominal; reference; day })));
                if (not postEvent("loan-start", l.id, "s", #loanStarted({ loan = l.id; lots; day }), null, ?l, cash, List.toArray(custody))) continue;
              };
            };
          };
        };
        case null continue;
      };
      switch (FinancingCore.planLoanDue(s.financing, l0.id, day)) {
        case (#ok(?ev)) { switch (FinancingCore.loan(s.financing, l0.id)) { case (?l) { if (not postEvent("loan-accrual", l.id, "a", ev, null, ?l, cash, [])) continue }; case null {} } };
        case (#ok(null)) {};
        case (#err(e)) { fail(acc, index, item.job, book, l0.id, debug_show e); continue };
      };
      switch (FinancingCore.loan(s.financing, l0.id)) {
        case (?l) {
          if (l.state == #recalled and day >= l.returnDay and SettlementCore.openInstructionOf(s.settlement, l.id, 1) == null) {
            switch (FinancingCore.planReturn(s.financing, l.id, day)) {
              case (#ok(ev)) {
                let custody = List.empty<T.Event>();
                for ((lot, n) in FinancingCore.lotsOfLoan(s.financing, l.id).vals()) List.add(custody, #custody(#lentReturned({ lot; depot = l.depot; nominal = n; reference; day })));
                if (l.collateralNominal > 0) List.add(custody, #custody(#collateralReturned({ isin = l.collateralIsin; depot = l.depot; nominal = l.collateralNominal; reference; day })));
                ignore postEvent("loan-return", l.id, "r", ev, null, ?l, cash, List.toArray(custody));
              };
              case (#err(e)) fail(acc, index, item.job, book, l.id, debug_show e);
            };
          };
        };
        case null {};
      };
    };
  };

  func runItem(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, acc : ChunkAcc, run : Eod.Run, item : Batch.PlanItem, index : Nat, day : Nat, onlyDeal : ?Nat) {
    let ?period = periodForDay(js, day) else { acc.examined += 1; fail(acc, index, item.job, run.book, 0, "no open period contains day " # Nat.toText(day)); return };
    if (Text.equal(item.job.name, Eod.JOB_TREASURY.name)) jobTreasury(s, bb, js, jb, journalCaller, now, acc, item, index, day, period, run.book, onlyDeal)
    else if (Text.equal(item.job.name, Eod.JOB_CALLS.name)) jobCalls(s, bb, js, jb, journalCaller, now, acc, item, index, day, period, run.book, onlyDeal)
    else if (Text.equal(item.job.name, Eod.JOB_CUSTODY.name)) jobCustody(s, bb, js, jb, journalCaller, now, acc, item, index, day, period, run.book, onlyDeal)
    else if (Text.equal(item.job.name, Eod.JOB_SETTLEMENT.name)) jobSettlement(s, js, acc, item, index, day, run.book, onlyDeal)
    else if (Text.equal(item.job.name, Eod.JOB_FINANCING.name)) jobFinancing(s, bb, js, jb, journalCaller, now, acc, item, index, day, period, run.book, onlyDeal)
    else fail(acc, index, item.job, run.book, 0, "unknown job " # item.job.name);
  };

  public type Advance = { completed : Bool; cursorFrom : Nat; cursorTo : Nat; posted : Nat; examined : Nat; zeroMovement : Nat; failures : [Batch.Failure]; resolved : [{ item : Nat; entity : Nat }]; retried : [Batch.Failure]; blocks : [Nat]; postings : [Nat] };

  /// Advance an open run by up to `limit` plan items. The plan is re-derived and checked against the recorded
  /// hash; the run records as it goes through the recorder, so every item reads what the items before it posted;
  /// the retry pass re-attempts the failures still carried below the book's limit before new work.
  public func runEndOfDayChunk(s : State, bb : Blocks, js : JCore.State, jb : JCore.Blocks, journalCaller : Principal, now : Nat64, book : Text, businessDate : Nat, limit : Nat, recorder : Recorder) : Res<Advance> {
    if (limit == 0 or limit > Batch.MAX_ADVANCE_LIMIT) return #err(#BatchError({ error = #AdvanceLimit({ limit; max = Batch.MAX_ADVANCE_LIMIT }) }));
    let ?run = Eod.getRun(s.eod, book, businessDate) else return #err(#BatchError({ error = #UnknownRun({ book; businessDate }) }));
    if (Eod.isComplete(run)) return #err(#BatchError({ error = #RunComplete({ book; businessDate }) }));
    // the plan, re-derived from the recorded inputs and checked against the recorded hash
    let items = switch (Batch.plan(Eod.planInput(book, run.shardSize))) {
      case (#ok(xs)) xs;
      case (#err(#invalidShardSize(d))) return #err(#BatchError({ error = #InvalidShardSize({ shardSize = d.shardSize }) }));
      case (#err(#planTooLarge(d))) return #err(#BatchError({ error = #PlanTooLarge({ items = d.items }) }));
      case (#err(_)) return #err(#BatchError({ error = #PlanTooLarge({ items = 0 }) }));
    };
    let recomputed = Batch.planHash(items);
    if (recomputed != run.planHash) return #err(#BatchError({ error = #PlanHashMismatch({ recorded = run.planHash; recomputed }) }));
    if (run.cursor >= items.size()) return #err(#BatchError({ error = #NothingToAdvance({ book; businessDate; cursor = run.cursor }) }));
    let acc = newAcc(recorder);
    let resolved = List.empty<{ item : Nat; entity : Nat }>();
    let reFailed = List.empty<Batch.Failure>();
    for (f in Eod.retryable(s.eod, run).vals()) {
      if (f.seq < items.size()) {
        let before = List.size(acc.failures);
        runItem(s, bb, js, jb, journalCaller, now, acc, run, items[f.seq], f.seq, businessDate, ?f.entity);
        var again = false;
        var i = before;
        while (i < List.size(acc.failures)) { let g = List.at(acc.failures, i); if (g.seq == f.seq and g.entity == f.entity) again := true; i += 1 };
        if (again) List.add(reFailed, { f with attempts = f.attempts + 1 }) else List.add(resolved, { item = f.seq; entity = f.entity });
      };
    };
    let retryPosted = acc.posted; let retryExamined = acc.examined; let retryZero = acc.zeroMovement; let retryFailures = List.size(acc.failures);
    if (List.size(resolved) > 0 or List.size(reFailed) > 0) {
      record(acc, #eod(#retry({ book; businessDate; resolved = List.toArray(resolved); failures = List.toArray(reFailed); posted = retryPosted })));
    };
    let from = run.cursor;
    var cursor = run.cursor;
    var done = 0;
    while (done < limit and cursor < items.size()) {
      runItem(s, bb, js, jb, journalCaller, now, acc, run, items[cursor], cursor, businessDate, null);
      cursor += 1; done += 1;
    };
    let chunkFailures = List.empty<Batch.Failure>();
    var fi = retryFailures;
    while (fi < List.size(acc.failures)) { List.add(chunkFailures, List.at(acc.failures, fi)); fi += 1 };
    record(acc, #eod(#chunk({ book; businessDate; cursorFrom = from; cursorTo = cursor; posted = acc.posted - retryPosted; examined = acc.examined - retryExamined; zeroMovement = acc.zeroMovement - retryZero; failures = List.toArray(chunkFailures) })));
    let completed = cursor >= items.size();
    if (completed) record(acc, #eod(#completed({ book; businessDate; posted = run.posted; examined = run.examined; zeroMovement = run.zeroMovement; failures = Eod.failureCount(run) })));
    #ok({
      completed; cursorFrom = from; cursorTo = cursor; posted = acc.posted; examined = acc.examined - retryExamined; zeroMovement = acc.zeroMovement - retryZero;
      failures = List.toArray(chunkFailures); resolved = List.toArray(resolved); retried = List.toArray(reFailed); blocks = List.toArray(acc.blocks); postings = List.toArray(acc.postings);
    })
  };

  // ═══════════════════════════════════════════════════════
  //  APPLY: the only place state changes
  // ═══════════════════════════════════════════════════════

  public func apply(s : State, block : Block) {
    switch (block.event) {
      case (#close(ce)) CloseCore.apply(s.close, block.index, ce);
      case (#fixingRecorded(f)) Fixings.fold(s.fixings, f);
      case (#eod(e)) Eod.fold(s.eod, block.index, e);
      case (#alert(ae)) AlertCore.apply(s.alerts, block.index, ae);
      case (#treasury(te)) {
        let before = switch (te) { case (#couponPaid(x)) TreasuryCore.row(s.treasury, x.deal); case (#legSettled(x)) TreasuryCore.row(s.treasury, x.deal); case (_) null };
        TreasuryCore.fold(s.treasury, block.index, te);
        CustodyCore.observeTreasury(s.custody, te, func(id : Nat) : ?TreasuryCore.DealRow { TreasuryCore.row(s.treasury, id) });
        Attribution.observeTreasury(s.attribution, block.index, te, before);
      };
      case (#call(ce)) CallCore.fold(s.calls, block.index, ce);
      case (#custody(ce)) CustodyCore.fold(s.custody, block.index, ce, isinOfLot(s));
      case (#settlement(se)) SettlementCore.fold(s.settlement, block.index, se);
      case (#financing(fe)) FinancingCore.fold(s.financing, block.index, fe);
      case (#valuation(ve)) { HedgeCore.fold(s.hedges, block.index, ve); switch (ve) { case (#thetaRecorded(x)) Attribution.observeTheta(s.attribution, x.deal, x.day, x.theta); case (_) {} } };
      case (#collateral(ce)) CollateralCore.fold(s.collateral, block.index, ce);
      case (#reconciliation(re)) ReconciliationCore.fold(s.reconciliation, block.index, re);
      case (#limits(le)) {
        LimitCore.fold(s.limits, block.index, le);
        switch (le) {
          case (#sweepSliced(x)) { for ((ag, c, n) in x.agreements.vals()) CollateralCore.accumulate(s.collateral, ag, c, n) };
          case (#sweepOpened(_)) CollateralCore.resetPending(s.collateral);
          case (_) {};
        };
      };
      case (_) Auth.apply(s.authority, block);
    };
    if (block.index + 1 > s.height) s.height := block.index + 1;
  };

  /// Rebuild the state from the two logs. The desk log is folded in order; the journal's posted blocks feed the
  /// nostro index exactly as the live contract fed it: at every journal mark the blocks below the recorded height
  /// are indexed before the act that follows is applied, so a registration meets only the postings after it and a
  /// statement finds the legs it matched. A block the fold cannot apply is named by a trap.
  public func replay(installer : Principal, blocks : [Block], journalBlocks : [JT.Block]) : State {
    let s = newState(installer);
    var j = 0;
    func indexThrough(height : Nat) {
      while (j < journalBlocks.size() and j < height) {
        let jb = journalBlocks[j];
        switch (jb.event) { case (#posted(p)) ignore TreasuryCore.indexJournalLegs(s.treasury, jb.index, p.valueDate, p.legs, p.sourceRef.id); case (_) {} };
        j += 1;
      };
    };
    for (b in blocks.vals()) {
      if (b.index != s.height) Runtime.trap("DeskCore.replay: block " # Nat.toText(b.index) # " out of order at height " # Nat.toText(s.height));
      switch (b.event) { case (#journalMark(x)) indexThrough(x.height); case (_) {} };
      apply(s, b);
    };
    indexThrough(journalBlocks.size());
    s
  };

  /// A replay in steps, for a log too long to fold inside one message: the target heights are fixed when it
  /// begins, the desk blocks are applied a page at a time in order, and the journal is indexed to each mark as the
  /// one-shot replay does. Complete when the desk log has been applied to its target and the journal indexed to its
  /// own.
  public type Replay = { state : State; var next : Nat; var journalNext : Nat; deskTarget : Nat; journalTarget : Nat; var complete : Bool };
  public func replayBegin(installer : Principal, arena : RI.Arena, deskTarget : Nat, journalTarget : Nat) : Replay {
    { state = newStateIn(installer, arena); var next = 0; var journalNext = 0; deskTarget; journalTarget; var complete = false }
  };
  /// Applies the blocks given, which must start at the replay's next height and not pass its target; `journalPage`
  /// reads the journal's posted blocks from a height, at most a page at a time. Returns what was applied.
  public func replayStep(r : Replay, blocks : [Block], journalPage : (Nat, Nat) -> [JT.Block]) : Nat {
    if (r.complete) return 0;
    func indexThrough(height : Nat) {
      let to = Nat.min(height, r.journalTarget);
      while (r.journalNext < to) {
        let page = journalPage(r.journalNext, to - r.journalNext);
        if (page.size() == 0) Runtime.trap("DeskCore.replayStep: the journal returned an empty page below its height");
        for (jb in page.vals()) {
          if (jb.index != r.journalNext) Runtime.trap("DeskCore.replayStep: journal block " # Nat.toText(jb.index) # " out of order at " # Nat.toText(r.journalNext));
          switch (jb.event) { case (#posted(p)) ignore TreasuryCore.indexJournalLegs(r.state.treasury, jb.index, p.valueDate, p.legs, p.sourceRef.id); case (_) {} };
          r.journalNext += 1;
        };
      };
    };
    var applied = 0;
    for (b in blocks.vals()) {
      if (r.next >= r.deskTarget) break;
      if (b.index != r.next or b.index != r.state.height) Runtime.trap("DeskCore.replayStep: block " # Nat.toText(b.index) # " out of order at height " # Nat.toText(r.next));
      switch (b.event) { case (#journalMark(x)) indexThrough(x.height); case (_) {} };
      apply(r.state, b);
      r.next += 1; applied += 1;
    };
    if (r.next == r.deskTarget) { indexThrough(r.journalTarget); r.complete := true };
    applied
  };

  public func fingerprintInto(w : JC.Writer, s : State) {
    w.nat(s.height);
    Auth.fingerprintInto(w, s.authority);
    CloseCore.fingerprintInto(w, s.close);
    Fixings.fingerprintInto(w, s.fixings);
    AlertCore.fingerprintInto(w, s.alerts);
    Eod.fingerprintInto(w, s.eod);
    TreasuryCore.fingerprintInto(w, s.treasury);
    CallCore.fingerprintInto(w, s.calls);
    CustodyCore.fingerprintInto(w, s.custody);
    SettlementCore.fingerprintInto(w, s.settlement);
    FinancingCore.fingerprintInto(w, s.financing);
    HedgeCore.fingerprintInto(w, s.hedges);
    Attribution.fingerprintInto(w, s.attribution);
    CollateralCore.fingerprintInto(w, s.collateral);
    LimitCore.fingerprintInto(w, s.limits);
    ReconciliationCore.fingerprintInto(w, s.reconciliation);
  };
  public func fingerprint(s : State) : Blob { let w = JC.Writer(); fingerprintInto(w, s); KC.hashWithDomainBlob("THEBES-DESK-STATE-v1", w.toBlob()) };
  /// The sections of the state, each fingerprinted on its own so a divergence between the live state and a fresh
  /// fold names the sub-state, and so a state too large to digest whole in one message is compared a section at
  /// a time.
  public func sectionNames() : [Text] { ["authority", "close", "fixings", "alerts", "eod", "treasury", "calls", "custody", "settlement", "financing", "hedges", "attribution", "collateral", "limits", "reconciliation"] };
  public func fingerprintSection(s : State, name : Text) : ?Blob {
    let w = JC.Writer();
    switch (name) {
      case "authority" Auth.fingerprintInto(w, s.authority);
      case "close" CloseCore.fingerprintInto(w, s.close);
      case "fixings" Fixings.fingerprintInto(w, s.fixings);
      case "alerts" AlertCore.fingerprintInto(w, s.alerts);
      case "eod" Eod.fingerprintInto(w, s.eod);
      case "treasury" TreasuryCore.fingerprintInto(w, s.treasury);
      case "calls" CallCore.fingerprintInto(w, s.calls);
      case "custody" CustodyCore.fingerprintInto(w, s.custody);
      case "settlement" SettlementCore.fingerprintInto(w, s.settlement);
      case "financing" FinancingCore.fingerprintInto(w, s.financing);
      case "hedges" HedgeCore.fingerprintInto(w, s.hedges);
      case "attribution" Attribution.fingerprintInto(w, s.attribution);
      case "collateral" CollateralCore.fingerprintInto(w, s.collateral);
      case "limits" LimitCore.fingerprintInto(w, s.limits);
      case "reconciliation" ReconciliationCore.fingerprintInto(w, s.reconciliation);
      case (_) return null;
    };
    ?KC.hashWithDomainBlob("THEBES-DESK-STATE-v1", w.toBlob())
  };
  public func fingerprintSections(s : State) : [(Text, Blob)] {
    Array.map<Text, (Text, Blob)>(sectionNames(), func(n) { (n, switch (fingerprintSection(s, n)) { case (?b) b; case null Blob.fromArray([]) }) })
  };

  /// The row indexes of the state, named, for a comparison in bounded messages: a state too large to digest in
  /// one message is compared index by index and page by page, the scalar figures of every core beside them.
  public func digestIndexes(s : State) : [(Text, RI.State)] {
    let t = s.treasury; let c = s.calls; let cu = s.custody; let se = s.settlement; let f = s.financing; let co = s.collateral; let l = s.limits;
    [
      ("treasury.deals", t.deals), ("treasury.securities", t.securities), ("treasury.curves", t.curves), ("treasury.limits", t.limits), ("treasury.buckets", t.buckets),
      ("treasury.nostros", t.nostros), ("treasury.nostroByAccount", t.nostroByAccount), ("treasury.nostroLegs", t.nostroLegs), ("treasury.legByPosting", t.legByPosting), ("treasury.statements", t.statements),
      ("treasury.breaks", t.breaks), ("treasury.byBook", t.byBook), ("treasury.byState", t.byState), ("treasury.byCounterparty", t.byCounterparty), ("treasury.byIsin", t.byIsin), ("treasury.breaksByStatus", t.breaksByStatus), ("treasury.subledgers", t.subledgers),
      ("calls.rows", c.rows), ("calls.byBook", c.byBook), ("calls.byState", c.byState), ("calls.byCounterparty", c.byCounterparty),
      ("custody.instruments", cu.instruments), ("custody.depots", cu.depots), ("custody.depotByHash", cu.depotByHash), ("custody.bookDepot", cu.bookDepot), ("custody.dealDepot", cu.dealDepot), ("custody.holdings", cu.holdings),
      ("custody.byDepotIsin", cu.byDepotIsin), ("custody.actions", cu.actions), ("custody.actionsByIsin", cu.actionsByIsin), ("custody.actionsByState", cu.actionsByState), ("custody.entitlements", cu.entitlements), ("custody.encumbered", cu.encumbered), ("custody.received", cu.received),
      ("settlement.instructions", se.instructions), ("settlement.byDeal", se.byDeal), ("settlement.byCycle", se.byCycle), ("settlement.byState", se.byState), ("settlement.cycles", se.cycles), ("settlement.ledgers", se.ledgers),
      ("financing.repos", f.repos), ("financing.loans", f.loans), ("financing.reposByBook", f.reposByBook), ("financing.loansByBook", f.loansByBook), ("financing.loansByLot", f.loansByLot), ("financing.reposByLot", f.reposByLot),
      ("hedges.rows", s.hedges.rows), ("attribution.rows", s.attribution.rows), ("attribution.captureDay", s.attribution.captureDay),
      ("collateral.agreements", co.agreements), ("collateral.byCounterparty", co.byCounterparty), ("collateral.haircuts", co.haircuts), ("collateral.cash", co.cash), ("collateral.securities", co.securities), ("collateral.securitiesByAgreement", co.securitiesByAgreement), ("collateral.calls", co.calls), ("collateral.callsByAgreement", co.callsByAgreement),
      ("limits.nodes", l.nodes), ("limits.counters", l.counters), ("limits.counterparties", l.counterparties),
      ("reconciliation.notifications", s.reconciliation.notifications), ("reconciliation.notified", s.reconciliation.notified), ("reconciliation.statements", s.reconciliation.statements), ("reconciliation.depotBreaks", s.reconciliation.depotBreaks), ("reconciliation.cash", s.reconciliation.cash), ("reconciliation.cashBreaks", s.reconciliation.cashBreaks),
    ]
  };
  /// One page of an index digested: the entries from the cursor, at most `limit` of them, hashed with their keys.
  public func digestPage(index : RI.State, cursor : ?Blob, limit : Nat) : { hash : Blob; entries : Nat; next : ?Blob } {
    let (lo, hi) = R.fullRange(index.spec.keyBytes);
    let page = RI.range(index, lo, hi, cursor, Nat.max(1, Nat.min(limit, 2048)));
    let w = JC.Writer();
    for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v) };
    w.nat(page.entries.size());
    { hash = KC.hashWithDomainBlob("THEBES-DESK-STATE-v1", w.toBlob()); entries = page.entries.size(); next = page.cursor }
  };
  /// The scalar figures of every core whose rows the digest walks, as text, so the two states compare whole.
  public func digestScalars(s : State) : [(Text, Text)] {
    [
      ("treasury", debug_show (TreasuryCore.status(s.treasury))), ("calls", debug_show (CallCore.status(s.calls))), ("custody", debug_show (CustodyCore.status(s.custody))),
      ("settlement", debug_show (SettlementCore.status(s.settlement))), ("financing", debug_show (FinancingCore.status(s.financing))),
      ("hedges", debug_show ((s.hedges.policy, s.hedges.count, s.hedges.open, s.hedges.quotes, s.hedges.thetas))), ("attribution", debug_show (s.attribution.rowCount)),
      ("collateral", debug_show ((CollateralCore.status(s.collateral), CollateralCore.policy(s.collateral)))), ("limits", debug_show ((LimitCore.status(s.limits), LimitCore.sweepView(s.limits)))),
      ("reconciliation", debug_show ((ReconciliationCore.status(s.reconciliation), ReconciliationCore.policy(s.reconciliation)))),
    ]
  };

  // ═══════════════════════════════════════════════════════
  //  VIEWS
  // ═══════════════════════════════════════════════════════

  public func alertBlocks(bb : Blocks) : AlertCore.Blocks {
    { get = func(i : Nat) : ?AlT.AlertEvent { switch (bb.get(i)) { case (?b) { switch (b.event) { case (#alert(ae)) ?ae; case (_) null } }; case null null } } }
  };
  public func dealView(s : State, bb : Blocks, r : TreasuryCore.DealRow) : TT.DealView {
    let (cp, reference) = treasuryCaptureOf(bb, r.id);
    TreasuryCore.view(s.treasury, r, cp, reference)
  };
  public func callView(bb : Blocks, r : CallCore.Row) : CallT.View { let (cp, reference, _) = callOpeningOf(bb, r.id); CallCore.view(r, cp, reference) };
  /// The positions of a book across both families: Manticore's treasury aggregation and the call money.
  public func positions(s : State, bb : Blocks, book : Text) : [TT.PositionView] {
    Array.concat<TT.PositionView>(TreasuryCore.positions(s.treasury, book, treasuryTerms(bb)), CallCore.positions(s.calls, book))
  };
  public func breakView(s : State, bb : Blocks, b : TreasuryCore.BreakRow, today : Nat) : TT.BreakView {
    let reference = switch (bb.get(b.id)) { case (?blk) { switch (blk.event) { case (#treasury(#nostroBreak(x))) x.reference; case (_) "" } }; case null "" };
    TreasuryCore.breakView(b, TreasuryCore.nostroIdOfHash(s.treasury, b.nostroHash), reference, today)
  };
}
