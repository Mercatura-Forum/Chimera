/// Catalogue.mo: the desk's permission table, checked in both directions.
///
/// One row per command family and per guarded method, built with the kernel's row constructor so a field added
/// to the row type is a compile error at every row. The thirteen treasury rows carry Manticore's identifiers,
/// actions and dual-control decisions unchanged: configuration and market data are dual acts of governance; the
/// capture records a contract and moves no money, so it is the trader's own act within the entitlements' ceiling
/// on the deal's notional; the legs that move money settle under dual control, as do marks, amendments,
/// cancellations and break resolutions; the connector records confirmations and statements alone.
///
/// `validate` is the kernel's: a command with no row and a row naming a command that does not exist are both
/// faults, and the unit battery and `tools/permission_audit.py` hold it against the built interface.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Text "mo:core/Text";

import AT "mo:kernel/auth/AuthTypes";
import P "mo:kernel/auth/Permissions";

import T "DeskTypes";

module {

  func p(id : Text, resource : Text, action : AT.Action, guards : AT.Guards, moneyMoving : Bool, dual : Bool) : AT.Permission {
    P.p(id, resource, action, guards, moneyMoving, false, dual)
  };

  public func catalogue() : P.Catalogue {
    [
      // ── the methods ──
      p("command.create", "command", #create, #method("propose"), false, false),
      p("command.approve", "command", #approve, #method("approve"), false, false),
      p("command.reject", "command", #reject, #method("reject"), false, false),
      p("command.perform", "command", #create, #method("perform"), false, false),
      p("command.breakGlass", "command", #breakGlass, #method("emergencyOverride"), false, false),
      p("override.review", "override", #approve, #method("reviewOverride"), false, false),
      // ── authority: governance is dual ──
      p("book.create", "book", #create, #command("openBook"), false, true),
      p("book.close", "book", #close, #command("closeBook"), false, true),
      p("role.create", "role", #create, #command("defineRole"), false, true),
      p("role.grant", "role", #update, #command("grantRole"), false, true),
      p("role.revoke", "role", #delete, #command("revokeRole"), false, true),
      p("policy.update", "policy", #update, #command("setDualPolicy"), false, true),
      p("policy.clear", "policy", #delete, #command("clearDualPolicy"), false, true),
      p("feature.activate", "feature", #activate, #command("setFeatureActivation"), false, true),
      p("desk.identity.update", "desk", #update, #command("setDeskIdentity"), false, true),
      // ── the embedded journal's configuration ──
      p("journal.currency.create", "journal", #create, #command("journalRegisterCurrency"), false, true),
      p("journal.account.create", "journal", #create, #command("journalOpenAccount"), false, true),
      p("journal.account.close", "journal", #close, #command("journalCloseAccount"), false, true),
      p("journal.period.create", "journal", #create, #command("journalOpenPeriod"), false, true),
      p("journal.period.close", "journal", #close, #command("journalClosePeriod"), false, true),
      p("journal.calendar.update", "journal", #update, #command("journalSetCalendar"), false, true),
      p("journal.calendar.authority", "journal", #update, #command("journalSetCalendarAuthority"), false, true),
      p("journal.businessdate.update", "journal", #update, #command("journalRollBusinessDate"), false, true),
      p("journal.activation.update", "journal", #activate, #command("journalSetActivationHeight"), false, true),
      // ── the close's market data ──
      p("fx.functional.update", "fx", #update, #command("setFunctionalCurrency"), false, true),
      p("fx.pair.update", "fx", #update, #command("setFxPair"), false, true),
      p("fx.rate.create", "fx", #create, #command("setFxRate"), false, true),
      p("market.fixing.record", "market", #create, #command("recordRateFixing"), false, true),
      // ── the end of day ──
      p("eod.open", "eod", #create, #command("openEndOfDay"), false, true),
      p("eod.retry.update", "eod", #update, #command("setRetryPolicy"), false, true),
      p("eod.failure.resolve", "eod", #update, #command("resolveEndOfDayFailure"), false, true),
      // ── alerts ──
      p("alert.clear", "alert", #update, #command("clearAlert"), false, true),
      // ── the revaluation ──
      p("fx.revalue", "fx", #update, #command("revaluePositions"), true, true),
      // ── call and notice money: the opening the trader's own act within the ceiling, the rest dual ──
      p("call.open", "call", #create, #command("openCall"), false, false),
      p("call.rate.reset", "call", #update, #command("resetCallRate"), true, true),
      p("call.balance.adjust", "call", #update, #command("adjustCallBalance"), true, true),
      p("call.notice.serve", "call", #update, #command("serveCallNotice"), false, true),
      p("call.settle", "call", #update, #command("settleCall"), true, true),
      // ── securities services: the master, the depots and the actions are dual acts; the processing moves money ──
      p("custody.policy", "custody", #update, #command("setCustodyPolicy"), false, true),
      p("instrument.extend", "instrument", #update, #command("extendInstrument"), false, true),
      p("depot.open", "depot", #create, #command("openDepot"), false, true),
      p("depot.book.update", "depot", #update, #command("setBookDepot"), false, true),
      p("depot.deal.assign", "depot", #update, #command("assignDealDepot"), false, true),
      p("depot.transfer", "depot", #update, #command("transferDepot"), true, true),
      p("depot.account.update", "depot", #update, #command("setDepotAccount"), false, true),
      p("depot.transfer.instruct", "depot", #create, #command("instructDepotTransfer"), true, true),
      p("corporate.action.announce", "corporate", #create, #command("announceCorporateAction"), false, true),
      p("corporate.action.cancel", "corporate", #reverse, #command("cancelCorporateAction"), false, true),
      p("corporate.action.process", "corporate", #update, #command("processCorporateAction"), true, true),
      // ── settlement through Tachyon ──
      p("settlement.venue", "settlement", #update, #command("setSettlementVenue"), false, true),
      p("settlement.ledger", "settlement", #update, #command("setSettlementLedger"), false, true),
      p("settlement.cycle.open", "settlement", #create, #command("openSettlementCycle"), false, true),
      p("settlement.instruct", "settlement", #create, #command("instructSettlement"), true, true),
      p("settlement.trade.assign", "settlement", #update, #command("setInstructionTrade"), false, true),
      p("settlement.recycle", "settlement", #update, #command("recycleSettlement"), false, true),
      p("settlement.status.record", "settlement", #create, #command("recordSettlementStatus"), false, true),
      p("settlement.buyin", "settlement", #update, #command("buyIn"), true, true),
      p("settlement.cancel", "settlement", #update, #command("cancelSettlement"), true, true),
      p("settlement.split", "settlement", #update, #command("splitDeal"), true, true),
      // ── financing: repo, reverse repo and securities lending ──
      p("financing.policy", "financing", #update, #command("setFinancingPolicy"), false, true),
      p("repo.open", "repo", #create, #command("openRepo"), false, false),
      p("repo.settle", "repo", #update, #command("settleRepoLeg"), true, true),
      p("repo.rate.reset", "repo", #update, #command("resetRepoRate"), true, true),
      p("repo.margin.meet", "repo", #update, #command("meetMarginCall"), true, true),
      p("repo.collateral.substitute", "repo", #update, #command("substituteCollateral"), false, true),
      p("loan.open", "loan", #create, #command("openLoan"), false, false),
      p("loan.settle", "loan", #update, #command("settleLoanLeg"), true, true),
      p("loan.recall", "loan", #update, #command("recallLoan"), false, true),
      p("settlement.instruct.financing", "settlement", #create, #command("instructFinancing"), true, true),
      // ── valuation ──
      p("valuation.policy", "valuation", #update, #command("setValuationPolicy"), false, true),
      p("market.yield.quote", "market", #create, #command("quoteBondYield"), false, true),
      p("hedge.designate", "hedge", #create, #command("designateHedge"), false, true),
      p("hedge.assess", "hedge", #update, #command("assessHedge"), true, true),
      p("hedge.dedesignate", "hedge", #update, #command("dedesignateHedge"), true, true),
      // ── collateral: the agreements are governance; every movement of the pool moves money or a depot ──
      p("collateral.policy", "collateral", #update, #command("setCollateralPolicy"), false, true),
      p("collateral.agreement.update", "collateral", #update, #command("setCollateralAgreement"), false, true),
      p("collateral.cash.post", "collateral", #update, #command("postCollateralCash"), true, true),
      p("collateral.pledge", "collateral", #update, #command("pledgeCollateral"), true, true),
      p("collateral.release", "collateral", #update, #command("releaseCollateral"), true, true),
      p("collateral.receive", "collateral", #update, #command("receiveCollateral"), true, true),
      p("collateral.return", "collateral", #update, #command("returnCollateral"), true, true),
      p("collateral.substitution.open", "collateral", #create, #command("openCollateralSubstitution"), true, true),
      p("collateral.substitution.settle", "collateral", #update, #command("settleCollateralSubstitution"), true, true),
      p("collateral.interest.settle", "collateral", #update, #command("settleCollateralInterest"), true, true),
      p("collateral.delivery.instruct", "collateral", #create, #command("instructCollateralDelivery"), true, true),
      // ── limits: the tree and the counterparty data are governance; the sweep is opened under dual control ──
      p("limit.node.update", "limit", #update, #command("setLimitNode"), false, true),
      p("limit.node.remove", "limit", #delete, #command("removeLimitNode"), false, true),
      p("counterparty.amend", "counterparty", #update, #command("amendCounterparty"), false, true),
      p("risk.sweep.open", "risk", #create, #command("openRiskSweep"), false, true),
      // ── reconciliation: the connector records notifications and statements alone; a break is resolved by a decision ──
      p("reconciliation.policy", "reconciliation", #update, #command("setReconciliationPolicy"), false, true),
      p("nostro.notification.record", "nostro", #create, #command("recordNostroNotification"), false, false),
      p("depot.statement.record", "depot", #create, #command("recordDepotStatement"), false, false),
      p("depot.break.resolve", "depot", #update, #command("resolveDepotBreak"), false, true),
      p("cash.break.resolve", "cash", #update, #command("resolveCashBreak"), false, true),
      // ── liquidity: every factor and class the ratios read is governance ──
      p("liquidity.factors", "liquidity", #update, #command("setLiquidityFactors"), false, true),
      p("liquidity.instrument.classify", "liquidity", #update, #command("classifyInstrument"), false, true),
      p("liquidity.counterparty.classify", "liquidity", #update, #command("classifyCounterparty"), false, true),
      p("liquidity.capital.declare", "liquidity", #update, #command("declareCapital"), false, true),
      // ── the feed: the declaration and the lift are governance; a figure is a connector's, under its own signature ──
      p("feed.declare", "feed", #update, #command("declareFeed"), false, true),
      p("feed.price.submit", "feed", #create, #command("submitPrice"), false, false),
      p("feed.halt.lift", "feed", #update, #command("liftHalt"), false, true),
      // ── the market: the declaration and a cycle are dual; an order is the trader's within the desk's standing ──
      p("market.declare", "market", #update, #command("declareMarket"), false, true),
      p("market.order.stage", "market", #create, #command("stageOrder"), false, false),
      p("market.order.cancel", "market", #update, #command("cancelOrder"), false, false),
      p("market.cycle.open", "market", #create, #command("openMarketCycle"), false, true),
      // ── treasury (Manticore's rows, verbatim) ──
      p("treasury.policy", "treasury", #update, #command("setTreasuryPolicy"), false, true),
      p("treasury.security.register", "treasury", #create, #command("registerSecurity"), false, true),
      p("treasury.curve.publish", "treasury", #update, #command("publishCurve"), false, true),
      p("treasury.limit.update", "treasury", #update, #command("setTreasuryLimit"), false, true),
      p("nostro.register", "nostro", #create, #command("registerNostro"), false, true),
      p("treasury.deal.capture", "treasury", #create, #command("captureDeal"), false, false),
      p("treasury.deal.confirm", "treasury", #update, #command("confirmDeal"), false, false),
      p("treasury.deal.amend", "treasury", #update, #command("amendDeal"), true, true),
      p("treasury.deal.cancel", "treasury", #reverse, #command("cancelDeal"), true, true),
      p("treasury.deal.settle", "treasury", #update, #command("settleDealLeg"), true, true),
      p("treasury.deal.mark", "treasury", #update, #command("markDeal"), true, true),
      p("nostro.statement.record", "nostro", #create, #command("recordNostroStatement"), false, false),
      p("nostro.break.resolve", "nostro", #update, #command("resolveNostroBreak"), true, true),
    ]
  };

  /// The variant name of a command, exhaustive over the union: what the catalogue's `#command` guards name.
  public func commandName(c : T.Command) : Text {
    switch (c) {
      case (#openBook(_)) "openBook";
      case (#closeBook(_)) "closeBook";
      case (#defineRole(_)) "defineRole";
      case (#grantRole(_)) "grantRole";
      case (#revokeRole(_)) "revokeRole";
      case (#setDualPolicy(_)) "setDualPolicy";
      case (#clearDualPolicy(_)) "clearDualPolicy";
      case (#setFeatureActivation(_)) "setFeatureActivation";
      case (#setDeskIdentity(_)) "setDeskIdentity";
      case (#journalRegisterCurrency(_)) "journalRegisterCurrency";
      case (#journalOpenAccount(_)) "journalOpenAccount";
      case (#journalCloseAccount(_)) "journalCloseAccount";
      case (#journalOpenPeriod(_)) "journalOpenPeriod";
      case (#journalClosePeriod(_)) "journalClosePeriod";
      case (#journalSetCalendar(_)) "journalSetCalendar";
      case (#journalSetCalendarAuthority(_)) "journalSetCalendarAuthority";
      case (#journalRollBusinessDate(_)) "journalRollBusinessDate";
      case (#journalSetActivationHeight(_)) "journalSetActivationHeight";
      case (#setFunctionalCurrency(_)) "setFunctionalCurrency";
      case (#setFxPair(_)) "setFxPair";
      case (#setFxRate(_)) "setFxRate";
      case (#recordRateFixing(_)) "recordRateFixing";
      case (#openEndOfDay(_)) "openEndOfDay";
      case (#setRetryPolicy(_)) "setRetryPolicy";
      case (#resolveEndOfDayFailure(_)) "resolveEndOfDayFailure";
      case (#clearAlert(_)) "clearAlert";
      case (#revaluePositions(_)) "revaluePositions";
      case (#openCall(_)) "openCall";
      case (#resetCallRate(_)) "resetCallRate";
      case (#adjustCallBalance(_)) "adjustCallBalance";
      case (#serveCallNotice(_)) "serveCallNotice";
      case (#settleCall(_)) "settleCall";
      case (#setCustodyPolicy(_)) "setCustodyPolicy";
      case (#extendInstrument(_)) "extendInstrument";
      case (#openDepot(_)) "openDepot";
      case (#setBookDepot(_)) "setBookDepot";
      case (#assignDealDepot(_)) "assignDealDepot";
      case (#transferDepot(_)) "transferDepot";
      case (#announceCorporateAction(_)) "announceCorporateAction";
      case (#cancelCorporateAction(_)) "cancelCorporateAction";
      case (#processCorporateAction(_)) "processCorporateAction";
      case (#setSettlementVenue(_)) "setSettlementVenue";
      case (#setSettlementLedger(_)) "setSettlementLedger";
      case (#openSettlementCycle(_)) "openSettlementCycle";
      case (#instructSettlement(_)) "instructSettlement";
      case (#setInstructionTrade(_)) "setInstructionTrade";
      case (#recycleSettlement(_)) "recycleSettlement";
      case (#recordSettlementStatus(_)) "recordSettlementStatus";
      case (#buyIn(_)) "buyIn";
      case (#cancelSettlement(_)) "cancelSettlement";
      case (#splitDeal(_)) "splitDeal";
      case (#setFinancingPolicy(_)) "setFinancingPolicy";
      case (#openRepo(_)) "openRepo";
      case (#settleRepoLeg(_)) "settleRepoLeg";
      case (#resetRepoRate(_)) "resetRepoRate";
      case (#meetMarginCall(_)) "meetMarginCall";
      case (#substituteCollateral(_)) "substituteCollateral";
      case (#openLoan(_)) "openLoan";
      case (#settleLoanLeg(_)) "settleLoanLeg";
      case (#recallLoan(_)) "recallLoan";
      case (#instructFinancing(_)) "instructFinancing";
      case (#setValuationPolicy(_)) "setValuationPolicy";
      case (#quoteBondYield(_)) "quoteBondYield";
      case (#designateHedge(_)) "designateHedge";
      case (#assessHedge(_)) "assessHedge";
      case (#dedesignateHedge(_)) "dedesignateHedge";
      case (#setCollateralPolicy(_)) "setCollateralPolicy";
      case (#setCollateralAgreement(_)) "setCollateralAgreement";
      case (#postCollateralCash(_)) "postCollateralCash";
      case (#pledgeCollateral(_)) "pledgeCollateral";
      case (#releaseCollateral(_)) "releaseCollateral";
      case (#receiveCollateral(_)) "receiveCollateral";
      case (#returnCollateral(_)) "returnCollateral";
      case (#openCollateralSubstitution(_)) "openCollateralSubstitution";
      case (#settleCollateralSubstitution(_)) "settleCollateralSubstitution";
      case (#settleCollateralInterest(_)) "settleCollateralInterest";
      case (#instructCollateralDelivery(_)) "instructCollateralDelivery";
      case (#setDepotAccount(_)) "setDepotAccount";
      case (#instructDepotTransfer(_)) "instructDepotTransfer";
      case (#setLimitNode(_)) "setLimitNode";
      case (#removeLimitNode(_)) "removeLimitNode";
      case (#amendCounterparty(_)) "amendCounterparty";
      case (#openRiskSweep(_)) "openRiskSweep";
      case (#setReconciliationPolicy(_)) "setReconciliationPolicy";
      case (#recordNostroNotification(_)) "recordNostroNotification";
      case (#recordDepotStatement(_)) "recordDepotStatement";
      case (#resolveDepotBreak(_)) "resolveDepotBreak";
      case (#resolveCashBreak(_)) "resolveCashBreak";
      case (#setLiquidityFactors(_)) "setLiquidityFactors";
      case (#classifyInstrument(_)) "classifyInstrument";
      case (#classifyCounterparty(_)) "classifyCounterparty";
      case (#declareCapital(_)) "declareCapital";
      case (#declareFeed(_)) "declareFeed";
      case (#submitPrice(_)) "submitPrice";
      case (#liftHalt(_)) "liftHalt";
      case (#declareMarket(_)) "declareMarket";
      case (#stageOrder(_)) "stageOrder";
      case (#cancelOrder(_)) "cancelOrder";
      case (#openMarketCycle(_)) "openMarketCycle";
      case (#setTreasuryPolicy(_)) "setTreasuryPolicy";
      case (#registerSecurity(_)) "registerSecurity";
      case (#publishCurve(_)) "publishCurve";
      case (#setTreasuryLimit(_)) "setTreasuryLimit";
      case (#registerNostro(_)) "registerNostro";
      case (#captureDeal(_)) "captureDeal";
      case (#confirmDeal(_)) "confirmDeal";
      case (#amendDeal(_)) "amendDeal";
      case (#cancelDeal(_)) "cancelDeal";
      case (#settleDealLeg(_)) "settleDealLeg";
      case (#markDeal(_)) "markDeal";
      case (#recordNostroStatement(_)) "recordNostroStatement";
      case (#resolveNostroBreak(_)) "resolveNostroBreak";
    }
  };

  /// Every command name, in the order of the union: what `validate` holds the catalogue against.
  public func commandNames() : [Text] {
    [
      "openBook", "closeBook", "defineRole", "grantRole", "revokeRole", "setDualPolicy", "clearDualPolicy", "setFeatureActivation", "setDeskIdentity",
      "journalRegisterCurrency", "journalOpenAccount", "journalCloseAccount", "journalOpenPeriod", "journalClosePeriod", "journalSetCalendar", "journalSetCalendarAuthority",
      "journalRollBusinessDate", "journalSetActivationHeight",
      "setFunctionalCurrency", "setFxPair", "setFxRate", "recordRateFixing",
      "openEndOfDay", "setRetryPolicy", "resolveEndOfDayFailure", "clearAlert",
      "revaluePositions", "openCall", "resetCallRate", "adjustCallBalance", "serveCallNotice", "settleCall",
      "setCustodyPolicy", "extendInstrument", "openDepot", "setBookDepot", "assignDealDepot", "transferDepot", "setDepotAccount", "instructDepotTransfer", "announceCorporateAction", "cancelCorporateAction", "processCorporateAction",
      "setSettlementVenue", "setSettlementLedger", "openSettlementCycle", "instructSettlement", "setInstructionTrade", "recycleSettlement", "recordSettlementStatus", "buyIn", "cancelSettlement", "splitDeal",
      "setFinancingPolicy", "openRepo", "settleRepoLeg", "resetRepoRate", "meetMarginCall", "substituteCollateral", "openLoan", "settleLoanLeg", "recallLoan", "instructFinancing",
      "setValuationPolicy", "quoteBondYield", "designateHedge", "assessHedge", "dedesignateHedge",
      "setCollateralPolicy", "setCollateralAgreement", "postCollateralCash", "pledgeCollateral", "releaseCollateral", "receiveCollateral", "returnCollateral",
      "openCollateralSubstitution", "settleCollateralSubstitution", "settleCollateralInterest", "instructCollateralDelivery",
      "setLimitNode", "removeLimitNode", "amendCounterparty", "openRiskSweep",
      "setReconciliationPolicy", "recordNostroNotification", "recordDepotStatement", "resolveDepotBreak", "resolveCashBreak",
      "setLiquidityFactors", "classifyInstrument", "classifyCounterparty", "declareCapital",
      "declareFeed", "submitPrice", "liftHalt", "declareMarket", "stageOrder", "cancelOrder", "openMarketCycle",
      "setTreasuryPolicy", "registerSecurity", "publishCurve", "setTreasuryLimit", "registerNostro", "captureDeal", "confirmDeal", "amendDeal", "cancelDeal",
      "settleDealLeg", "markDeal", "recordNostroStatement", "resolveNostroBreak",
    ]
  };

  /// The update methods that take a permission. Every other update method is open by design and listed in
  /// `openMethods`, so the audit can hold the built interface against both lists.
  public func guardedMethods() : [Text] { ["propose", "approve", "reject", "perform", "emergencyOverride", "reviewOverride"] };

  /// Open by design, each with its reason: the audit refuses an open method without one.
  public func openMethods() : [(Text, Text)] {
    [
      ("advanceEndOfDay", "the plan was fixed at the run's opening and is checked against its hash; a caller chooses nothing, and the blocks are attributed to the contract"),
      ("expireProposals", "expiry is a fact of the clock; the block is attributed to the contract and the caller chooses nothing"),
      ("beginReplay", "a verification: the fold of the logs recomputed into the replay's own arena, which writes nothing the desk reads; the caller chooses nothing"),
      ("advanceReplay", "a verification step over the recorded blocks in order; the caller chooses nothing but how many"),
      ("driveSettlement", "the next step of an instruction recorded through four eyes: the calls it makes are the instruction's, the outcome is Tachyon's, and the caller chooses nothing"),
      ("advanceRiskSweep", "one slice of a sweep opened under dual control: the rows walked are the next in order, the figures are the fold's, and the caller chooses nothing"),
      ("reconcileCash", "the desk's settlement cash account against the ledger declared for its currency: the intent is recorded before the call and the ledger's reply after it, and the caller chooses nothing"),
      ("driveMarketCycle", "the next step of a cycle opened through four eyes: the staged orders handed to the engine, the clear advanced, the fills read back, each captured and instructed on the engine's own trade; every step is the cycle's own and the caller chooses nothing"),
    ]
  };

  /// The reads that answer about the caller's own standing and nothing of the desk's: a stranger's reply from one
  /// of these is empty or about the stranger, never about the desk.
  public func selfOnlyReads() : [Text] { ["listBooks", "planMeter", "treasuryDealsByState", "treasuryDealsOfCounterparty", "callsOfCounterparty", "marketOrders"] };
  /// The reads scoped to the caller's books: a stranger is refused with the book named. Every other query method
  /// is public by design, since the desk's log is verifiable by anyone who holds its bytes, and the isolation suite
  /// lists it for the record rather than waving it through.
  public func scopedReads() : [Text] {
    ["attribution", "attributionOfBook", "call", "callTerms", "callsOfBook", "counterpartyExposure", "loan", "loansOfBook", "markOf", "marketOrder",
     "pendingSettlement", "positions", "repo", "reposOfBook", "treasuryConfirmation", "treasuryDeal", "treasuryDealTerms", "treasuryDealsOfBook",
     "treasuryLimits", "treasuryLots", "treasuryPositions", "treasurySettlementInstruction"]
  };
  /// The desk's own wording of an authority refusal, beside the kernel's phrases.
  public func authorityPhrases() : [Text] { ["NoGrant", "NoReadableBook", "OutsideBookScope", "AnonymousCaller", "NotEligible", "NotTheOperator", "NoRole", "not in scope"] };

  public func forCommand(c : T.Command) : ?AT.Permission { P.byCommand(catalogue(), commandName(c)) };
  public func byMethod(method : Text) : ?AT.Permission { P.byMethod(catalogue(), method) };
  public func byId(id : Text) : ?AT.Permission { P.byId(catalogue(), id) };
  public func exists(id : Text) : Bool { byId(id) != null };
  public func ids() : [Text] { P.ids(catalogue()) };

  public func validate() : P.Report { P.validate(catalogue(), commandNames(), guardedMethods()) };

  public func isOpenMethod(m : Text) : Bool { for ((x, _) in openMethods().vals()) { if (Text.equal(x, m)) return true }; false };
}
