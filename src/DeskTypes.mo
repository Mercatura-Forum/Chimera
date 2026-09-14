/// DeskTypes.mo: the vocabulary of the desk contract: its commands, the events its log records, the errors it
/// refuses with, and the views it answers.
///
/// The treasury vocabulary is Manticore's (`TreasuryTypes`), reached through the `manticore` package and never
/// copied; the thirteen treasury commands below carry exactly the fields Manticore's carry, so the bodies are
/// written by `TreasuryCanonical` byte for byte. The authority vocabulary is the kernel's (`AuthTypes`): a scope
/// names partitions, which a desk calls books. The journal's configuration commands are the journal's own
/// prepare functions with a command shape around them. The close vocabulary (the functional currency, the position
/// pairs, the recorded rates) is Manticore's `CloseTypes`, because the valuation context the treasury reads is the
/// one the close records.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Nat64 "mo:core/Nat64";

import JT "mo:journal/JournalTypes";
import AT "mo:kernel/auth/AuthTypes";
import Cmd "mo:kernel/domain/Command";
import Batch "mo:kernel/batch/Batch";

import TT "mo:manticore/TreasuryTypes";
import CT "mo:manticore/CloseTypes";
import Fx "mo:manticore/Fx";
import AlT "mo:manticore/AlertTypes";

import CallT "CallTypes";
import CuT "CustodyTypes";
import ST "SettlementTypes";

module {

  public type BookId = Text;
  public type RoleId = AT.RoleId;
  public type PermissionId = AT.PermissionId;
  public type FeatureId = Text;
  public type Day = Nat;
  public type Scope = AT.Scope;
  public type DualPolicy = AT.DualPolicy;

  /// A money-visible feature is inactive until an activation height at or below the desk log's own height is
  /// recorded; no entry at all reads as this.
  public let ACTIVATION_OFF : Nat64 = 0xFFFF_FFFF_FFFF_FFFF;
  /// The one money-visible feature of the treasury: the capture, the settlement and the marking of deals, and the
  /// end-of-day job that does the same.
  public let FEATURE_TREASURY : FeatureId = "treasury";
  public let FEATURE_END_OF_DAY : FeatureId = "end-of-day";

  public let MAX_BOOK_ID_BYTES : Nat = 32;
  public let MAX_BOOK_DEPTH : Nat = 4;
  public let MAX_IDENTITY_BYTES : Nat = 128;

  /// Who the desk is on a confirmation: its name as the trading side, its BIC and its LEI.
  public type Identity = { name : Text; bic : Text; lei : Text };

  public type Dates = { postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };

  public type Command = {
    // ── authority ──
    #openBook : { id : BookId; name : Text; parent : ?BookId; sharia : Bool };
    #closeBook : { id : BookId };
    #defineRole : { id : RoleId; name : Text; permissions : [PermissionId] };
    #grantRole : { subject : Principal; role : RoleId; scope : Scope };
    #revokeRole : { subject : Principal; role : RoleId };
    #setDualPolicy : DualPolicy;
    #clearDualPolicy : { permission : PermissionId };
    #setFeatureActivation : { feature : FeatureId; height : Nat64 };
    #setDeskIdentity : Identity;
    // ── the embedded journal's configuration ──
    #journalRegisterCurrency : { code : JT.Currency; minorUnits : Nat8 };
    #journalOpenAccount : { code : JT.AccountCode; name : Text; normalSide : JT.Side; category : JT.Category; constraint : JT.BalanceConstraint };
    #journalCloseAccount : { code : JT.AccountCode };
    #journalOpenPeriod : { id : JT.PeriodId; start : Day; end : Day };
    #journalClosePeriod : { id : JT.PeriodId };
    #journalSetCalendar : { calendar : ?JT.CalendarConfig };
    #journalSetCalendarAuthority : { authority : JT.CalendarAuthority; maxRollDays : Nat; businessDate : ?Day };
    #journalRollBusinessDate : { day : Day };
    #journalSetActivationHeight : { height : Nat64 };
    // ── the close's market data: the functional currency, the position pairs, the spot of a day, a fixing ──
    #setFunctionalCurrency : { currency : JT.Currency };
    #setFxPair : { pair : Fx.PositionPair };
    #setFxRate : { rate : Fx.Rate };
    #recordRateFixing : { index : Text; day : Day; rateBps : Nat };
    // ── the end of day ──
    #openEndOfDay : { book : BookId; businessDate : Day; shardSize : Nat };
    #setRetryPolicy : { book : BookId; limit : Nat };
    #resolveEndOfDayFailure : { book : BookId; businessDate : Day; item : Nat; entity : Nat; reason : Text };
    // ── alerts ──
    #clearAlert : { alert : Nat; reason : Text };
    // ── the close's revaluation of every monetary position at the day's rate ──
    #revaluePositions : { period : JT.PeriodId; postingDate : Day; valueDate : Day; narration : Text };
    // ── call and notice money, the desk's own family ──
    #openCall : { book : BookId; counterparty : TT.Counterparty; terms : CallT.Terms; reference : Text; approver : ?Principal };
    #resetCallRate : { call : CallT.CallId; rateBps : Nat; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    #adjustCallBalance : { call : CallT.CallId; delta : Int; approver : ?Principal; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    #serveCallNotice : { call : CallT.CallId };
    #settleCall : { call : CallT.CallId; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    // ── securities services: the instrument master extended, depots, corporate actions ──
    #setCustodyPolicy : CuT.Policy;
    #extendInstrument : { extension : CuT.Extension };
    #openDepot : { depot : CuT.Depot };
    #setBookDepot : { book : BookId; depot : Text };
    #assignDealDepot : { deal : TT.DealId; depot : Text };
    #transferDepot : { lot : TT.DealId; from : Text; to : Text; nominal : Nat; reference : Text };
    #announceCorporateAction : { announcement : CuT.Announcement };
    #cancelCorporateAction : { action : CuT.ActionId; reason : Text };
    #processCorporateAction : { action : CuT.ActionId; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    // ── settlement through Tachyon: the venue, the cycles, the instructions and the decisions on a fail ──
    #setSettlementVenue : { venue : ST.Venue };
    #setSettlementLedger : { declaration : ST.LedgerDeclaration };
    #openSettlementCycle : { cycle : ST.Cycle };
    #instructSettlement : { deal : TT.DealId; counterparty : Principal; tradeId : ?Nat; reference : Text };
    #setInstructionTrade : { instruction : ST.InstructionId; tradeId : Nat };
    #recycleSettlement : { instruction : ST.InstructionId; cycle : Day };
    #recordSettlementStatus : { instruction : ST.InstructionId; document : Blob };
    #buyIn : { instruction : ST.InstructionId; counterparty : TT.Counterparty; priceMicro : Nat; settlement : Day; reference : Text; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    #cancelSettlement : { instruction : ST.InstructionId; ourConsent : Blob; theirConsent : Blob; reason : Text };
    #splitDeal : { deal : TT.DealId; parts : [Nat] };
    // ── treasury: the thirteen commands of Manticore's treasury domain, their bodies Manticore's ──
    #setTreasuryPolicy : TT.Policy;
    #registerSecurity : { terms : TT.SecurityTerms };
    #publishCurve : { curve : TT.Curve };
    #setTreasuryLimit : { limit : TT.Limit };
    #registerNostro : { nostro : TT.Nostro };
    #captureDeal : { book : BookId; counterparty : TT.Counterparty; kind : TT.DealKind; reference : Text; approver : ?Principal };
    #confirmDeal : { deal : TT.DealId; confirmation : Blob; fields : ?TT.ConfirmationFields; document : ?Blob };
    #amendDeal : { deal : TT.DealId; kind : TT.DealKind; reason : Text };
    #cancelDeal : { deal : TT.DealId; reason : Text };
    #settleDealLeg : { deal : TT.DealId; leg : Nat; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    #markDeal : { deal : TT.DealId; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
    #recordNostroStatement : { nostro : TT.NostroId; statement : Blob; from : Day; to : Day; entries : [TT.StatementEntry]; document : ?Blob };
    #resolveNostroBreak : { breakId : TT.BreakId; resolution : Text; correction : ?{ account : Text; sub : ?Text; debit : Bool; amount : Nat; currency : Text }; postingDate : Day; valueDate : Day; period : JT.PeriodId; narration : Text };
  };

  /// The end-of-day run's events: the plan fixed at the opening, every chunk as it lands, the retry pass, the
  /// completion; the retry policy and a failure resolved by a recorded decision.
  public type Failure = Batch.Failure;
  public type EodEvent = {
    #opened : { book : BookId; businessDate : Day; shardSize : Nat; openedAtHeight : Nat; maxDeal : Nat; planHash : Blob; items : Nat; entities : Nat };
    #chunk : { book : BookId; businessDate : Day; cursorFrom : Nat; cursorTo : Nat; posted : Nat; examined : Nat; zeroMovement : Nat; failures : [Failure] };
    #retry : { book : BookId; businessDate : Day; resolved : [{ item : Nat; entity : Nat }]; failures : [Failure]; posted : Nat };
    #completed : { book : BookId; businessDate : Day; posted : Nat; examined : Nat; zeroMovement : Nat; failures : Nat };
    #retryPolicySet : { book : BookId; limit : Nat };
    #failureResolved : { book : BookId; businessDate : Day; item : Nat; entity : Nat; reason : Text };
  };

  public type Event = {
    /// Block 0: who installed the desk.
    #deskInstalled : { installer : Principal };
    #bookOpened : { id : BookId; name : Text; parent : ?BookId; sharia : Bool };
    #bookClosed : { id : BookId };
    #roleDefined : { id : RoleId; name : Text; permissions : [PermissionId] };
    #roleGranted : { subject : Principal; role : RoleId; scope : Scope };
    #roleRevoked : { subject : Principal; role : RoleId };
    #dualPolicySet : DualPolicy;
    #dualPolicyCleared : { permission : PermissionId };
    #featureActivationSet : { feature : FeatureId; height : Nat64 };
    #identitySet : Identity;
    /// The maker-checker trail, in the kernel's shapes; the proposal's body travels in the block's trailer.
    #commandProposed : Cmd.Proposed;
    #commandApproved : Cmd.Approved;
    #commandExecuted : Cmd.Executed;
    #commandRejected : Cmd.Rejected;
    #commandExpired : Cmd.Expired;
    /// A refusal to exceed authority, recorded because a desk proves what it prevented.
    #operationRefused : { subject : Principal; permission : PermissionId; reason : AT.RefusalReason; detail : Text };
    /// The emergency path: the body travels in the trailer like a proposal's; the witness could have approved.
    #emergencyOverride : { commandHash : Blob; commandEncoding : Nat8; actor_ : Principal; witness : Principal; justification : Text };
    #overrideReviewed : { override_ : Nat; reviewer : Principal; disposition : Text };
    /// What an executed command consumed of its authority's daily limit, per currency, recorded after the execution so
    /// the fold measures tomorrow's limit without reading the command body back.
    #dailyConsumed : { subject : Principal; currency : Text; day : Day; amount : Nat };
    #close : CT.CloseEvent;
    #fixingRecorded : { index : Text; day : Day; rateBps : Nat };
    /// The journal's height at this point of the desk log, recorded before an act whose fold reads the nostro index:
    /// a nostro's registration (its postings are indexed from that block on) and a statement (its matches mark legs
    /// the journal committed before it). A fresh fold of the two logs feeds the journal's posted blocks to the index
    /// up to this height before applying what follows, so the index meets every act as the live contract's did.
    #journalMark : { height : Nat };
    #eod : EodEvent;
    #alert : AlT.AlertEvent;
    #treasury : TT.TreasuryEvent;
    #call : CallT.Event;
    #custody : CuT.Event;
    #settlement : ST.Event;
  };

  public type BatchError = {
    #RunExists : { book : BookId; businessDate : Day };
    #UnknownRun : { book : BookId; businessDate : Day };
    #RunComplete : { book : BookId; businessDate : Day };
    #BusinessDateMismatch : { businessDate : Day; requested : Day };
    #InvalidShardSize : { shardSize : Nat };
    #PlanTooLarge : { items : Nat };
    #PlanHashMismatch : { recorded : Blob; recomputed : Blob };
    #AdvanceLimit : { limit : Nat; max : Nat };
    #NothingToAdvance : { book : BookId; businessDate : Day; cursor : Nat };
    #InvalidRetry : { reason : Text };
    #UnknownFailure : { book : BookId; businessDate : Day; item : Nat; entity : Nat };
  };

  public type Error = {
    #AnonymousCaller;
    // the authority's refusals, the kernel's error union flattened so a reader sees the reason as the tag
    #NoGrant : { permission : PermissionId };
    #OutsideBookScope : { book : BookId };
    #OutsideCurrencyScope : { currency : Text };
    #OverCeiling : { currency : Text; amount : Nat; ceiling : Nat };
    #OverDailyLimit : { currency : Text; amount : Nat; consumed : Nat; limit : Nat };
    #ProposalNotAwaiting : { index : Nat };
    #ProposalExpired : { index : Nat; expiresAt : Nat64 };
    #SelfApproval : { maker : Principal };
    #AlreadyApproved : { checker : Principal };
    #NotEligibleChecker : { checker : Principal; eligibleRole : RoleId };
    #CommandHashMismatch : { recorded : Blob; recomputed : Blob };
    #NoDualPolicy : { permission : PermissionId };
    #WitnessRequired;
    #WitnessIsActor;
    #WitnessNotEligible : { witness : Principal };
    #UnknownPermission : { permission : PermissionId };
    #UnknownRole : { role : RoleId };
    #UnknownProposal : { index : Nat };
    #UnknownOverride : { index : Nat };
    #OverrideAlreadyReviewed : { index : Nat };
    #RequiresDualAuthorisation : { permission : PermissionId; required : Nat };
    #InvalidPolicy : { reason : Text };
    #InvalidRole : { reason : Text };
    #RoleExists : { role : RoleId };
    #GrantExists : { subject : Principal; role : RoleId };
    #NoSuchGrant : { subject : Principal; role : RoleId };
    #InvalidScope : { reason : Text };
    #InvalidBook : { reason : Text };
    #BookExists : { book : BookId };
    #UnknownBook : { book : BookId };
    #BookClosed : { book : BookId };
    #InvalidFeature : { reason : Text };
    #FeatureInactive : { feature : FeatureId; activationHeight : Nat64; height : Nat64 };
    #InvalidIdentity : { reason : Text };
    #UnrepresentableCommand : { family : Text; version : Nat8 };
    // the journal's refusals, as the journal states them
    #JournalConfigError : { error : JT.ConfigError };
    #JournalError : { error : JT.PostError };
    #UnknownAccount : { role : Text; account : Text };
    // the close's market data
    #NoFunctionalCurrency;
    #FunctionalCurrencyAlreadySet : { currency : Text };
    #InvalidRate : { reason : Text };
    #InvalidPair : { reason : Text };
    #UnknownPair : { currency : Text };
    #InvalidFixing : { reason : Text };
    #BatchError : { error : BatchError };
    #AlertError : { error : AlT.AlertError };
    #TreasuryError : { error : TT.TreasuryError };
    #CallError : { error : CallT.Error };
    #CustodyError : { error : CuT.Error };
    #SettlementError : { error : ST.Error };
    #MissingRate : { currency : Text; asOf : Day };
  };

  /// The kernel's authority error, flattened into the desk's: one switch, as the kernel advises.
  public func ofAuth(e : AT.Error) : Error {
    switch (e) {
      case (#NoGrant(x)) #NoGrant(x);
      case (#OutsidePartitionScope(x)) #OutsideBookScope({ book = x.partition });
      case (#OutsideCurrencyScope(x)) #OutsideCurrencyScope(x);
      case (#OverCeiling(x)) #OverCeiling(x);
      case (#OverDailyLimit(x)) #OverDailyLimit(x);
      case (#ProposalNotAwaiting(x)) #ProposalNotAwaiting(x);
      case (#ProposalExpired(x)) #ProposalExpired(x);
      case (#SelfApproval(x)) #SelfApproval(x);
      case (#AlreadyApproved(x)) #AlreadyApproved(x);
      case (#NotEligibleChecker(x)) #NotEligibleChecker(x);
      case (#CommandHashMismatch(_)) #CommandHashMismatch({ recorded = "" : Blob; recomputed = "" : Blob });
      case (#NoPolicy(x)) #NoDualPolicy(x);
      case (#WitnessRequired) #WitnessRequired;
      case (#WitnessIsActor) #WitnessIsActor;
      case (#WitnessNotEligible(x)) #WitnessNotEligible(x);
      case (#UnknownPermission(x)) #UnknownPermission(x);
      case (#UnknownRole(x)) #UnknownRole(x);
    }
  };

  /// The reason code a refusal block records; every refusal that is about authority has one.
  public func refusalReason(e : Error) : AT.RefusalReason {
    switch (e) {
      case (#NoGrant(_)) #noGrant;
      case (#OutsideBookScope(_)) #outsidePartition;
      case (#OutsideCurrencyScope(_)) #outsideCurrency;
      case (#OverCeiling(_)) #overCeiling;
      case (#OverDailyLimit(_)) #overDailyLimit;
      case (#NotEligibleChecker(_)) #notEligibleChecker;
      case (#SelfApproval(_) or #AlreadyApproved(_)) #selfApproval;
      case (#CommandHashMismatch(_)) #commandHashMismatch;
      case (#ProposalExpired(_) or #ProposalNotAwaiting(_)) #proposalExpired;
      case (#NoDualPolicy(_)) #noPolicy;
      case (#WitnessRequired or #WitnessIsActor or #WitnessNotEligible(_)) #noWitness;
      case (_) #unknown;
    }
  };

  // ── views ──

  public type Book = { id : BookId; name : Text; parent : ?BookId; sharia : Bool; open : Bool; openedAtBlock : Nat; closedAtBlock : ?Nat };
  public type Role = AT.Role;
  public type GrantView = { subject : Principal; role : RoleId; scope : Scope; permissions : [PermissionId] };
  public type ConsumedView = { subject : Principal; currency : Text; day : Day; amount : Nat };
  public type ProposalView = {
    index : Nat; commandHash : Blob; commandEncoding : Nat8; permission : PermissionId; book : ?BookId; maker : Principal;
    required : Nat; eligibleRole : RoleId; expiresAt : Nat64; justification : Text; approvals : [Principal]; status : Text;
    executedAt : ?Nat; effects : [Nat];
  };
  public type OverrideView = { index : Nat; actor_ : Principal; witness : Principal; justification : Text; commandHash : Blob; reviewedBy : ?Principal; disposition : ?Text };
  public type RunView = {
    book : BookId; businessDate : Day; shardSize : Nat; openedAtBlock : Nat; openedAtHeight : Nat; maxDeal : Nat; planHash : Blob; items : Nat; entities : Nat;
    cursor : Nat; done : Nat; posted : Nat; examined : Nat; zeroMovement : Nat; chunks : Nat; failures : [Failure]; state : Text;
  };
  public type Status = {
    height : Nat; books : Nat; roles : Nat; grants : Nat; policies : Nat; proposals : Nat; openProposals : Nat; overrides : Nat;
    refused : Nat; executed : Nat; identity : ?Identity;
  };

  public func eventName(e : Event) : Text {
    switch (e) {
      case (#deskInstalled(_)) "deskInstalled";
      case (#bookOpened(_)) "bookOpened";
      case (#bookClosed(_)) "bookClosed";
      case (#roleDefined(_)) "roleDefined";
      case (#roleGranted(_)) "roleGranted";
      case (#roleRevoked(_)) "roleRevoked";
      case (#dualPolicySet(_)) "dualPolicySet";
      case (#dualPolicyCleared(_)) "dualPolicyCleared";
      case (#featureActivationSet(_)) "featureActivationSet";
      case (#identitySet(_)) "identitySet";
      case (#commandProposed(_)) "commandProposed";
      case (#commandApproved(_)) "commandApproved";
      case (#commandExecuted(_)) "commandExecuted";
      case (#commandRejected(_)) "commandRejected";
      case (#commandExpired(_)) "commandExpired";
      case (#operationRefused(_)) "operationRefused";
      case (#emergencyOverride(_)) "emergencyOverride";
      case (#overrideReviewed(_)) "overrideReviewed";
      case (#dailyConsumed(_)) "dailyConsumed";
      case (#close(_)) "close";
      case (#fixingRecorded(_)) "fixingRecorded";
      case (#journalMark(_)) "journalMark";
      case (#eod(_)) "eod";
      case (#alert(_)) "alert";
      case (#treasury(_)) "treasury";
      case (#call(_)) "call";
      case (#custody(_)) "custody";
      case (#settlement(_)) "settlement";
    }
  };
}
