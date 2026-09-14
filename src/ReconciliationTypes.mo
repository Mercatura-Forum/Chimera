/// ReconciliationTypes.mo: the desk's reconciliation beyond the nostro statement: intraday notifications
/// (camt.054) matched by the statement's rule, the custodian's holdings (semt.002) and transactions (semt.017)
/// matched against the depot fold and the settlement outcomes, the settlement cash accounts against the ledgers
/// the desk settles on, breaks on every side aged into alerts and closed by a recorded decision, and the report.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";

module {

  public type Day = Nat;
  public type BreakId = Nat;   // the desk block index of the break

  /// The settlement cash account of each currency, reconciled against the ledger declared for it (section 9),
  /// and the age past which an open break is alerted.
  public type CashAccount = { currency : Text; cash : TT.CashAccount };
  public type Policy = { cashAccounts : [CashAccount]; breakAgeAlertDays : Nat };

  public type StatementKind = { #holdings; #transactions };
  public func statementKindText(k : StatementKind) : Text { switch (k) { case (#holdings) "holdings"; case (#transactions) "transactions" } };
  public type BreakSide = { #onStatementOnly; #inOurBooksOnly };
  public func breakSideText(s : BreakSide) : Text { switch (s) { case (#onStatementOnly) "onStatementOnly"; case (#inOurBooksOnly) "inOurBooksOnly" } };
  public type BreakKind = { #position; #transaction };
  public func breakKindText(k : BreakKind) : Text { switch (k) { case (#position) "position"; case (#transaction) "transaction" } };

  /// A holding as the custodian reports it, per instrument, as of the statement date.
  public type ReportedHolding = { isin : Text; nominal : Nat };
  /// A posting as the custodian reports it: the account owner's reference, the instrument, the quantity, the
  /// direction from the desk's side (delivered or received), the effective settlement day.
  public type ReportedTransaction = { reference : Text; isin : Text; nominal : Nat; delivered : Bool; effectiveDay : Day };

  public type Event = {
    #policySet : Policy;
    /// A camt.054 notification matched entry by entry against the open legs of the nostro by the statement's
    /// rule; the matched legs are marked through Manticore's own statement event, the unmatched entries wait for
    /// the statement of the day, which decides them as the batch-only path would.
    #notificationRecorded : { nostro : Text; notification : Blob; entries : Nat; matched : [(TT.StatementEntry, Nat)]; unmatched : Nat; day : Day };
    /// Entries of a camt.053 a notification had already matched: dropped before the statement's own matching.
    #statementEntriesNotified : { nostro : Text; statement : Blob; entries : Nat; day : Day };
    #depotStatementRecorded : { depot : Text; statement : Blob; kind : StatementKind; statementDate : Day; from : Day; to : Day; reported : Nat; matched : Nat; explained : Nat; breaks : Nat; day : Day };
    /// A difference between the custodian and the depot fold: a position (theirs against ours) or a transaction
    /// the custodian posted that the desk has not settled, or the desk settled and the custodian did not post.
    #depotBreak : { depot : Text; statement : Blob; kind : BreakKind; side : BreakSide; isin : Text; ours : Nat; theirs : Nat; reference : Text; instruction : ?Nat; day : Day };
    /// A desk instruction of the window the custodian did not post because it failed: explained by the fail,
    /// never an item of its own.
    #breakExplainedByFail : { depot : Text; statement : Blob; isin : Text; reference : Text; instruction : Nat; nominal : Nat; day : Day };
    #depotBreakAged : { break_ : BreakId; ageDays : Nat; day : Day };
    #depotBreakResolved : { break_ : BreakId; resolution : Text; correction : ?Nat; day : Day };
    /// The desk's settlement cash account against the ledger it settles on: the intent before the call, the
    /// ledger's reply after it with the ledger's log height recorded beside the balance.
    #cashReconciliationIntended : { currency : Text; ledger : Principal; day : Day };
    #cashReconciled : { currency : Text; ledger : Principal; ledgerBalance : Nat; bookBalance : Int; difference : Int; tipHeight : ?Nat; day : Day };
    #cashReconciliationFailed : { currency : Text; ledger : Principal; reason : Text; day : Day };
    #cashBreak : { currency : Text; ledger : Principal; ledgerBalance : Nat; bookBalance : Int; difference : Int; day : Day };
    #cashBreakAged : { break_ : BreakId; ageDays : Nat; day : Day };
    #cashBreakCleared : { break_ : BreakId; day : Day };
    #cashBreakResolved : { break_ : BreakId; resolution : Text; correction : ?Nat; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidPolicy : { reason : Text };
    #BadDocument : { reason : Text };
    #UnknownDepot : { depot : Text };
    #WrongAccount : { depot : Text; expected : Text; got : Text };
    #StatementKnown : { statement : Blob };
    #UnknownBreak : { break_ : BreakId };
    #BreakNotOpen : { break_ : BreakId; state : Text };
    #NoLedger : { currency : Text };
    #NoCashAccount : { currency : Text };
    #ReconciliationInFlight : { currency : Text };
    #LedgerUnreachable : { currency : Text; reason : Text };
  };

  public type DepotBreakView = {
    id : BreakId; depot : Text; kind : Text; side : Text; isin : Text; ours : Nat; theirs : Nat; reference : Text; instruction : ?Nat; statement : Blob;
    openedDay : Day; ageDays : Nat; state : Text; resolvedDay : ?Day; correction : ?Nat;
  };
  public type CashBreakView = { id : BreakId; currency : Text; ledgerBalance : Nat; bookBalance : Int; difference : Int; openedDay : Day; ageDays : Nat; state : Text; resolvedDay : ?Day };
  public type CashView = { currency : Text; ledger : ?Principal; ledgerBalance : Nat; bookBalance : Int; difference : Int; day : Day; tipHeight : ?Nat; openBreak : ?BreakId };
  public type Status = { notifications : Nat; depotStatements : Nat; depotBreaks : Nat; depotBreaksOpen : Nat; explainedFails : Nat; cashReconciliations : Nat; cashBreaks : Nat; cashBreaksOpen : Nat };
}
