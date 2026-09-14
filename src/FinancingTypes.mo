/// FinancingTypes.mo: repo, reverse repo and securities lending as the desk's own deal families, financing under
/// IFRS 9: the seller keeps the security on its balance sheet (Manticore's lot stays, amortising and accruing as
/// before) and records a collateralised borrowing; the buyer records a collateralised loan and never books the
/// security. The collateral's movement is a pledge in the depot; margin is measured at every end of day against
/// the day's price; a securities loan keeps the lot with the lender, who records the fee and, for a coupon paid
/// while the security is out, a manufactured payment claimed from the borrower.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import DC "mo:manticore/DayCount";
import TT "mo:manticore/TreasuryTypes";

module {

  public type RepoId = Nat;   // the desk block index of #repoOpened
  public type LoanId = Nat;   // the desk block index of #loanOpened
  public type Day = Nat;

  /// The accounts the financing families post to, and the grace the margin call has before an alert.
  public type Policy = {
    repoPayable : Text; reverseRepoReceivable : Text; repoInterestPayable : Text; repoInterestReceivable : Text; repoInterestExpense : Text; repoInterestIncome : Text;
    marginCashGiven : Text; marginCashReceived : Text;
    lendingFeeReceivable : Text; lendingFeeIncome : Text; cashCollateralPayable : Text; rebateExpense : Text; manufacturedPaymentReceivable : Text;
    marginGraceDays : Nat;
  };

  public type Collateral = { isin : Text; nominal : Nat };

  /// A classic repo: the desk sells the collateral and repurchases it (a borrowing), or the reverse (a loan of
  /// cash). The purchase price is the cash; the repurchase price is the cash plus interest at the rate by the day
  /// count to the close; an open repo has no maturity and its rate is reset by an act. The haircut discounts the
  /// collateral's value; a shortfall beyond the threshold raises a margin call.
  public type RepoTerms = {
    reverse : Bool;                // false: the desk borrows cash against its collateral; true: the desk lends cash
    currency : Text;
    cash : Nat;                    // the purchase price
    rateBps : Nat;
    dayCount : DC.Convention;
    start : Day;
    maturity : ?Day;               // null: an open repo, closed by an act
    collateral : Collateral;
    haircutBps : Nat;
    thresholdBps : Nat;            // the margin threshold as a share of the exposure
    cashAccount : TT.CashAccount;
    depot : Text;                  // the depot the desk's collateral is pledged from, or received collateral is held in
  };

  public type RepoState = { #open; #started; #closed };
  public func repoStateText(s : RepoState) : Text { switch (s) { case (#open) "open"; case (#started) "started"; case (#closed) "closed" } };

  public type MarginPayer = { #desk; #counterparty };
  public func payerText(p : MarginPayer) : Text { switch (p) { case (#desk) "desk"; case (#counterparty) "counterparty" } };

  /// A securities loan: the desk lends the nominal against cash (with a rebate paid on it) or against securities,
  /// for a fee in basis points per annum on the loan's value, recallable on notice.
  public type LoanCollateral = { #cash : { amount : Nat; rebateBps : Nat }; #securities : Collateral };
  public type LoanTerms = {
    isin : Text; nominal : Nat; currency : Text; valueMicro : Nat;   // the loan's value: the price per 100 the fee accrues on
    feeBps : Nat; dayCount : DC.Convention; collateral : LoanCollateral; start : Day; noticeDays : Nat; cashAccount : TT.CashAccount; depot : Text;
  };
  public type LoanState = { #open; #started; #recalled; #returned };
  public func loanStateText(s : LoanState) : Text { switch (s) { case (#open) "open"; case (#started) "started"; case (#recalled) "recalled"; case (#returned) "returned" } };

  public type Event = {
    #policySet : Policy;
    #repoOpened : { book : Text; counterparty : TT.Counterparty; terms : RepoTerms; reference : Text; trader : Principal; day : Day };
    /// The start leg settled: the collateral pledged (or received) and the cash moved.
    #repoStarted : { repo : RepoId; lots : [(TT.DealId, Nat)]; day : Day };
    #repoAccrued : { repo : RepoId; interest : Int; day : Day };
    #repoRateReset : { repo : RepoId; rateBps : Nat; day : Day; catchUp : Int };
    /// The collateral marked against the exposure: the value after the haircut, the exposure, and the shortfall
    /// or the excess beyond the threshold, which raises a call payable by one side.
    #collateralMarked : { repo : RepoId; value : Nat; exposure : Nat; priceMicro : Nat; day : Day };
    #marginCallRaised : { repo : RepoId; amount : Nat; payer : MarginPayer; day : Day; due : Day };
    #marginMet : { repo : RepoId; cash : Nat; collateral : ?Collateral; lots : [(TT.DealId, Nat)]; payer : MarginPayer; day : Day };
    #collateralSubstituted : { repo : RepoId; out : Collateral; in_ : Collateral; outLots : [(TT.DealId, Nat)]; inLots : [(TT.DealId, Nat)]; day : Day };
    /// The close leg settled: the collateral released (or returned) and the repurchase price moved, the margin
    /// cash returned.
    #repoClosed : { repo : RepoId; principal : Nat; interest : Nat; marginReturned : Int; day : Day };
    #loanOpened : { book : Text; counterparty : TT.Counterparty; terms : LoanTerms; reference : Text; trader : Principal; day : Day };
    #loanStarted : { loan : LoanId; lots : [(TT.DealId, Nat)]; day : Day };
    #loanAccrued : { loan : LoanId; fee : Int; rebate : Int; day : Day };
    #loanRecalled : { loan : LoanId; day : Day; returnDay : Day };
    #loanReturned : { loan : LoanId; fee : Nat; rebate : Nat; day : Day };
    /// A coupon paid on a lent lot: the lender's income, claimed from the borrower.
    #manufacturedPayment : { loan : LoanId; action : Nat; lot : TT.DealId; amount : Nat; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidTerms : { reason : Text };
    #UnknownRepo : { repo : RepoId };
    #UnknownLoan : { loan : LoanId };
    #RepoNotIn : { repo : RepoId; state : Text; wanted : Text };
    #LoanNotIn : { loan : LoanId; state : Text; wanted : Text };
    #NotDue : { id : Nat; due : Day; day : Day };
    #InsufficientCollateral : { isin : Text; available : Nat; wanted : Nat };
    #NoPrice : { isin : Text; day : Day };
    #NoMarginCall : { repo : RepoId };
    #ShortMargin : { repo : RepoId; called : Nat; offered : Nat };
  };

  public type RepoView = {
    id : RepoId; book : Text; counterparty : Text; reference : Text; reverse : Bool; currency : Text; cash : Nat; rateBps : Nat; start : Day; maturity : ?Day; isin : Text; nominal : Nat;
    haircutBps : Nat; thresholdBps : Nat; state : Text; accruedPosted : Int; marginCash : Int; marginCalled : Nat; marginPayer : ?Text; marginDue : ?Day; lastValue : Nat; lastBlock : Nat;
  };
  public type LoanView = {
    id : LoanId; book : Text; counterparty : Text; reference : Text; isin : Text; nominal : Nat; currency : Text; feeBps : Nat; start : Day; state : Text; feePosted : Int; rebatePosted : Int; returnDay : ?Day; manufactured : Nat; lastBlock : Nat;
  };
  public type Status = { repos : Nat; loans : Nat; openRepos : Nat; openLoans : Nat; marginCallsOpen : Nat };
}
