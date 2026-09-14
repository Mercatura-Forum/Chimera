/// CallTypes.mo: call and notice money, the desk's own deal family beside Manticore's six.
///
/// A call deposit is a placement or a taking with no maturity: a balance that can be drawn or added to, a rate
/// that can be reset, interest accrued daily by the day count and settled at a declared frequency (paid through
/// the cash account, or capitalised into the balance), and a notice that, once served, fixes the repayment day as
/// the notice period ahead shifted to the next business day of the desk's calendar. Every act is a command; the
/// balance and the accrual are the fold of the acts; the arithmetic is Manticore's `TreasuryMath`, so the Python
/// twin is the same file.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import DC "mo:manticore/DayCount";
import TT "mo:manticore/TreasuryTypes";

module {

  public type CallId = Nat;   // the desk block index of #opened
  public type Day = Nat;

  public type Terms = {
    placement : Bool;              // true: the desk places (asset); false: the desk takes (liability)
    currency : Text;
    principal : Nat;               // the opening balance, funded on the start day
    rateBps : Nat;
    dayCount : DC.Convention;
    noticeDays : Nat;              // 0: overnight money, repayable the business day after notice
    interestEveryDays : Nat;       // 0: interest settled at repayment only
    capitalise : Bool;             // interest added to the balance at each settlement, else paid through cash
    cash : TT.CashAccount;         // the nostro or the settlement cash the principal and the interest move through
    start : Day;
  };

  public type State = { #open; #noticed; #closed };
  public func stateText(s : State) : Text { switch (s) { case (#open) "open"; case (#noticed) "noticed"; case (#closed) "closed" } };

  public type Event = {
    #opened : { book : Text; counterparty : TT.Counterparty; terms : Terms; reference : Text; trader : Principal; day : Day; withinLimits : Bool; approver : ?Principal };
    /// The principal moved on the start day.
    #funded : { call : CallId; amount : Nat; day : Day };
    /// The rate changed, the accrual to the day caught up at the old rate first.
    #rateReset : { call : CallId; rateBps : Nat; day : Day; catchUp : Int };
    /// The balance drawn (negative) or added to (positive), the accrual to the day caught up first.
    #balanceAdjusted : { call : CallId; delta : Int; day : Day; catchUp : Int };
    #noticeServed : { call : CallId; day : Day; repayDay : Day };
    #accrued : { call : CallId; interest : Int; day : Day };
    /// Interest settled at an interest date: paid through cash, or capitalised into the balance.
    #interestSettled : { call : CallId; amount : Nat; capitalised : Bool; day : Day };
    /// The balance and the interest to the day repaid on the repayment day; the call is closed.
    #repaid : { call : CallId; principal : Nat; interest : Nat; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidTerms : { reason : Text };
    #UnknownCall : { call : CallId };
    #CallNotIn : { call : CallId; state : Text; wanted : Text };
    #NotFunded : { call : CallId };
    #NotDue : { call : CallId; due : Day; day : Day };
    #InsufficientBalance : { call : CallId; balance : Nat; wanted : Nat };
    #LimitBreached : { kind : Text; subject : Text; limit : Nat; measured : Nat };
    #ShariaBook : { book : Text };
  };

  public type View = {
    id : CallId; book : Text; counterparty : Text; reference : Text; placement : Bool; currency : Text; balance : Nat; rateBps : Nat; noticeDays : Nat;
    interestEveryDays : Nat; capitalise : Bool; start : Day; funded : Bool; state : Text; accruedPosted : Int; interestPaid : Nat; lastInterestDay : Day; repayDay : ?Day; lastBlock : Nat;
  };
}
