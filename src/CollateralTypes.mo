/// CollateralTypes.mo: collateral agreements, the pools they hold, and the calls the exposure raises.
///
/// An agreement (a credit support annex, a repo annex) stands with one counterparty in one currency and names
/// the families of deals it covers. Its pool holds cash by currency and securities by ISIN on both sides: what
/// the counterparty posted to the desk and what the desk posted to the counterparty. Every security in the pool
/// is valued at the day's price less a haircut from the agreement's schedule, or from the supervisory table
/// (Basel III, CRE22) when the agreement records none, with the currency-mismatch add-on when the security's
/// currency is not the agreement's; cash in another currency takes the same add-on.
///
/// The exposure of an agreement is folded by the risk sweep from the covered open rows with the counterparty:
/// the replacement value of every derivative, a placement's principal and accrued, a taking's the same with the
/// opposite sign, a repo's cash and interest against its collateral, a loan's value against the collateral it
/// holds. Under a netting agreement the signed sum stands; without netting only the positive contributions do.
/// The call is the credit support annex's: the requirement above the threshold less the credit support balance,
/// rounded, raised when it passes the minimum transfer amount, due the next business day.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";
import DC "mo:manticore/DayCount";
import CuT "CustodyTypes";

module {

  public type Day = Nat;
  public type AgreementId = Text;

  public type Coverage = { #treasury; #calls; #repos; #loans };
  public func coverageText(c : Coverage) : Text { switch (c) { case (#treasury) "treasury"; case (#calls) "calls"; case (#repos) "repos"; case (#loans) "loans" } };

  /// A haircut for an instrument class over a residual maturity bucket, in basis points.
  public type HaircutRow = { classification : CuT.Classification; fromDays : Nat; toDays : Nat; haircutBps : Nat };

  public type Agreement = {
    id : AgreementId;
    counterparty : TT.Counterparty;
    currency : Text;
    threshold : Nat;
    minimumTransfer : Nat;
    rounding : Nat;
    netting : Bool;
    covers : [Coverage];
    /// The bilateral schedule, or none for the supervisory table.
    schedule : ?[HaircutRow];
    /// Interest on the net cash balance, accrued daily by the sweep.
    cashRateBps : Nat;
    dayCount : DC.Convention;
    cash : TT.CashAccount;
    graceDays : Nat;
  };

  /// The accounts collateral cash and its interest post through.
  public type Policy = {
    cashReceivedPayable : Text;
    cashGivenReceivable : Text;
    interestPayable : Text;
    interestReceivable : Text;
    interestExpense : Text;
    interestIncome : Text;
  };

  /// A cash movement under an agreement: posted to the desk by the counterparty or by the desk to the
  /// counterparty, or either one returned.
  public type CashMove = { #received; #given; #receivedReturned; #givenReturned };
  public func cashMoveText(m : CashMove) : Text { switch (m) { case (#received) "received"; case (#given) "given"; case (#receivedReturned) "receivedReturned"; case (#givenReturned) "givenReturned" } };

  /// `#delivering`: a movement into the pool instructed as a delivery through the venue and not yet delivered;
  /// `#returning`: a live row instructed back through the venue.
  public type SecuritiesState = { #pledged; #instructed; #live; #returned; #delivering; #returning };
  public func securitiesStateText(s : SecuritiesState) : Text { switch (s) { case (#pledged) "pledged"; case (#instructed) "instructed"; case (#live) "live"; case (#returned) "returned"; case (#delivering) "delivering"; case (#returning) "returning" } };
  /// The four movements of securities under an agreement that settle as deliveries free of payment through the
  /// venue: the desk's lot pledged and released, the counterparty's securities received and returned.
  public type DeliveryMove = { #pledge; #release; #receive; #return_ };
  public func deliveryMoveText(m : DeliveryMove) : Text { switch (m) { case (#pledge) "pledge"; case (#release) "release"; case (#receive) "receive"; case (#return_) "return" } };

  public type Event = {
    #policySet : Policy;
    #agreementSet : { agreement : Agreement; day : Day };
    /// Every movement carries what it credits to the open call in its direction, valued as the pool values it. A
    /// cash movement first catches the interest up on the net balance it changes.
    #cashMoved : { agreement : AgreementId; move : CashMove; amount : Nat; currency : Text; callCredit : Nat; interestCatchUp : Int; day : Day };
    /// One of the desk's lots pledged to the counterparty: the depot keeps it, encumbered under the agreement.
    #securitiesPledged : { agreement : AgreementId; id : Nat; lot : TT.DealId; isin : Text; depot : Text; nominal : Nat; callCredit : Nat; day : Day };
    #securitiesReleased : { agreement : AgreementId; pledge : Nat; callCredit : Nat; day : Day };
    /// The counterparty's securities received into a depot of the desk's.
    #securitiesReceived : { agreement : AgreementId; id : Nat; isin : Text; depot : Text; nominal : Nat; callCredit : Nat; day : Day };
    #securitiesReturned : { agreement : AgreementId; receipt : Nat; callCredit : Nat; day : Day };
    /// A substitution: the desk's securities delivered against cash the counterparty holds of the desk's,
    /// settled through the venue as a delivery versus payment or by hand.
    #substitutionOpened : { agreement : AgreementId; id : Nat; lot : TT.DealId; isin : Text; depot : Text; nominal : Nat; cashReturned : Nat; currency : Text; day : Day };
    #substitutionSettled : { agreement : AgreementId; substitution : Nat; callCredit : Nat; interestCatchUp : Int; day : Day };
    /// A movement instructed as a delivery through the venue: a pledge or a receipt opens its row here, a release
    /// or a return names its live row; the pool credits it when the receipt lands.
    #deliveryInstructed : { agreement : AgreementId; id : Nat; move : DeliveryMove; lot : ?TT.DealId; isin : Text; depot : Text; nominal : Nat; instruction : Nat; day : Day };
    /// The delivery's receipt verified: the row live or returned, the call credited in the movement's direction.
    #deliverySettled : { agreement : AgreementId; id : Nat; move : DeliveryMove; callCredit : Nat; day : Day };
    #interestAccrued : { agreement : AgreementId; currency : Text; interest : Int; day : Day };
    #interestSettled : { agreement : AgreementId; currency : Text; amount : Int; day : Day };
    /// The sweep's figures for the day: the exposure from the covered rows, the credit support balance of the
    /// pool after haircuts, both in the agreement's currency.
    #exposureRecorded : { agreement : AgreementId; day : Day; exposure : Int; balance : Int; rows : Nat };
    /// A call: the desk delivers when `deliver`, the counterparty otherwise; due the next business day.
    #callRaised : { agreement : AgreementId; id : Nat; amount : Nat; deliver : Bool; day : Day; due : Day };
    /// A call met by the movements in its direction, valued as the pool values them.
    #callMet : { agreement : AgreementId; call : Nat; day : Day };
    /// A call standing when the next sweep valued the pool again: the next figure supersedes it.
    #callSuperseded : { agreement : AgreementId; call : Nat; outstanding : Nat; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidAgreement : { reason : Text };
    #UnknownAgreement : { agreement : AgreementId };
    #AgreementExists : { counterparty : Text; agreement : AgreementId };
    #InvalidMove : { reason : Text };
    #PoolShort : { agreement : AgreementId; currency : Text; held : Nat; wanted : Nat };
    #UnknownPledge : { agreement : AgreementId; id : Nat };
    #PledgeNotIn : { id : Nat; state : Text; wanted : Text };
    #NoPrice : { isin : Text; day : Day };
    #NoRate : { currency : Text; day : Day };
    #NoHaircut : { isin : Text };
  };

  public type AgreementView = {
    id : AgreementId; counterparty : Text; currency : Text; threshold : Nat; minimumTransfer : Nat; rounding : Nat; netting : Bool; covers : [Text];
    bilateral : Bool; cashRateBps : Nat; graceDays : Nat; exposure : Int; balance : Int; exposureDay : Day; openCall : ?Nat; interestAccrued : Int; lastBlock : Nat;
  };
  public type CashView = { agreement : AgreementId; currency : Text; received : Nat; given : Nat; interestAccrued : Int };
  public type SecuritiesView = { agreement : AgreementId; id : Nat; lot : ?TT.DealId; isin : Text; depot : Text; nominal : Nat; given : Bool; state : Text; cashReturned : Nat };
  public type CallView = { agreement : AgreementId; id : Nat; amount : Nat; outstanding : Nat; deliver : Bool; day : Day; due : Day; state : Text };
  public type Status = { agreements : Nat; cashRows : Nat; securitiesRows : Nat; calls : Nat; openCalls : Nat; exposures : Nat };
}
