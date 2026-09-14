/// CustodyTypes.mo: securities services over Manticore's securities: the instrument master extended, the depots
/// and the settled positions in them, and corporate actions as recorded events with entitlements by record date.
///
/// Manticore's treasury holds a lot per purchase deal and consumes lots on sales and redemptions; the trade-date
/// position of a book is its fold. The desk adds the custody side: a lot is held in a depot (a safekeeping account
/// at a custodian), transfers between depots are recorded movements free of payment, and the settled position per
/// depot and ISIN is a fold over the holdings. A corporate action is announced as data with its source hash; its
/// entitlement is computed at the end of day of the record date on the recorded basis, and its payment is posted
/// at the payment date.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";

module {

  public type Day = Nat;
  public type ActionId = Nat;   // the desk block index of #announced

  /// What the security master does not carry: the issuer's LEI, the regulatory classification, the market and its
  /// settlement cycle, the quotation basis, the minimum denomination.
  public type Classification = { #sovereign; #supranational; #financial; #corporate };
  public func classificationText(c : Classification) : Text { switch (c) { case (#sovereign) "sovereign"; case (#supranational) "supranational"; case (#financial) "financial"; case (#corporate) "corporate" } };
  public type Quotation = { #pricePer100; #yield };
  public type Extension = { isin : Text; lei : Text; classification : Classification; market : Text; settlementCycleDays : Nat; quotation : Quotation; minDenomination : Nat };

  public type Depot = { id : Text; custodian : TT.Counterparty; place : Text; safekeepingAccount : Text };

  /// The basis on which an entitlement is computed at the record date: the contractual settlement position (the
  /// lots bought with a settlement date on or before the record date, whether or not settled) or the actual
  /// settlement position (the depot holdings).
  public type Basis = { #contractual; #actual };
  public func basisText(b : Basis) : Text { switch (b) { case (#contractual) "contractual"; case (#actual) "actual" } };
  public type Policy = { entitlementBasis : Basis };

  /// The kinds a fixed-income instrument's life has. An announced coupon (an amount per 100 of nominal, in micro)
  /// replaces the instrument's own coupon for the period it covers; a partial redemption returns a ratio of every
  /// lot at par; an early redemption returns the whole position at a price per 100; a cash distribution is an
  /// extraordinary payment per 100 of nominal to income.
  public type Kind = {
    #coupon : { perHundredMicro : Nat };
    #partialRedemption : { ratioBps : Nat };
    #earlyRedemption : { priceMicro : Nat };
    #cashDistribution : { perHundredMicro : Nat };
  };
  public func kindText(k : Kind) : Text { switch (k) { case (#coupon(_)) "coupon"; case (#partialRedemption(_)) "partialRedemption"; case (#earlyRedemption(_)) "earlyRedemption"; case (#cashDistribution(_)) "cashDistribution" } };
  public type Announcement = { isin : Text; kind : Kind; recordDate : Day; exDate : Day; paymentDate : Day; source : Blob };

  public type ActionState = { #announced; #entitled; #paid; #cancelled };
  public func actionStateText(s : ActionState) : Text { switch (s) { case (#announced) "announced"; case (#entitled) "entitled"; case (#paid) "paid"; case (#cancelled) "cancelled" } };

  public type Event = {
    #policySet : Policy;
    #instrumentExtended : { extension : Extension; day : Day };
    #depotOpened : { depot : Depot; day : Day };
    #bookDepotSet : { book : Text; depot : Text; day : Day };
    /// A security deal's depot: the book's default at capture, or the one assigned before settlement.
    #dealDepotAssigned : { deal : TT.DealId; depot : Text; day : Day };
    /// A lot's holding moved between depots, free of payment.
    #transferred : { lot : TT.DealId; from : Text; to : Text; nominal : Nat; reference : Text; day : Day };
    #announced : { announcement : Announcement; day : Day };
    #cancelled : { action : ActionId; reason : Text; day : Day };
    /// One lot's entitlement at the record date on the recorded basis.
    #entitlementRecorded : { action : ActionId; lot : TT.DealId; depot : Text; nominal : Nat; amount : Nat; basis : Basis; day : Day };
    /// The action's record date passed: how many lots were entitled, for how much.
    #entitled : { action : ActionId; lots : Nat; total : Nat; day : Day };
    /// A coupon entitlement claimed on the lot's coupon date: the announced amount becomes the receivable under
    /// the action, the lot's accrued coupon is cleared into it and the difference is income, so the lot's accrual
    /// restarts on the instrument's grid while the claim waits for the payment date.
    #entitlementClaimed : { action : ActionId; lot : TT.DealId; amount : Nat; accrued : Int; day : Day };
    /// One lot's entitlement paid, with the figures the treasury row moved by (a redemption's nominal and cost, the
    /// realised result), or nothing for a cash distribution.
    #entitlementPaid : { action : ActionId; lot : TT.DealId; amount : Nat; nominal : Nat; realised : Int; day : Day };
    #paid : { action : ActionId; lots : Nat; total : Nat; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidTerms : { reason : Text };
    #UnknownInstrument : { isin : Text };
    #UnknownDepot : { depot : Text };
    #UnknownAction : { action : ActionId };
    #ActionNotIn : { action : ActionId; state : Text; wanted : Text };
    #NotDue : { action : ActionId; due : Day; day : Day };
    #DepotShort : { depot : Text; isin : Text; held : Nat; wanted : Nat };
    #NoBookDepot : { book : Text };
    #LotNotIn : { lot : TT.DealId; state : Text };
  };

  public type HoldingView = { lot : TT.DealId; depot : Text; isin : Text; book : Text; nominal : Nat };
  public type PositionView = { depot : Text; isin : Text; nominal : Nat; lots : Nat };
  public type ActionView = { id : ActionId; isin : Text; kind : Text; recordDate : Day; exDate : Day; paymentDate : Day; state : Text; lots : Nat; entitled : Nat; paid : Nat; lastBlock : Nat };
  public type EntitlementView = { action : ActionId; lot : TT.DealId; depot : Text; nominal : Nat; amount : Nat; basis : Text; claimed : Bool; paid : Bool };
  public type Status = { instruments : Nat; depots : Nat; holdings : Nat; actions : Nat; entitlements : Nat; transfers : Nat };
}
