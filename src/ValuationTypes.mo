/// ValuationTypes.mo: the valuation family: a bond quoted by yield and the price it implies, the theta of a
/// marked deal recorded beside its mark, hedge relationships designated, assessed and dedesignated, and the
/// attribution of every deal's result into new, carry and market move.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";

module {

  public type Day = Nat;
  public type HedgeId = Nat;   // the desk block index of #hedgeDesignated

  /// The hedge reserve the effective portion of a cash-flow hedge goes to.
  public type Policy = { hedgeReserve : Text };

  /// A cash-flow hedge of an amount of a floating item by a swap, measured against the hypothetical derivative
  /// (the hedging swap's own terms on the hedged amount, recorded at designation); a fair-value hedge of a fixed
  /// bond lot, whose carrying amount is adjusted by the hedged risk's change.
  public type HedgeKind = { #cashFlow : { hedgedAmount : Nat }; #fairValue };
  public func hedgeKindText(k : HedgeKind) : Text { switch (k) { case (#cashFlow(_)) "cashFlow"; case (#fairValue) "fairValue" } };
  public type HedgeState = { #designated; #dedesignated };

  public type Event = {
    #policySet : Policy;
    /// A bond quoted by yield: the clean price the yield implies, which the price curve of the day records.
    #yieldQuoted : { isin : Text; day : Day; yieldBps : Nat; priceMicro : Nat };
    /// The theta of a marked deal on the day: the mark from the previous day's data at the day, less the mark
    /// before the day's; carry, not market move.
    #thetaRecorded : { deal : TT.DealId; day : Day; theta : Int };
    #hedgeDesignated : { hedging : TT.DealId; hedged : TT.DealId; kind : HedgeKind; hypothetical : ?TT.Irs; hedgingMark : Int; hedgedValue : Int; day : Day };
    /// An assessment over a period: the hedging instrument's change, the hedged item's (or the hypothetical
    /// derivative's), the effectiveness in basis points, and the effective portion to the reserve or the basis
    /// adjustment to the carrying amount.
    #hedgeAssessed : { hedge : HedgeId; hedgingChange : Int; hedgedChange : Int; effectivenessBps : Nat; effective : Int; ineffective : Int; day : Day };
    #hedgeDedesignated : { hedge : HedgeId; reclassified : Int; day : Day };
  };

  public type Error = {
    #NoPolicy;
    #InvalidTerms : { reason : Text };
    #UnknownHedge : { hedge : HedgeId };
    #HedgeNotIn : { hedge : HedgeId; state : Text; wanted : Text };
    #NotMarkable : { deal : TT.DealId; reason : Text };
    #NoPrice : { isin : Text; day : Day };
  };

  public type HedgeView = { id : HedgeId; hedging : TT.DealId; hedged : TT.DealId; kind : Text; state : Text; designatedDay : Day; hedgingMark : Int; hedgedValue : Int; reserve : Int; basisAdjustment : Int; lastEffectivenessBps : Nat; lastAssessedDay : Day; lastBlock : Nat };
  /// A deal's result over a period: what it made on the day it was captured, what it carried (accruals,
  /// amortisation, coupons, theta), what the market moved, and the whole; new plus carry plus market is the
  /// whole by construction, so the residual reported is the rounding of exact figures summed, which is zero.
  public type AttributionView = { deal : TT.DealId; period : Text; currency : Text; new : Int; carry : Int; market : Int; total : Int; marks : Nat; accruals : Nat };
  public type Status = { hedges : Nat; openHedges : Nat; quotes : Nat; thetas : Nat; attributionRows : Nat };
}
