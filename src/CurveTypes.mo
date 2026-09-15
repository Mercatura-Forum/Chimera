/// CurveTypes.mo: curve construction. A curve is built from the instruments that define it, not published as
/// points: a specification records the quotes (deposits, forward rate agreements, futures, par swaps, overnight
/// index swaps, tenor basis swaps, FX swaps) with the source hash of each, the day count, the interpolation and
/// the role (a discount curve per currency; a projection curve per index tenor, built with a discount curve
/// given; a discount curve for a currency under collateral in another, built from FX swaps against the
/// collateral currency's discount curve), and the build solves the
/// discount factor at every maturity in order, exactly where the instrument is linear in it and by a bounded
/// bracketed search on a fixed grid where it is not, so two builds of the same quotes give the same factors. The built
/// curve is a block of nodes; an index names the projection and the discount curve a swap on it reads.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import DC "mo:manticore/DayCount";

module {

  public type CurveId = Text;
  public type Day = Nat;

  /// A discount curve stands on its own; a projection curve projects the forwards of an index with a tenor in
  /// whole months and is built with a discount curve given; a collateral discount curve discounts the curve's
  /// currency under collateral posted in another currency and is built from FX swaps against that currency's
  /// discount curve, given as the discount curve.
  public type Role = { #discount; #projection : { indexMonths : Nat }; #collateralDiscount : { collateral : Text } };
  public func roleText(r : Role) : Text { switch (r) { case (#discount) "discount"; case (#projection(p)) "projection:" # debug_show p.indexMonths; case (#collateralDiscount(c)) "collateral:" # c.collateral } };

  /// Log-linear on discount factors (the forward rate constant between nodes), or linear on the simple zero rate
  /// the treasury domain's published curves carry.
  public type Interpolation = { #logLinearDiscount; #linearZero };
  public func interpolationText(i : Interpolation) : Text { switch (i) { case (#logLinearDiscount) "logLinearDiscount"; case (#linearZero) "linearZero" } };

  /// The instruments a curve is built from. Tenors in days are from the build day; swap tenors in whole months
  /// so their payment grids are Manticore's `swapPeriods`. A future is quoted as the rate its price implies (one
  /// hundred less the price, in basis points) with the convexity adjustment the desk declares for the contract. A
  /// tenor basis swap exchanges the index of the projection curve being built against the index of a reference
  /// projection curve, the spread quoted on the leg the instrument names. An FX swap is quoted as forward points
  /// against the spot it names, in millionths of the rate, the rate being units of the collateral currency per
  /// unit of the curve's currency.
  public type Instrument = {
    #deposit : { days : Nat };
    #fra : { startDays : Nat; endDays : Nat };
    #future : { startDays : Nat; endDays : Nat; convexityBps : Int };
    #swap : { months : Nat; fixedMonths : Nat; floatMonths : Nat };
    #ois : { months : Nat; fixedMonths : Nat };
    #basis : { months : Nat; reference : CurveId; spreadOnReference : Bool };
    #fxSwap : { days : Nat; spotMicro : Nat };
  };
  public func instrumentText(i : Instrument) : Text {
    switch (i) {
      case (#deposit(x)) "deposit " # debug_show x.days # "d";
      case (#fra(x)) "fra " # debug_show x.startDays # "x" # debug_show x.endDays;
      case (#future(x)) "future " # debug_show x.startDays # "x" # debug_show x.endDays;
      case (#swap(x)) "swap " # debug_show x.months # "m";
      case (#ois(x)) "ois " # debug_show x.months # "m";
      case (#basis(x)) "basis " # debug_show x.months # "m vs " # x.reference;
      case (#fxSwap(x)) "fx swap " # debug_show x.days # "d";
    }
  };

  /// A quote: the instrument, its figure (a rate or a spread in basis points; forward points in millionths of
  /// the rate for an FX swap), and the hash of the source that gave it.
  public type Quote = { instrument : Instrument; value : Int; source : Blob };

  public type Spec = {
    id : CurveId;
    currency : Text;
    role : Role;
    dayCount : DC.Convention;
    interpolation : Interpolation;
    /// The discount curve a projection curve is built with; none for a discount curve, which discounts itself.
    discountCurve : ?CurveId;
    quotes : [Quote];
  };

  /// A node of a built curve: the tenor in days from the build day and the discount factor in eighteen decimals.
  public type Node = { days : Nat; df : Nat };

  public type Event = {
    #curveBuilt : { spec : Spec; day : Day; nodes : [Node]; iterations : Nat };
    /// The curves a floating index reads: its projection curve and the discount curve, both built.
    #indexCurvesSet : { index : Text; projection : CurveId; discount : CurveId; day : Day };
    /// A swap marked on two curves: the desk's own multi-curve valuation beside the treasury's posted mark.
    #swapMarked : { deal : Nat; day : Day; discount : CurveId; projection : CurveId; value : Int };
  };

  public type Error = {
    #InvalidSpec : { reason : Text };
    #UnknownCurve : { curve : CurveId; day : Day };
    #NoDiscountCurve : { curve : CurveId; day : Day };
    #Gap : { instrument : Text; needs : Nat; last : Nat };
    #UnknownReference : { instrument : Text; curve : CurveId; day : Day };
    #NotConverged : { instrument : Text; iterations : Nat };
    #BeyondCurve : { curve : CurveId; days : Nat; last : Nat };
    #UnknownIndex : { index : Text };
    #NotASwap : { deal : Nat };
  };

  public type CurveView = { id : CurveId; currency : Text; role : Text; dayCount : Text; interpolation : Text; discountCurve : ?CurveId; day : Day; quotes : Nat; nodes : [Node]; iterations : Nat; block : Nat };
  public type IndexView = { index : Text; projection : CurveId; discount : CurveId; day : Day };
  public type Status = { specs : Nat; builds : Nat; nodes : Nat; indexes : Nat; swapMarks : Nat };
}
