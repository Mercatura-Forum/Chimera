/// Valuation.mo: the desk's own valuation over Manticore's arithmetic, with the market data injected: the mark of
/// a deal at a day from any curves and any spot, which is what a theta needs (the previous day's data at the
/// current day); a bond's price from a yield by the inversion of the effective-interest present value, so a bond
/// quoted by yield and a bond quoted by price agree when the yield is the price's.
///
/// The formulas are `TreasuryMath`'s and the dispatch mirrors `TreasuryCore.planMark` kind by kind, so the mark
/// from the day's own data equals Manticore's posted mark; the batteries hold the two against each other.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Int "mo:core/Int";
import Nat "mo:core/Nat";
import Result "mo:core/Result";

import DC "mo:manticore/DayCount";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import M "mo:manticore/TreasuryMath";
import Fx "mo:manticore/Fx";

module {

  /// What a mark reads: a curve by id and kind, a spot by currency, a fixing by index and day.
  public type Data = {
    curve : (Text, TT.CurveKind) -> ?[(Nat, Int)];
    spot : Text -> ?Fx.Rate;
    fixing : (Text, Nat) -> ?Nat;
  };
  public type Error = { #NoCurve : { curve : Text }; #NoSpot : { currency : Text }; #UnknownSecurity : { isin : Text }; #NotMarked };
  type Res<X> = Result.Result<X, Error>;

  func spotQ(r : Fx.Rate) : M.Q { M.rateMicro(r.numerator, r.denominator) };

  /// The mark of a deal at `day` from the data given: the figure Manticore's `#marked` records as `value` for
  /// the kind (a forward's or swap's mark, an option's value signed by the side, a lot's fair-value adjustment
  /// over its carrying amount); none for a kind or a state that carries no mark.
  public func markWith(t : TreasuryCore.State, r : TreasuryCore.DealRow, kind : TT.DealKind, day : Nat, data : Data) : Res<?Int> {
    func curveOf(id : Text, k : TT.CurveKind) : Res<[(Nat, Int)]> { switch (data.curve(id, k)) { case (?c) #ok(c); case null #err(#NoCurve({ curve = id })) } };
    func spotOf(ccy : Text) : Res<Fx.Rate> { switch (data.spot(ccy)) { case (?s) #ok(s); case null #err(#NoSpot({ currency = ccy })) } };
    func forwardValue(f : TT.FxForward) : Res<Int> {
      if (day >= f.valueDate) return #ok(0);
      let rate = switch (spotOf(f.base)) { case (#err(e)) return #err(e); case (#ok(x)) x };
      let pts = switch (curveOf(f.pointsCurve, #forwardPoints)) { case (#err(e)) return #err(e); case (#ok(x)) x };
      let zero = switch (curveOf(f.discountCurve, #zeroRates)) { case (#err(e)) return #err(e); case (#ok(x)) x };
      #ok(M.forwardMark(f.direction == #buy, f.baseAmount, f.rateMicro, spotQ(rate), pts, zero, f.valueDate - day))
    };
    switch (kind) {
      case (#fxForward(f)) { if (TreasuryCore.legSettled(r, 0)) return #ok(null); switch (forwardValue(f)) { case (#err(e)) #err(e); case (#ok(v)) #ok(?v) } };
      case (#fxSwap(x)) {
        var v : Int = 0;
        if (not TreasuryCore.legSettled(r, 0)) v += (switch (forwardValue(x.near)) { case (#err(e)) return #err(e); case (#ok(m)) m });
        if (not TreasuryCore.legSettled(r, 1)) v += (switch (forwardValue(x.far)) { case (#err(e)) return #err(e); case (#ok(m)) m });
        #ok(?v)
      };
      case (#irs(i)) {
        if (day >= i.maturity) return #ok(null);
        let zero = switch (curveOf(i.discountCurve, #zeroRates)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        #ok(?M.swapMark(i.notional, i.payFixed, i.fixedBps, i.spreadBps, i.dayCount, TreasuryCore.swapPeriodsOf(i), zero, day, func(start : Nat) : ?Nat { data.fixing(i.floatingIndex, start) }))
      };
      case (#fxOption(o)) {
        if (not TreasuryCore.legSettled(r, 0) or day >= o.expiry) return #ok(null);
        let rate = switch (spotOf(o.base)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        let tenor = o.expiry - day;
        let dom = switch (curveOf(o.domesticCurve, #zeroRates)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        let fgn = switch (curveOf(o.foreignCurve, #zeroRates)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        let vol = switch (curveOf(o.volCurve, #volatility)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        let v = M.garmanKohlhagen(o.call, o.baseAmount, M.roundHalfEven(spotQ(rate)), o.strikeMicro, M.roundHalfEven(M.interpolate(dom, tenor)), M.roundHalfEven(M.interpolate(fgn, tenor)), M.roundNat(M.interpolate(vol, tenor)), tenor);
        #ok(?(if (o.bought) v else -(v : Int)))
      };
      case (#security(s)) {
        let marked = (r.flags & TreasuryCore.F_FVOCI) != 0 or (r.flags & TreasuryCore.F_FVTPL) != 0;
        if (s.direction != #buy or not TreasuryCore.legSettled(r, 0) or r.nominalLeft == 0 or not marked) return #ok(null);
        let ?sec = TreasuryCore.security(t, s.isin) else return #err(#UnknownSecurity({ isin = s.isin }));
        if (day >= sec.maturity) return #ok(null);
        let px = switch (curveOf(s.priceCurve, #securityPrice)) { case (#err(e)) return #err(e); case (#ok(x)) x };
        let price = M.roundNat(M.interpolate(px, 0));
        let fv : Int = M.cleanCost(r.nominalLeft, price);
        #ok(?(fv - ((r.costLeft : Int) + r.amortisedPosted)))
      };
      case (#moneyMarket(_)) #ok(null);
    }
  };

  /// The data as recorded for a day: the latest curve on or before it, the spot of the day, the fixing recorded.
  public func dataOn(t : TreasuryCore.State, day : Nat, spot : (Text, Nat) -> ?Fx.Rate, fixing : (Text, Nat) -> ?Nat) : Data {
    {
      curve = func(id : Text, k : TT.CurveKind) : ?[(Nat, Int)] { switch (TreasuryCore.curveOn(t, id, day)) { case (?c) { if (c.kind == k) ?c.points else null }; case null null } };
      spot = func(ccy : Text) : ?Fx.Rate { spot(ccy, day) };
      fixing;
    }
  };

  /// A bond's clean price per 100, in micro, at a yield in basis points on a settlement day: the dirty price is
  /// the present value of the cash flows per 100 of face at the yield by the effective-interest arithmetic, the
  /// clean price that less the coupon accrued to the day; one rounding of the exact rational.
  public func priceOfYield(sec : TreasuryCore.SecurityRow, yieldBps : Nat, day : Nat) : ?Nat {
    if (day >= sec.maturity) return null;
    let face = 100 * 1_000_000;   // 100 of face in micro
    let periods = TreasuryCore.couponPeriodsOf(sec, face);
    let conv = TreasuryCore.conventionOf(sec);
    let dirty = M.presentValue(face, periods, conv, sec.couponsPerYear, day, M.q(yieldBps, 10_000));
    let accrued = M.accruedCoupon(face, sec.couponBps, conv, periods, day);
    let clean = M.roundHalfEven(dirty) - (accrued : Int);
    if (clean <= 0) null else ?Int.abs(clean)
  };
  /// The yield in basis points that prices the bond at a clean price, by the same bisection as the effective
  /// yield of a lot, on the price per 100 (the dirty price is the clean plus the accrued coupon).
  public func yieldOfPrice(sec : TreasuryCore.SecurityRow, priceMicro : Nat, day : Nat) : ?Nat {
    if (day >= sec.maturity) return null;
    let face = 100 * 1_000_000;
    let periods = TreasuryCore.couponPeriodsOf(sec, face);
    let conv = TreasuryCore.conventionOf(sec);
    let dirty = priceMicro + M.accruedCoupon(face, sec.couponBps, conv, periods, day);
    let millionths = M.effectiveYieldMillionths(face, periods, conv, sec.couponsPerYear, day, dirty);
    ?(millionths / 100)
  };
}
