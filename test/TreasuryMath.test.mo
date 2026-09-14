/// TreasuryMath.test.mo - S3.2 treasury arithmetic against its Python twin.
///
/// Every figure in `TreasuryVectors.mo` was computed by `integration/treasury_twin.py` (via
/// `tools/gen_treasury_vectors.py`); this test recomputes each with `TreasuryMath.mo` and demands equality - the
/// rationals to the last unit of the reduced numerator and denominator, the rounded amounts exactly, the fixed-point
/// figures bit for bit. What is proved: curve interpolation and simple-compounded discounting; the mark, realised
/// result and quote amount of forwards; money-market interest under six day counts; bond coupon schedules, accrued
/// coupon, clean cost, the effective yield by bisection and the constant-yield amortisation schedule that closes to
/// face; swap marks with recorded fixings and projected floating periods; the fixed-point exp, ln and normal CDF;
/// the Garman–Kohlhagen value across moneyness, tenor and a zero volatility. Also the algebraic properties the twin
/// cannot vouch for on its own: a rational rounded is within a half of itself, the amortisation schedule ends at face,
/// a bond bought at par with a coupon equal to its yield amortises nothing, exp(ln x) returns x within the fixed
/// point's resolution, N(x) + N(−x) = 1 exactly.

import Debug "mo:core/Debug";
import Int "mo:core/Int";
import Nat "mo:core/Nat";
import Array "mo:core/Array";

import M "mo:manticore/TreasuryMath";
import V "TreasuryVectors";

func fail(what : Text) { Debug.print("FAIL: " # what); assert false };
func eqQ(a : M.Q, n : Int, d : Nat, what : Text) { if (a.n != n or a.d != d) fail(what # ": got " # debug_show (a.n, a.d) # " want " # debug_show (n, d)) };
func eqI(a : Int, b : Int, what : Text) { if (a != b) fail(what # ": got " # debug_show a # " want " # debug_show b) };
func eqN(a : Nat, b : Nat, what : Text) { if (a != b) fail(what # ": got " # debug_show a # " want " # debug_show b) };

var checks = 0;

// 1. curves
for ((pts, tenor, vn, vd, dn, dd) in V.CURVES.vals()) {
  eqQ(M.interpolate(pts, tenor), vn, vd, "interpolate");
  eqQ(M.discountOn(pts, tenor), dn, dd, "discountOn");
  checks += 2;
};
Debug.print("count: curve vectors equal to the twin = " # Nat.toText(V.CURVES.size()));

// 2. forwards
for ((buy, base, dealRate, sn, sd, fwdPts, zero, days, mark, realised, qa) in V.FORWARDS.vals()) {
  let spot = M.rateMicro(sn, sd);
  eqI(M.forwardMark(buy, base, dealRate, spot, fwdPts, zero, days), mark, "forwardMark");
  eqI(M.forwardRealised(buy, base, dealRate, spot), realised, "forwardRealised");
  eqN(M.quoteAmount(base, dealRate), qa, "quoteAmount");
  checks += 3;
};
Debug.print("count: forward vectors equal to the twin = " # Nat.toText(V.FORWARDS.size()));

// 3. money market
for ((conv, principal, rate, start, day, interest) in V.MONEY_MARKET.vals()) {
  eqN(M.simpleInterestTo(principal, rate, conv, start, day), interest, "simpleInterestTo");
  checks += 1;
};
Debug.print("count: money-market vectors equal to the twin = " # Nat.toText(V.MONEY_MARKET.size()));

// 4. bonds
var scheduleRows = 0;
for ((conv, cpy, nominal, coupon, issue, maturity, periods, settlement, price, clean, accrued, y, sched, probeDay, amortProbe) in V.BONDS.vals()) {
  let ps = M.couponPeriods(nominal, coupon, cpy, conv, issue, maturity);
  if (ps.size() != periods.size()) fail("couponPeriods size " # debug_show (ps.size(), periods.size()));
  var i = 0;
  while (i < ps.size()) {
    let (s, e, a) = periods[i];
    if (ps[i].start != s or ps[i].end != e or ps[i].amount != a) fail("coupon period " # Nat.toText(i));
    i += 1;
  };
  eqN(M.cleanCost(nominal, price), clean, "cleanCost");
  eqN(M.accruedCoupon(nominal, coupon, conv, ps, settlement), accrued, "accruedCoupon");
  eqN(M.effectiveYieldMillionths(nominal, ps, conv, cpy, settlement, clean + accrued), y, "effectiveYield");
  let steps = M.amortisationSchedule(nominal, ps, conv, cpy, settlement, clean, y);
  if (steps.size() != sched.size()) fail("schedule size " # debug_show (steps.size(), sched.size()));
  i := 0;
  while (i < steps.size()) {
    let (s, e, intr, cp, am, ce) = sched[i];
    let st = steps[i];
    if (st.start != s or st.end != e or st.interest != intr or st.couponPart != cp or st.amortisation != am or st.carryingEnd != ce) {
      fail("schedule step " # Nat.toText(i) # ": " # debug_show (st) # " want " # debug_show (s, e, intr, cp, am, ce));
    };
    scheduleRows += 1;
    i += 1;
  };
  // property: the schedule closes to face
  if (steps.size() > 0 and steps[steps.size() - 1].carryingEnd != nominal) fail("schedule does not close to face");
  var probe : Int = 0;
  for (st in steps.vals()) { if (probeDay >= st.start and probeDay < st.end) probe := M.amortisationTo(st, probeDay) };
  eqI(probe, amortProbe, "amortisationTo");
  checks += 5;
};
Debug.print("count: bond vectors equal to the twin = " # Nat.toText(V.BONDS.size()));
Debug.print("count: amortisation schedule rows equal to the twin = " # Nat.toText(scheduleRows));

// a par bond whose coupon equals its yield amortises nothing: 5 % annual, ACT/ACT ICMA, bought at 100 on issue
do {
  let issue = 20_700;
  let ps = M.couponPeriods(1_000_000_00, 500, 1, #a001_ActActIcma({ couponsPerYear = 1 }), issue, 21_796);
  // 20_700 = 2026-09-04; three annual periods end on 2029-09-04 = 21_796
  if (ps.size() != 3) fail("par bond periods " # debug_show (ps.size()));
  let y = M.effectiveYieldMillionths(1_000_000_00, ps, #a001_ActActIcma({ couponsPerYear = 1 }), 1, issue, 1_000_000_00);
  eqN(y, 50_000, "par yield is the coupon");
  let steps = M.amortisationSchedule(1_000_000_00, ps, #a001_ActActIcma({ couponsPerYear = 1 }), 1, issue, 1_000_000_00, y);
  for (st in steps.vals()) { if (st.amortisation != 0) fail("par bond amortises " # debug_show (st)) };
};
Debug.print("count: par bond amortising nothing = 1");

// 5. swaps
for ((conv, notional, payFixed, fixed, spread, periods, zero, day, fixings, mark) in V.SWAPS.vals()) {
  let ps = Array.map<(Nat, Nat), M.SwapPeriod>(periods, func((s, e)) { { start = s; end = e } });
  func fixingFor(start : Nat) : ?Nat { for ((s, b) in fixings.vals()) { if (s == start) return ?b }; null };
  eqI(M.swapMark(notional, payFixed, fixed, spread, conv, ps, zero, day, fixingFor), mark, "swapMark");
  checks += 1;
};
Debug.print("count: swap vectors equal to the twin = " # Nat.toText(V.SWAPS.size()));

// 6. fixed point
for ((x, y) in V.EXP.vals()) { eqI(M.fexp(x), y, "fexp"); checks += 1 };
for ((x, y) in V.LN.vals()) { eqI(M.fln(x), y, "fln"); checks += 1 };
for ((x, y) in V.NORM.vals()) {
  eqI(M.normCdf(x), y, "normCdf");
  eqI(M.normCdf(x) + M.normCdf(-x), M.ONE, "N(x) + N(-x) = 1");
  checks += 2;
};
Debug.print("count: fixed-point vectors equal to the twin = " # Nat.toText(V.EXP.size() + V.LN.size() + V.NORM.size()));
// exp(ln x) = x within the fixed point's resolution
for (x in [M.ONE, 2 * M.ONE, M.ONE / 3, 17 * M.ONE + 5].vals()) {
  let back = M.fexp(M.fln(x));
  let diff = Int.abs(back - x);
  if (diff > x / 1_000_000_000_000) fail("exp(ln x) drifted " # debug_show (x, back));
};
Debug.print("count: exp(ln x) round trips = 4");

// 7. options
for ((call, base, spot, strike, rd, rf, vol, days, value) in V.OPTIONS.vals()) {
  eqN(M.garmanKohlhagen(call, base, spot, strike, rd, rf, vol, days), value, "garmanKohlhagen");
  checks += 1;
};
// put–call parity in the deterministic model: C − P = S e^{−r_f T} − K e^{−r_d T} within a few units of truncation
do {
  let base = 100_000_00; let spot : Int = 48_000_000; let strike = 50_000_000;
  let c = M.garmanKohlhagen(true, base, spot, strike, 2000, 500, 1500, 90);
  let p = M.garmanKohlhagen(false, base, spot, strike, 2000, 500, 1500, 90);
  let T = M.fdiv(90 * M.ONE, 365 * M.ONE);
  let s = M.fmul(spot * M.ONE / 1_000_000, M.fexp(-M.fmul(M.fdiv(500 * M.ONE, 10_000 * M.ONE), T)));
  let k = M.fmul(strike * M.ONE / 1_000_000, M.fexp(-M.fmul(M.fdiv(2000 * M.ONE, 10_000 * M.ONE), T)));
  let parity = ((s - k) * base) / M.ONE;
  let lhs : Int = c; let rhs : Int = p;
  if (Int.abs((lhs - rhs) - parity) > 3) fail("put-call parity " # debug_show (c, p, parity));
};
Debug.print("count: option vectors equal to the twin = " # Nat.toText(V.OPTIONS.size()));
Debug.print("count: put-call parity held = 1");

// 8. rounding properties
for ((n, d) in [(7, 2), (-7, 2), (5, 2), (-5, 2), (1, 3), (-1, 3), (0, 5)].vals()) {
  let r = M.roundHalfEven(M.q(n, d));
  // |r·d − n| ≤ d/2
  if (2 * Int.abs(r * d - n) > d) fail("round " # debug_show (n, d, r));
  if (M.roundHalfEven(M.q(-n, d)) != -r) fail("round not symmetric " # debug_show (n, d));
};
eqI(M.roundHalfEven(M.q(5, 2)), 2, "half to even down");
eqI(M.roundHalfEven(M.q(7, 2)), 4, "half to even up");
eqI(M.roundHalfEven(M.q(-5, 2)), -2, "half to even negative");
Debug.print("count: rounding properties = 7");

Debug.print("count: figures compared with the twin = " # Nat.toText(checks));
Debug.print("TreasuryMath: all checks passed");
