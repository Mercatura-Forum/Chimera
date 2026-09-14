#!/usr/bin/env python3
"""quantlib_oracle.py: QuantLib as a second oracle for the desk's valuation arithmetic.

The Python twin (`treasury_twin.py`) is Manticore's arithmetic bit for bit and is what the contract is held against;
QuantLib is an independent implementation with its own conventions. This oracle prices a corpus with both and
reports the differences against a declared precision per instrument class, with the convention mapping stated:

  * bonds (30/360 ISDA, ACT/365F, ACT/360; annual, semi-annual, quarterly): the dirty price at a yield on coupon
    dates from QuantLib's cash flows (the coupons by the bond's day count) discounted with QuantLib's InterestRate
    under the twin's stated model: simple interest over the first period's fraction by the bond's day count, then
    compounded per regular period (the periods measured on 30/360); the accrued interest at mid-period dates by the
    day count. ACT/ACT ICMA is not in the corpus: the twin's ICMA fraction of a partial period is the period's
    `1 / f`, Manticore's stated simplification, which QuantLib does not share.
  * FX forwards: the mark as the difference between the interpolated forward and the deal rate on the base
    amount, discounted simply on ACT/360 at the linearly interpolated zero rate; QuantLib's `LinearInterpolation`
    and `InterestRate(Simple, Actual360)` are the two pieces.
  * interest-rate swaps: NPV by QuantLib's discounting engine against a discount curve built from the simple zero
    rate interpolated linearly on tenor days (`LinearInterpolation`) and `InterestRate(Simple, Actual360)`, a
    factor per day (QuantLib's own `ZeroCurve` interpolates continuously compounded rates, which is not the twin's
    convention); the floating leg projected from the same curve with a fixing for the period that has started;
    schedules unadjusted on a null calendar; the twin rounds each leg amount to a minor unit, so the precision is a
    minor unit per period and a part in a hundred million of the notional.
  * FX options: Garman-Kohlhagen with continuous domestic and foreign rates on ACT/365 and a flat volatility; the
    twin computes in eighteen-decimal fixed point with Abramowitz and Stegun's 26.2.17 normal distribution (within
    7.5e-8), so the precision is that error on each of the premium's two terms.

Usage: quantlib_oracle.py --out oracle.json
"""
import argparse
import json
import random
from fractions import Fraction

import QuantLib as ql

import treasury_twin as T
import desk_world as W

counts = {}


def count(label, n):
    counts[label] = n
    print(f"count: {label} = {n}")
    assert n > 0, f"{label}: examined nothing"


EPOCH = ql.Date(1, 1, 1970)


def qd(day):
    return EPOCH + int(day)


CONV = {"A006": (ql.Thirty360(ql.Thirty360.ISDA), "30/360 ISDA"), "A004": (ql.Actual365Fixed(), "ACT/365F"), "A003": (ql.Actual360(), "ACT/360")}
FREQ = {1: ql.Annual, 2: ql.Semiannual, 4: ql.Quarterly}


def bond_corpus(rng):
    """Bonds across the three conventions, three frequencies, eight coupons and three tenors: 216 instruments."""
    out = []
    for conv in CONV:
        for cpy in (1, 2, 4):
            for coupon_bps in (500, 700, 900, 1100, 1300, 1500, 1700, 1900):
                for years in (2, 3, 5):
                    issue = W.day_of(2026, 1, 10) + rng.randint(0, 20) * 30
                    maturity = T.add_months(issue, 12 * years)
                    out.append({"conv": conv, "cpy": cpy, "coupon_bps": coupon_bps, "issue": issue, "maturity": maturity})
    return out


def bond_checks(bonds, rng):
    face = 100_000_000   # 100 of face in micro
    max_price_diff = 0.0
    max_accrued_diff = 0
    prices = accrueds = 0
    for b in bonds:
        dc, _ = CONV[b["conv"]]
        periods = T.coupon_periods(face, b["coupon_bps"], b["cpy"], b["conv"], b["issue"], b["maturity"])
        schedule = ql.Schedule(qd(b["issue"]), qd(b["maturity"]), ql.Period(FREQ[b["cpy"]]), ql.NullCalendar(), ql.Unadjusted, ql.Unadjusted, ql.DateGeneration.Forward, False)
        bond = ql.FixedRateBond(0, 100.0, schedule, [b["coupon_bps"] / 10_000], dc)
        # the dirty price at a yield on coupon dates: QuantLib's cash flows (the coupons by the bond's day count)
        # discounted with QuantLib's InterestRate under the twin's stated model, simple over the first period's
        # fraction by the bond's day count and compounded per regular period after it, the periods measured on 30/360
        flows = [(cf.date(), cf.amount()) for cf in bond.cashflows()]
        for p in periods[:-1]:
            d = p["end"]
            if d <= b["issue"]:
                continue
            y_bps = rng.choice([800, 1000, 1200, 1400, 1600])
            y = y_bps / 10_000
            twin = float(T.present_value(face, periods, b["conv"], b["cpy"], d, Fraction(y_bps, 10_000))) / 1_000_000
            simple = ql.InterestRate(y, dc, ql.Simple, ql.Annual)
            compounded = ql.InterestRate(y, ql.Thirty360(ql.Thirty360.ISDA), ql.Compounded, FREQ[b["cpy"]])
            first_end = None
            quant = 0.0
            for cf_date, amount in flows:
                if cf_date <= qd(d):
                    continue
                if first_end is None:
                    first_end = cf_date
                    df = simple.discountFactor(qd(d), first_end)
                else:
                    df = simple.discountFactor(qd(d), first_end) * compounded.discountFactor(first_end, cf_date)
                quant += amount * df
            diff = abs(twin - quant)
            max_price_diff = max(max_price_diff, diff)
            assert diff < 2e-5, (b, d, twin, quant)   # the twin rounds every coupon to a micro; up to twenty flows
            prices += 1
        # the accrued coupon at a day inside each period, by the day count
        for p in periods:
            d = p["start"] + (p["end"] - p["start"]) // 3
            twin_acc = T.accrued_coupon(face, b["coupon_bps"], b["conv"], periods, d, b["cpy"])
            ql.Settings.instance().evaluationDate = qd(d)
            quant_acc = ql.BondFunctions.accruedAmount(bond, qd(d)) * 1_000_000
            diff = abs(twin_acc - quant_acc)
            max_accrued_diff = max(max_accrued_diff, diff)
            assert diff <= 1.0, (b, d, twin_acc, quant_acc)
            accrueds += 1
    return prices, accrueds, max_price_diff, max_accrued_diff


def forward_checks(rng):
    """FX forwards: the mark against QuantLib's interpolation and simple discounting."""
    checks = 0
    max_diff = 0
    for i in range(60):
        pts = [(1, 20_000), (30, 600_000 + i * 1000), (90, 1_800_000), (180, 3_500_000), (365, 7_000_000)]
        zero = [(1, 2000), (30, 2050), (90, 2100 + i), (180, 2150), (365, 2200)]
        amount = 100_000_00 * rng.randint(1, 50)
        deal_rate = 48_000_000 + rng.randint(-500_000, 900_000)
        for j in range(20):
            spot = 48_000_000 + j * 37_000
            remaining = 5 + j * 13 + i % 7
            twin = T.forward_mark(True, amount, deal_rate, Fraction(spot), pts, zero, remaining)
            interp_pts = ql.LinearInterpolation([float(t) for t, _ in pts], [float(v) for _, v in pts])
            interp_zero = ql.LinearInterpolation([float(t) for t, _ in zero], [float(v) for _, v in zero])
            x = min(max(remaining, pts[0][0]), pts[-1][0])
            fwd = spot + interp_pts(float(x), True)
            r = interp_zero(float(min(max(remaining, zero[0][0]), zero[-1][0])), True) / 10_000
            df = ql.InterestRate(r, ql.Actual360(), ql.Simple, ql.Annual).discountFactor(remaining / 360.0)
            quant = (fwd - deal_rate) * amount / 1_000_000 * df
            diff = abs(twin - quant)
            max_diff = max(max_diff, diff)
            assert diff <= 1.0, (i, j, twin, quant)
            checks += 1
    return checks, max_diff


def swap_checks(rng):
    """Interest-rate swaps: NPV against a simply compounded zero curve, the current period fixed."""
    checks = 0
    max_diff = 0.0
    for i in range(40):
        notional = 1_000_000_00 * rng.randint(5, 80)
        fixed_bps = 1900 + i * 10
        spread_bps = rng.choice([0, 10, 25, 50])
        months = rng.choice([1, 3, 6])
        tenor_months = rng.choice([6, 12, 24])
        start = W.day_of(2026, 3, 2) + i
        maturity = T.add_months(start, tenor_months)
        periods = T.swap_periods(start, maturity, months)
        for j in range(20):
            day = start + (j * (maturity - start)) // 22
            zero = [(1, 2000 + j * 3), (30, 2050 + j * 3), (90, 2100 + j * 2), (180, 2150 + j), (365, 2200), (730, 2250)]
            fixing = 2000 + i
            twin = T.swap_mark(notional, True, fixed_bps, spread_bps, "A003", periods, zero, day, lambda s: fixing if s <= day else None)
            ql.Settings.instance().evaluationDate = qd(day)
            # the curve: the simple zero rate interpolated linearly on tenor days (QuantLib's LinearInterpolation), a
            # discount factor per day from QuantLib's InterestRate(Simple, Actual360); QuantLib's ZeroCurve would
            # interpolate continuously compounded rates, which is not the twin's convention
            interp = ql.LinearInterpolation([float(t) for t, _ in zero], [float(v) for _, v in zero])
            horizon = maturity - day + 1
            dfs = [1.0]
            for t in range(1, horizon + 1):
                r = interp(float(min(max(t, zero[0][0]), zero[-1][0])), True) / 10_000
                dfs.append(ql.InterestRate(r, ql.Actual360(), ql.Simple, ql.Annual).discountFactor(t / 360.0))
            curve = ql.DiscountCurve([qd(day + t) for t in range(0, horizon + 1)], dfs, ql.Actual360(), ql.NullCalendar())
            curve.enableExtrapolation()
            ts = ql.YieldTermStructureHandle(curve)
            index = ql.IborIndex("TWIN", ql.Period(months, ql.Months), 0, ql.EGPCurrency(), ql.NullCalendar(), ql.Unadjusted, False, ql.Actual360(), ts)
            schedule = ql.Schedule(qd(start), qd(maturity), ql.Period(months, ql.Months), ql.NullCalendar(), ql.Unadjusted, ql.Unadjusted, ql.DateGeneration.Forward, False)
            # the period that has started is fixed at the recorded fixing
            for p in periods:
                if p["start"] <= day < p["end"]:
                    index.addFixing(qd(p["start"]), fixing / 10_000, True)
            swap = ql.VanillaSwap(ql.Swap.Payer, notional / 100.0, schedule, fixed_bps / 10_000, ql.Actual360(), schedule, index, spread_bps / 10_000, ql.Actual360())
            swap.setPricingEngine(ql.DiscountingSwapEngine(ts))
            quant = swap.NPV() * 100.0   # back to minor units
            twin_v = -twin if False else twin
            diff = abs(twin_v - quant)
            max_diff = max(max_diff, diff)
            # one minor unit of rounding per fixed-leg amount, plus the discount of the whole
            assert diff <= len(periods) + 2 + notional * 1e-8, (i, j, twin, quant, diff)
            checks += 1
    return checks, max_diff


def option_checks(rng):
    """FX options: Garman-Kohlhagen premiums, continuous rates on ACT/365, flat volatility; relative precision."""
    checks = 0
    max_rel = 0.0
    for i in range(40):
        call = i % 2 == 0
        amount = 100_000_00 * rng.randint(1, 30)
        strike = 48_000_000 + rng.randint(-2_000_000, 2_000_000)
        for j in range(20):
            spot = 48_000_000 + j * 60_000
            days = 10 + j * 17
            dom, fgn, vol = 2000 + i, 500 + (i % 5) * 10, 1000 + j * 25
            twin = T.garman_kohlhagen(call, amount, spot, strike, dom, fgn, vol, days)
            ql.Settings.instance().evaluationDate = qd(20_500)
            S = spot / 1_000_000; K = strike / 1_000_000
            dc = ql.Actual365Fixed()
            spot_h = ql.QuoteHandle(ql.SimpleQuote(S))
            dom_ts = ql.YieldTermStructureHandle(ql.FlatForward(qd(20_500), dom / 10_000, dc, ql.Continuous))
            fgn_ts = ql.YieldTermStructureHandle(ql.FlatForward(qd(20_500), fgn / 10_000, dc, ql.Continuous))
            vol_ts = ql.BlackVolTermStructureHandle(ql.BlackConstantVol(qd(20_500), ql.NullCalendar(), vol / 10_000, dc))
            process = ql.GarmanKohlagenProcess(spot_h, fgn_ts, dom_ts, vol_ts)
            payoff = ql.PlainVanillaPayoff(ql.Option.Call if call else ql.Option.Put, K)
            exercise = ql.EuropeanExercise(qd(20_500 + days))
            option = ql.VanillaOption(payoff, exercise)
            option.setPricingEngine(ql.AnalyticEuropeanEngine(process))
            quant = option.NPV() * amount
            # the twin's normal distribution is Abramowitz and Stegun 26.2.17, within 7.5e-8 absolute: the premium's
            # two terms each carry that error on a value bounded by the spot or the strike
            tol = 7.5e-8 * (S + K) * amount + 2
            diff = abs(twin - quant)
            rel = diff / max(tol, 1.0)
            max_rel = max(max_rel, rel)
            assert diff <= tol, (i, j, twin, quant, diff, tol)
            checks += 1
    return checks, max_rel


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    rng = random.Random(29)
    print(f"QuantLib {ql.__version__} as the second oracle")
    bonds = bond_corpus(rng)
    prices, accrueds, mp, ma = bond_checks(bonds, rng)
    count("bonds in the corpus (30/360, ACT/365F, ACT/360; annual, semi-annual, quarterly)", len(bonds))
    count("bond dirty prices at a yield equal to QuantLib on coupon dates, within two hundred-thousandths per 100 (a micro per coupon rounded)", prices)
    count("accrued coupons equal to QuantLib at mid-period dates, within a micro", accrueds)
    fwd_n, fwd_d = forward_checks(rng)
    count("FX forward marks equal to QuantLib's interpolation and simple discounting, within a minor unit", fwd_n)
    swap_n, swap_d = swap_checks(rng)
    count("interest-rate swap values equal to QuantLib's discounting engine, within a minor unit per period and a part in a hundred million of the notional", swap_n)
    opt_n, opt_r = option_checks(rng)
    count("Garman-Kohlhagen premiums equal to QuantLib's analytic engine, within the twin's stated normal-distribution error on each term", opt_n)
    evidence = {"quantlib": ql.__version__, "counts": counts, "max_differences": {"bond_price_per_100": mp, "accrued_micro": ma, "forward_minor": fwd_d, "swap_minor": swap_d, "option_share_of_tolerance": opt_r},
                "conventions": {"bonds": "QuantLib cash flows by the bond's day count, discounted simple over the first period then compounded per regular period on 30/360, the twin's model; ICMA excluded (the twin's partial-period fraction is 1/f)",
                                "forwards": "linear interpolation of points and zero rates on tenor days; simple discounting ACT/360",
                                "swaps": "DiscountCurve from the simple zero rate interpolated linearly on tenor days, a factor per day; the floating leg projected from the same curve; the started period at its fixing; unadjusted schedules on a null calendar",
                                "options": "continuous domestic and foreign rates on ACT/365; flat Black volatility; the twin in 18-decimal fixed point"}}
    json.dump(evidence, open(a.out, "w"), indent=2)
    print(f"max differences: {evidence['max_differences']}")
    print("QUANTLIB ORACLE GREEN")


if __name__ == "__main__":
    main()
