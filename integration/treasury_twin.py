"""treasury_twin.py - the Python twin of src/bank/TreasuryMath.mo (S3.2).

Every function here follows the Motoko function of the same name step for step: exact rationals
(`fractions.Fraction`) rounded half-even once, and 18-decimal fixed point with truncation toward zero for
the Garman-Kohlhagen price. The twin is the oracle the treasury battery compares the contract's figures
against, and `tools/gen_treasury_vectors.py` writes its outputs into `test/TreasuryVectors.mo` so the unit
suite proves the two agree without a replica.

Conventions (stated once, as in the Motoko header): rates in micro = quote minor per base minor x 1e6; zero
rates and volatilities in bps; bond prices in micro per 100 of face; discount factors by simple compounding
ACT/360; the option's time ACT/365.
"""
from __future__ import annotations

from fractions import Fraction
from typing import Callable, List, Optional, Sequence, Tuple

MICRO = 1_000_000
BPS = 10_000
ONE = 10**18
LN2 = 693_147_180_559_945_309
INV_SQRT_2PI = 398_942_280_401_432_678

# ─── civil dates (mo:journal/CivilDate: day 0 = 1970-01-01) ───────────────────────────────────────────


def is_leap(y: int) -> bool:
    return y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)


def days_in_month(y: int, m: int) -> int:
    if m == 2:
        return 29 if is_leap(y) else 28
    return 30 if m in (4, 6, 9, 11) else 31


def from_civil(y: int, m: int, d: int) -> int:
    import datetime
    return (datetime.date(y, m, d) - datetime.date(1970, 1, 1)).days


def to_civil(day: int) -> Tuple[int, int, int]:
    import datetime
    dt = datetime.date(1970, 1, 1) + datetime.timedelta(days=day)
    return dt.year, dt.month, dt.day


def add_months(d: int, n: int) -> int:
    y, m, day = to_civil(d)
    total = (m - 1) + n
    y2 = y + total // 12
    m2 = total % 12 + 1
    mx = days_in_month(y2, m2)
    return from_civil(y2, m2, min(day, mx))


# ─── day counts (DayCount.mo) ──────────────────────────────────────────────────────────────────────────
# conventions: "A001" ACT/ACT ICMA (needs coupons_per_year), "A003" ACT/360, "A004" ACT/365F,
# "A005" ACT/ACT ISDA, "A006" 30/360 ISDA, "A007" 30E/360, "A011" 30/365


def _thirty360(frm: int, to: int, eurobond: bool) -> int:
    y1, m1, d1 = to_civil(frm)
    y2, m2, d2 = to_civil(to)
    if eurobond:
        d1 = min(d1, 30)
        d2 = min(d2, 30)
    else:
        if d1 == 31:
            d1 = 30
        if d2 == 31 and d1 == 30:
            d2 = 30
    months = 12 * (y2 - y1) + m2 - m1
    return 30 * months + (d2 - d1)


def fraction(conv: str, frm: int, to: int, coupons_per_year: int = 1) -> Fraction:
    if to <= frm:
        return Fraction(0)
    days = to - frm
    if conv == "A003":
        return Fraction(days, 360)
    if conv == "A004":
        return Fraction(days, 365)
    if conv == "A005":
        D = 365 * 366
        num = 0
        cursor = frm
        while cursor < to:
            y, _, _ = to_civil(cursor)
            year_end = from_civil(y + 1, 1, 1)
            seg_end = min(year_end, to)
            num += (seg_end - cursor) * (D // (366 if is_leap(y) else 365))
            cursor = seg_end
        return Fraction(num, D)
    if conv == "A001":
        f = coupons_per_year if coupons_per_year else 1
        return Fraction(days, f * days)
    if conv == "A006":
        return Fraction(_thirty360(frm, to, False), 360)
    if conv == "A007":
        return Fraction(_thirty360(frm, to, True), 360)
    if conv == "A011":
        return Fraction(_thirty360(frm, to, False), 365)
    raise ValueError(conv)


# ─── exact rationals ───────────────────────────────────────────────────────────────────────────────────


def round_half_even(x: Fraction) -> int:
    """Banker's rounding on the signed value; Python's round() on a Fraction is exactly that."""
    return int(round(x))


def round_nat(x: Fraction) -> int:
    r = round_half_even(x)
    return r if r > 0 else 0


def interpolate(points: Sequence[Tuple[int, int]], tenor: int) -> Fraction:
    n = len(points)
    if n == 0:
        return Fraction(0)
    if tenor <= points[0][0]:
        return Fraction(points[0][1])
    if tenor >= points[-1][0]:
        return Fraction(points[-1][1])
    for i in range(1, n):
        t1, v1 = points[i]
        if tenor <= t1:
            t0, v0 = points[i - 1]
            if t1 == t0:
                return Fraction(v1)
            return Fraction(v0) + Fraction((v1 - v0) * (tenor - t0), t1 - t0)
    return Fraction(points[-1][1])


def discount_factor(rate_bps: Fraction, days: int) -> Fraction:
    rt = rate_bps * days / (BPS * 360)
    return 1 / (1 + rt)


def discount_on(zero_curve, days: int) -> Fraction:
    return discount_factor(interpolate(zero_curve, days), days)


def rate_micro(numerator: int, denominator: int) -> Fraction:
    return Fraction(numerator * MICRO, denominator)


def quote_amount(base_amount: int, rate_micro_: int) -> int:
    return round_nat(Fraction(base_amount * rate_micro_, MICRO))


def forward_mark(buy_base: bool, base_amount: int, deal_rate_micro: int, spot: Fraction, forward_points, quote_zero, remaining_days: int) -> int:
    fwd = spot + interpolate(forward_points, remaining_days)
    diff = fwd - deal_rate_micro
    undiscounted = diff * base_amount / MICRO
    pv = undiscounted * discount_on(quote_zero, remaining_days)
    m = round_half_even(pv)
    return m if buy_base else -m


def forward_realised(buy_base: bool, base_amount: int, deal_rate_micro: int, spot: Fraction) -> int:
    at_spot = round_half_even(spot * base_amount / MICRO)
    contractual = quote_amount(base_amount, deal_rate_micro)
    return at_spot - contractual if buy_base else contractual - at_spot


# ─── money market ──────────────────────────────────────────────────────────────────────────────────────


def simple_interest_to(principal: int, rate_bps: int, conv: str, start: int, day: int, cpy: int = 1) -> int:
    if day <= start:
        return 0
    return round_nat(Fraction(principal * rate_bps, BPS) * fraction(conv, start, day, cpy))


# ─── fixed income ──────────────────────────────────────────────────────────────────────────────────────


def coupon_amount(nominal: int, coupon_bps: int, conv: str, frm: int, to: int, cpy: int) -> int:
    return round_nat(Fraction(nominal * coupon_bps) * fraction(conv, frm, to, cpy) / BPS)


def coupon_periods(nominal: int, coupon_bps: int, cpy: int, conv: str, issue: int, maturity: int) -> List[dict]:
    if cpy == 0 or 12 % cpy != 0:
        return []
    step = 12 // cpy
    out = []
    k = 1
    start = issue
    while True:
        end = add_months(issue, k * step)
        if end > maturity:
            return []
        out.append({"start": start, "end": end, "amount": coupon_amount(nominal, coupon_bps, conv, start, end, cpy)})
        if end == maturity:
            break
        start = end
        k += 1
        if k > 1200:
            return []
    return out


def period_of(periods, day: int):
    for p in periods:
        if p["start"] <= day < p["end"]:
            return p
    return None


def accrued_coupon(nominal: int, coupon_bps: int, conv: str, periods, day: int, cpy: int) -> int:
    p = period_of(periods, day)
    return coupon_amount(nominal, coupon_bps, conv, p["start"], day, cpy) if p else 0


def clean_cost(nominal: int, price_micro: int) -> int:
    return round_nat(Fraction(nominal * price_micro, 100 * MICRO))


def present_value(face: int, periods, conv: str, cpy: int, settlement: int, y: Fraction) -> Fraction:
    pv = Fraction(0)
    df = Fraction(1)
    first = True
    per_period = 1 / (1 + y / cpy)
    for i, p in enumerate(periods):
        if p["end"] > settlement:
            if first:
                stub_start = max(settlement, p["start"])
                s = fraction(conv, stub_start, p["end"], cpy)
                df = 1 / (1 + y * s)
                first = False
            else:
                df = df * per_period
            cf = p["amount"] + (face if i + 1 == len(periods) else 0)
            pv += cf * df
    return pv


def effective_yield_millionths(face: int, periods, conv: str, cpy: int, settlement: int, dirty_cost: int) -> int:
    lo, hi, it = 0, 2_000_000, 0
    while it < 40 and lo < hi:
        mid = (lo + hi) // 2
        pv = present_value(face, periods, conv, cpy, settlement, Fraction(mid, MICRO))
        if pv > dirty_cost:
            lo = mid + 1
        else:
            hi = mid
        it += 1
    return lo


def _accrued_before(p, conv: str, settlement: int, cpy: int) -> int:
    whole = fraction(conv, p["start"], p["end"], cpy)
    if whole == 0:
        return 0
    part = fraction(conv, p["start"], settlement, cpy)
    a = round_nat(Fraction(p["amount"]) * part / whole)
    return min(a, p["amount"])


def amortisation_schedule(face: int, periods, conv: str, cpy: int, settlement: int, clean: int, y_millionths: int) -> List[dict]:
    y = Fraction(y_millionths, MICRO)
    carrying = clean
    out = []
    first = True
    remaining = [p for p in periods if p["end"] > settlement]
    for i, p in enumerate(remaining):
        last = i + 1 == len(remaining)
        stub_start = settlement if (first and settlement > p["start"]) else p["start"]
        frac = fraction(conv, stub_start, p["end"], cpy) if first else Fraction(1, cpy)
        interest = round_nat(Fraction(carrying) * y * frac)
        coupon_part = p["amount"] - _accrued_before(p, conv, settlement, cpy) if (first and settlement > p["start"]) else p["amount"]
        amort = interest - coupon_part
        if last:
            amort = face - carrying
        carrying += amort
        out.append({"start": stub_start, "end": p["end"], "interest": interest, "couponPart": coupon_part, "amortisation": amort, "carryingEnd": abs(carrying)})
        first = False
    return out


def amortisation_to(step: dict, day: int) -> int:
    if day <= step["start"]:
        return 0
    if day >= step["end"]:
        return step["amortisation"]
    span = step["end"] - step["start"]
    return round_half_even(Fraction(step["amortisation"] * (day - step["start"]), span))


# ─── interest-rate swaps ───────────────────────────────────────────────────────────────────────────────


def swap_periods(start: int, maturity: int, payment_months: int) -> List[dict]:
    if payment_months == 0:
        return []
    out = []
    k, s = 1, start
    while True:
        e = add_months(start, k * payment_months)
        if e > maturity:
            return []
        out.append({"start": s, "end": e})
        if e == maturity:
            break
        s, k = e, k + 1
        if k > 1200:
            return []
    return out


def leg_amount(notional: int, rate_bps: Fraction, conv: str, p: dict, cpy: int = 1) -> int:
    return round_half_even(Fraction(notional) * rate_bps * fraction(conv, p["start"], p["end"], cpy) / BPS)


def implied_forward_bps(zero_curve, days0: int, days1: int, frac: Fraction) -> Fraction:
    if frac == 0:
        return Fraction(0)
    ratio = discount_on(zero_curve, days0) / discount_on(zero_curve, days1)
    return (ratio - 1) / frac * BPS


def _projected(notional: int, spread_bps: int, conv: str, p: dict, zero_curve, day: int, frac: Fraction, cpy: int) -> Fraction:
    d0 = p["start"] - day if p["start"] > day else 0
    fwd = implied_forward_bps(zero_curve, d0, p["end"] - day, frac) + spread_bps
    return Fraction(notional) * fwd * fraction(conv, p["start"], p["end"], cpy) / BPS


def swap_mark(notional: int, pay_fixed: bool, fixed_bps: int, spread_bps: int, conv: str, periods, zero_curve, day: int,
              fixing_for: Callable[[int], Optional[int]], cpy: int = 1) -> int:
    pv = Fraction(0)
    for p in periods:
        if p["end"] > day:
            frac = fraction(conv, p["start"], p["end"], cpy)
            fixed = Fraction(leg_amount(notional, Fraction(fixed_bps), conv, p, cpy))
            if p["start"] <= day:
                f = fixing_for(p["start"])
                floating = Fraction(leg_amount(notional, Fraction(f + spread_bps), conv, p, cpy)) if f is not None \
                    else _projected(notional, spread_bps, conv, p, zero_curve, day, frac, cpy)
            else:
                floating = _projected(notional, spread_bps, conv, p, zero_curve, day, frac, cpy)
            pv += (floating - fixed) * discount_on(zero_curve, p["end"] - day)
    m = round_half_even(pv)
    return m if pay_fixed else -m


# ─── deterministic fixed point and Garman-Kohlhagen ─────────────────────────────────────────────────────


def tdiv(a: int, b: int) -> int:
    """Division truncating toward zero, as Motoko's Int division."""
    if b == 0:
        return 0
    q = abs(a) // abs(b)
    return q if (a >= 0) == (b >= 0) else -q


def fmul(a: int, b: int) -> int:
    return tdiv(a * b, ONE)


def fdiv(a: int, b: int) -> int:
    return 0 if b == 0 else tdiv(a * ONE, b)


def isqrt(n: int) -> int:
    if n < 2:
        return n
    x = n
    y = (x + 1) // 2
    while y < x:
        x = y
        y = (x + n // x) // 2
    return x


def fsqrt(a: int) -> int:
    return 0 if a <= 0 else isqrt(a * ONE)


def fexp(x: int) -> int:
    r = tdiv(x, 256)
    term = ONE
    s = ONE
    for i in range(1, 25):
        term = tdiv(fmul(term, r), i)
        s += term
    for _ in range(8):
        s = fmul(s, s)
    return s


def fln(x: int) -> int:
    if x <= 0:
        return 0
    m = x
    e = 0
    while m >= 2 * ONE:
        m = tdiv(m, 2)
        e += 1
    while m < ONE:
        m = m * 2
        e -= 1
    z = fdiv(m - ONE, m + ONE)
    z2 = fmul(z, z)
    term = z
    s = 0
    for i in range(40):
        s += tdiv(term, 2 * i + 1)
        term = fmul(term, z2)
    return e * LN2 + 2 * s


def norm_cdf(x: int) -> int:
    ax = -x if x < 0 else x
    t = fdiv(ONE, ONE + fmul(231_641_900_000_000_000, ax))
    b1 = 319_381_530_000_000_000
    b2 = -356_563_782_000_000_000
    b3 = 1_781_477_937_000_000_000
    b4 = -1_821_255_978_000_000_000
    b5 = 1_330_274_429_000_000_000
    poly = fmul(t, b1 + fmul(t, b2 + fmul(t, b3 + fmul(t, b4 + fmul(t, b5)))))
    pdf = fmul(INV_SQRT_2PI, fexp(tdiv(-fmul(ax, ax), 2)))
    upper = ONE - fmul(pdf, poly)
    return ONE - upper if x < 0 else upper


def garman_kohlhagen(call: bool, base_amount: int, spot_micro: int, strike_micro: int, domestic_bps: int, foreign_bps: int, vol_bps: int, days: int) -> int:
    if spot_micro <= 0:
        return 0
    if days == 0 or vol_bps == 0:
        intrinsic = spot_micro - strike_micro if call else strike_micro - spot_micro
        if intrinsic <= 0:
            return 0
        return abs(tdiv(intrinsic * base_amount, MICRO))
    T = fdiv(days * ONE, 365 * ONE)
    sigma = fdiv(vol_bps * ONE, BPS * ONE)
    rd = fdiv(domestic_bps * ONE, BPS * ONE)
    rf = fdiv(foreign_bps * ONE, BPS * ONE)
    S = tdiv(spot_micro * ONE, MICRO)
    K = tdiv(strike_micro * ONE, MICRO)
    sqrt_t = fsqrt(T)
    sig_t = fmul(sigma, sqrt_t)
    ln_sk = fln(fdiv(S, K))
    drift = fmul(rd - rf + tdiv(fmul(sigma, sigma), 2), T)
    d1 = fdiv(ln_sk + drift, sig_t)
    d2 = d1 - sig_t
    df_d = fexp(-fmul(rd, T))
    df_f = fexp(-fmul(rf, T))
    if call:
        price = fmul(fmul(S, df_f), norm_cdf(d1)) - fmul(fmul(K, df_d), norm_cdf(d2))
    else:
        price = fmul(fmul(K, df_d), norm_cdf(-d2)) - fmul(fmul(S, df_f), norm_cdf(-d1))
    if price <= 0:
        return 0
    return abs(tdiv(price * base_amount, ONE))


if __name__ == "__main__":
    # a smoke check against closed forms where they exist
    import math
    assert abs(fexp(ONE) / ONE - math.e) < 1e-12, fexp(ONE) / ONE
    assert abs(fln(2 * ONE) / ONE - math.log(2)) < 1e-15
    assert abs(norm_cdf(ONE) / ONE - 0.8413447460685429) < 1e-7
    assert abs(fsqrt(4 * ONE) - 2 * ONE) == 0
    # GK vs a float reference
    from math import erf, exp, log, sqrt
    def N(x): return 0.5 * (1 + erf(x / sqrt(2)))
    S, K, rd, rf, v, T = 48.0, 50.0, 0.20, 0.05, 0.15, 90 / 365
    d1 = (log(S / K) + (rd - rf + v * v / 2) * T) / (v * sqrt(T)); d2 = d1 - v * sqrt(T)
    ref = S * exp(-rf * T) * N(d1) - K * exp(-rd * T) * N(d2)
    got = garman_kohlhagen(True, 100_000_00, 48_000_000, 50_000_000, 2000, 500, 1500, 90) / 100_000_00
    assert abs(got - ref) < 1e-5, (got, ref)
    print("treasury twin smoke: ok", got, ref)
