"""curve_twin.py: the Python twin of src/CurveCore.mo, the bootstrap and the multi-curve swap mark, on the
treasury twin's exact arithmetic (rationals, the eighteen-decimal fixed point with the same exp and ln), so every
discount factor the desk records is reproduced integer for integer. `tools/gen_curve_vectors.py` writes the
twin's builds as Motoko constants for `test/Curve.test.mo`; the chain battery holds the desk's builds against
the twin and against QuantLib.
"""
from fractions import Fraction
from typing import Dict, List, Optional, Tuple

import hashlib
import struct

import treasury_twin as T

ONE = 10 ** 18
DF_MAX = 2 * ONE
BPS = 10_000
MAX_ITERATIONS = 64


class CurveError(Exception):
    pass


# ─── a spec: the same shape as CurveTypes.Spec, instruments as dicts ─────────────────────────────────────
# instrument: {"kind": "deposit", "days": n} | {"kind": "fra", "start": a, "end": b} | {"kind": "future", "start", "end", "convexity"}
#             | {"kind": "swap", "months": m, "fixed": fm, "float": flm} | {"kind": "ois", "months": m, "fixed": fm}
#             | {"kind": "basis", "months": m, "reference": id, "spreadOnReference": bool} | {"kind": "fxSwap", "days": n, "spot": micro}
# quote: {"instrument": ..., "value": bps (points in millionths for an FX swap)}


def maturity_days(day: int, i: dict) -> int:
    k = i["kind"]
    if k in ("deposit", "fxSwap"):
        return i["days"]
    if k in ("fra", "future"):
        return i["end"]
    return T.add_months(day, i["months"]) - day


def q_of_fixed(df: int) -> Fraction:
    return Fraction(df, ONE)


def fixed_of(q: Fraction) -> int:
    v = T.round_half_even(q * ONE)
    return 0 if v < 0 else v


class Curve:
    def __init__(self, day: int, conv: str, interpolation: str, nodes: List[Tuple[int, int]]):
        self.day, self.conv, self.interpolation, self.nodes = day, conv, interpolation, list(nodes)

    def zero_fixed(self, days: int, df: int) -> int:
        """A node's simple zero rate on the fixed grid: (1/DF - 1) / fraction in eighteen decimals."""
        frac = T.fraction(self.conv, self.day, self.day + days)
        if frac == 0:
            return 0
        return T.round_half_even((Fraction(1) / q_of_fixed(df) - 1) / frac * ONE)

    def between(self, t0: int, df0: int, t1: int, df1: int, t: int) -> Fraction:
        if self.interpolation == "logLinearDiscount":
            l0, l1 = T.fln(df0), T.fln(df1)
            l = T.tdiv(l0 * (t1 - t) + l1 * (t - t0), t1 - t0)
            return q_of_fixed(abs(T.fexp(l)))
        z0 = self.zero_fixed(t1, df1) if t0 == 0 else self.zero_fixed(t0, df0)
        z1 = self.zero_fixed(t1, df1)
        z = T.round_half_even(Fraction(z0 * (t1 - t) + z1 * (t - t0), t1 - t0))
        frac = T.fraction(self.conv, self.day, self.day + t)
        return q_of_fixed(abs(T.round_half_even(Fraction(ONE) / (1 + Fraction(z, ONE) * frac))))

    def discount_at(self, days: int) -> Fraction:
        if days == 0:
            return Fraction(1)
        n = len(self.nodes)
        if n == 0 or days > self.nodes[-1][0]:
            raise CurveError(f"beyond the curve: {days} past {self.nodes[-1][0] if n else 0}")
        for i, (nd, df) in enumerate(self.nodes):
            if days == nd:
                return q_of_fixed(df)
            if days < nd:
                t0, df0 = (0, ONE) if i == 0 else self.nodes[i - 1]
                return self.between(t0, df0, nd, df, days)
        return q_of_fixed(self.nodes[-1][1])

    def forward_bps(self, days0: int, days1: int) -> Fraction:
        d0, d1 = self.discount_at(days0), self.discount_at(days1)
        frac = T.fraction(self.conv, self.day + days0, self.day + days1)
        if frac == 0:
            return Fraction(0)
        return (d0 / d1 - 1) / frac * BPS

    def derived_zero_points(self) -> List[Tuple[int, int]]:
        """The treasury domain's zero points: the simple rate on actual over 360 (the treasury's one convention)
        that reproduces each node's factor, in whole basis points."""
        return [(d, (0 if d == 0 else T.round_half_even((Fraction(1) / q_of_fixed(df) - 1) * Fraction(360, d) * BPS))) for d, df in self.nodes]


def swap_value(day: int, conv: str, rate_bps: int, months: int, fixed_months: int, float_months: int, ois: bool, disc: Curve, proj: Curve) -> Fraction:
    maturity = T.add_months(day, months)
    fixed = Fraction(0)
    for p in T.swap_periods(day, maturity, fixed_months):
        fixed += disc.discount_at(p["end"] - day) * T.fraction(conv, p["start"], p["end"]) * Fraction(rate_bps, BPS)
    floating = Fraction(0)
    if ois:
        for p in T.swap_periods(day, maturity, fixed_months):
            floating += disc.discount_at(p["start"] - day) - disc.discount_at(p["end"] - day)
    else:
        for p in T.swap_periods(day, maturity, float_months):
            df = disc.discount_at(p["end"] - day)
            fwd = proj.forward_bps(p["start"] - day, p["end"] - day)
            floating += df * T.fraction(conv, p["start"], p["end"]) * (fwd / BPS)
    return fixed - floating


def basis_value(day: int, conv: str, spread_bps: int, months: int, spread_on_reference: bool, disc: Curve, built: Curve, built_months: int, reference: Curve, reference_months: int) -> Fraction:
    """CurveCore.basisValue: the reference index's leg less the built index's leg, both on the discount curve."""
    maturity = T.add_months(day, months)

    def leg(proj, tenor, spread):
        pv = Fraction(0)
        for p in T.swap_periods(day, maturity, tenor):
            df = disc.discount_at(p["end"] - day)
            fwd = proj.forward_bps(p["start"] - day, p["end"] - day)
            pv += df * T.fraction(conv, p["start"], p["end"]) * ((fwd + spread) / BPS)
        return pv
    return leg(reference, reference_months, spread_bps if spread_on_reference else 0) - leg(built, built_months, 0 if spread_on_reference else spread_bps)


def bisect(f, name: str, guess: int) -> Tuple[int, int]:
    """CurveCore.bisect: a bracket from the guess, widened by a sixty-fourth a step until the residual changes
    sign, then secant steps through the last two iterates on the integer grid, the bracket's midpoint when the
    secant leaves the bracket or stops halving the residual; the negative end of the closed bracket and the
    number of evaluations."""
    g = min(max(guess, 1), DF_MAX)
    fg = f(g)
    n = 1
    lo = hi = g
    flo = fhi = fg
    if fg < 0:
        while fhi < 0:
            if hi >= DF_MAX or n >= MAX_ITERATIONS:
                raise CurveError(f"{name}: no sign change across the grid")
            hi = min(hi + hi // 64 + 1, DF_MAX)
            fhi = f(hi); n += 1
    else:
        while flo >= 0:
            if lo <= 1 or n >= MAX_ITERATIONS:
                raise CurveError(f"{name}: no sign change across the grid")
            lo = max(lo - lo // 64 - 1, 1)
            flo = f(lo); n += 1
    # the secant through the last two iterates, the bracket's midpoint when the secant leaves the bracket or
    # two steps running fail to halve the residual; a step that rounds to the same integer moves one unit toward
    # the root so the bracket closes
    if abs(flo) < abs(fhi):
        a, fa, b, fb = hi, fhi, lo, flo
    else:
        a, fa, b, fb = lo, flo, hi, fhi
    slow = 0
    while hi - lo > 1:
        if n >= MAX_ITERATIONS:
            raise CurveError(f"{name}: not converged in {n}")
        m = (lo + hi) // 2
        if slow >= 2 or fb == fa:
            s = m
        else:
            s = b - T.round_half_even(fb * (b - a) / (fb - fa))
            if s == b:
                s = b + 1 if fb < 0 else b - 1
            if not (lo < s < hi):
                s = m
        fs = f(s); n += 1
        slow = 0 if abs(fs) * 2 <= abs(fb) else slow + 1
        a, fa, b, fb = b, fb, s, fs
        if fs < 0:
            lo, flo = s, fs
        else:
            hi, fhi = s, fs
    return lo, n


def bootstrap(spec: dict, day: int, discount: Optional[Curve], reference_of=None) -> Tuple[List[Tuple[int, int]], int]:
    """The nodes (days, df) and the residual evaluations; the same integers as CurveCore.bootstrap.
    `reference_of(id)` gives a basis swap's reference projection curve as (Curve, index months)."""
    conv, interp = spec["dayCount"], spec["interpolation"]
    quotes = sorted(spec["quotes"], key=lambda q: maturity_days(day, q["instrument"]))
    for a, b in zip(quotes, quotes[1:]):
        if maturity_days(day, a["instrument"]) == maturity_days(day, b["instrument"]):
            raise CurveError("two instruments mature on one day")
    built: List[Tuple[int, int]] = []
    iterations = 0

    def so_far(extra=None):
        return Curve(day, conv, interp, built + ([extra] if extra else []))

    for q in quotes:
        i = q["instrument"]
        t = maturity_days(day, i)
        name = i["kind"]
        last = built[-1][0] if built else 0
        last_df = built[-1][1] if built else ONE
        k = i["kind"]
        # the solver's first guess: the previous node's factor carried to this maturity at the quoted rate (at
        # the reference curve's forward for a basis swap, whose quote is a spread)
        carry = q["value"]
        if k == "basis":
            ref = reference_of(i["reference"]) if reference_of else None
            carry = 0
            if ref is not None:
                try:
                    carry = T.round_half_even(ref[0].forward_bps(last, t))
                except CurveError:
                    carry = 0
        guess = fixed_of(q_of_fixed(last_df) / (1 + Fraction(carry) * T.fraction(conv, day + last, day + t) / BPS))
        if k == "deposit":
            frac = T.fraction(conv, day, day + i["days"])
            df = Fraction(1) / (1 + Fraction(q["value"]) * frac / BPS)
            built.append((t, fixed_of(df)))
        elif k in ("fra", "future"):
            if i["start"] > last:
                raise CurveError(f"{name}: gap")
            d0 = so_far().discount_at(i["start"])
            frac = T.fraction(conv, day + i["start"], day + i["end"])
            rate = q["value"] - (i.get("convexity", 0) if k == "future" else 0)
            df = d0 / (1 + Fraction(rate) * frac / BPS)
            built.append((t, fixed_of(df)))
        elif k == "swap":
            if spec["role"] == "discount":
                def f(df, i=i, q=q, t=t):
                    c = so_far((t, df))
                    return swap_value(day, conv, q["value"], i["months"], i["fixed"], i["float"], False, c, c)
            else:
                if discount is None:
                    raise CurveError("no discount curve")

                def f(df, i=i, q=q, t=t):
                    return swap_value(day, conv, q["value"], i["months"], i["fixed"], i["float"], False, discount, so_far((t, df)))
            df, n = bisect(f, name, guess)
            built.append((t, df)); iterations += n
        elif k == "ois":
            def f(df, i=i, q=q, t=t):
                c = so_far((t, df))
                return swap_value(day, conv, q["value"], i["months"], i["fixed"], i["fixed"], True, c, c)
            df, n = bisect(f, name, guess)
            built.append((t, df)); iterations += n
        elif k == "basis":
            if discount is None:
                raise CurveError("no discount curve")
            ref = reference_of(i["reference"]) if reference_of else None
            if ref is None:
                raise CurveError(f"{name}: unknown reference {i['reference']}")
            rc, rm = ref

            def f(df, i=i, q=q, t=t):
                return basis_value(day, conv, q["value"], i["months"], i["spreadOnReference"], discount, so_far((t, df)), spec["indexMonths"], rc, rm)
            df, n = bisect(f, name, guess)
            built.append((t, df)); iterations += n
        elif k == "fxSwap":
            if discount is None:
                raise CurveError("no discount curve")
            d0 = discount.discount_at(i["days"])
            built.append((t, fixed_of(d0 * Fraction(i["spot"] + q["value"], i["spot"]))))
        else:
            raise CurveError(f"unknown instrument {k}")
    return built, iterations


# ─── the specification's canonical bytes: DeskCanonical.wCurveSpec, the source hash of the treasury curve ────
CONV_CODE = {"A001": 0x01, "A003": 0x03, "A004": 0x04, "A005": 0x05, "A006": 0x06, "A007": 0x07, "A011": 0x0B}
INTERP_CODE = {"logLinearDiscount": 1, "linearZero": 2}


def _nat(n: int) -> bytes:
    if n == 0:
        return b"\x00"
    bs = n.to_bytes((n.bit_length() + 7) // 8, "big")
    return bytes([len(bs)]) + bs


def _int(i: int) -> bytes:
    return (b"\x01" + _nat(-i)) if i < 0 else (b"\x00" + _nat(i))


def _text(t: str) -> bytes:
    b = t.encode("utf-8")
    return struct.pack(">H", len(b)) + b


def _blob(b: bytes) -> bytes:
    return struct.pack(">H", len(b)) + bytes(b)


def _instrument(i: dict) -> bytes:
    k = i["kind"]
    if k == "deposit":
        return b"\x01" + _nat(i["days"])
    if k == "fra":
        return b"\x02" + _nat(i["start"]) + _nat(i["end"])
    if k == "future":
        return b"\x03" + _nat(i["start"]) + _nat(i["end"]) + _int(i["convexity"])
    if k == "swap":
        return b"\x04" + _nat(i["months"]) + _nat(i["fixed"]) + _nat(i["float"])
    if k == "ois":
        return b"\x05" + _nat(i["months"]) + _nat(i["fixed"])
    if k == "basis":
        return b"\x06" + _nat(i["months"]) + _text(i["reference"]) + (b"\x01" if i["spreadOnReference"] else b"\x00")
    return b"\x07" + _nat(i["days"]) + _nat(i["spot"])


def canonical_spec(spec: dict, sources) -> bytes:
    """The bytes DeskCanonical.wCurveSpec writes for a specification; `sources` gives each quote's source hash."""
    out = _text(spec["id"]) + _text(spec["currency"])
    if spec["role"] == "discount":
        out += b"\x01"
    elif spec["role"] == "projection":
        out += b"\x02" + _nat(spec["indexMonths"])
    else:
        out += b"\x03" + _text(spec["collateral"])
    out += bytes([CONV_CODE[spec["dayCount"]]])
    out += bytes([INTERP_CODE[spec["interpolation"]]])
    out += b"\x00" if spec["discountCurve"] is None else b"\x01" + _text(spec["discountCurve"])
    out += struct.pack(">H", len(spec["quotes"]))
    for q, src in zip(spec["quotes"], sources):
        out += _instrument(q["instrument"]) + _int(q["value"]) + _blob(src)
    return out


def spec_source(spec: dict, sources) -> bytes:
    return hashlib.sha256(canonical_spec(spec, sources)).digest()


def swap_mark(irs: dict, day: int, disc: Curve, proj: Curve, fixing_for) -> int:
    """CurveCore.swapMark: the floating leg from the projection curve, both legs on the discount curve."""
    pv = Fraction(0)
    conv = irs["dayCount"]
    for p in T.swap_periods(irs["start"], irs["maturity"], irs["paymentMonths"]):
        if p["end"] > day:
            frac = T.fraction(conv, p["start"], p["end"])
            fixed = Fraction(T.leg_amount(irs["notional"], Fraction(irs["fixedBps"]), conv, p))
            fx = fixing_for(p["start"]) if p["start"] <= day else None
            if fx is not None:
                floating = Fraction(T.leg_amount(irs["notional"], Fraction(fx + irs["spreadBps"]), conv, p))
            else:
                d0 = p["start"] - day if p["start"] > day else 0
                fwd = proj.forward_bps(d0, p["end"] - day)
                floating = Fraction(irs["notional"]) * (fwd + irs["spreadBps"]) * frac / BPS
            pv += (floating - fixed) * disc.discount_at(p["end"] - day)
    m = T.round_half_even(pv)
    return m if irs["payFixed"] else -m
