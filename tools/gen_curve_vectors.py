#!/usr/bin/env python3
"""Generate test/CurveVectors.mo: the curve twin's builds as Motoko constants.

`integration/curve_twin.py` follows `src/CurveCore.mo` integer for integer; this tool runs it over a seeded set
of curve specifications (discount curves from deposits, forward rate agreements, futures, overnight index swaps
and par swaps under both interpolations and two day counts; projection curves built on them; a swap marked on
the pair) and writes the nodes, the iteration counts, the derived zero points and the marks, so
`test/Curve.test.mo` proves the contract's bootstrap equals the twin's without a replica.

Attribution: Thebes Core Team. Licence: Apache 2.0.
"""
import os
import random
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "integration"))
import treasury_twin as T  # noqa: E402
import curve_twin as CT  # noqa: E402

rng = random.Random(20260914)
DAY = T.from_civil(2026, 4, 6)
CONV_MO = {"A003": "#a003_Act360", "A004": "#a004_Act365Fixed", "A006": "#a006_Thirty360Isda"}


def instrument_mo(i):
    k = i["kind"]
    if k == "deposit":
        return "#deposit({ days = %d })" % i["days"]
    if k == "fra":
        return "#fra({ startDays = %d; endDays = %d })" % (i["start"], i["end"])
    if k == "future":
        return "#future({ startDays = %d; endDays = %d; convexityBps = %d })" % (i["start"], i["end"], i["convexity"])
    if k == "swap":
        return "#swap({ months = %d; fixedMonths = %d; floatMonths = %d })" % (i["months"], i["fixed"], i["float"])
    if k == "ois":
        return "#ois({ months = %d; fixedMonths = %d })" % (i["months"], i["fixed"])
    if k == "basis":
        return '#basis({ months = %d; reference = "%s"; spreadOnReference = %s })' % (i["months"], i["reference"], "true" if i["spreadOnReference"] else "false")
    return "#fxSwap({ days = %d; spotMicro = %d })" % (i["days"], i["spot"])


def spec_mo(s):
    role = {"discount": "#discount", "projection": "#projection({ indexMonths = %d })" % s.get("indexMonths", 0), "collateralDiscount": '#collateralDiscount({ collateral = "%s" })' % s.get("collateral", "")}[s["role"]]
    dc = "null" if s["discountCurve"] is None else '?"%s"' % s["discountCurve"]
    quotes = ", ".join('{ instrument = %s; value = %d; source = h(%d) }' % (instrument_mo(q["instrument"]), q["value"], n) for n, q in enumerate(s["quotes"], 1))
    return '{ id = "%s"; currency = "%s"; role = %s; dayCount = %s; interpolation = #%s; discountCurve = %s; quotes = [%s] }' % (
        s["id"], s["currency"], role, CONV_MO[s["dayCount"]], s["interpolation"], dc, quotes)


def discount_spec(name, conv, interp, base):
    qs = [{"instrument": {"kind": "deposit", "days": 7}, "value": base - 10 + rng.randint(-5, 5)},
          {"instrument": {"kind": "deposit", "days": 30}, "value": base + rng.randint(-5, 5)},
          {"instrument": {"kind": "deposit", "days": 90}, "value": base + 20 + rng.randint(-5, 5)},
          {"instrument": {"kind": "fra", "start": 90, "end": 180}, "value": base + 40 + rng.randint(-5, 5)},
          {"instrument": {"kind": "future", "start": 180, "end": 270, "convexity": rng.randint(1, 4)}, "value": base + 55 + rng.randint(-5, 5)},
          {"instrument": {"kind": "ois", "months": 12, "fixed": 12}, "value": base + 100 + rng.randint(-10, 10)},
          {"instrument": {"kind": "ois", "months": 24, "fixed": 12}, "value": base + 150 + rng.randint(-10, 10)},
          {"instrument": {"kind": "ois", "months": 36, "fixed": 12}, "value": base + 190 + rng.randint(-10, 10)},
          {"instrument": {"kind": "ois", "months": 60, "fixed": 12}, "value": base + 250 + rng.randint(-10, 10)}]
    return {"id": name, "currency": "EGP", "role": "discount", "dayCount": conv, "interpolation": interp, "discountCurve": None, "quotes": qs}


def projection_spec(name, disc, conv, interp, base, months):
    qs = [{"instrument": {"kind": "deposit", "days": 30 * months + 1}, "value": base + 60 + rng.randint(-5, 5)},
          {"instrument": {"kind": "swap", "months": 12, "fixed": 12, "float": months}, "value": base + 180 + rng.randint(-10, 10)},
          {"instrument": {"kind": "swap", "months": 24, "fixed": 12, "float": months}, "value": base + 230 + rng.randint(-10, 10)},
          {"instrument": {"kind": "swap", "months": 36, "fixed": 6, "float": months}, "value": base + 270 + rng.randint(-10, 10)},
          {"instrument": {"kind": "swap", "months": 60, "fixed": 12, "float": months}, "value": base + 330 + rng.randint(-10, 10)}]
    return {"id": name, "currency": "EGP", "role": "projection", "indexMonths": months, "dayCount": conv, "interpolation": interp, "discountCurve": disc, "quotes": qs}


def basis_spec(name, disc, reference, conv, interp, base, months):
    """A projection curve for a longer tenor from tenor basis swaps against the reference curve's index: the
    spread on the reference (shorter) leg, widening with the tenor; a deposit for the first node."""
    qs = [{"instrument": {"kind": "deposit", "days": 30 * months + 1}, "value": base + 75 + rng.randint(-5, 5)},
          {"instrument": {"kind": "basis", "months": 12, "reference": reference, "spreadOnReference": True}, "value": 8 + rng.randint(-2, 2)},
          {"instrument": {"kind": "basis", "months": 24, "reference": reference, "spreadOnReference": True}, "value": 11 + rng.randint(-2, 2)},
          {"instrument": {"kind": "basis", "months": 36, "reference": reference, "spreadOnReference": True}, "value": 14 + rng.randint(-2, 2)},
          {"instrument": {"kind": "basis", "months": 60, "reference": reference, "spreadOnReference": False}, "value": -18 + rng.randint(-2, 2)}]
    return {"id": name, "currency": "EGP", "role": "projection", "indexMonths": months, "dayCount": conv, "interpolation": interp, "discountCurve": disc, "quotes": qs}


def fx_swap_spec(name, disc, conv, interp, spot):
    """A discount curve for a foreign currency under collateral in the domestic one, from FX swap points against
    the domestic discount curve: points rising with the tenor for a higher domestic rate."""
    qs = []
    for days, pts in ((7, 90_000), (30, 380_000), (90, 1_150_000), (180, 2_300_000), (365, 4_700_000), (730, 9_800_000)):
        qs.append({"instrument": {"kind": "fxSwap", "days": days, "spot": spot}, "value": pts + rng.randint(-20_000, 20_000)})
    return {"id": name, "currency": "USD", "role": "collateralDiscount", "collateral": "EGP", "dayCount": conv, "interpolation": interp, "discountCurve": disc, "quotes": qs}


def par_swap_spec(name, conv, interp, base):
    """A discount curve built from par swaps against its own forwards (a single-curve bootstrap)."""
    qs = [{"instrument": {"kind": "deposit", "days": 91}, "value": base + rng.randint(-5, 5)},
          {"instrument": {"kind": "swap", "months": 12, "fixed": 12, "float": 3}, "value": base + 90 + rng.randint(-10, 10)},
          {"instrument": {"kind": "swap", "months": 24, "fixed": 12, "float": 3}, "value": base + 140 + rng.randint(-10, 10)},
          {"instrument": {"kind": "swap", "months": 48, "fixed": 12, "float": 3}, "value": base + 200 + rng.randint(-10, 10)}]
    return {"id": name, "currency": "EGP", "role": "discount", "dayCount": conv, "interpolation": interp, "discountCurve": None, "quotes": qs}


out = ["/// CurveVectors.mo: the curve twin's builds, generated by tools/gen_curve_vectors.py. Every node is the twin's",
       "/// integer; a build in the contract that differs by one unit of the fixed point is a build that drifted.",
       "///", "/// Attribution: Thebes Core Team. Licence: Apache 2.0.", "",
       "import CT \"../src/CurveTypes\";", "import Blob \"mo:core/Blob\";", "import Nat8 \"mo:core/Nat8\";", "import Array \"mo:core/Array\";", "", "module {",
       "  public let DAY : Nat = %d;" % DAY,
       "  public func h(n : Nat) : Blob { Blob.fromArray(Array.tabulate<Nat8>(32, func(i) { Nat8.fromNat((n + i) % 256) })) };",
       "  public type Vector = { spec : CT.Spec; nodes : [CT.Node]; iterations : Nat; zero : [(Nat, Int)]; discountOf : ?Text };",
       "  public func vectors() : [Vector] {", "    ["]
built = {}
count = 0
for conv in ("A003", "A004"):
    for interp in ("logLinearDiscount", "linearZero"):
        base = 2000 + rng.randint(-200, 200)
        d = discount_spec("D-%s-%s" % (conv, interp[:3]), conv, interp, base)
        nodes, it = CT.bootstrap(d, DAY, None)
        c = CT.Curve(DAY, conv, interp, nodes)
        built[d["id"]] = c
        out.append("      { spec = %s; nodes = [%s]; iterations = %d; zero = [%s]; discountOf = null }," % (
            spec_mo(d), ", ".join("{ days = %d; df = %d }" % n for n in nodes), it, ", ".join("(%d, %d)" % z for z in c.derived_zero_points())))
        count += 1
        p = projection_spec("P-%s-%s" % (conv, interp[:3]), d["id"], conv, interp, base, 3 if conv == "A003" else 6)
        pn, pit = CT.bootstrap(p, DAY, c)
        pc = CT.Curve(DAY, conv, interp, pn)
        built[p["id"]] = pc
        out.append("      { spec = %s; nodes = [%s]; iterations = %d; zero = [%s]; discountOf = ?\"%s\" }," % (
            spec_mo(p), ", ".join("{ days = %d; df = %d }" % n for n in pn), pit, ", ".join("(%d, %d)" % z for z in pc.derived_zero_points()), d["id"]))
        count += 1
s = par_swap_spec("S-A003-log", "A003", "logLinearDiscount", 2100)
nodes, it = CT.bootstrap(s, DAY, None)
c = CT.Curve(DAY, "A003", "logLinearDiscount", nodes)
out.append("      { spec = %s; nodes = [%s]; iterations = %d; zero = [%s]; discountOf = null }," % (
    spec_mo(s), ", ".join("{ days = %d; df = %d }" % n for n in nodes), it, ", ".join("(%d, %d)" % z for z in c.derived_zero_points())))
count += 1
# a six-month projection curve from basis swaps against the three-month curve, on the first discount curve
disc, proj = built["D-A003-log"], built["P-A003-log"]
b = basis_spec("B-A003-6M", "D-A003-log", "P-A003-log", "A003", "logLinearDiscount", 2000, 6)
bn, bit = CT.bootstrap(b, DAY, disc, lambda i: (proj, 3) if i == "P-A003-log" else None)
bc = CT.Curve(DAY, "A003", "logLinearDiscount", bn)
built[b["id"]] = bc
out.append("      { spec = %s; nodes = [%s]; iterations = %d; zero = [%s]; discountOf = ?\"%s\" }," % (
    spec_mo(b), ", ".join("{ days = %d; df = %d }" % n for n in bn), bit, ", ".join("(%d, %d)" % z for z in bc.derived_zero_points()), "D-A003-log"))
count += 1
# a dollar discount curve under pound collateral, from FX swap points against the first discount curve
f = fx_swap_spec("U-EGP-COLL", "D-A003-log", "A003", "logLinearDiscount", 48_000_000)
fn, fit = CT.bootstrap(f, DAY, disc)
fc = CT.Curve(DAY, "A003", "logLinearDiscount", fn)
out.append("      { spec = %s; nodes = [%s]; iterations = %d; zero = [%s]; discountOf = ?\"%s\" }," % (
    spec_mo(f), ", ".join("{ days = %d; df = %d }" % n for n in fn), fit, ", ".join("(%d, %d)" % z for z in fc.derived_zero_points()), "D-A003-log"))
count += 1
out.append("    ]")
out.append("  };")
# a swap marked on the first pair of curves, three days after the build, with one fixing recorded
disc, proj = built["D-A003-log"], built["P-A003-log"]
irs = {"notional": 50_000_000_00, "payFixed": True, "fixedBps": 2150, "spreadBps": 15, "dayCount": "A003", "start": DAY - 40, "maturity": T.add_months(DAY - 40, 36), "paymentMonths": 3}
mark_day = DAY + 3
fixings = {DAY - 40: 2075}
m = CT.swap_mark(irs, mark_day, disc, proj, lambda s: fixings.get(s))
irs2 = dict(irs, payFixed=False)
m2 = CT.swap_mark(irs2, mark_day, disc, proj, lambda s: fixings.get(s))
out.append("  public let MARK_DAY : Nat = %d;" % mark_day)
out.append("  public let FIXING_DAY : Nat = %d;" % (DAY - 40))
out.append("  public let FIXING_BPS : Nat = %d;" % fixings[DAY - 40])
out.append("  public let IRS_START : Nat = %d;" % irs["start"])
out.append("  public let IRS_MATURITY : Nat = %d;" % irs["maturity"])
out.append("  public let SWAP_MARK_PAY_FIXED : Int = %d;" % m)
out.append("  public let SWAP_MARK_RECEIVE_FIXED : Int = %d;" % m2)
out.append("}")
open(os.path.join(os.path.dirname(__file__), "..", "test", "CurveVectors.mo"), "w").write("\n".join(out) + "\n")
print("wrote test/CurveVectors.mo (%d builds)" % count)
