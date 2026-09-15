"""curve_oracle.py: QuantLib as the second oracle of curve construction.

The twin (`curve_twin.py`) is the contract's arithmetic integer for integer and is what the desk is held against;
QuantLib is an independent implementation with its own bootstrap, its own interpolation and its own pricers. This
module states the convention mapping and gives three checks:

  * `bootstrap`: QuantLib's `PiecewiseLogLinearDiscount` over the same instruments (`DepositRateHelper`,
    `FraRateHelper`, `FuturesRateHelper` with the declared convexity, `OISRateHelper`, `SwapRateHelper` with a
    discounting curve for a projection build, `IborIborBasisSwapRateHelper` with the spread on the base index's
    leg and the curve bootstrapped on whichever side the desk's quote names, `FxSwapRateHelper` with the
    collateral currency's curve, the curve's currency the base of the rate) on a null calendar, unadjusted, no
    settlement lag, the day count the specification's; for a curve interpolated log-linearly on the factors the
    scheme is the same as the desk's, so every node agrees to the bootstrap's own accuracy. The desk's linear
    scheme on simple zero rates has no QuantLib bootstrap (QuantLib's `ZeroCurve` interpolates continuously
    compounded rates), so those curves are held by the repricing check alone.
  * `reprice`: the desk's built curve handed to QuantLib as a `DiscountCurve` with a node on every day up to the
    last tenor (every cash flow the instruments carry falls on a whole day, so QuantLib reads the desk's own
    factor at every date it visits), and each instrument's fair rate by QuantLib's pricers (`forwardRate` for a
    deposit or an agreement, `VanillaSwap` and `OvernightIndexedSwap` under `DiscountingSwapEngine`, two `IborLeg`s
    under `CashFlows` for a basis swap's fair spread, the forward the two curves imply for an FX swap's points)
    against its quote: a bootstrap that did not reprice its instruments would show here whichever scheme the
    curve declares.
  * `swap_npv`: a swap on two of the desk's curves (the floating leg from a projection curve through an
    `IborIndex` with a forwarding curve, both legs on the discount curve), the fixing of the started period given,
    against the desk's multi-curve mark; the desk rounds every leg amount to a minor unit, so the precision is a
    minor unit per period.
"""
from fractions import Fraction

import QuantLib as ql

import treasury_twin as T
import curve_twin as CT

EPOCH = ql.Date(1, 1, 1970)
DC = {"A003": ql.Actual360(), "A004": ql.Actual365Fixed(), "A006": ql.Thirty360(ql.Thirty360.ISDA)}
FREQ = {12: ql.Annual, 6: ql.Semiannual, 3: ql.Quarterly, 1: ql.Monthly}
CAL = ql.NullCalendar()
NODE_TOLERANCE = 1e-10       # a factor's difference between the desk's bootstrap and QuantLib's, log-linear curves
REPRICE_TOLERANCE_BPS = 1e-6  # a fair rate's distance from the quote on the desk's curve, in basis points


def qd(day):
    return EPOCH + int(day)


def ibor(name, days=None, months=None, conv="A003", handle=None):
    per = ql.Period(days, ql.Days) if days is not None else ql.Period(months, ql.Months)
    if handle is None:
        return ql.IborIndex(name, per, 0, ql.EGPCurrency(), CAL, ql.Unadjusted, False, DC[conv])
    return ql.IborIndex(name, per, 0, ql.EGPCurrency(), CAL, ql.Unadjusted, False, DC[conv], handle)


def schedule(start, end, months):
    return ql.Schedule(qd(start), qd(end), ql.Period(months, ql.Months), CAL, ql.Unadjusted, ql.Unadjusted, ql.DateGeneration.Forward, False)


def helpers(spec, day, disc_handle=None, references=None):
    """`references` maps a basis swap's reference curve id to (its QuantLib curve, its index months)."""
    conv = spec["dayCount"]
    hs = []
    for q in spec["quotes"]:
        i = q["instrument"]
        r = ql.QuoteHandle(ql.SimpleQuote(q["value"] / 10_000))
        k = i["kind"]
        if k == "basis":
            rcrv, rmonths = references[i["reference"]]
            ref = ibor("reference", months=rmonths, conv=conv, handle=ql.YieldTermStructureHandle(rcrv))
            own = ibor("index", months=spec["indexMonths"], conv=conv)
            if i["spreadOnReference"]:
                hs.append(ql.IborIborBasisSwapRateHelper(r, ql.Period(i["months"], ql.Months), 0, CAL, ql.Unadjusted, False, ref, own, disc_handle, False))
            else:
                hs.append(ql.IborIborBasisSwapRateHelper(r, ql.Period(i["months"], ql.Months), 0, CAL, ql.Unadjusted, False, own, ref, disc_handle, True))
            continue
        if k == "fxSwap":
            points = ql.QuoteHandle(ql.SimpleQuote(q["value"] / 1_000_000))
            spot = ql.QuoteHandle(ql.SimpleQuote(i["spot"] / 1_000_000))
            hs.append(ql.FxSwapRateHelper(points, spot, ql.Period(i["days"], ql.Days), 0, CAL, ql.Unadjusted, False, False, disc_handle))
            continue
        if k == "deposit":
            hs.append(ql.DepositRateHelper(r, ibor("deposit", days=i["days"], conv=conv)))
        elif k == "fra":
            hs.append(ql.FraRateHelper(r, ql.Period(i["start"], ql.Days), ibor("fra", days=i["end"] - i["start"], conv=conv)))
        elif k == "future":
            price = ql.QuoteHandle(ql.SimpleQuote(100 - q["value"] / 100))
            hs.append(ql.FuturesRateHelper(price, qd(day + i["start"]), qd(day + i["end"]), DC[conv], ql.QuoteHandle(ql.SimpleQuote(i["convexity"] / 10_000)), ql.Futures.Custom))
        elif k == "ois":
            on = ql.OvernightIndex("overnight", 0, ql.EGPCurrency(), CAL, DC[conv])
            hs.append(ql.OISRateHelper(0, ql.Period(i["months"], ql.Months), r, on, ql.YieldTermStructureHandle(), False, 0, ql.Unadjusted, FREQ[i["fixed"]], CAL))
        elif k == "swap":
            idx = ibor("ibor", months=i["float"], conv=conv)
            if spec["role"] == "discount":
                hs.append(ql.SwapRateHelper(r, ql.Period(i["months"], ql.Months), CAL, FREQ[i["fixed"]], ql.Unadjusted, DC[conv], idx))
            else:
                hs.append(ql.SwapRateHelper(r, ql.Period(i["months"], ql.Months), CAL, FREQ[i["fixed"]], ql.Unadjusted, DC[conv], idx,
                                            ql.QuoteHandle(ql.SimpleQuote(0.0)), ql.Period(0, ql.Days), disc_handle))
        else:
            raise ValueError(k)
    return hs


def bootstrap(spec, day, discount=None, references=None):
    """QuantLib's log-linear bootstrap of the specification: the factor at every node of the desk's build, and the
    curve. `discount` is the QuantLib curve a projection or a collateral discount build discounts on;
    `references` the QuantLib curves a basis swap refers to."""
    assert spec["interpolation"] == "logLinearDiscount", "QuantLib's bootstrap is compared on the log-linear scheme only"
    ql.Settings.instance().evaluationDate = qd(day)
    dh = ql.YieldTermStructureHandle(discount) if discount is not None else None
    crv = ql.PiecewiseLogLinearDiscount(qd(day), helpers(spec, day, dh, references), DC[spec["dayCount"]])
    crv.enableExtrapolation()
    return crv


def daily(c, ref):
    """The desk's curve as QuantLib reads it: its factor at every whole day from `ref` to the last node."""
    last = c.nodes[-1][0]
    return ql.DiscountCurve([qd(ref + t) for t in range(0, last + 1)], [float(c.discount_at(t)) for t in range(0, last + 1)], DC[c.conv])


def reprice(spec, day, c, disc=None, references=None):
    """Every instrument's fair figure by QuantLib's pricers on the desk's curve beside its quote (a rate or a
    spread in basis points; forward points in millionths for an FX swap): [(kind, quote, fair)]. `references`
    maps a basis swap's reference curve id to (the desk's twin curve, its index months)."""
    ql.Settings.instance().evaluationDate = qd(day)
    conv = spec["dayCount"]
    crv = daily(c, day)
    h = ql.YieldTermStructureHandle(crv)
    dcrv = daily(disc, day) if disc is not None else None
    dh = ql.YieldTermStructureHandle(dcrv) if disc is not None else h
    out = []
    for q in spec["quotes"]:
        i = q["instrument"]
        k = i["kind"]
        if k == "basis":
            rc, rmonths = references[i["reference"]]
            maturity = T.add_months(day, i["months"])
            ref = ibor("reference", months=rmonths, conv=conv, handle=ql.YieldTermStructureHandle(daily(rc, day)))
            own = ibor("index", months=spec["indexMonths"], conv=conv, handle=h)
            leg_r = ql.IborLeg([1.0], schedule(day, maturity, rmonths), ref, DC[conv])
            leg_o = ql.IborLeg([1.0], schedule(day, maturity, spec["indexMonths"]), own, DC[conv])
            npv_r, npv_o = ql.CashFlows.npv(leg_r, dh, False), ql.CashFlows.npv(leg_o, dh, False)
            ann_r, ann_o = ql.CashFlows.bps(leg_r, dh, False) * 10_000, ql.CashFlows.bps(leg_o, dh, False) * 10_000
            fair = (npv_o - npv_r) / ann_r if i["spreadOnReference"] else (npv_r - npv_o) / ann_o
            out.append((k, q["value"], fair * 10_000))
            continue
        if k == "fxSwap":
            # the forward the two curves imply, over the spot, less the spot: the points in millionths
            fwd = i["spot"] / 1_000_000 * crv.discount(qd(day + i["days"])) / dcrv.discount(qd(day + i["days"]))
            out.append((k, q["value"], (fwd - i["spot"] / 1_000_000) * 1_000_000))
            continue
        if k == "deposit":
            r = crv.forwardRate(qd(day), qd(day + i["days"]), DC[conv], ql.Simple).rate()
        elif k in ("fra", "future"):
            r = crv.forwardRate(qd(day + i["start"]), qd(day + i["end"]), DC[conv], ql.Simple).rate() + (i["convexity"] / 10_000 if k == "future" else 0)
        elif k == "swap":
            idx = ibor("ibor", months=i["float"], conv=conv, handle=h)
            maturity = T.add_months(day, i["months"])
            sw = ql.VanillaSwap(ql.VanillaSwap.Payer, 1.0, schedule(day, maturity, i["fixed"]), 0.02, DC[conv], schedule(day, maturity, i["float"]), idx, 0.0, DC[conv])
            sw.setPricingEngine(ql.DiscountingSwapEngine(dh))
            r = sw.fairRate()
        elif k == "ois":
            on = ql.OvernightIndex("overnight", 0, ql.EGPCurrency(), CAL, DC[conv], h)
            sw = ql.OvernightIndexedSwap(ql.OvernightIndexedSwap.Payer, 1.0, schedule(day, T.add_months(day, i["months"]), i["fixed"]), 0.02, DC[conv], on)
            sw.setPricingEngine(ql.DiscountingSwapEngine(h))
            r = sw.fairRate()
        else:
            raise ValueError(k)
        out.append((k, q["value"], r * 10_000))
    return out


def swap_npv(irs, day, disc, proj, fixings):
    """QuantLib's NPV in minor units of the swap on the desk's two curves as of `day`, positive to the payer of
    the fixed leg when `irs["payFixed"]`; `fixings` maps a period's start day to its rate in basis points."""
    ql.Settings.instance().evaluationDate = qd(day)
    dh = ql.YieldTermStructureHandle(daily(disc, day))
    ph = ql.YieldTermStructureHandle(daily(proj, day))
    conv = irs["dayCount"]
    idx = ibor("index", months=irs["paymentMonths"], conv=conv, handle=ph)
    for start, bps in fixings.items():
        if start <= day:
            idx.addFixing(qd(start), bps / 10_000, True)
    sch = schedule(irs["start"], irs["maturity"], irs["paymentMonths"])
    side = ql.VanillaSwap.Payer if irs["payFixed"] else ql.VanillaSwap.Receiver
    sw = ql.VanillaSwap(side, irs["notional"] / 100, sch, irs["fixedBps"] / 10_000, DC[conv], sch, idx, irs["spreadBps"] / 10_000, DC[conv])
    sw.setPricingEngine(ql.DiscountingSwapEngine(dh))
    return sw.NPV() * 100


def periods_of(irs):
    return T.swap_periods(irs["start"], irs["maturity"], irs["paymentMonths"])


def mark_tolerance(irs, day):
    """A minor unit per period still to pay, the desk's rounding of each leg amount."""
    return sum(1 for p in periods_of(irs) if p["end"] > day) + 1
