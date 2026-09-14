/// Valuation.test.mo: the desk's valuation on the pure layer: a bond's price from a yield and the yield of that
/// price meeting again; the desk's mark from the day's data equal to Manticore's posted mark for a forward, a
/// swap, an option and a fair-value lot; the theta from the previous day's data at the day; the attribution fold
/// summing new, carry and market to the whole over a deal's events; the hedge effectiveness by the lower-of test
/// and the legs of an assessment and a dedesignation; the fingerprints equal under a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";
import Array "mo:core/Array";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import TT "mo:manticore/TreasuryTypes";
import Treasury "mo:manticore/TreasuryCore";
import Fx "mo:manticore/Fx";
import M "mo:manticore/TreasuryMath";

import VT "../src/ValuationTypes";
import V "../src/Valuation";
import Hedges "../src/HedgeCore";
import Attr "../src/Attribution";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func okT<X>(r : { #ok : X; #err : TT.TreasuryError }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func okV<X>(r : { #ok : X; #err : V.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func h(n : Nat8) : Blob { S.h(n) };

let arena = RI.newArena();
let t = Treasury.newState(arena);
let attr = Attr.newState(arena);
let hedges = Hedges.newState(arena);
let attrEvents = List.empty<(Nat, TT.TreasuryEvent, ?Treasury.DealRow)>();
let thetas = List.empty<(Nat, Nat, Int)>();
var block = 700;
func next() : Nat { block += 1; block };
func applyT(ev : TT.TreasuryEvent) : Nat {
  let b = next();
  let before = switch (ev) { case (#couponPaid(x)) Treasury.row(t, x.deal); case (#legSettled(x)) Treasury.row(t, x.deal); case (_) null };
  Treasury.fold(t, b, ev); Attr.observeTreasury(attr, b, ev, before); List.add(attrEvents, (b, ev, before)); b
};
let D0 = 20_710;
let EGP = "EGP"; let USD = "USD";
let cash = S.nostro; let cashEgp = S.cash;
let trader = Principal.fromText("aaaaa-aa");
// the market: a spot per day, curves per day, fixings
var rates : [(Nat, Nat)] = [(D0 - 1, 47_900_000), (D0, 48_000_000), (D0 + 1, 48_150_000), (D0 + 2, 48_100_000)];
func spotOn(ccy : Text, day : Nat) : ?Fx.Rate { if (not Text.equal(ccy, USD)) return null; for ((d, n) in rates.vals()) { if (d == day) return ?{ currency = USD; functional = EGP; numerator = n; denominator = 1_000_000; asOf = day; source = "test" } }; null };
func fixingOn(_ : Text, day : Nat) : ?Nat { if (day == D0) ?2000 else null };
let ctx : Treasury.Ctx = { functional = EGP; spot = spotOn; pair = func(_ : Text) : ?Fx.PositionPair { null }; fixing = fixingOn; isShariaBook = func(_ : Text) : Bool { false } };
var termsTable : [(Nat, TT.DealKind)] = [];
func terms(b : Nat) : ?TT.DealKind { for ((k, v) in termsTable.vals()) { if (k == b) return ?v }; null };
ignore applyT(okT(Treasury.planPolicy(S.policy), "policy"));
// an annual 15 percent bond on 30/360, so the accrued coupon and the stub are proper fractions of the period
let bond : TT.SecurityTerms = { isin = "EG0000034567"; issuer = "NBE"; currency = EGP; couponBps = 1500; couponsPerYear = 1; dayCount = #a006_Thirty360Isda; issue = 20_463; maturity = 22_289 };
ignore applyT(okT(Treasury.planRegisterSecurity(t, bond, D0), "security"));
let ?sec = Treasury.security(t, bond.isin) else Runtime.trap("sec");
ignore applyT(okT(Treasury.planRegisterNostro(t, { id = "NOSTRO-USD-CITI"; account = "1100"; sub = ?"NOSTRO-USD"; currency = USD; correspondent = S.citi; iban = ""; valueDateToleranceDays = 2 }, D0), "nostro"));
func publish(id : Text, kind : TT.CurveKind, ccy : Text, day : Nat, points : [(Nat, Int)]) { switch (okT(Treasury.planPublishCurve(t, { id; kind; currency = ccy; day; points; source = h(1) }), "publish")) { case (?ev) ignore applyT(ev); case null {} } };
for ((day, shift) in [(D0 - 1, -10), (D0, 0), (D0 + 1, 15), (D0 + 2, 5)].vals()) {
  publish("EGP-ZERO", #zeroRates, EGP, day, [(1, 2000 + shift), (30, 2050 + shift), (90, 2100 + shift), (365, 2200 + shift)]);
  publish("USD-ZERO", #zeroRates, USD, day, [(1, 500), (365, 520)]);
  publish("USDEGP-PTS", #forwardPoints, USD, day, [(1, 20_000), (30, 600_000 + shift * 1000), (90, 1_800_000), (365, 7_000_000)]);
  publish("USDEGP-VOL", #volatility, USD, day, [(30, 1200 + shift), (365, 1500)]);
  publish(bond.isin, #securityPrice, EGP, day, [(0, 106_500_000 + shift * 10_000)]);
};

// ─── the price of a yield and the yield of a price ───
let ?p1 = V.priceOfYield(sec, 1200, D0) else Runtime.trap("price");
check(p1 > 105_000_000 and p1 < 112_000_000, "a 15 percent bond at a 12 percent yield prices above par: " # Nat.toText(p1));
let ?y1 = V.yieldOfPrice(sec, p1, D0) else Runtime.trap("yield");
check(y1 == 1200 or y1 == 1199, "the yield of the price meets the yield again: " # Nat.toText(y1));
let ?pPar = V.priceOfYield(sec, 1500, 20_463 + 360) else Runtime.trap("price");
check(pPar > 99_900_000 and pPar < 100_100_000, "at its own coupon rate on a coupon date the bond prices at par: " # Nat.toText(pPar));
let ?p2 = V.priceOfYield(sec, 1400, D0) else Runtime.trap("price");
check(p2 < p1, "a higher yield is a lower price");
let ?p3 = V.priceOfYield(sec, 1200, D0 + 100) else Runtime.trap("price");
check(p3 != p1, "the price moves with the settlement day");
check(V.priceOfYield(sec, 1200, bond.maturity) == null, "no price at maturity");
Debug.print("count: yield and price round trips = 4");

// ─── the desk's mark equals Manticore's, kind by kind; the theta from the previous day's data ───
func remember(b : Nat, k : TT.DealKind) { termsTable := Array.concat(termsTable, [(b, k)]) };
func capture(kind : TT.DealKind) : Nat {
  let id = block + 1;
  let r = okT(Treasury.planCapture(t, id, "BR01", S.citi, kind, "REF-" # Nat.toText(id), trader, D0, null, ctx, terms), "capture");
  let b = applyT(r.ev); remember(b, kind);
  for (e in r.extras.vals()) ignore applyT(e);
  b
};
let fwd : TT.FxForward = { base = USD; quote = EGP; direction = #buy; baseAmount = 500_000_00; rateMicro = 48_600_000; valueDate = D0 + 30; spotMicro = 48_000_000; forwardPointsMicro = 600_000; baseAccount = cash; quoteAccount = cashEgp; pointsCurve = "USDEGP-PTS"; discountCurve = "EGP-ZERO" };
let irs : TT.Irs = { currency = EGP; notional = 50_000_000_00; payFixed = true; fixedBps = 2100; floatingIndex = "CBE-ON"; spreadBps = 25; start = D0; maturity = 21_075; paymentMonths = 3; dayCount = #a003_Act360; cash = cashEgp; discountCurve = "EGP-ZERO" };
let opt : TT.FxOption = { base = USD; quote = EGP; call = true; bought = true; baseAmount = 200_000_00; strikeMicro = 49_000_000; expiry = D0 + 60; premium = 150_000_00; start = D0; cash = cashEgp; domesticCurve = "EGP-ZERO"; foreignCurve = "USD-ZERO"; volCurve = "USDEGP-VOL" };
let buy : TT.SecurityTrade = { isin = bond.isin; direction = #buy; nominal = 10_000_000_00; priceMicro = 106_000_000; settlement = D0; classification = #fvoci; cash = cashEgp; priceCurve = bond.isin; venue = null };
let fwdId = capture(#fxForward(fwd));
let irsId = capture(#irs(irs));
let optId = capture(#fxOption(opt));
let lotId = capture(#security(buy));
// the option's premium and the lot's settlement, so both carry a mark
for ((id, k) in [(optId, #fxOption(opt) : TT.DealKind), (lotId, #security(buy))].vals()) {
  let ?r = Treasury.row(t, id) else Runtime.trap("row");
  let a = okT(Treasury.planSettleLeg(t, r, k, 0, D0, ctx), "settle"); ignore applyT(a.ev); for (e in a.extras.vals()) ignore applyT(e);
};
func dataOn(day : Nat) : V.Data { V.dataOn(t, day, spotOn, fixingOn) };
var agreed = 0;
var thetaChecks = 0;
for (day in [D0 + 1, D0 + 2].vals()) {
  for (id in [fwdId, irsId, optId, lotId].vals()) {
    let ?r = Treasury.row(t, id) else Runtime.trap("row");
    let ?k = terms(r.termsBlock) else Runtime.trap("terms");
    // the theta: the previous day's data at the day, against the mark standing
    let standing : Int = if (r.kind == 4) r.fvPosted else r.markPosted;
    let withPrevious = okV(V.markWith(t, r, k, day, dataOn(day - 1)), "theta");
    let ?wp = withPrevious else Runtime.trap("no theta mark");
    List.add(thetas, (id, day, wp - standing));
    // the mark from the day's own data equals what Manticore posts
    let mine = okV(V.markWith(t, r, k, day, dataOn(day)), "mark");
    switch (okT(Treasury.planMark(t, r, k, day, ctx), "manticore mark")) {
      case (?a) { switch (a.ev) { case (#marked(x)) { check(mine == ?x.value, "the desk's mark equals Manticore's for deal " # Nat.toText(id) # ": " # debug_show (mine, x.value)); agreed += 1; ignore applyT(a.ev) }; case (_) {} } };
      case null { check(mine == ?standing, "no movement: the desk's mark equals the standing mark"); agreed += 1 };
    };
    // the theta recorded beside the mark, into the attribution
    let (_, _, theta) = List.last(thetas) |> (func(x : ?(Nat, Nat, Int)) : (Nat, Nat, Int) { switch (x) { case (?v) v; case null Runtime.trap("theta") } })(_);
    Attr.observeTheta(attr, id, day, theta);
    thetaChecks += 1;
  };
};
Debug.print("count: marks agreeing with Manticore's = " # Nat.toText(agreed));
Debug.print("count: thetas from the previous day's data = " # Nat.toText(thetaChecks));

// ─── the attribution: new plus carry plus market is the whole, and the whole is what the events moved ───
// an accrual and a coupon on the lot, then the rows
let ?lot = Treasury.row(t, lotId) else Runtime.trap("lot");
switch (okT(Treasury.planAccrue(t, lot, #security(buy), D0 + 2), "accrue")) { case (?a) ignore applyT(a.ev); case null {} };
var total : Int = 0;
var rows = 0;
for (id in [fwdId, irsId, optId, lotId].vals()) {
  for (v in Attr.rowsOfDeal(attr, id, EGP).vals()) {
    check(v.total == v.new + v.carry + v.market, "new + carry + market = total for deal " # Nat.toText(id));
    total += v.total; rows += 1;
  };
};
// the whole result of the four deals from the events themselves: every mark's movement, every accrual, every realised
var fromEvents : Int = 0;
for ((_, ev, before_) in List.values(attrEvents)) {
  switch (ev) {
    case (#marked(x)) fromEvents += x.value - x.previous;
    case (#accrued(x)) fromEvents += x.interest + x.amortisation;
    case (#legSettled(x)) { let premiumLeg = switch (before_) { case (?b) b.kind == 6 and x.leg == 0; case null false }; fromEvents += x.realised + (if (premiumLeg) 0 else x.fv) };
    case (_) {};
  };
};
check(total == fromEvents, "the attribution's whole equals the events' result: " # debug_show (total, fromEvents));
// the theta moved between carry and market and nothing else
var carryTheta : Int = 0;
for ((_, _, th) in List.values(thetas)) carryTheta += th;
var carrySum : Int = 0; var marketSum : Int = 0; var newSum : Int = 0;
for (id in [fwdId, irsId, optId, lotId].vals()) { for (v in Attr.rowsOfDeal(attr, id, EGP).vals()) { carrySum += v.carry; marketSum += v.market; newSum += v.new } };
var accrualsPosted : Int = 0;
for ((_, ev, _) in List.values(attrEvents)) { switch (ev) { case (#accrued(x)) accrualsPosted += x.interest + x.amortisation; case (_) {} } };
check(carrySum == carryTheta + accrualsPosted, "carry is the theta plus the accruals: " # debug_show (carrySum, carryTheta, accrualsPosted));
check(newSum == 0, "nothing marked on the capture day itself here");
Debug.print("count: attribution rows with the identity held = " # Nat.toText(rows));

// ─── hedges: the lower-of test and the legs ───
let (bps1, eff1) = Hedges.effectiveness(10_000, -8_000);
check(bps1 == 8_000 and eff1 == 8_000, "80 percent offset: the effective portion is the hedged change's size with the hedging change's sign");
let (bps2, eff2) = Hedges.effectiveness(10_000, -12_000);
check(bps2 == 12_000 and eff2 == 10_000, "over-hedged: the whole hedging change is effective");
let (bps3, eff3) = Hedges.effectiveness(-5_000, 4_000);
check(bps3 == 8_000 and eff3 == -4_000, "a loss on the hedging instrument offset by a gain on the item");
let (_, eff4) = Hedges.effectiveness(5_000, 3_000);
check(eff4 == 0, "both moving the same way: nothing offsets");
let (bps5, eff5) = Hedges.effectiveness(0, 0);
check(bps5 == 10_000 and eff5 == 0, "no movement on either side");
Debug.print("count: effectiveness checks = 5");
let vp : VT.Policy = { hedgeReserve = "3600" };
ignore Hedges.planPolicy(vp);
Hedges.fold(hedges, next(), #policySet(vp));
let hedgeId = next();
Hedges.fold(hedges, hedgeId, #hedgeDesignated({ hedging = irsId; hedged = fwdId; kind = #cashFlow({ hedgedAmount = 40_000_000_00 }); hypothetical = ?{ irs with notional = 40_000_000_00 }; hedgingMark = 100_000; hedgedValue = 80_000; day = D0 }));
let assessed = Hedges.planAssess(hedges, hedgeId, 110_000, 72_000, D0 + 2);
switch (assessed) {
  case (#ok(#hedgeAssessed(x))) {
    check(x.hedgingChange == 10_000 and x.hedgedChange == -8_000 and x.effective == 8_000 and x.ineffective == 2_000 and x.effectivenessBps == 8_000, "the assessment: " # debug_show x);
    let ?hr = Hedges.hedge(hedges, hedgeId) else Runtime.trap("hedge");
    let legs = Hedges.legsOf(vp, S.policy, hr, EGP, "", #hedgeAssessed(x));
    var reserve : Int = 0; var pl : Int = 0;
    for (l in legs.vals()) { if (Text.equal(l.account, "3600")) reserve += (if (l.side == #credit) l.amount else -l.amount); if (Text.equal(l.account, S.policy.unrealisedTradingGain)) pl += (if (l.side == #debit) l.amount else -l.amount) };
    check(reserve == 8_000 and pl == 8_000, "the effective portion out of the result into the reserve");
    Hedges.fold(hedges, next(), #hedgeAssessed(x));
  };
  case (_) check(false, "assess");
};
let ?hr2 = Hedges.hedge(hedges, hedgeId) else Runtime.trap("hedge");
check(hr2.reserve == 8_000 and hr2.hedgingMark == 110_000 and hr2.lastEffectivenessBps == 8_000, "the row after the assessment");
switch (Hedges.planAssess(hedges, hedgeId, 110_000, 72_000, D0 + 1)) { case (#err(_)) {}; case (#ok(_)) check(false, "an assessment before the last accepted") };
switch (Hedges.planDedesignate(hedges, hedgeId, D0 + 3)) {
  case (#ok(#hedgeDedesignated(x))) {
    check(x.reclassified == 8_000, "the reserve reclassified on dedesignation");
    let legs = Hedges.legsOf(vp, S.policy, hr2, EGP, "", #hedgeDedesignated(x));
    check(legs.size() == 2 and Text.equal(legs[0].account, "3600") and legs[0].side == #debit, "the reserve debited back to the result");
    Hedges.fold(hedges, next(), #hedgeDedesignated(x));
  };
  case (_) check(false, "dedesignate");
};
let ?hr3 = Hedges.hedge(hedges, hedgeId) else Runtime.trap("hedge");
check(hr3.state == #dedesignated and hr3.reserve == 0, "dedesignated, the reserve gone");
switch (Hedges.planDedesignate(hedges, hedgeId, D0 + 4)) { case (#err(_)) {}; case (#ok(_)) check(false, "a dedesignation twice") };
Debug.print("count: hedges through designation, assessment and dedesignation = 1");
Debug.print("count: refusals = 2");

// ─── the folds under a re-fold ───
let attr2 = Attr.newState(RI.newArena());
let t2 = Treasury.newState(RI.newArena());
for ((b, ev, _) in List.values(attrEvents)) { let before = switch (ev) { case (#couponPaid(x)) Treasury.row(t2, x.deal); case (#legSettled(x)) Treasury.row(t2, x.deal); case (_) null }; Treasury.fold(t2, b, ev); Attr.observeTreasury(attr2, b, ev, before) };
for ((id, day, th) in List.values(thetas)) Attr.observeTheta(attr2, id, day, th);
func fpA(x : Attr.State) : Blob { let w = C.Writer(); Attr.fingerprintInto(w, x); w.toBlob() };
check(fpA(attr) == fpA(attr2), "the attribution fingerprint is reproduced by the re-fold");
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Valuation: all checks passed");
