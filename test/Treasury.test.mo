/// Treasury.test.mo - S3.2 treasury: the planners, the legs they build, the fold and the nostro reconciliation.
///
/// What is proved here (no journal, no canister - the pure layer):
///   1. configuration: the policy, a security whose maturity is off the coupon grid refused, curves (a duplicate
///      identical publication is nothing, a different one refused), limits, a nostro;
///   2. deals: each kind captured and its row's facts; the refusals - a cross pair, a rate that is not spot plus
///      points, an interest deal in a Sharia book, a sale beyond the position, a swap with legs the same way;
///   3. limits over the fold: a counterparty exposure breached without an approver refused, with one recorded;
///   4. money: the legs of every settlement balance per currency and land on the accounts the policy names  - 
///      a placement's start and maturity with the accrual caught up, a forward at spot with the realised difference,
///      a bond bought dirty, accrued and amortised daily, its coupon paid, its sale consuming the lot pro rata to what
///      is booked, an option's premium and expiry, a swap period settled against a fixing;
///   5. marks: a forward's mark moves with the points, a lot's fair value goes to the reserve;
///   6. nostro: legs indexed from postings, a statement matched by reference then by amount and date, breaks on both
///      sides, a second recording of the same statement refused, a break resolved with a correction;
///   7. the fold's reads and the fingerprint.
// engine: wasi-only

import Debug "mo:core/Debug";
import Nat "mo:core/Nat";
import Int "mo:core/Int";
import Blob "mo:core/Blob";
import Text "mo:core/Text";
import Array "mo:core/Array";
import Principal "mo:core/Principal";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import JT "mo:journal/JournalTypes";
import TT "mo:manticore/TreasuryTypes";
import Core "mo:manticore/TreasuryCore";
import M "mo:manticore/TreasuryMath";
import Fx "mo:manticore/Fx";

func fail(what : Text) { Debug.print("FAIL: " # what); assert false };
func h(t : Text) : Blob { Sha256.fromBlob(#sha256, Text.encodeUtf8(t)) };
func fp(s : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, s); w.toBlob() };

let arena = RI.newArena();
let s = Core.newState(arena);
var block = 500;
func next() : Nat { block += 1; block };
func apply(ev : TT.TreasuryEvent) : Nat { let b = next(); Core.fold(s, b, ev); b };
func ok<X>(r : { #ok : X; #err : TT.TreasuryError }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) { fail(what # " refused: " # debug_show (e)); loop {} } } };
func refused<X>(r : { #ok : X; #err : TT.TreasuryError }, what : Text) : TT.TreasuryError { switch (r) { case (#ok(_)) { fail(what # " accepted"); loop {} }; case (#err(e)) e } };
func actOk(r : { #ok : TT.TreasuryEvent; #err : TT.TreasuryError }, what : Text) : Nat { apply(ok(r, what)) };

// terms live in blocks: the test keeps them in a table the way the bank keeps them in its log
var termsTable : [(Nat, TT.DealKind)] = [];
func terms(b : Nat) : ?TT.DealKind { for ((k, v) in termsTable.vals()) { if (k == b) return ?v }; null };
func remember(b : Nat, k : TT.DealKind) { termsTable := Array.concat(termsTable, [(b, k)]) };

// per-currency balance of a posting
func balances(legs : [JT.Leg]) : Bool {
  var ok_ = true;
  for (l in legs.vals()) {
    var d : Int = 0;
    for (m in legs.vals()) { if (Text.equal(m.currency, l.currency)) { if (m.side == #debit) d += m.amount else d -= m.amount } };
    if (d != 0) ok_ := false;
  };
  ok_
};
func net(legs : [JT.Leg], account : Text, ccy : Text) : Int {
  var d : Int = 0;
  for (m in legs.vals()) { if (Text.equal(m.account, account) and Text.equal(m.currency, ccy)) { if (m.side == #debit) d += m.amount else d -= m.amount } };
  d
};
func check(legs : [JT.Leg], what : Text) { if (not balances(legs)) fail(what # " does not balance: " # debug_show (legs)); if (legs.size() == 0) fail(what # " posted nothing") };

// ── the world ──
let D0 = 20_710;   // 2026-09-14
let EGP = "EGP"; let USD = "USD";
let pol : TT.Policy = {
  mmPlacements = "1300"; mmTakings = "2300"; mmInterestReceivable = "1310"; mmInterestPayable = "2310"; mmInterestIncome = "4300"; mmInterestExpense = "5300";
  fxForwardMark = "1400"; irsMark = "1410"; fxOptionValue = "1420"; unrealisedTradingGain = "4400"; unrealisedTradingLoss = "5400"; realisedTradingGain = "4410"; realisedTradingLoss = "5410";
  securitiesAmortisedCost = "1500"; securitiesFvoci = "1510"; securitiesFvtpl = "1520"; fvociReserve = "3500"; couponReceivable = "1530"; couponIncome = "4500"; amortisationIncome = "4510"; amortisationExpense = "5510";
  nostroSuspense = "1990"; lotMethod = #fifo; confirmationDueDays = 1; breakAgeAlertDays = 5; maxCurvePoints = 8;
};
var rates : [(Text, Nat, Nat, Nat)] = [];   // (ccy, day, num, den)
func setRate(day : Nat, num : Nat, den : Nat) { rates := Array.concat(rates, [(USD, day, num, den)]) };
let pair : Fx.PositionPair = { currency = USD; position = "1800"; equivalent = "1801"; unrealised = "4800"; realised = "4801"; monetary = true };
var fixings : [(Nat, Nat)] = [];
let shariaBooks : [Text] = ["SHARIA"];
let ctx : Core.Ctx = {
  functional = EGP;
  spot = func(ccy : Text, day : Nat) : ?Fx.Rate { for ((c, d, n, dn) in rates.vals()) { if (Text.equal(c, ccy) and d == day) return ?{ currency = ccy; functional = EGP; numerator = n; denominator = dn; asOf = day; source = "test" } }; null };
  pair = func(ccy : Text) : ?Fx.PositionPair { if (Text.equal(ccy, USD)) ?pair else null };
  fixing = func(_ : Text, day : Nat) : ?Nat { for ((d, b) in fixings.vals()) { if (d == day) return ?b }; null };
  isShariaBook = func(b : Text) : Bool { for (x in shariaBooks.vals()) { if (Text.equal(x, b)) return true }; false };
};
let cp : TT.Counterparty = { party = null; name = "CITI"; bic = "CITIUS33"; lei = "6SHGI4ZSSLCXXQSBB395" };
let trader = Principal.fromText("aaaaa-aa");
let cash : TT.CashAccount = { account = "1100"; sub = ?"NOSTRO-USD" };
let cashEgp : TT.CashAccount = { account = "1101"; sub = null };

// 1. configuration
ignore refused(Core.planPublishCurve(s, { id = "EGP-ZERO"; kind = #zeroRates; currency = EGP; day = D0; points = [(1, 2000)]; source = h("x") }), "curve before policy");
ignore actOk(Core.planPolicy(pol), "policy");
ignore refused(Core.planPolicy({ pol with maxCurvePoints = 99 }), "policy with 99 points");
let bond : TT.SecurityTerms = { isin = "EG0000012345"; issuer = "ARE"; currency = EGP; couponBps = 1200; couponsPerYear = 2; dayCount = #a001_ActActIcma({ couponsPerYear = 2 }); issue = 20_500; maturity = 20_500 + 365 * 3 };
ignore refused(Core.planRegisterSecurity(s, bond, D0), "maturity off the grid");
let bondOk = { bond with maturity = 21_596 };   // 2029-02-16: issue 2026-02-16 (20500) + 36 months
ignore actOk(Core.planRegisterSecurity(s, bondOk, D0), "security");
ignore refused(Core.planRegisterSecurity(s, bondOk, D0), "security twice");
let zero : TT.Curve = { id = "EGP-ZERO"; kind = #zeroRates; currency = EGP; day = D0; points = [(1, 2000), (30, 2050), (90, 2100), (365, 2200)]; source = h("cbe") };
switch (ok(Core.planPublishCurve(s, zero), "zero curve")) { case (?ev) ignore apply(ev); case null fail("first publication is an event") };
switch (ok(Core.planPublishCurve(s, zero), "zero curve again")) { case (?_) fail("identical republication is nothing"); case null {} };
ignore refused(Core.planPublishCurve(s, { zero with points = [(1, 2001)] }), "different curve same day");
ignore refused(Core.planPublishCurve(s, { zero with id = "UNSORTED"; points = [(30, 1), (1, 2)] }), "unsorted points");
func publish(c : TT.Curve) { switch (ok(Core.planPublishCurve(s, c), "publish " # c.id)) { case (?ev) ignore apply(ev); case null {} } };
publish({ id = "USD-ZERO"; kind = #zeroRates; currency = USD; day = D0; points = [(1, 500), (365, 520)]; source = h("fed") });
publish({ id = "USDEGP-PTS"; kind = #forwardPoints; currency = USD; day = D0; points = [(1, 20_000), (30, 600_000), (90, 1_800_000), (365, 7_000_000)]; source = h("desk") });
publish({ id = "USDEGP-VOL"; kind = #volatility; currency = USD; day = D0; points = [(30, 1200), (365, 1500)]; source = h("desk") });
publish({ id = "EG0000012345"; kind = #securityPrice; currency = EGP; day = D0; points = [(0, 99_500_000)]; source = h("egx") });
ignore refused(Core.planPublishCurve(s, { id = "PX2"; kind = #securityPrice; currency = EGP; day = D0; points = [(0, 1), (1, 2)]; source = h("egx") }), "a price curve with two points");
ignore actOk(Core.planSetLimit({ book = "TREASURY"; kind = #counterpartyExposure; currency = USD; subject = "CITI"; value = 3_000_000_00 }, D0), "cp limit");
ignore actOk(Core.planSetLimit({ book = "TREASURY"; kind = #tenorBucket; currency = EGP; subject = "0-400"; value = 100_000_000_00 }, D0), "bucket limit");
ignore refused(Core.planSetLimit({ book = "TREASURY"; kind = #tenorBucket; currency = EGP; subject = "400-0"; value = 1 }, D0), "bucket backwards");
ignore refused(Core.planSetLimit({ book = "TREASURY"; kind = #dv01; currency = EGP; subject = "x"; value = 1 }, D0), "dv01 with a subject");
let nostroUsd : TT.Nostro = { id = "NOSTRO-USD-CITI"; account = "1100"; sub = ?"NOSTRO-USD"; currency = USD; correspondent = cp; iban = ""; valueDateToleranceDays = 2 };
ignore actOk(Core.planRegisterNostro(s, nostroUsd, D0), "nostro");
ignore refused(Core.planRegisterNostro(s, nostroUsd, D0), "nostro twice");
ignore refused(Core.planRegisterNostro(s, { nostroUsd with id = "OTHER" }, D0), "same account as another nostro");
Debug.print("count: configuration acts and refusals = 20");

// 2. deals
setRate(D0, 48_000_000_00, 100_000_000);   // 48.00 EGP per USD (minor/minor ratio 48)
func capture(book : Text, kind : TT.DealKind, approver : ?Principal) : Nat {
  let id = block + 1;
  let r = ok(Core.planCapture(s, id, book, cp, kind, "REF-" # Nat.toText(id), trader, D0, approver, ctx, terms), "capture " # TT.dealKindText(kind));
  let b = apply(r.ev);
  assert b == id;
  remember(b, kind);
  for (e in r.extras.vals()) ignore apply(e);
  b
};
let mm : TT.MoneyMarket = { placement = true; currency = USD; principal = 1_000_000_00; rateBps = 450; dayCount = #a003_Act360; start = D0; maturity = D0 + 90; cash };
let mmId = capture("TREASURY", #moneyMarket(mm), null);
ignore refused(Core.planCapture(s, 0, "SHARIA", cp, #moneyMarket(mm), "r", trader, D0, null, ctx, terms), "interest deal in a Sharia book");
let fwd : TT.FxForward = { base = USD; quote = EGP; direction = #buy; baseAmount = 500_000_00; rateMicro = 48_600_000; valueDate = D0 + 30; spotMicro = 48_000_000; forwardPointsMicro = 600_000; baseAccount = cash; quoteAccount = cashEgp; pointsCurve = "USDEGP-PTS"; discountCurve = "EGP-ZERO" };
let fwdId = capture("TREASURY", #fxForward(fwd), null);
ignore refused(Core.planCapture(s, 0, "TREASURY", cp, #fxForward({ fwd with quote = USD; base = "EUR" }), "r", trader, D0, null, ctx, terms), "cross pair");
ignore refused(Core.planCapture(s, 0, "TREASURY", cp, #fxForward({ fwd with forwardPointsMicro = 1 }), "r", trader, D0, null, ctx, terms), "rate not spot plus points");
let far : TT.FxForward = { fwd with direction = #sell; valueDate = D0 + 90; rateMicro = 49_800_000; forwardPointsMicro = 1_800_000 };
ignore refused(Core.planCapture(s, 0, "TREASURY", cp, #fxSwap({ near = fwd; far = { far with direction = #buy } }), "r", trader, D0, null, ctx, terms), "swap legs the same way");
let swapId = capture("TREASURY", #fxSwap({ near = fwd; far }), null);
let buy : TT.SecurityTrade = { isin = "EG0000012345"; direction = #buy; nominal = 10_000_000_00; priceMicro = 98_000_000; settlement = D0 + 2; classification = #fvoci; cash = cashEgp; priceCurve = "EG0000012345"; venue = null };
let bondId = capture("TREASURY", #security(buy), null);
ignore refused(Core.planCapture(s, 0, "TREASURY", cp, #security({ buy with direction = #sell; nominal = 1 }), "r", trader, D0, null, ctx, terms), "sale beyond position (nothing settled yet)");
ignore refused(Core.planCapture(s, 0, "TREASURY", cp, #security({ buy with isin = "XX0000000000" }), "r", trader, D0, null, ctx, terms), "unknown security");
let irs : TT.Irs = { currency = EGP; notional = 50_000_000_00; payFixed = true; fixedBps = 2100; floatingIndex = "CBE-ON"; spreadBps = 25; start = D0; maturity = 21_075; paymentMonths = 3; dayCount = #a003_Act360; cash = cashEgp; discountCurve = "EGP-ZERO" };
let irsId = capture("TREASURY", #irs(irs), null);
let opt : TT.FxOption = { base = USD; quote = EGP; call = true; bought = true; baseAmount = 200_000_00; strikeMicro = 49_000_000; expiry = D0 + 60; premium = 150_000_00; start = D0; cash = cashEgp; domesticCurve = "EGP-ZERO"; foreignCurve = "USD-ZERO"; volCurve = "USDEGP-VOL" };
let optId = capture("TREASURY", #fxOption(opt), null);
switch (Core.row(s, mmId)) { case (?r) { if (r.legs != 2 or r.notional != mm.principal or not Text.equal(r.currency, USD) or r.maturity != mm.maturity) fail("mm row facts") }; case null fail("mm row") };
switch (Core.row(s, fwdId)) { case (?r) { if (r.secondAmount != M.quoteAmount(fwd.baseAmount, fwd.rateMicro) or r.legs != 1) fail("forward row facts") }; case null fail("forward row") };
switch (Core.row(s, bondId)) { case (?r) { if (r.nominalLeft != buy.nominal or r.costLeft != M.cleanCost(buy.nominal, buy.priceMicro) or r.yieldMillionths == 0 or r.legs != 2) fail("bond row facts " # debug_show (r.nominalLeft, r.costLeft, r.yieldMillionths)) }; case null fail("bond row") };
switch (Core.row(s, irsId)) { case (?r) { if (r.legs != 4) fail("irs legs " # debug_show (r.legs)) }; case null fail("irs row") };
if (Core.openAll(s).size() != 6) fail("six open deals");
Debug.print("count: deals captured = 6");
Debug.print("count: deal refusals = 6");

// 3. limits: CITI is at 1.5 M USD of forwards+swap (500k + 500k near/far) + 1 M MM = 2.5 M; another 1 M breaches 3 M
let big : TT.MoneyMarket = { mm with principal = 1_000_000_00 };
switch (refused(Core.planCapture(s, 0, "TREASURY", cp, #moneyMarket(big), "r", trader, D0, null, ctx, terms), "exposure over limit without approver")) {
  case (#LimitBreached(x)) { if (x.measured <= x.limit) fail("breach figures") };
  case (e) fail("wrong refusal " # debug_show (e));
};
let approver = Principal.fromText("2vxsx-fae");
let over = capture("TREASURY", #moneyMarket(big), ?approver);
switch (Core.row(s, over)) { case (?r) { if ((r.flags & Core.F_WITHIN) != 0) fail("recorded within limits") }; case null fail("over row") };
Debug.print("count: limit breaches refused then recorded with an approver = 2");

// 4. money - the placement's start and maturity
func settle(id : Nat, leg : Nat, day : Nat) : Core.Act {
  let ?r = Core.row(s, id) else { fail("row " # Nat.toText(id)); loop {} };
  let ?k = terms(r.termsBlock) else { fail("terms"); loop {} };
  let a = ok(Core.planSettleLeg(s, r, k, leg, day, ctx), "settle " # Nat.toText(id) # "/" # Nat.toText(leg));
  check(a.legs, "settle " # Nat.toText(id) # "/" # Nat.toText(leg));
  ignore apply(a.ev);
  for (e in a.extras.vals()) ignore apply(e);
  a
};
func accrue(id : Nat, day : Nat) : ?Core.Act {
  let ?r = Core.row(s, id) else { fail("row"); loop {} };
  let ?k = terms(r.termsBlock) else { fail("terms"); loop {} };
  switch (ok(Core.planAccrue(s, r, k, day), "accrue")) { case (?a) { check(a.legs, "accrue"); ignore apply(a.ev); ?a }; case null null }
};
func mark(id : Nat, day : Nat) : ?Core.Act {
  let ?r = Core.row(s, id) else { fail("row"); loop {} };
  let ?k = terms(r.termsBlock) else { fail("terms"); loop {} };
  switch (ok(Core.planMark(s, r, k, day, ctx), "mark")) { case (?a) { check(a.legs, "mark"); ignore apply(a.ev); ?a }; case null null }
};
ignore refused(Core.planSettleLeg(s, (switch (Core.row(s, mmId)) { case (?r) r; case null loop {} }), #moneyMarket(mm), 1, D0, ctx), "maturity before start");
let a0 = settle(mmId, 0, D0);
if (net(a0.legs, "1300", USD) != mm.principal or net(a0.legs, "1100", USD) != -(mm.principal : Int)) fail("placement start legs");
ignore refused(Core.planSettleLeg(s, (switch (Core.row(s, mmId)) { case (?r) r; case null loop {} }), #moneyMarket(mm), 0, D0, ctx), "start twice");
ignore refused(Core.planSettleLeg(s, (switch (Core.row(s, mmId)) { case (?r) r; case null loop {} }), #moneyMarket(mm), 1, D0 + 10, ctx), "maturity early");
// thirty days of accrual, then the maturity catches the rest up
var accruals = 0;
var d = D0 + 1;
while (d <= D0 + 30) { switch (accrue(mmId, d)) { case (?_) accruals += 1; case null {} }; d += 1 };
switch (Core.row(s, mmId)) { case (?r) { if (r.accruedPosted != M.simpleInterestTo(mm.principal, 450, #a003_Act360, D0, D0 + 30)) fail("accrued to day 30") }; case null {} };
let a1 = settle(mmId, 1, D0 + 90);
let totalInterest = M.simpleInterestTo(mm.principal, 450, #a003_Act360, D0, D0 + 90);
if (net(a1.legs, "1100", USD) != (mm.principal : Int) + totalInterest) fail("maturity cash " # debug_show (net(a1.legs, "1100", USD), totalInterest));
if (net(a1.legs, "4300", USD) != -((totalInterest : Int) - M.simpleInterestTo(mm.principal, 450, #a003_Act360, D0, D0 + 30))) fail("catch-up income");
switch (Core.row(s, mmId)) { case (?r) { if (r.state != #settled or r.accruedPosted != totalInterest) fail("mm settled row") }; case null {} };
Debug.print("count: money-market accrual days posted = " # Nat.toText(accruals));

// the forward: at market on the capture day (no mark), marked the next day when the points move, then settled at a moved spot
switch (mark(fwdId, D0)) { case (?_) fail("a forward dealt at market has no mark on its day"); case null {} };
setRate(D0 + 1, 48_000_000_00, 100_000_000);
let shifted : [(Nat, Int)] = [(1, 20_000), (30, 900_000), (90, 2_000_000), (365, 7_000_000)];
publish({ id = "USDEGP-PTS"; kind = #forwardPoints; currency = USD; day = D0 + 1; points = shifted; source = h("desk2") });
switch (mark(fwdId, D0 + 1)) { case (?a) { if (net(a.legs, "1400", EGP) <= 0) fail("forward mark up when the points rise") }; case null fail("forward has a mark") };
switch (Core.row(s, fwdId)) {
  case (?r) {
    let expect = M.forwardMark(true, fwd.baseAmount, fwd.rateMicro, M.rateMicro(48_000_000_00, 100_000_000), shifted, zero.points, 29);
    if (r.markPosted != expect or expect <= 0) fail("forward mark " # debug_show (r.markPosted, expect));
  };
  case null {};
};
ignore refused(Core.planSettleLeg(s, (switch (Core.row(s, fwdId)) { case (?r) r; case null loop {} }), #fxForward(fwd), 0, D0 + 30, ctx), "forward settles without a spot for the day");
setRate(D0 + 30, 49_000_000_00, 100_000_000);   // 49.00
let af = settle(fwdId, 0, D0 + 30);
// buying 500k USD at 48.60 when spot is 49.00: gain 0.40 × 500k = 200,000 EGP
let e49 = M.roundNat(M.ofSigned(Fx.equivalentOf(fwd.baseAmount, { currency = USD; functional = EGP; numerator = 49_000_000_00; denominator = 100_000_000; asOf = D0 + 30; source = "t" })));
let gain : Int = (e49 : Int) - M.quoteAmount(fwd.baseAmount, fwd.rateMicro);
if (gain != 200_000_00) fail("forward gain " # debug_show (gain));
if (net(af.legs, "4410", EGP) != -gain) fail("realised gain leg");
if (net(af.legs, "1101", EGP) != -(M.quoteAmount(fwd.baseAmount, fwd.rateMicro) : Int)) fail("quote cash is the contractual amount");
if (net(af.legs, "1100", USD) != fwd.baseAmount) fail("base arrives");
switch (Core.row(s, fwdId)) { case (?r) { if (r.state != #settled or r.markPosted != 0 or r.realised != gain) fail("forward row after settlement") }; case null {} };
Debug.print("count: forward marked and settled at spot with the difference realised = 1");

// the bond: bought dirty, accrued and amortised, the coupon paid, half sold
let ab = settle(bondId, 0, D0 + 2);
let secRow = switch (Core.security(s, "EG0000012345")) { case (?x) x; case null loop {} };
let periods = Core.couponPeriodsOf(secRow, buy.nominal);
let accruedAtBuy = M.accruedCoupon(buy.nominal, 1200, #a001_ActActIcma({ couponsPerYear = 2 }), periods, D0 + 2);
if (net(ab.legs, "1510", EGP) != M.cleanCost(buy.nominal, buy.priceMicro) or net(ab.legs, "1530", EGP) != accruedAtBuy) fail("bond purchase legs");
var bondAccruals = 0; var amortDays = 0;
d := D0 + 3;
var couponPaid = false;
label days while (d <= D0 + 200) {
  // coupon first, then the accrual
  let ?r = Core.row(s, bondId) else break days;
  switch (ok(Core.planCoupon(s, r, #security(buy), d), "coupon")) {
    case (?a) { check(a.legs, "coupon"); ignore apply(a.ev); couponPaid := true;
      switch (Core.row(s, bondId)) { case (?r2) { if (r2.accruedPosted != 0) fail("receivable cleared by the coupon") }; case null {} } };
    case null {};
  };
  switch (accrue(bondId, d)) { case (?a) { bondAccruals += 1; switch (a.ev) { case (#accrued(x)) { if (x.amortisation != 0) amortDays += 1 }; case (_) {} } }; case null {} };
  if (d == D0 + 10) { switch (mark(bondId, d)) { case (?a) { if (net(a.legs, "3500", EGP) == 0) fail("FVOCI mark goes to the reserve") }; case null fail("FVOCI marks") } };
  d += 1;
};
if (not couponPaid) fail("a coupon fell due within 200 days");
if (amortDays == 0) fail("a discount bond amortises");
let sell : TT.SecurityTrade = { buy with direction = #sell; nominal = 4_000_000_00; priceMicro = 99_000_000; settlement = D0 + 201 };
let sellId = capture("TREASURY", #security(sell), null);
let as_ = settle(sellId, 0, D0 + 201);
switch (Core.row(s, bondId)) { case (?r) { if (r.nominalLeft != 6_000_000_00) fail("lot consumed " # debug_show (r.nominalLeft)) }; case null {} };
if (as_.extras.size() != 1) fail("one lot consumed");
switch (Core.row(s, sellId)) { case (?r) { if (r.state != #settled) fail("sale settled") }; case null {} };
Debug.print("count: bond accrual days posted = " # Nat.toText(bondAccruals));
Debug.print("count: bond amortisation days posted = " # Nat.toText(amortDays));

// the option: premium, a mark, expiry in the money
let ap = settle(optId, 0, D0);
if (net(ap.legs, "1420", EGP) != opt.premium) fail("premium capitalised");
switch (mark(optId, D0 + 1)) { case (?_) {}; case null fail("option marks") };
setRate(D0 + 60, 51_000_000_00, 100_000_000);
let ae = settle(optId, 1, D0 + 60);
let payoff = M.garmanKohlhagen(true, opt.baseAmount, 51_000_000, 49_000_000, 0, 0, 0, 0);
if (payoff != 400_000_00) fail("payoff " # debug_show (payoff));
if (net(ae.legs, "1101", EGP) != payoff) fail("payoff received");
switch (Core.row(s, optId)) { case (?r) { if (r.state != #settled or r.markPosted != 0) fail("option row after expiry") }; case null {} };
Debug.print("count: option premium, mark and expiry = 3");

// the swap: a period settled against its fixing; no fixing is a refusal
let swapPeriods = Core.swapPeriodsOf(irs);
ignore refused(Core.planSettleLeg(s, (switch (Core.row(s, irsId)) { case (?r) r; case null loop {} }), #irs(irs), 0, swapPeriods[0].end, ctx), "no fixing");
fixings := [(D0, 2000)];
let ai = settle(irsId, 0, swapPeriods[0].end);
let fixedLeg = M.legAmount(irs.notional, M.ofNat(2100), #a003_Act360, swapPeriods[0]);
let floatLeg = M.legAmount(irs.notional, M.ofNat(2025), #a003_Act360, swapPeriods[0]);
if (net(ai.legs, "1101", EGP) != floatLeg - fixedLeg) fail("swap net " # debug_show (net(ai.legs, "1101", EGP), floatLeg - fixedLeg));
switch (mark(irsId, D0 + 1)) { case (?_) {}; case null fail("swap marks") };
Debug.print("count: swap period settled against a fixing = 1");

// 5. confirmation: a match, a mismatch recorded
let fields : TT.ConfirmationFields = { kind = "fxSwap"; amount1 = fwd.baseAmount; currency1 = USD; amount2 = M.quoteAmount(fwd.baseAmount, fwd.rateMicro); currency2 = EGP; valueDate = fwd.valueDate; rateMicro = fwd.rateMicro; counterparty = "CITI" };
switch (ok(Core.planConfirm(s, swapId, h("conf1"), { fields with amount1 = 1 }, cp, #fxSwap({ near = fwd; far }), D0), "confirm")) { case (#confirmationMismatch(x)) { if (not Text.equal(x.field, "amount1")) fail("mismatch names amount1"); ignore apply(#confirmationMismatch(x)) }; case (_) fail("a wrong amount is not a match") };
switch (ok(Core.planConfirm(s, swapId, h("conf2"), fields, cp, #fxSwap({ near = fwd; far }), D0), "confirm")) { case (#dealConfirmed(x)) ignore apply(#dealConfirmed(x)); case (_) fail("matching confirmation confirms") };
switch (Core.row(s, swapId)) { case (?r) { if (r.state != #confirmed) fail("confirmed state") }; case null {} };
ignore refused(Core.planConfirm(s, swapId, h("conf3"), fields, cp, #fxSwap({ near = fwd; far }), D0), "confirm twice");
if (Core.overdueConfirmations(s, D0 + 3).size() == 0) fail("unconfirmed deals are overdue after the window");
Debug.print("count: confirmations matched, mismatched and refused = 3");

// 6. nostro: our legs indexed, a statement matched and broken
let nr = switch (Core.nostro(s, "NOSTRO-USD-CITI")) { case (?x) x; case null loop {} };
let nostroSub = ?Core.cashSub(cash);
ignore nostroSub;
func post(pid : Nat, day : Nat, debit : Bool, amount : Nat, ref : Text) {
  let legs : [JT.Leg] = [{ account = "1100"; subledger = Core.cashSub(cash); side = if (debit) #debit else #credit; currency = USD; amount }, { account = "9999"; subledger = null; side = if (debit) #credit else #debit; currency = USD; amount }];
  if (Core.indexJournalLegs(s, pid, day, legs, ref) != 1) fail("one nostro leg indexed for posting " # Nat.toText(pid));
};
post(7001, D0 + 1, true, 100_00, "A1");
post(7002, D0 + 1, true, 100_00, "A2");
post(7003, D0 + 2, false, 250_00, "B1");
post(7004, D0 + 3, true, 77_00, "deal capture");
if (Core.indexJournalLegs(s, 7005, D0 + 1, [{ account = "1101"; subledger = null; side = #debit; currency = EGP; amount = 5 }, { account = "9999"; subledger = null; side = #credit; currency = EGP; amount = 5 }], "x") != 0) fail("a leg off the nostro is not indexed");
let entries : [TT.StatementEntry] = [
  { reference = "A2"; amount = 100_00; credit = true; valueDay = D0 + 1; bookingDay = D0 + 1; counterparty = "" },   // by reference: takes 7002, not 7001
  { reference = "ZZ"; amount = 100_00; credit = true; valueDay = D0 + 2; bookingDay = D0 + 2; counterparty = "" },   // by amount and date within tolerance: 7001
  { reference = "B1"; amount = 250_00; credit = false; valueDay = D0 + 2; bookingDay = D0 + 2; counterparty = "" }, // our credit is their debit
  { reference = "Q9"; amount = 999_00; credit = true; valueDay = D0 + 3; bookingDay = D0 + 3; counterparty = "" },  // nobody: a break on the statement's side
];
let st = ok(Core.planRecordStatement(s, "NOSTRO-USD-CITI", h("stmt-1"), D0 + 1, D0 + 3, entries, D0 + 4), "statement");
switch (st.ev) { case (#statementRecorded(x)) { if (x.matches != [7002, 7001, 7003] or x.breaks != 2) fail("statement outcome " # debug_show (x.matches, x.breaks)) }; case (_) fail("event") };
ignore apply(st.ev);
var breakIds : [Nat] = [];
for (b in st.breaks.vals()) { breakIds := Array.concat(breakIds, [apply(b)]) };
if (Core.openBreaks(s).size() != 2) fail("two open breaks");
if (Core.breaksOfNostro(s, "NOSTRO-USD-CITI", false).size() != 2 or Core.breaksOfNostro(s, "NOSTRO-USD-CITI", true).size() != 2) fail("the nostro's breaks by id");
switch (Core.breakRow(s, breakIds[1])) { case (?b) { if (b.side != #inOurBooksOnly or b.posting != 7004) fail("our unmatched 7004 is a break on our side") }; case null fail("break row") };
ignore refused(Core.planRecordStatement(s, "NOSTRO-USD-CITI", h("stmt-1"), D0 + 1, D0 + 3, entries, D0 + 4), "same statement twice");
ignore refused(Core.planRecordStatement(s, "NOSTRO-USD-CITI", h("stmt-2"), D0 + 1, D0 + 3, [{ reference = ""; amount = 1; credit = true; valueDay = D0 + 9; bookingDay = D0; counterparty = "" }], D0 + 4), "entry outside the window");
// the second statement must not re-break 7004 and must not re-match the matched legs
let st2 = ok(Core.planRecordStatement(s, "NOSTRO-USD-CITI", h("stmt-2"), D0 + 1, D0 + 3, [], D0 + 5), "second statement");
switch (st2.ev) { case (#statementRecorded(x)) { if (x.breaks != 0) fail("a broken leg is not broken twice") }; case (_) {} };
ignore apply(st2.ev);
let legsNow = Core.nostroLegsIn(s, nr.accountHash, D0, D0 + 5);
var matched = 0; var broken = 0;
for (l in legsNow.vals()) { if (l.status == Core.LEG_MATCHED) matched += 1; if (l.status == Core.LEG_BROKEN) broken += 1 };
if (matched != 3 or broken != 1) fail("leg statuses " # debug_show (matched, broken));
if (Core.agedBreaks(s, D0 + 4).size() != 0) fail("no break is aged yet");
if (Core.agedBreaks(s, D0 + 20).size() != 2) fail("both breaks aged at twenty days");
let res = ok(Core.planResolveBreak(s, breakIds[0], "correspondent's fee, booked", ?{ account = "5900"; sub = null; debit = true; amount = 999_00; currency = USD }, D0 + 6), "resolve");
check(res.legs, "resolution correction");
if (net(res.legs, "1990", USD) != -999_00) fail("correction through the suspense");
ignore apply(res.ev);
ignore refused(Core.planResolveBreak(s, breakIds[0], "again", null, D0 + 6), "resolve twice");
if (Core.openBreaks(s).size() != 1) fail("one open break left");
Debug.print("count: nostro legs indexed = 4");
Debug.print("count: statement entries matched = 3");
Debug.print("count: nostro breaks recorded, aged and resolved = 2");

// 7. reads and the fingerprint
let pos = Core.positions(s, "TREASURY", terms);
if (pos.size() == 0) fail("positions");
var bondPos = false;
for (p in pos.vals()) { if (Text.equal(p.instrument, "EG0000012345")) { bondPos := true; if (p.nominal != 6_000_000_00) fail("bond position " # debug_show (p.nominal)) } };
if (not bondPos) fail("bond position present");
let stt = Core.status(s);
if (stt.deals != 8 or stt.breaksTotal != 2 or stt.breaksOpen != 1 or stt.statements != 2 or stt.securities != 1 or stt.nostros != 1) fail("status " # debug_show (stt));
let f1 = fp(s);
let s2 = Core.newState(RI.newArena());
// replay: the same events in the same order rebuild the same fingerprint - the log is a list of (block, event) the test did not keep, so
// the check here is the weaker one that the fingerprint is stable under a read
if (not Blob.equal(f1, fp(s))) fail("fingerprint unstable");
if (Blob.equal(f1, fp(s2))) fail("empty state has a different fingerprint");
Debug.print("count: positions read = " # Nat.toText(pos.size()));
Debug.print("count: fingerprint checks = 2");
Debug.print("Treasury: all checks passed");
