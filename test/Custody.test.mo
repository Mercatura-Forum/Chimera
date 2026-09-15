/// Custody.test.mo: securities services on the pure layer: the instrument extended, two depots opened, two lots
/// bought and settled into their depots, a transfer free of payment, a sale checked against the delivering depot,
/// a depot held by a custodian's account and a transfer to it and back instructed to the venue (the nominal
/// encumbered where it leaves, moved by the settlement), a sale from that depot refused,
/// an announced coupon entitled on the contractual basis at the record date, claimed on the instrument's own coupon
/// date with the lot's accrual cleared into it and the accrual restarting without a reversal, then paid at the
/// payment date; a partial redemption on the actual basis across the depots a lot sits in, an early redemption
/// consuming a lot to nothing, a cash distribution to income, a cancellation; every posting balancing and landing
/// on the policy's accounts; the amounts equal to hand-computed figures; the fold's fingerprint equal under a
/// re-fold of the same events.
// engine: wasi-only

import Array "mo:core/Array";
import Debug "mo:core/Debug";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import TT "mo:manticore/TreasuryTypes";
import Treasury "mo:manticore/TreasuryCore";
import Fx "mo:manticore/Fx";
import M "mo:manticore/TreasuryMath";

import CT "../src/CustodyTypes";
import Core "../src/CustodyCore";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func fpCustody(c : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, c); w.toBlob() };
func fpTreasury(t : Treasury.State) : Blob { let w = C.Writer(); Treasury.fingerprintInto(w, t); w.toBlob() };

// ─── two states folded side by side, and the events kept for the re-fold ───
type Folded = { #treasury : TT.TreasuryEvent; #custody : CT.Event };
let arena = RI.newArena();
let t = Treasury.newState(arena);
let c = Core.newState(arena);
let folded = List.empty<(Nat, Folded)>();
var block = 300;
func next() : Nat { block += 1; block };
func rowOf(id : Nat) : ?Treasury.DealRow { Treasury.row(t, id) };
func isinOf(id : Nat) : Text { switch (Treasury.row(t, id)) { case (?r) r.isin; case null "" } };
func applyT(ev : TT.TreasuryEvent) : Nat { let b = next(); Treasury.fold(t, b, ev); Core.observeTreasury(c, ev, rowOf); List.add(folded, (b, #treasury(ev))); b };
func applyC(ev : CT.Event) : Nat { let b = next(); Core.fold(c, b, ev, isinOf); List.add(folded, (b, #custody(ev))); b };
func balances(legs : [JT.Leg]) : Bool {
  for (l in legs.vals()) {
    var d : Int = 0;
    for (m in legs.vals()) { if (Text.equal(m.currency, l.currency)) { if (m.side == #debit) d += m.amount else d -= m.amount } };
    if (d != 0) return false;
  };
  true
};
func net(legs : [JT.Leg], account : Text) : Int { var d : Int = 0; for (m in legs.vals()) { if (Text.equal(m.account, account)) { if (m.side == #debit) d += m.amount else d -= m.amount } }; d };
func netSub(legs : [JT.Leg], account : Text, sub : ?JT.SubledgerKey) : Int { var d : Int = 0; for (m in legs.vals()) { if (Text.equal(m.account, account) and m.subledger == sub) { if (m.side == #debit) d += m.amount else d -= m.amount } }; d };
func okC<X>(r : { #ok : X; #err : CT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) { check(false, what # " refused: " # debug_show e); loop {} } } };
func okT<X>(r : { #ok : X; #err : TT.TreasuryError }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) { check(false, what # " refused: " # debug_show e); loop {} } } };
var refused = 0;
func refusedC<X>(r : { #ok : X; #err : CT.Error }, what : Text) { switch (r) { case (#ok(_)) check(false, what # " accepted"); case (#err(_)) refused += 1 } };

// ─── the world: Manticore's treasury with one bond and the desk's policy ───
let D0 = 20_710;
let EGP = "EGP";
let p = S.policy;
let cashEgp : TT.CashAccount = { account = "1101"; sub = null };
let trader = Principal.fromText("aaaaa-aa");
let ctx : Treasury.Ctx = {
  functional = EGP;
  spot = func(_ : Text, _ : Nat) : ?Fx.Rate { null };
  pair = func(_ : Text) : ?Fx.PositionPair { null };
  fixing = func(_ : Text, _ : Nat) : ?Nat { null };
  isShariaBook = func(_ : Text) : Bool { false };
};
var termsTable : [(Nat, TT.DealKind)] = [];
func terms(b : Nat) : ?TT.DealKind { for ((k, v) in termsTable.vals()) { if (k == b) return ?v }; null };
ignore applyT(okT(Treasury.planPolicy(p), "treasury policy"));
let bond : TT.SecurityTerms = { isin = "EG0000012345"; issuer = "ARE"; currency = EGP; couponBps = 1200; couponsPerYear = 2; dayCount = #a001_ActActIcma({ couponsPerYear = 2 }); issue = 20_500; maturity = 21_596 };
ignore applyT(okT(Treasury.planRegisterSecurity(t, bond, D0), "security"));
let ?sec = Treasury.security(t, bond.isin) else { check(false, "security row"); loop {} };

// ─── configuration and its refusals ───
let x : CT.Extension = { isin = bond.isin; lei = "5493001KJTIIGC8Y1R12"; classification = #sovereign; market = "EGX"; settlementCycleDays = 2; quotation = #pricePer100; minDenomination = 100_00 };
refusedC(Core.planAnnounce(c, t, S.announcement, D0), "announce before the policy");
ignore applyC(okC(Core.planPolicy({ entitlementBasis = #contractual }), "policy"));
refusedC(Core.planExtend(c, t, { x with isin = "XX0000000000" }, D0), "extend an unknown instrument");
refusedC(Core.planExtend(c, t, { x with lei = "short" }, D0), "extend with a malformed LEI");
ignore applyC(okC(Core.planExtend(c, t, x, D0), "extend"));
let d1 : CT.Depot = { id = "DEPOT-CITI"; custodian = S.citi; place = "MCSD"; safekeepingAccount = "SAFE-001" };
let d2 : CT.Depot = { id = "DEPOT-HSBC"; custodian = { S.citi with name = "HSBC"; bic = "MIDLGB22" }; place = "MCSD"; safekeepingAccount = "SAFE-002" };
ignore applyC(okC(Core.planOpenDepot(c, d1, D0), "depot 1"));
refusedC(Core.planOpenDepot(c, d1, D0), "depot twice");
refusedC(Core.planOpenDepot(c, { d2 with custodian = { d2.custodian with bic = "X" } }, D0), "depot with a malformed BIC");
ignore applyC(okC(Core.planOpenDepot(c, d2, D0), "depot 2"));
refusedC(Core.planSetBookDepot(c, "BR01", "DEPOT-NONE", D0), "book depot unknown");
ignore applyC(okC(Core.planSetBookDepot(c, "BR01", d1.id, D0), "book depot"));
check(Core.bookDepotOf(c, "BR01") == ?d1.id, "the book's depot read back");
check(Core.instrument(c, bond.isin) != null, "the extension read back");
Debug.print("count: configuration acts = 5");

// ─── two lots bought and settled into their depots ───
func capture(kind : TT.DealKind) : Nat {
  let id = block + 1;
  let r = okT(Treasury.planCapture(t, id, "BR01", S.citi, kind, "REF-" # Nat.toText(id), trader, D0, null, ctx, terms), "capture");
  let b = applyT(r.ev);
  check(b == id, "the deal id is its block");
  termsTable := Array.concat(termsTable, [(b, kind)]);
  for (e in r.extras.vals()) ignore applyT(e);
  b
};
func settle(id : Nat, leg : Nat, day : Nat) : Treasury.Act {
  let ?r = Treasury.row(t, id) else { check(false, "row"); loop {} };
  let ?k = terms(r.termsBlock) else { check(false, "terms"); loop {} };
  let a = okT(Treasury.planSettleLeg(t, r, k, leg, day, ctx), "settle");
  check(balances(a.legs), "settlement balances");
  ignore applyT(a.ev);
  for (e in a.extras.vals()) ignore applyT(e);
  a
};
let buyA : TT.SecurityTrade = { isin = bond.isin; direction = #buy; nominal = 10_000_000_00; priceMicro = 98_000_000; settlement = D0 + 2; classification = #fvoci; cash = cashEgp; priceCurve = bond.isin; venue = null };
let buyB : TT.SecurityTrade = { buyA with nominal = 5_000_000_00; priceMicro = 97_500_000; settlement = D0 + 5; classification = #amortisedCost };
let lotA = capture(#security(buyA));
ignore applyC(okC(Core.planAssignDealDepot(c, t, lotA, d1.id, D0), "assign A"));
refusedC(Core.planAssignDealDepot(c, t, lotA, "DEPOT-NONE", D0), "assign to an unknown depot");
ignore settle(lotA, 0, D0 + 2);
refusedC(Core.planAssignDealDepot(c, t, lotA, d2.id, D0 + 2), "assign a settled lot");
let lotB = capture(#security(buyB));
ignore applyC(okC(Core.planAssignDealDepot(c, t, lotB, d2.id, D0), "assign B"));
ignore settle(lotB, 0, D0 + 5);
check(Core.holding(c, lotA, d1.id) == buyA.nominal, "lot A lands in its depot at settlement");
check(Core.holding(c, lotB, d2.id) == buyB.nominal, "lot B lands in its depot at settlement");
check(Core.position(c, d1.id, bond.isin).nominal == buyA.nominal, "depot 1 position");
Debug.print("count: lots settled into their depots = 2");

// ─── a transfer free of payment, and the delivering depot checked before a sale ───
refusedC(Core.planTransfer(c, lotA, d1.id, d2.id, buyA.nominal + 1, "fop-0", D0 + 6), "transfer beyond the holding");
refusedC(Core.planTransfer(c, lotA, d1.id, d1.id, 1, "fop-0", D0 + 6), "transfer to the same depot");
ignore applyC(okC(Core.planTransfer(c, lotA, d1.id, d2.id, 3_000_000_00, "fop-1", D0 + 6), "transfer"));
check(Core.holding(c, lotA, d1.id) == 7_000_000_00 and Core.holding(c, lotA, d2.id) == 3_000_000_00, "the holding moved");
let pos2 = Core.position(c, d2.id, bond.isin);
check(pos2.nominal == 8_000_000_00 and pos2.lots == 2, "depot 2 holds two lots after the transfer");
check(Core.heldOfLot(c, lotA) == buyA.nominal, "the lot's holdings across depots equal its nominal");
let sellRow : Treasury.DealRow = { (switch (Treasury.row(t, lotA)) { case (?r) r; case null loop {} }) with id = 999 };
let sale8 : TT.SecurityTrade = { buyA with direction = #sell; nominal = 8_000_000_00; settlement = D0 + 8 };
ignore applyC(#dealDepotAssigned({ deal = 999; depot = d1.id; day = D0 + 6 }));
switch (Core.checkSaleDepot(c, t, sellRow, sale8, #fifo)) { case (?#DepotShort(e)) { check(e.held == 7_000_000_00 and e.wanted == 8_000_000_00, "the short is measured against the lot's holding in the depot"); refused += 1 }; case (_) check(false, "a sale of 8M from a depot holding 7M is short") };
check(Core.checkSaleDepot(c, t, sellRow, { sale8 with nominal = 6_000_000_00 }, #fifo) == null, "a sale of 6M from a depot holding 7M delivers");
Debug.print("count: transfers = 1");
Debug.print("count: sale depot checks = 2");

// ─── a depot held by another party's account, and a transfer to it through the venue ───
let custodian = Principal.fromText("2ibo7-dia");
refusedC(Core.planSetDepotAccount(c, "DEPOT-NONE", ?custodian, D0 + 6), "an account on an unknown depot");
refusedC(Core.planSetDepotAccount(c, d2.id, ?Principal.fromText("2vxsx-fae"), D0 + 6), "the anonymous principal as a holder");
refusedC(Core.planSetDepotAccount(c, d2.id, null, D0 + 6), "clearing an account never set");
refusedC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 1_000_000_00, "fop-2"), "a transfer through the venue between two depots of the desk's own");
ignore applyC(okC(Core.planSetDepotAccount(c, d2.id, ?custodian, D0 + 6), "the custodian holds depot 2"));
check(Core.depotAccount(c, d2.id) == ?custodian and Core.heldByDesk(c, d1.id) and not Core.heldByDesk(c, d2.id), "depot 2 is held by the custodian's account, depot 1 by the desk's");
refusedC(Core.planSetDepotAccount(c, d2.id, ?custodian, D0 + 6), "the same account set twice");
check(Core.checkSaleDepot(c, t, sellRow, { sale8 with nominal = 1_000_000_00 }, #fifo) == null, "a sale delivering from depot 1 stays the desk's");
ignore applyC(#dealDepotAssigned({ deal = 999; depot = d2.id; day = D0 + 6 }));
switch (Core.checkSaleDepot(c, t, sellRow, { sale8 with nominal = 1_000_000_00 }, #fifo)) { case (?#InvalidTerms(_)) refused += 1; case (_) check(false, "a sale from a depot another party holds is refused: the desk cannot escrow it") };
refusedC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 7_000_000_01, "fop-2"), "a transfer beyond the depot's available");
refusedC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 0, "fop-2"), "a transfer of nothing");
refusedC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 1_000_000_00, ""), "a transfer without a reference");
let out = okC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 2_000_000_00, "fop-2"), "a transfer to the custodian's depot");
check(out.deskDelivers and out.counterparty == custodian, "the desk delivers to the custodian");
let back = okC(Core.planInstructTransfer(c, lotA, d2.id, d1.id, 1_000_000_00, "fop-3"), "a transfer back from the custodian's depot");
check(not back.deskDelivers and back.counterparty == custodian, "the custodian delivers to the desk");
ignore applyC(#transferInstructed({ lot = lotA; from = d1.id; to = d2.id; nominal = 2_000_000_00; reference = "fop-2"; instruction = 777; day = D0 + 6 }));
check(Core.available(c, lotA, d1.id) == 5_000_000_00 and Core.holding(c, lotA, d1.id) == 7_000_000_00 and Core.holding(c, lotA, d2.id) == 3_000_000_00, "instructed, the nominal is encumbered where it leaves and has not moved");
switch (Core.transfer(c, 777)) { case (?tr) check(tr.lot == lotA and tr.nominal == 2_000_000_00 and not tr.settled and Core.transferView(c, tr).from == d1.id, "the transfer stands under its instruction"); case null check(false, "the transfer row") };
refusedC(Core.planInstructTransfer(c, lotA, d1.id, d2.id, 5_000_000_01, "fop-4"), "a second transfer beyond what the first leaves available");
ignore applyC(#transferSettled({ instruction = 777; day = D0 + 7 }));
check(Core.holding(c, lotA, d1.id) == 5_000_000_00 and Core.holding(c, lotA, d2.id) == 5_000_000_00 and Core.available(c, lotA, d1.id) == 5_000_000_00, "settled, the nominal moved and the encumbrance lifted");
ignore applyC(#transferSettled({ instruction = 777; day = D0 + 7 }));
check(Core.holding(c, lotA, d1.id) == 5_000_000_00 and Core.holding(c, lotA, d2.id) == 5_000_000_00, "a settlement folded twice moves nothing twice");
switch (Core.transfer(c, 777)) { case (?tr) check(tr.settled, "the transfer is settled"); case null check(false, "the transfer row") };
check(Core.heldOfLot(c, lotA) == buyA.nominal, "the lot's holdings across depots still equal its nominal");
// the custodian delivers the nominal back: the desk the taker, the holdings as before
ignore applyC(#transferInstructed({ lot = lotA; from = d2.id; to = d1.id; nominal = 2_000_000_00; reference = "fop-3"; instruction = 778; day = D0 + 7 }));
check(Core.available(c, lotA, d2.id) == 3_000_000_00, "the nominal coming back is encumbered in the custodian's depot");
ignore applyC(#transferSettled({ instruction = 778; day = D0 + 7 }));
check(Core.holding(c, lotA, d1.id) == 7_000_000_00 and Core.holding(c, lotA, d2.id) == 3_000_000_00 and Core.available(c, lotA, d2.id) == 3_000_000_00, "back, the holdings are as before the two transfers");
ignore applyC(#dealDepotAssigned({ deal = 999; depot = d1.id; day = D0 + 7 }));
Debug.print("count: transfers through the venue = 2");
Debug.print("count: depot account checks = 3");

// ─── the announced coupon: entitled at the record date, claimed on the grid date, paid at the payment date ───
let periods = Treasury.couponPeriodsOf(sec, 100_00);
var grid = 0;
for (pd in periods.vals()) { if (grid == 0 and pd.end > D0 + 10) grid := pd.end };
check(grid > 0, "a coupon date after the purchases");
func accrueAll(day : Nat) : Int {
  var posted : Int = 0;
  for (id in [lotA, lotB].vals()) {
    let ?r = Treasury.row(t, id) else { check(false, "row"); loop {} };
    let ?k = terms(r.termsBlock) else { check(false, "terms"); loop {} };
    switch (okT(Treasury.planAccrue(t, r, k, day), "accrue")) { case (?a) { check(balances(a.legs), "accrual balances"); ignore applyT(a.ev); switch (a.ev) { case (#accrued(e)) posted += e.interest; case (_) {} } }; case null {} };
  };
  posted
};
var d = D0 + 6;
while (d < grid - 2) { ignore accrueAll(d); d += 1 };
let coupon : CT.Announcement = { isin = bond.isin; kind = #coupon({ perHundredMicro = 6_000_000 }); recordDate = grid - 2; exDate = grid - 3; paymentDate = grid + 2; source = S.announcement.source };
refusedC(Core.planAnnounce(c, t, { coupon with recordDate = coupon.paymentDate + 1 }, D0), "dates out of order");
refusedC(Core.planAnnounce(c, t, { coupon with isin = "XX0000000000" }, D0), "announce for an unknown instrument");
refusedC(Core.planAnnounce(c, t, { coupon with kind = #partialRedemption({ ratioBps = 10_000 }) }, D0), "a partial redemption of everything");
let act1 = applyC(okC(Core.planAnnounce(c, t, coupon, D0 + 6), "announce the coupon"));
check(Core.announcedCouponCovers(c, bond.isin, grid) and not Core.announcedCouponCovers(c, bond.isin, grid + 3), "the announcement covers the grid date and not the day after its payment");
func lotsOfIsin() : [(Treasury.DealRow, ?Text)] { Array.map<Treasury.DealRow, (Treasury.DealRow, ?Text)>(Array.filter<Treasury.DealRow>(Treasury.dealsOfBook(t, "BR01"), func(r) { r.kind == 4 and Treasury.isOpen(r) and r.nominalLeft > 0 and (r.flags & Treasury.F_BUY) != 0 }), func(r) { (r, Core.dealDepotOf(c, r.id)) }) };
let ?r1 = Core.action(c, act1) else { check(false, "action row"); loop {} };
refusedC(Core.planEntitle(c, r1, lotsOfIsin, coupon.recordDate - 1), "entitle before the record date");
ignore accrueAll(coupon.recordDate);
let ents = okC(Core.planEntitle(c, r1, lotsOfIsin, coupon.recordDate), "entitle");
for (e in ents.vals()) ignore applyC(e);
var entitledTotal = 0;
for (e in Core.entitlementsOf(c, act1).vals()) {
  entitledTotal += e.amount;
  if (e.lot == lotA) check(e.amount == 600_000_00 and e.nominal == buyA.nominal, "lot A entitled to 6.00 per 100 on 10,000,000.00");
  if (e.lot == lotB) check(e.amount == 300_000_00 and e.nominal == buyB.nominal, "lot B entitled to 6.00 per 100 on 5,000,000.00");
};
check(entitledTotal == 900_000_00, "the entitlement total");
check(Core.claimDay(sec, r1) == grid, "the claim day is the instrument's own coupon date inside the announcement");
Debug.print("count: entitlements on the contractual basis = " # Nat.toText(Core.entitlementsOf(c, act1).size()));
// accrue to the grid date, claim there: the announced amount into the claim's receivable, the lot's accrual cleared
d := coupon.recordDate + 1;
while (d <= grid) { ignore accrueAll(d); d += 1 };
let ?r1e = Core.action(c, act1) else loop {};
check(r1e.state == #entitled, "the action is entitled");
var claims = 0;
for (e in Core.entitlementsOf(c, act1).vals()) {
  let ?lot = Treasury.row(t, e.lot) else loop {};
  refusedC(Core.planPay(c, p, r1e, e, lot, cashEgp, EGP, grid + 2), "pay before the claim");
  let claim = okC(Core.planClaim(p, r1e, e, lot, EGP, grid), "claim");
  check(balances(claim.legs), "the claim balances");
  check(netSub(claim.legs, p.couponReceivable, ?Core.claimSub(act1, e.lot)) == e.amount, "the claim's receivable carries the announced amount");
  check(netSub(claim.legs, p.couponReceivable, ?Treasury.dealSub(e.lot)) == -lot.accruedPosted, "the lot's accrued coupon is cleared into the claim");
  check(net(claim.legs, p.couponIncome) == -((e.amount : Int) - lot.accruedPosted), "the difference is income");
  switch (claim.treasury) { case (?te) ignore applyT(te); case null check(false, "the claim resets the lot's accrual through Manticore's coupon event") };
  ignore applyC(claim.ev);
  switch (Treasury.row(t, e.lot)) { case (?l2) check(l2.accruedPosted == 0, "accrual reset after the claim"); case null {} };
  claims += 1;
};
// the day after: the accrual restarts from the grid and posts a positive figure, no reversal
let restart = accrueAll(grid + 1);
check(restart > 0, "the accrual after the claim is positive: " # debug_show restart);
Debug.print("count: coupon entitlements claimed on the grid date = " # Nat.toText(claims));
var paid = 0;
for (e in Core.entitlementsOf(c, act1).vals()) {
  let ?lot = Treasury.row(t, e.lot) else loop {};
  refusedC(Core.planPay(c, p, r1e, e, lot, cashEgp, EGP, grid + 1), "pay before the payment date");
  let pay = okC(Core.planPay(c, p, r1e, e, lot, cashEgp, EGP, grid + 2), "pay");
  check(balances(pay.legs) and net(pay.legs, cashEgp.account) == e.amount and netSub(pay.legs, p.couponReceivable, ?Core.claimSub(act1, e.lot)) == -(e.amount : Int), "cash against the claim");
  check(pay.treasury == null, "a claimed coupon's payment moves no treasury row");
  ignore applyC(pay.ev);
  paid += 1;
};
ignore applyC(#paid({ action = act1; lots = paid; total = entitledTotal; day = grid + 2 }));
switch (Core.action(c, act1)) { case (?r) check(r.state == #paid and r.paid == 900_000_00, "the action is paid in full"); case null check(false, "action") };
Debug.print("count: coupon entitlements paid = " # Nat.toText(paid));

// ─── a partial redemption on the actual basis, across the depots lot A sits in ───
ignore applyC(okC(Core.planPolicy({ entitlementBasis = #actual }), "policy actual"));
let partial : CT.Announcement = { coupon with kind = #partialRedemption({ ratioBps = 2500 }); recordDate = grid + 5; exDate = grid + 4; paymentDate = grid + 7 };
let act2 = applyC(okC(Core.planAnnounce(c, t, partial, grid + 3), "announce the partial redemption"));
let ?r2 = Core.action(c, act2) else loop {};
for (e in okC(Core.planEntitle(c, r2, lotsOfIsin, grid + 5), "entitle partial").vals()) ignore applyC(e);
for (e in Core.entitlementsOf(c, act2).vals()) {
  check(e.basis == #actual, "actual basis recorded");
  if (e.lot == lotA) check(e.nominal == buyA.nominal and e.amount == 2_500_000_00, "lot A's actual holdings across both depots, a quarter returned at par");
  if (e.lot == lotB) check(e.nominal == buyB.nominal and e.amount == 1_250_000_00, "lot B's quarter");
};
let ?r2e = Core.action(c, act2) else loop {};
var realisedTotal : Int = 0;
for (e in Core.entitlementsOf(c, act2).vals()) {
  let ?lot = Treasury.row(t, e.lot) else loop {};
  let before = lot.nominalLeft;
  let pay = okC(Core.planPay(c, p, r2e, e, lot, cashEgp, EGP, grid + 7), "pay partial");
  check(balances(pay.legs), "the redemption balances");
  switch (pay.ev) { case (#entitlementPaid(x)) { check(x.nominal == before / 4 and x.amount == before / 4, "a quarter of the lot at par"); realisedTotal += x.realised }; case (_) check(false, "paid event") };
  switch (pay.treasury) { case (?te) ignore applyT(te); case null check(false, "a redemption consumes the lot through Manticore's event") };
  ignore applyC(pay.ev);
  switch (Treasury.row(t, e.lot)) { case (?l2) check(l2.nominalLeft == before - before / 4, "the lot's nominal left after the partial redemption"); case null {} };
};
ignore applyC(#paid({ action = act2; lots = 2; total = 3_750_000_00; day = grid + 7 }));
check(Core.holding(c, lotA, d1.id) == 4_500_000_00 and Core.holding(c, lotA, d2.id) == 3_000_000_00, "the redeemed nominal left lot A's own depot first");
check(Core.holding(c, lotB, d2.id) == 3_750_000_00, "lot B's holding after the partial redemption");
check(realisedTotal != 0, "a discount lot redeemed at par realises a result: " # debug_show realisedTotal);
Debug.print("count: partial redemption entitlements paid = 2");

// ─── an early redemption at 101.00 consumes lot B to nothing ───
let early : CT.Announcement = { coupon with kind = #earlyRedemption({ priceMicro = 101_000_000 }); recordDate = grid + 9; exDate = grid + 9; paymentDate = grid + 10 };
let act3 = applyC(okC(Core.planAnnounce(c, t, early, grid + 8), "announce the early redemption"));
let ?r3 = Core.action(c, act3) else loop {};
for (e in okC(Core.planEntitle(c, r3, lotsOfIsin, grid + 9), "entitle early").vals()) ignore applyC(e);
let ?r3e = Core.action(c, act3) else loop {};
var consumed = 0;
for (e in Core.entitlementsOf(c, act3).vals()) {
  if (e.lot != lotB) continue;
  check(e.amount == 3_787_500_00, "lot B's 3,750,000.00 at 101.00");
  let ?lot = Treasury.row(t, lotB) else loop {};
  let pay = okC(Core.planPay(c, p, r3e, e, lot, cashEgp, EGP, grid + 10), "pay early");
  check(balances(pay.legs) and net(pay.legs, cashEgp.account) == 3_787_500_00 + lot.accruedPosted, "the proceeds and the accrued coupon come in");
  switch (pay.treasury) { case (?te) ignore applyT(te); case null check(false, "consumed") };
  ignore applyC(pay.ev);
  consumed += 1;
};
switch (Treasury.row(t, lotB)) { case (?l2) check(l2.nominalLeft == 0 and l2.costLeft == 0, "lot B consumed to nothing"); case null {} };
check(Core.holding(c, lotB, d2.id) == 0 and Core.position(c, d2.id, bond.isin).lots == 1, "lot B left its depot");
Debug.print("count: early redemption lots consumed = " # Nat.toText(consumed));

// ─── a cash distribution to income, and a cancellation ───
let dist : CT.Announcement = { coupon with kind = #cashDistribution({ perHundredMicro = 500_000 }); recordDate = grid + 12; exDate = grid + 12; paymentDate = grid + 12 };
let act4 = applyC(okC(Core.planAnnounce(c, t, dist, grid + 11), "announce the distribution"));
let ?r4 = Core.action(c, act4) else loop {};
for (e in okC(Core.planEntitle(c, r4, lotsOfIsin, grid + 12), "entitle distribution").vals()) ignore applyC(e);
let ?r4e = Core.action(c, act4) else loop {};
for (e in Core.entitlementsOf(c, act4).vals()) {
  let ?lot = Treasury.row(t, e.lot) else loop {};
  let pay = okC(Core.planPay(c, p, r4e, e, lot, cashEgp, EGP, grid + 12), "pay distribution");
  check(e.amount == 37_500_00 and net(pay.legs, p.couponIncome) == -37_500_00, "0.50 per 100 on 7,500,000.00 to income");
  ignore applyC(pay.ev);
};
let act5 = applyC(okC(Core.planAnnounce(c, t, { dist with recordDate = grid + 20; exDate = grid + 20; paymentDate = grid + 20 }, grid + 12), "announce one to cancel"));
ignore applyC(okC(Core.planCancel(c, act5, "withdrawn by the issuer", grid + 12), "cancel"));
refusedC(Core.planCancel(c, act5, "again", grid + 12), "cancel twice");
refusedC(Core.planCancel(c, 12345, "unknown", grid + 12), "cancel an unknown action");
let ?r5 = Core.action(c, act5) else loop {};
refusedC(Core.planEntitle(c, r5, lotsOfIsin, grid + 20), "entitle a cancelled action");
check(Core.actionsOf(c, bond.isin).size() == 5 and Core.actionsInState(c, #paid).size() == 2 and Core.actionsInState(c, #cancelled).size() == 1, "the actions by instrument and by state");
Debug.print("count: cash distribution entitlements paid = 1");
Debug.print("count: cancellations = 1");
Debug.print("count: refusals = " # Nat.toText(refused));

// ─── the fold's fingerprint under a re-fold of the same events ───
let st = Core.status(c);
check(st.instruments == 1 and st.depots == 2 and st.actions == 5 and st.transfers == 3, "the status counts");
Debug.print("count: holdings rows = " # Nat.toText(st.holdings));
Debug.print("count: entitlement rows = " # Nat.toText(st.entitlements));
let arena2 = RI.newArena();
let t2 = Treasury.newState(arena2);
let c2 = Core.newState(arena2);
for ((b, ev) in List.values(folded)) {
  switch (ev) {
    case (#treasury(te)) { Treasury.fold(t2, b, te); Core.observeTreasury(c2, te, func(id : Nat) : ?Treasury.DealRow { Treasury.row(t2, id) }) };
    case (#custody(ce)) Core.fold(c2, b, ce, func(id : Nat) : Text { switch (Treasury.row(t2, id)) { case (?r) r.isin; case null "" } });
  };
};
check(fpCustody(c) == fpCustody(c2), "the custody fingerprint is reproduced by the re-fold");
check(fpTreasury(t) == fpTreasury(t2), "the treasury fingerprint is reproduced by the re-fold");
Debug.print("count: fingerprint checks = 2");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Custody: all checks passed");
