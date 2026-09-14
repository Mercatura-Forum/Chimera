/// Financing.test.mo: repo, reverse repo and securities lending on the pure layer: the terms validated, a term
/// repo started with its collateral pledged lot by lot from the depot's available holdings (the pledge leaving the
/// holding but not the lot), accrued daily at the rate by the day count, marked at a falling price until the
/// shortfall beyond the threshold raises a margin call on the desk equal to the hand computation, met in cash and
/// in collateral, a substitution of at least the same haircut value, the close at maturity with the margin cash
/// back; an open repo re-priced with the accrual caught up at the old rate first; a reverse repo whose collateral
/// is received and whose call falls on the counterparty; a loan against cash collateral through fee and rebate
/// accrual, recall and return, with a coupon's lent part a manufactured payment; every posting balancing and
/// landing on the policy's accounts; the fold's fingerprint equal under a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import TT "mo:manticore/TreasuryTypes";
import M "mo:manticore/TreasuryMath";

import FT "../src/FinancingTypes";
import Core "../src/FinancingCore";
import CT "../src/CustodyTypes";
import Custody "../src/CustodyCore";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func okF<X>(r : { #ok : X; #err : FT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
var refused = 0;
func refusedF<X>(r : { #ok : X; #err : FT.Error }, what : Text) { switch (r) { case (#ok(_)) check(false, what # " accepted"); case (#err(_)) refused += 1 } };
func balances(legs : [JT.Leg]) : Bool {
  for (l in legs.vals()) { var d : Int = 0; for (m in legs.vals()) { if (Text.equal(m.currency, l.currency)) { if (m.side == #debit) d += m.amount else d -= m.amount } }; if (d != 0) return false };
  true
};
func net(legs : [JT.Leg], account : Text) : Int { var d : Int = 0; for (m in legs.vals()) { if (Text.equal(m.account, account)) { if (m.side == #debit) d += m.amount else d -= m.amount } }; d };

let arena = RI.newArena();
let f = Core.newState(arena);
let c = Custody.newState(arena);
let folded = List.empty<(Nat, FT.Event)>();
var block = 600;
func next() : Nat { block += 1; block };
func applyF(ev : FT.Event) : Nat { let b = next(); Core.fold(f, b, ev); List.add(folded, (b, ev)); b };
func applyC(ev : CT.Event) { Custody.fold(c, next(), ev, func(_ : Nat) : Text { "EG0000012345" }) };
let D0 = 20_710;
let EGP = "EGP";
let p = S.financingPolicy;
let cash = S.cash;
let ISIN = "EG0000012345";
let DEPOT = "DEPOT-CITI";

// two lots held in the depot, as the custody fold would have them after two settled purchases
applyC(#depotOpened({ depot = S.depot; day = D0 }));
applyC(#dealDepotAssigned({ deal = 40; depot = DEPOT; day = D0 }));
applyC(#dealDepotAssigned({ deal = 41; depot = DEPOT; day = D0 }));
applyC(#transferred({ lot = 40; from = "DEPOT-NONE"; to = DEPOT; nominal = 6_000_000_00; reference = "seed"; day = D0 }));
applyC(#transferred({ lot = 41; from = "DEPOT-NONE"; to = DEPOT; nominal = 6_000_000_00; reference = "seed"; day = D0 }));
check(Custody.availableView(c, DEPOT, ISIN).available == 12_000_000_00, "twelve million available before any pledge");

// ─── the policy and the terms ───
refusedF(Core.planPolicy({ p with marginGraceDays = 99 }), "a grace of 99 days");
ignore applyF(okF(Core.planPolicy(p), "policy"));
let terms : FT.RepoTerms = { S.repoTerms with start = D0; maturity = ?(D0 + 30); cash = 9_500_000_00; rateBps = 1900; collateral = { isin = ISIN; nominal = 10_000_000_00 }; haircutBps = 500; thresholdBps = 200 };
check(Core.validateRepo(terms, D0) == null, "the sample terms validate");
check(Core.validateRepo({ terms with cash = 0 }, D0) != null and Core.validateRepo({ terms with maturity = ?D0 }, D0) != null and Core.validateRepo({ terms with haircutBps = 10_000 }, D0) != null, "bad repo terms refused");
refused += 3;
Debug.print("count: policy and terms checks = 5");

// ─── a term repo: the desk borrows against its collateral ───
let repoId = applyF(#repoOpened({ book = "BR01"; counterparty = S.citi; terms; reference = "repo-1"; trader = S.alice(); day = D0 }));
let ?r0 = Core.repo(f, repoId) else Runtime.trap("repo row");
check(r0.state == #open and r0.nominal == 10_000_000_00 and not r0.reverse, "the repo opened");
let lots = okF(Core.allocate(Custody.availableIn(c, DEPOT, ISIN), terms.collateral.nominal, ISIN), "allocate");
check(lots.size() == 2 and lots[0] == (40, 6_000_000_00) and lots[1] == (41, 4_000_000_00), "the collateral pledged first in first out: " # debug_show lots);
refusedF(Core.allocate(Custody.availableIn(c, DEPOT, ISIN), 13_000_000_00, ISIN), "a pledge beyond what is available");
let startEv : FT.Event = #repoStarted({ repo = repoId; lots; day = D0 });
let startLegs = Core.legsOf(p, cash, ?r0, null, startEv);
check(balances(startLegs) and net(startLegs, cash.account) == 9_500_000_00 and net(startLegs, p.repoPayable) == -9_500_000_00, "the start: cash in, the payable up, the lot untouched");
ignore applyF(startEv);
for ((lot, n) in lots.vals()) applyC(#pledged({ lot; depot = DEPOT; nominal = n; reference = "repo/" # Nat.toText(repoId); day = D0 }));
check(Custody.availableView(c, DEPOT, ISIN).available == 2_000_000_00 and Custody.availableView(c, DEPOT, ISIN).held == 12_000_000_00, "the holding stays, the available falls by the pledge");
check(Core.lotsOfRepo(f, repoId).size() == 2, "the repo's pledges indexed");
// the accrual day by day at 19 percent ACT/365
var d = D0 + 1;
var accruals = 0;
while (d <= D0 + 10) {
  switch (okF(Core.planRepoDue(f, repoId, d), "due")) {
    case (?ev) {
      let ?r = Core.repo(f, repoId) else Runtime.trap("row");
      let ls = Core.legsOf(p, cash, ?r, null, ev);
      check(balances(ls) and net(ls, p.repoInterestExpense) > 0, "the accrual to expense against the payable");
      ignore applyF(ev); accruals += 1;
    };
    case null check(false, "an accrual every day");
  };
  d += 1;
};
let ?r10 = Core.repo(f, repoId) else Runtime.trap("row");
check(r10.accruedPosted == M.simpleInterestTo(9_500_000_00, 1900, #a004_Act365Fixed, D0, D0 + 10), "ten days of interest by the day count: " # debug_show r10.accruedPosted);
Debug.print("count: daily repo accruals = " # Nat.toText(accruals));
// the mark: at 99.00 the collateral after the haircut covers the exposure; at 92.00 it does not
let v99 = Core.collateralValue(10_000_000_00, 99_000_000, 500);
check(v99 == 9_405_000_00, "value after a 5 percent haircut at 99.00");
check(Core.planMark(f, r10, 99_000_000, D0 + 10, D0 + 11).size() == 1, "no call while the value covers the exposure within the threshold");
let marks = Core.planMark(f, r10, 92_000_000, D0 + 10, D0 + 11);
check(marks.size() == 2, "a call raised at 92.00");
let exposure10 = Core.exposure(r10, D0 + 10);
let expectedCall = exposure10 - Core.collateralValue(10_000_000_00, 92_000_000, 500);
switch (marks[1]) { case (#marginCallRaised(x)) check(x.amount == expectedCall and x.payer == #desk and x.due == D0 + 11, "the call is the shortfall, on the desk, due the next day: " # debug_show (x.amount, expectedCall)); case (_) check(false, "a margin call") };
for (ev in marks.vals()) ignore applyF(ev);
let ?rc = Core.repo(f, repoId) else Runtime.trap("row");
check(rc.marginCalled == expectedCall and Core.status(f).marginCallsOpen == 1, "the call is open on the row");
check(Core.planMark(f, rc, 90_000_000, D0 + 11, D0 + 12).size() == 1, "no second call while one is open");
// met in cash for half and collateral for the rest, at 92.00
refusedF(Core.planMeetMargin(f, repoId, 1_00, null, 92_000_000, [], D0 + 11), "a short offer");
let half = expectedCall / 2;
let collNominal = ((expectedCall - half) * 100 * 1_000_000 * 10_000) / (92_000_000 * 9_500) + 100;
let more = okF(Core.allocate(Custody.availableIn(c, DEPOT, ISIN), collNominal, ISIN), "allocate the margin collateral");
let met = okF(Core.planMeetMargin(f, repoId, half, ?{ isin = ISIN; nominal = collNominal }, 92_000_000, more, D0 + 11), "meet the call");
let metLegs = Core.legsOf(p, cash, ?rc, null, met);
check(balances(metLegs) and net(metLegs, p.marginCashGiven) == half and net(metLegs, cash.account) == -half, "the cash margin given");
ignore applyF(met);
for ((lot, n) in more.vals()) applyC(#pledged({ lot; depot = DEPOT; nominal = n; reference = "repo/" # Nat.toText(repoId); day = D0 + 11 }));
let ?rm = Core.repo(f, repoId) else Runtime.trap("row");
check(rm.marginCalled == 0 and rm.marginCash == -half and rm.nominal == 10_000_000_00 + collNominal, "the call met: margin cash given, the collateral up");
check(Core.exposure(rm, D0 + 11) == Int.abs((9_500_000_00 : Int) + Core.repoInterestTarget(rm, D0 + 11) - (half : Int)), "the exposure counts the margin cash given");
Debug.print("count: margin calls raised and met = 1");
// a substitution: a million out, a million and fifty thousand in, at the same price
refusedF(Core.planSubstitute(f, repoId, { isin = ISIN; nominal = 1_000_000_00 }, { isin = ISIN; nominal = 900_000_00 }, 92_000_000, 92_000_000, [(40, 1_000_000_00)], [], D0 + 12), "collateral in worth less than out");
let subst = okF(Core.planSubstitute(f, repoId, { isin = ISIN; nominal = 1_000_000_00 }, { isin = ISIN; nominal = 1_050_000_00 }, 92_000_000, 92_000_000, [(40, 1_000_000_00)], [(41, 1_050_000_00)], D0 + 12), "substitute");
ignore applyF(subst);
applyC(#released({ lot = 40; depot = DEPOT; nominal = 1_000_000_00; reference = "repo/" # Nat.toText(repoId); day = D0 + 12 }));
applyC(#pledged({ lot = 41; depot = DEPOT; nominal = 1_050_000_00; reference = "repo/" # Nat.toText(repoId); day = D0 + 12 }));
let ?rs = Core.repo(f, repoId) else Runtime.trap("row");
check(rs.nominal == 10_000_000_00 + collNominal + 50_000_00, "the collateral after the substitution");
Debug.print("count: substitutions = 1");
// to maturity: accrue, then close
d := D0 + 11;
while (d <= D0 + 30) { switch (okF(Core.planRepoDue(f, repoId, d), "due")) { case (?ev) ignore applyF(ev); case null {} }; d += 1 };
refusedF(Core.planClose(f, repoId, D0 + 29), "a close before maturity");

let close = okF(Core.planClose(f, repoId, D0 + 30), "close");
let ?rx = Core.repo(f, repoId) else Runtime.trap("row");
let closeLegs = Core.legsOf(p, cash, ?rx, null, close);
let totalInterest = M.simpleInterestTo(9_500_000_00, 1900, #a004_Act365Fixed, D0, D0 + 30);
check(balances(closeLegs) and net(closeLegs, p.repoPayable) == 9_500_000_00 and net(closeLegs, p.repoInterestPayable) == totalInterest and net(closeLegs, p.marginCashGiven) == -half, "the close: the payable, the interest and the margin cash given all back");
let pledgedAtClose = Core.lotsOfRepo(f, repoId);
ignore applyF(close);
for ((lot, n) in pledgedAtClose.vals()) applyC(#released({ lot; depot = DEPOT; nominal = n; reference = "repo/" # Nat.toText(repoId); day = D0 + 30 }));
check(Custody.availableView(c, DEPOT, ISIN).available == 12_000_000_00, "every pledge released at the close");
let ?rz = Core.repo(f, repoId) else Runtime.trap("row");
check(rz.state == #closed and Core.lotsOfRepo(f, repoId).size() == 0 and Core.status(f).openRepos == 0, "closed, the pledges gone");
Debug.print("count: term repos through their life = 1");

// ─── an open repo re-priced ───
let openTerms : FT.RepoTerms = { terms with maturity = null; start = D0 + 30 };
let open1 = applyF(#repoOpened({ book = "BR01"; counterparty = S.citi; terms = openTerms; reference = "repo-open"; trader = S.alice(); day = D0 + 30 }));
ignore applyF(#repoStarted({ repo = open1; lots = [(40, 10_000_000_00)]; day = D0 + 30 }));
d := D0 + 31;
while (d <= D0 + 35) { switch (okF(Core.planRepoDue(f, open1, d), "due")) { case (?ev) ignore applyF(ev); case null {} }; d += 1 };
let ?ro = Core.repo(f, open1) else Runtime.trap("row");
let reset = okF(Core.planRateReset(f, open1, 2100, D0 + 36), "reset");
switch (reset) { case (#repoRateReset(x)) check(x.catchUp == Core.repoInterestTarget(ro, D0 + 36) - ro.accruedPosted and x.catchUp > 0, "the catch-up at the old rate to the reset day"); case (_) check(false, "reset event") };
ignore applyF(reset);
let ?ro2 = Core.repo(f, open1) else Runtime.trap("row");
check(ro2.rateBps == 2100 and ro2.accrualFrom == D0 + 36 and ro2.accruedBefore == ro2.accruedPosted, "the base moved to the reset day at the new rate");
let after = Core.repoInterestTarget(ro2, D0 + 37) - ro2.accruedPosted;
check(after == M.simpleInterestTo(9_500_000_00, 2100, #a004_Act365Fixed, D0 + 36, D0 + 37), "the next day accrues at the new rate");
refusedF(Core.planRateReset(f, repoId, 2100, D0 + 36), "a re-pricing of a closed term repo");
Debug.print("count: open repos re-priced = 1");

// ─── a reverse repo: the collateral received, the call on the counterparty ───
let revTerms : FT.RepoTerms = { terms with reverse = true; start = D0 + 30; maturity = ?(D0 + 60) };
let rev = applyF(#repoOpened({ book = "BR01"; counterparty = S.citi; terms = revTerms; reference = "reverse-1"; trader = S.alice(); day = D0 + 30 }));
let ?rv0 = Core.repo(f, rev) else Runtime.trap("row");
let revStart = Core.legsOf(p, cash, ?rv0, null, #repoStarted({ repo = rev; lots = []; day = D0 + 30 }));
check(balances(revStart) and net(revStart, p.reverseRepoReceivable) == 9_500_000_00 and net(revStart, cash.account) == -9_500_000_00, "the reverse: cash out, the receivable up");
ignore applyF(#repoStarted({ repo = rev; lots = []; day = D0 + 30 }));
applyC(#collateralReceived({ isin = ISIN; depot = DEPOT; nominal = 10_000_000_00; reference = "repo/" # Nat.toText(rev); day = D0 + 30 }));
check(Custody.availableView(c, DEPOT, ISIN).received == 10_000_000_00, "the collateral received is held apart from the desk's own lots");
let ?rv = Core.repo(f, rev) else Runtime.trap("row");
let revMarks = Core.planMark(f, rv, 92_000_000, D0 + 31, D0 + 32);
switch (revMarks[1]) { case (#marginCallRaised(x)) check(x.payer == #counterparty, "a shortfall on a reverse repo is the counterparty's call"); case (_) check(false, "a call") };
for (ev in revMarks.vals()) ignore applyF(ev);
refusedF(Core.planMeetMargin(f, rev, 0, ?{ isin = ISIN; nominal = 1 }, 92_000_000, [], D0 + 32), "a call on the counterparty met with the desk's collateral");
let ?rv2 = Core.repo(f, rev) else Runtime.trap("row");
let revMet = okF(Core.planMeetMargin(f, rev, rv2.marginCalled, null, 92_000_000, [], D0 + 32), "the counterparty pays cash margin");
let revMetLegs = Core.legsOf(p, cash, ?rv2, null, revMet);
check(net(revMetLegs, cash.account) == rv2.marginCalled and net(revMetLegs, p.marginCashReceived) == -(rv2.marginCalled : Int), "cash margin received");
ignore applyF(revMet);
let ?rv3 = Core.repo(f, rev) else Runtime.trap("row");
check(rv3.marginCash == rv2.marginCalled and Core.exposure(rv3, D0 + 32) < Core.exposure(rv2, D0 + 32), "the margin received reduces the exposure");
// the excess: at 110.00 the counterparty may call the margin back
let excess = Core.planMark(f, rv3, 110_000_000, D0 + 33, D0 + 34);
switch (excess[1]) { case (#marginCallRaised(x)) check(x.payer == #desk, "an excess beyond the threshold is the lender's to return"); case (_) check(false, "an excess call") };
Debug.print("count: reverse repo calls both ways = 2");

// ─── a securities loan against cash collateral ───
let loanTerms : FT.LoanTerms = { S.loanTerms with start = D0 + 30; nominal = 2_000_000_00; collateral = #cash({ amount = 2_100_000_00; rebateBps = 1800 }); feeBps = 50; valueMicro = 98_000_000 };
check(Core.validateLoan(loanTerms, D0 + 30) == null and Core.validateLoan({ loanTerms with nominal = 0 }, D0 + 30) != null, "loan terms validated");
refused += 1;
let loanId = applyF(#loanOpened({ book = "BR01"; counterparty = S.citi; terms = loanTerms; reference = "loan-1"; trader = S.alice(); day = D0 + 30 }));
let ?l0 = Core.loan(f, loanId) else Runtime.trap("loan row");
let loanLots = okF(Core.allocate(Custody.availableIn(c, DEPOT, ISIN), 2_000_000_00, ISIN), "allocate the loan");
let ls0 = Core.legsOf(p, cash, null, ?l0, #loanStarted({ loan = loanId; lots = loanLots; day = D0 + 30 }));
check(balances(ls0) and net(ls0, cash.account) == 2_100_000_00 and net(ls0, p.cashCollateralPayable) == -2_100_000_00, "the cash collateral in against its payable");
ignore applyF(#loanStarted({ loan = loanId; lots = loanLots; day = D0 + 30 }));
for ((lot, n) in loanLots.vals()) applyC(#lent({ lot; depot = DEPOT; nominal = n; reference = "loan/" # Nat.toText(loanId); day = D0 + 30 }));
check(Core.lentOfLot(f, loanLots[0].0) == loanLots[0].1, "the lot's nominal on loan");
d := D0 + 31;
var loanAccruals = 0;
while (d <= D0 + 40) { switch (okF(Core.planLoanDue(f, loanId, d), "loan due")) { case (?ev) { let ?l = Core.loan(f, loanId) else Runtime.trap("l"); check(balances(Core.legsOf(p, cash, null, ?l, ev)), "loan accrual balances"); ignore applyF(ev); loanAccruals += 1 }; case null {} }; d += 1 };
let ?l10 = Core.loan(f, loanId) else Runtime.trap("l");
check(l10.feePosted == M.simpleInterestTo(M.cleanCost(2_000_000_00, 98_000_000), 50, #a004_Act365Fixed, D0 + 30, D0 + 40) and l10.rebatePosted == M.simpleInterestTo(2_100_000_00, 1800, #a004_Act365Fixed, D0 + 30, D0 + 40), "ten days of fee on the loan's value and rebate on the collateral");
Debug.print("count: daily loan accruals = " # Nat.toText(loanAccruals));
// a coupon of 60,000.00 on a lot of 6,000,000.00 of which 2,000,000.00 is out: a third is manufactured
check(Custody.manufacturedPart(60_000_00, 6_000_000_00, 2_000_000_00) == 20_000_00, "the manufactured part pro rata");
let mp : FT.Event = #manufacturedPayment({ loan = loanId; action = 70; lot = loanLots[0].0; amount = 20_000_00; day = D0 + 40 });
let mpLegs = Core.legsOf(p, cash, null, ?l10, mp);
check(net(mpLegs, p.manufacturedPaymentReceivable) == 20_000_00, "the manufactured payment is a receivable from the borrower");
ignore applyF(mp);
// recall and return
refusedF(Core.planReturn(f, loanId, D0 + 41), "a return before the recall");
ignore applyF(okF(Core.planRecall(f, loanId, D0 + 41, D0 + 44), "recall"));
refusedF(Core.planReturn(f, loanId, D0 + 42), "a return before the return day");
d := D0 + 41;
while (d <= D0 + 44) { switch (okF(Core.planLoanDue(f, loanId, d), "loan due")) { case (?ev) ignore applyF(ev); case null {} }; d += 1 };
let ret = okF(Core.planReturn(f, loanId, D0 + 44), "return");
let ?lr = Core.loan(f, loanId) else Runtime.trap("l");
let retLegs = Core.legsOf(p, cash, null, ?lr, ret);
check(balances(retLegs) and net(retLegs, p.lendingFeeReceivable) == -lr.feePosted and net(retLegs, p.cashCollateralPayable) == 2_100_000_00 + lr.rebatePosted, "the return: the fee in, the collateral and the rebate back");
let lentAtReturn = Core.lotsOfLoan(f, loanId);
ignore applyF(ret);
for ((lot, n) in lentAtReturn.vals()) applyC(#lentReturned({ lot; depot = DEPOT; nominal = n; reference = "loan/" # Nat.toText(loanId); day = D0 + 44 }));
let ?lz = Core.loan(f, loanId) else Runtime.trap("l");
check(lz.state == #returned and lz.manufactured == 20_000_00 and Core.lentOfLot(f, loanLots[0].0) == 0, "returned, the lot back, the manufactured payment on the row");
Debug.print("count: loans through their life = 1");
Debug.print("count: refusals = " # Nat.toText(refused));

// ─── the fold under a re-fold ───
let f2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(f2, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(f) == fp(f2), "the financing fingerprint is reproduced by the re-fold");
let st = Core.status(f);
check(st.repos == 3 and st.loans == 1 and st.openRepos == 2 and st.openLoans == 0, "the status counts: " # debug_show st);
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Financing: all checks passed");
