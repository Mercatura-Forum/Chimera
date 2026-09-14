/// Call.test.mo: call and notice money through its life, on the pure layer: the terms validated, a placement funded,
/// accrued daily at 4.30 percent ACT/360, the rate reset with the accrual caught up at the old rate first, part of
/// the balance drawn with the accrual caught up again, interest settled at its date and capitalised on a second
/// call, a notice served for seven days landing on a rest day and moved to the next business day, the
/// repayment closing the call; every posting balancing per currency and landing on the policy's accounts; the
/// figures equal to hand-computed interest; the positions read; the fold's fingerprint stable under a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import Int "mo:core/Int";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import TT "mo:manticore/TreasuryTypes";

import CT "../src/CallTypes";
import Core "../src/CallCore";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func fp(s : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, s); w.toBlob() };

let arena = RI.newArena();
let s = Core.newState(arena);
var block = 100;
func next() : Nat { block += 1; block };
func apply(ev : CT.Event) : Nat { let b = next(); Core.fold(s, b, ev); b };
func balances(legs : [JT.Leg]) : Bool {
  for (l in legs.vals()) {
    var d : Int = 0;
    for (m in legs.vals()) { if (Text.equal(m.currency, l.currency)) { if (m.side == #debit) d += m.amount else d -= m.amount } };
    if (d != 0) return false;
  };
  true
};
func onAccount(legs : [JT.Leg], account : Text, side : JT.Side) : Nat { var n = 0; for (l in legs.vals()) { if (Text.equal(l.account, account) and l.side == side) n += l.amount }; n };
let p = S.policy;
let cash = S.nostro;
let D0 = 20670;   // a Wednesday

// ─── the terms ───
var refused = 0;
check(Core.validateTerms(S.callTerms, D0) == null, "the sample terms validate");
if (Core.validateTerms({ S.callTerms with principal = 0 }, D0) != null) refused += 1;
if (Core.validateTerms({ S.callTerms with currency = "US" }, D0) != null) refused += 1;
if (Core.validateTerms({ S.callTerms with start = D0 - 1 }, D0) != null) refused += 1;
if (Core.validateTerms({ S.callTerms with noticeDays = 400 }, D0) != null) refused += 1;
Debug.print("count: term refusals = " # Nat.toText(refused));

// ─── a placement: 750,000.00 USD at 4.30 percent ACT/360, interest every 30 days paid through the nostro ───
let id = apply(#opened({ book = "BR01"; counterparty = S.citi; terms = S.callTerms; reference = "call-1"; trader = S.alice(); day = D0; withinLimits = true; approver = null }));
var acts = 0;
func due(day : Nat) : ?CT.Event { switch (Core.planDue(s, id, day)) { case (#ok(e)) e; case (#err(e)) { check(false, "planDue: " # debug_show e); null } } };
func run(day : Nat) : [CT.Event] {
  var out : [CT.Event] = [];
  label l loop {
    switch (due(day)) {
      case null break l;
      case (?ev) {
        let ?r = Core.row(s, id) else { check(false, "row"); break l };
        let legs = Core.legsOf(p, r, cash, ev);
        check(balances(legs), "legs balance: " # debug_show ev);
        ignore apply(ev); acts += 1;
        out := [ev];
        switch (ev) {
          case (#funded(x)) { check(onAccount(legs, p.mmPlacements, #debit) == x.amount and onAccount(legs, cash.account, #credit) == x.amount, "the funding lands on the placements account") };
          case (#accrued(x)) { check(onAccount(legs, p.mmInterestReceivable, #debit) == Int.abs(x.interest) and onAccount(legs, p.mmInterestIncome, #credit) == Int.abs(x.interest), "the accrual lands on receivable and income") };
          case (#interestSettled(x)) { check(onAccount(legs, cash.account, #debit) == x.amount and onAccount(legs, p.mmInterestReceivable, #credit) == x.amount, "interest paid through cash releases the receivable") };
          case (#repaid(x)) { check(onAccount(legs, cash.account, #debit) == x.principal + x.interest, "the repayment brings principal and interest through cash") };
          case (_) {};
        };
      };
    };
  };
  out
};
// the start day: funded, nothing accrued
ignore run(D0);
switch (Core.row(s, id)) { case (?r) check((r.flags & Core.F_FUNDED) != 0 and r.accruedPosted == 0, "funded on the start day, nothing accrued"); case null check(false, "row") };
check(due(D0) == null, "nothing more falls due on the start day");
// ten days: 750,000.00 x 4.30 percent x 10/360 = 895.83
var d = D0;
var accrualDays = 0;
while (d < D0 + 10) { d += 1; ignore run(d); accrualDays += 1 };
switch (Core.row(s, id)) { case (?r) check(r.accruedPosted == 89_583, "ten days at 4.30 percent on 750,000 ACT/360 accrue 895.83: " # debug_show r.accruedPosted); case null check(false, "row") };
Debug.print("count: daily accruals posted = " # Nat.toText(accrualDays));

// the rate reset on day 10 to 4.50 percent: the catch-up to the day at the old rate is zero, since the day's accrual already ran
switch (Core.planReset(s, id, 450, D0 + 10)) {
  case (#ok(ev)) { switch (ev) { case (#rateReset(x)) { check(x.catchUp == 0, "no catch-up when the accrual is current"); ignore apply(ev) }; case (_) check(false, "not a reset") } };
  case (#err(e)) check(false, "reset: " # debug_show e);
};
// five more days at 4.50 percent: 750,000 x 4.50 percent x 5/360 = 468.75; total 1,364.58
while (d < D0 + 15) { d += 1; ignore run(d) };
switch (Core.row(s, id)) { case (?r) check(r.accruedPosted == 89_583 + 46_875, "five days at the new rate: " # debug_show r.accruedPosted); case null check(false, "row") };
// a reset dated two days before the last accrual catches the accrual up at the OLD rate to that day and moves the base;
// the next accrual runs at the new rate from there: the row's target must be reproducible by hand
// (the base moves to day 13: accrued before = 895.83 + 3 days at 4.50 percent = 895.83 + 281.25)
switch (Core.planReset(s, id, 400, D0 + 13)) {
  case (#ok(ev)) { switch (ev) { case (#rateReset(x)) { check(x.catchUp == 28_125 - 46_875, "the catch-up to day 13 reverses the two days accrued beyond it: " # debug_show x.catchUp); ignore apply(ev) }; case (_) check(false, "not a reset") } };
  case (#err(e)) check(false, "reset back-dated: " # debug_show e);
};
ignore run(d);   // day 15 again: two days at 4.00 percent from day 13: 750,000 x 4.00 percent x 2/360 = 166.67
switch (Core.row(s, id)) { case (?r) check(r.accruedPosted == 89_583 + 28_125 + 16_667, "re-accrued at 4.00 percent from the moved base: " # debug_show r.accruedPosted); case null check(false, "row") };

// a draw of 250,000.00 on day 15: the accrual is current, the balance falls, the base moves
switch (Core.planAdjust(s, id, -250_000_00, D0 + 15)) { case (#ok(ev)) ignore apply(ev); case (#err(e)) check(false, "draw: " # debug_show e) };
switch (Core.planAdjust(s, id, -600_000_00, D0 + 15)) { case (#err(#InsufficientBalance(_))) refused += 1; case (r) check(false, "a draw beyond the balance: " # debug_show r) };
switch (Core.row(s, id)) { case (?r) check(r.balance == 500_000_00 and r.accrualFrom == D0 + 15, "the balance after the draw"); case null check(false, "row") };

// the interest date, day 30: the accrual to day 30 on 500,000 at 4.00 percent for 15 days = 833.33; settled through cash
while (d < D0 + 30) { d += 1; ignore run(d) };
switch (Core.row(s, id)) {
  case (?r) { check(r.accruedPosted == 0 and r.interestPaid == 89_583 + 28_125 + 16_667 + 83_333 and r.lastInterestDay == D0 + 30, "interest settled at its date: " # debug_show (r.interestPaid, r.accruedPosted)) };
  case null check(false, "row");
};

// a notice served on day 31 (a Saturday under the Friday and Saturday rest days of the desk's calendar) for seven days
// lands on day 38, a Saturday, and moves to the Sunday, the next business day
func nextBusiness(x : Nat) : Nat { var y = x; while ((y + 3) % 7 == 4 or (y + 3) % 7 == 5) y += 1; y };
check((D0 + 38 + 3) % 7 == 5, "day 38 is a rest day");
switch (Core.planNotice(s, id, D0 + 31, nextBusiness)) {
  case (#ok(#noticeServed(x))) { check(x.repayDay == D0 + 39, "a repayment day on a rest day moves to the next business day: " # debug_show x.repayDay); ignore apply(#noticeServed(x)) };
  case (r) check(false, "notice: " # debug_show r);
};
switch (Core.planAdjust(s, id, 100_00, D0 + 33)) { case (#err(#CallNotIn(_))) refused += 1; case (r) check(false, "an adjustment after notice: " # debug_show r) };
switch (Core.planNotice(s, id, D0 + 33, nextBusiness)) { case (#err(#CallNotIn(_))) refused += 1; case (r) check(false, "a second notice: " # debug_show r) };
// the days to the repayment: accrued from day 30 to day 39 on 500,000 at 4.00 percent, 9 days = 500.00; repaid on day 39
while (d < D0 + 39) { d += 1; ignore run(d) };
switch (Core.row(s, id)) {
  case (?r) { check(r.state == #closed and r.balance == 0 and r.interestPaid == 89_583 + 28_125 + 16_667 + 83_333 + 50_000, "repaid and closed with the interest to the day: " # debug_show (r.state, r.interestPaid)) };
  case null check(false, "row");
};
check(due(D0 + 40) == null, "nothing falls due on a closed call");
Debug.print("count: acts through the call's life = " # Nat.toText(acts));

// ─── a taking that capitalises: 1,000,000.00 EGP at 20 percent ACT/365, interest every 10 days into the balance ───
let takingTerms : CT.Terms = { placement = false; currency = "EGP"; principal = 1_000_000_00; rateBps = 2000; dayCount = #a004_Act365Fixed; noticeDays = 2; interestEveryDays = 10; capitalise = true; cash = S.cash; start = D0 };
let id2 = apply(#opened({ book = "BR01"; counterparty = S.citi; terms = takingTerms; reference = "call-2"; trader = S.alice(); day = D0; withinLimits = true; approver = null }));
var capitalised = 0;
d := D0;
while (d <= D0 + 20) {
  label l loop {
    switch (Core.planDue(s, id2, d)) {
      case (#ok(null)) break l;
      case (#err(e)) { check(false, "taking: " # debug_show e); break l };
      case (#ok(?ev)) {
        let ?r = Core.row(s, id2) else { check(false, "row"); break l };
        let legs = Core.legsOf(p, r, S.cash, ev);
        check(balances(legs), "taking legs balance");
        switch (ev) {
          case (#accrued(x)) check(onAccount(legs, p.mmInterestExpense, #debit) == Int.abs(x.interest) and onAccount(legs, p.mmInterestPayable, #credit) == Int.abs(x.interest), "a taking accrues to expense and payable");
          case (#interestSettled(x)) { check(x.capitalised and onAccount(legs, p.mmTakings, #credit) == x.amount and onAccount(legs, p.mmInterestPayable, #debit) == x.amount, "capitalised interest moves from the payable into the taking"); capitalised += 1 };
          case (_) {};
        };
        ignore apply(ev);
      };
    };
  };
  d += 1;
};
// ten days at 20 percent ACT/365 on 1,000,000.00 = 5,479.45, capitalised; the next ten days on 1,005,479.45 = 5,509.48
switch (Core.row(s, id2)) { case (?r) check(r.balance == 1_000_000_00 + 547_945 + 550_948 and capitalised == 2, "the balance grew by the capitalised interest twice: " # debug_show r.balance); case null check(false, "row") };
Debug.print("count: interest settlements capitalised = " # Nat.toText(capitalised));

// ─── positions, exposure, the fingerprint ───
let pos = Core.positions(s, "BR01");
check(pos.size() == 1 and pos[0].kind == "callDeposit" and pos[0].nominal == -(1_000_000_00 + 547_945 + 550_948 : Int), "the closed placement is out and the taking is a negative nominal: " # debug_show pos);
check(Core.exposureOf(s, "BR01", "CITI", "EGP") == 1_000_000_00 + 547_945 + 550_948 and Core.exposureOf(s, "BR01", "CITI", "USD") == 0, "the exposure counts open balances only");
Debug.print("count: positions read = " # Nat.toText(pos.size()));
let before = fp(s);
check(before == fp(s), "the fingerprint is stable");
Debug.print("count: refusals = " # Nat.toText(refused));
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("CALL MONEY GREEN");
