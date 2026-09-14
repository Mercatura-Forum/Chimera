/// Collateral.test.mo: the collateral agreements on the pure layer: the supervisory haircut table and a bilateral
/// schedule over it, the value of securities and cash in a pool under the haircut and the mismatch add-on, the
/// credit support arithmetic (the threshold both ways, the minimum transfer, the rounding up of a delivery and
/// down of a return), the interest on net cash, the agreements' refusals, the pool's fold under every movement,
/// the call credited by the movements in its direction and met, the legs balancing, the fingerprint reproduced.
// engine: wasi-only

import Debug "mo:core/Debug";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import M "mo:manticore/TreasuryMath";
import Posting "mo:manticore/Posting";

import CoT "../src/CollateralTypes";
import Core "../src/CollateralCore";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : CoT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func refused(r : { #ok : CoT.Event; #err : CoT.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };

let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, CoT.Event)>();
var block = 200;
func apply(ev : CoT.Event) : Nat { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)); block };
let D0 = 20_700;
let EGP = "EGP"; let USD = "USD";

// ─── the haircuts ───
var haircuts = 0;
for ((cls, days, want) in [(#sovereign, 100, 50), (#supranational, 365, 50), (#sovereign, 366, 200), (#sovereign, 1825, 200), (#sovereign, 1826, 400), (#financial, 10, 100), (#corporate, 1000, 400), (#corporate, 4000, 800)].vals()) {
  check(Core.supervisoryHaircutBps(cls, days) == want, "the supervisory haircut of " # debug_show cls # " at " # Nat.toText(days) # " days is " # Nat.toText(want)); haircuts += 1;
};
Debug.print("count: supervisory haircuts = " # Nat.toText(haircuts));
// the values: 10m nominal at 98.5, 2 percent: 9,850,000 less 197,000; the mismatch adds 8 percent
check(Core.securitiesValue(10_000_000_00, 98_500_000, 200, false) == 9_653_000_00, "the haircut value: " # Nat.toText(Core.securitiesValue(10_000_000_00, 98_500_000, 200, false)));
check(Core.securitiesValue(10_000_000_00, 98_500_000, 200, true) == 8_865_000_00, "the haircut value with the mismatch add-on: " # Nat.toText(Core.securitiesValue(10_000_000_00, 98_500_000, 200, true)));
check(Core.securitiesValue(1, 100_000_000, 9_500, true) == 0, "a cut at or past the whole is worth nothing");
check(Core.cashValue(1_000_000_00, false) == 1_000_000_00 and Core.cashValue(1_000_000_00, true) == 920_000_00, "cash in the agreement's currency at face, another at 92 percent");
Debug.print("count: valuation checks = 4");

// ─── the agreements ───
ignore apply(ok(Core.planPolicy(S.collateralPolicy), "policy"));
ignore apply(ok(Core.planSetAgreement(s, S.agreement, D0), "CSA-CITI"));
ignore apply(ok(Core.planSetAgreement(s, S.agreementSupervisory, D0), "CSA-HSBC"));
var refusals = 0;
if (refused(Core.planSetAgreement(s, { S.agreement with id = "CSA-CITI-2" }, D0), "a second agreement with the same counterparty")) refusals += 1;
if (refused(Core.planSetAgreement(s, { S.agreement with currency = USD }, D0), "an agreement changing its currency")) refusals += 1;
if (refused(Core.planSetAgreement(s, { S.agreement with counterparty = { S.citi with name = "OTHER" } }, D0), "an agreement changing its counterparty")) refusals += 1;
if (refused(Core.planSetAgreement(s, { S.agreementSupervisory with id = "X"; counterparty = { S.citi with name = "X" }; covers = [] }, D0), "an agreement covering nothing")) refusals += 1;
if (refused(Core.planSetAgreement(s, { S.agreementSupervisory with id = "X"; counterparty = { S.citi with name = "X" }; schedule = ?[{ classification = #sovereign; fromDays = 10; toDays = 5; haircutBps = 100 }] }, D0), "a schedule bucket upside down")) refusals += 1;
if (refused(Core.planSetAgreement(s, { S.agreementSupervisory with id = "X"; counterparty = { S.citi with name = "X" }; schedule = ?[{ classification = #sovereign; fromDays = 0; toDays = 5; haircutBps = 10_000 }] }, D0), "a haircut of the whole")) refusals += 1;
// re-set with a higher threshold keeps the row
ignore apply(ok(Core.planSetAgreement(s, { S.agreement with threshold = 600_000_00 }, D0 + 1), "CSA-CITI re-set"));
let citi = switch (Core.agreement(s, "CSA-CITI")) { case (?a) a; case null Runtime.trap("no agreement") };
check(citi.threshold == 600_000_00 and citi.bilateral and citi.netting and Core.covers(citi, #repos) and Core.status(s).agreements == 2, "the agreement re-set in place: " # debug_show Core.status(s));
let hsbc = switch (Core.agreement(s, "CSA-HSBC")) { case (?a) a; case null Runtime.trap("no agreement") };
check(not hsbc.bilateral and not hsbc.netting and not Core.covers(hsbc, #repos), "the supervisory agreement records no schedule and nets nothing");
check(Core.agreementOfCounterparty(s, "CITI") != null and Core.agreementOfCounterparty(s, "NOBODY") == null, "the agreement is found by its counterparty");
// the bilateral schedule over the supervisory table
check(Core.haircutBps(s, citi, #sovereign, 100) == ?100 and Core.haircutBps(s, citi, #sovereign, 400) == ?300 and Core.haircutBps(s, citi, #corporate, 100) == null, "the bilateral rows apply to the classes they name and nothing else");
check(Core.haircutBps(s, hsbc, #corporate, 100) == ?100 and Core.haircutBps(s, hsbc, #sovereign, 2000) == ?400, "the supervisory table applies where no schedule is recorded");
Debug.print("count: agreement checks = 5");

// ─── the credit support arithmetic ───
var arithmetic = 0;
// the threshold both ways
check(Core.requirement(citi, 500_000_00) == 0 and Core.requirement(citi, 800_000_00) == 200_000_00 and Core.requirement(citi, -800_000_00) == -200_000_00, "the requirement is the excess over the threshold either way"); arithmetic += 1;
// a delivery by the counterparty: 1.85m exposure, 1.15m held: requirement 1.25m, shortfall 100,000, at the MTA, rounded up to 100,000
check(Core.callFor(citi, 1_850_000_00, 1_150_000_00) == ?(100_000_00, false), "the counterparty delivers the shortfall: " # debug_show Core.callFor(citi, 1_850_000_00, 1_150_000_00)); arithmetic += 1;
// below the MTA: nothing
check(Core.callFor(citi, 1_840_000_00, 1_150_000_00) == null, "a shortfall under the minimum transfer raises nothing"); arithmetic += 1;
// rounding up: a shortfall of 123,456.78 rounds to 130,000
check(Core.callFor(citi, 1_850_000_00 + 23_456_78, 1_150_000_00) == ?(130_000_00, false), "a delivery rounds up: " # debug_show Core.callFor(citi, 1_850_000_00 + 23_456_78, 1_150_000_00)); arithmetic += 1;
// the desk returns excess: exposure fell to 1m, 1.15m held: requirement 400,000, excess 750,000 returned, rounded down
check(Core.callFor(citi, 1_000_000_00, 1_150_000_00 + 5_000_00) == ?(750_000_00, true), "the desk returns the excess rounded down: " # debug_show Core.callFor(citi, 1_000_000_00, 1_150_000_00 + 5_000_00)); arithmetic += 1;
// the desk is exposed the other way and has posted nothing: it delivers, rounded up
check(Core.callFor(citi, -1_000_000_00, 0) == ?(400_000_00, true), "the desk delivers when the counterparty is exposed: " # debug_show Core.callFor(citi, -1_000_000_00, 0)); arithmetic += 1;
// the desk posted 500,000 and the counterparty's exposure fell away: the counterparty returns it, rounded down
check(Core.callFor(citi, 0, -500_000_00) == ?(500_000_00, false), "the counterparty returns what the desk posted: " # debug_show Core.callFor(citi, 0, -500_000_00)); arithmetic += 1;
// the interest on net cash: 1.2m received for 30 days at 18 percent ACT/360, an expense
let c0 : Core.CashRow = { agreement = "CSA-CITI"; currency = EGP; received = 1_200_000_00; given = 0; interestAccrued = 0; accrualFrom = D0 };
check(Core.interestTarget(citi, c0, D0 + 30) == -(M.simpleInterestTo(1_200_000_00, 1800, #a003_Act360, D0, D0 + 30) : Int), "interest on cash received is an expense: " # debug_show Core.interestTarget(citi, c0, D0 + 30)); arithmetic += 1;
check(Core.interestTarget(citi, { c0 with received = 0; given = 1_200_000_00 }, D0 + 30) > 0 and Core.interestTarget(citi, { c0 with given = 1_200_000_00 }, D0 + 30) == 0, "interest on cash given is income; a flat net accrues nothing"); arithmetic += 1;
Debug.print("count: credit support arithmetic checks = " # Nat.toText(arithmetic));

// ─── the pool's fold ───
var movements = 0;
ignore apply(ok(Core.planCashMove(s, "CSA-CITI", #received, 1_200_000_00, EGP, 0, D0), "cash received")); movements += 1;
ignore apply(ok(Core.planCashMove(s, "CSA-CITI", #given, 300_000_00, USD, 0, D0), "cash given in USD")); movements += 1;
if (refused(Core.planCashMove(s, "CSA-CITI", #receivedReturned, 1_300_000_00, EGP, 0, D0), "a return beyond what is held")) refusals += 1;
if (refused(Core.planCashMove(s, "CSA-CITI", #givenReturned, 1, EGP, 0, D0), "a return of cash never given in that currency")) refusals += 1;
if (refused(Core.planCashMove(s, "CSA-NONE", #received, 1, EGP, 0, D0), "an unknown agreement")) refusals += 1;
let partReturned = ok(Core.planCashMove(s, "CSA-CITI", #receivedReturned, 200_000_00, EGP, 0, D0 + 1), "part returned");
switch (partReturned) { case (#cashMoved(x)) check(x.interestCatchUp == -600_00, "a day's interest on the net received caught up by the movement: " # debug_show x.interestCatchUp); case (_) check(false, "a cash move") };
ignore apply(partReturned); movements += 1;
check(Core.cashRow(s, "CSA-CITI", EGP).interestAccrued == -600_00 and Core.cashRow(s, "CSA-CITI", EGP).accrualFrom == D0 + 1, "the catch-up is on the row and the accrual restarts at the movement");
let cash = Core.cashOf(s, "CSA-CITI");
check(cash.size() == 2 and Core.cashRow(s, "CSA-CITI", EGP).received == 1_000_000_00 and Core.cashRow(s, "CSA-CITI", USD).given == 300_000_00, "two cash rows folded: " # debug_show cash);
// securities: a pledge is live at once, a substitution is pledged until settled
let pledge = apply(#securitiesPledged({ agreement = "CSA-CITI"; id = block + 1; lot = 40; isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 2_000_000_00; callCredit = 0; day = D0 })); movements += 1;
let receipt = apply(#securitiesReceived({ agreement = "CSA-CITI"; id = block + 1; isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 1_000_000_00; callCredit = 0; day = D0 })); movements += 1;
let sub = apply(#substitutionOpened({ agreement = "CSA-CITI"; id = block + 1; lot = 40; isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 500_000_00; cashReturned = 300_000_00; currency = USD; day = D0 })); movements += 1;
check(Core.securitiesOf(s, "CSA-CITI").size() == 3, "three securities rows");
check(ok(Core.requireSecurities(s, "CSA-CITI", pledge, #live), "the pledge is live").given, "the pledge is the desk's");
check(not ok(Core.requireSecurities(s, "CSA-CITI", receipt, #live), "the receipt is live").given, "the receipt is the counterparty's");
switch (Core.requireSecurities(s, "CSA-CITI", sub, #live)) { case (#err(#PledgeNotIn(x))) { check(x.state == "pledged", "the substitution waits: " # x.state); refusals += 1 }; case (_) check(false, "the substitution is not live yet") };
switch (Core.requireSecurities(s, "CSA-HSBC", pledge, #live)) { case (#err(#UnknownPledge(_))) refusals += 1; case (_) check(false, "a pledge is its agreement's") };
ignore apply(#substitutionSettled({ agreement = "CSA-CITI"; substitution = sub; callCredit = 0; interestCatchUp = 0; day = D0 + 1 })); movements += 1;
check(ok(Core.requireSecurities(s, "CSA-CITI", sub, #live), "the substitution settled").state == #live and Core.cashRow(s, "CSA-CITI", USD).given == 0, "the settlement makes the securities live and takes the given cash back");
ignore apply(#securitiesReleased({ agreement = "CSA-CITI"; pledge; callCredit = 0; day = D0 + 2 })); movements += 1;
ignore apply(#securitiesReturned({ agreement = "CSA-CITI"; receipt; callCredit = 0; day = D0 + 2 })); movements += 1;
check(ok(Core.requireSecurities(s, "CSA-CITI", pledge, #returned), "released").state == #returned and ok(Core.requireSecurities(s, "CSA-CITI", receipt, #returned), "returned").state == #returned, "released and returned rows are closed");
Debug.print("count: pool movements folded = " # Nat.toText(movements));

// ─── calls ───
var calls = 0;
ignore apply(#exposureRecorded({ agreement = "CSA-CITI"; day = D0 + 2; exposure = 1_850_000_00; balance = 1_150_000_00; rows = 4 }));
let call = apply(#callRaised({ agreement = "CSA-CITI"; id = block + 1; amount = 100_000_00; deliver = false; day = D0 + 2; due = D0 + 3 })); calls += 1;
let a1 = switch (Core.agreement(s, "CSA-CITI")) { case (?a) a; case null Runtime.trap("no agreement") };
check(a1.openCall == call and a1.exposure == 1_850_000_00 and a1.balance == 1_150_000_00 and a1.exposureDay == D0 + 2 and Core.status(s).openCalls == 1, "the call is open on the agreement with the day's figures");
// a movement in the other direction credits nothing; one in the call's direction reduces the outstanding
ignore apply(ok(Core.planCashMove(s, "CSA-CITI", #given, 50_000_00, EGP, 0, D0 + 3), "cash given: not the call's way"));
check(ok(Core.requireCall(s, call), "call").outstanding == 100_000_00, "nothing credited");
ignore apply(ok(Core.planCashMove(s, "CSA-CITI", #received, 60_000_00, EGP, 60_000_00, D0 + 3), "cash received: credited"));
check(ok(Core.requireCall(s, call), "call").outstanding == 40_000_00, "60,000 credited, 40,000 outstanding");
ignore apply(ok(Core.planCashMove(s, "CSA-CITI", #received, 50_000_00, EGP, 50_000_00, D0 + 3), "cash received: the rest"));
ignore apply(#callMet({ agreement = "CSA-CITI"; call; day = D0 + 3 }));
let c1 = ok(Core.requireCall(s, call), "call");
check(c1.outstanding == 0 and c1.state == Core.CALL_MET and Core.status(s).openCalls == 0, "the call is met, the outstanding floored at zero");
let call2 = apply(#callRaised({ agreement = "CSA-CITI"; id = block + 1; amount = 400_000_00; deliver = true; day = D0 + 4; due = D0 + 5 })); calls += 1;
ignore apply(#callSuperseded({ agreement = "CSA-CITI"; call = call2; outstanding = 400_000_00; day = D0 + 5 }));
check(ok(Core.requireCall(s, call2), "call2").state == Core.CALL_SUPERSEDED and Core.status(s).openCalls == 0 and (switch (Core.agreement(s, "CSA-CITI")) { case (?a) a.openCall == 0; case null false }), "a superseded call leaves the agreement without one");
check(Core.callsOf(s, "CSA-CITI").size() == 2, "two calls on the agreement");
Debug.print("count: calls raised, met and superseded = " # Nat.toText(calls));

// ─── the legs ───
var legs = 0;
for (ev in [#cashMoved({ agreement = "CSA-CITI"; move = #received; amount = 1_200_000_00; currency = EGP; callCredit = 0; interestCatchUp = 0; day = D0 }), #cashMoved({ agreement = "CSA-CITI"; move = #given; amount = 1_200_000_00; currency = EGP; callCredit = 0; interestCatchUp = 0; day = D0 }), #cashMoved({ agreement = "CSA-CITI"; move = #receivedReturned; amount = 100_00; currency = EGP; callCredit = 0; interestCatchUp = 0; day = D0 }), #cashMoved({ agreement = "CSA-CITI"; move = #givenReturned; amount = 100_00; currency = EGP; callCredit = 0; interestCatchUp = 0; day = D0 }), #interestAccrued({ agreement = "CSA-CITI"; currency = EGP; interest = -600_00; day = D0 }), #interestAccrued({ agreement = "CSA-CITI"; currency = EGP; interest = 600_00; day = D0 }), #interestSettled({ agreement = "CSA-CITI"; currency = EGP; amount = -600_00; day = D0 }), #interestSettled({ agreement = "CSA-CITI"; currency = EGP; amount = 600_00; day = D0 })].vals()) {
  let ls = Core.legsOf(S.collateralPolicy, citi, ev);
  check(ls.size() == 2 and Posting.balances(ls), "the legs of " # debug_show ev # " balance"); legs += 1;
};
let subRow : Core.SecuritiesRow = { id = 1; agreement = "CSA-CITI"; lot = ?40; isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 500_000_00; given = true; state = #pledged; cashReturned = 300_000_00; currency = USD; day = D0 };
check(Posting.balances(Core.substitutionLegs(S.collateralPolicy, citi, subRow, -500)) and Core.substitutionLegs(S.collateralPolicy, citi, subRow, -500).size() == 4 and Core.substitutionLegs(S.collateralPolicy, citi, { subRow with cashReturned = 0 }, 0).size() == 0, "the substitution's cash legs balance with the interest caught up; nothing without cash"); legs += 1;
Debug.print("count: leg checks = " # Nat.toText(legs));
Debug.print("count: refusals = " # Nat.toText(refusals));

// ─── the re-fold ───
let s2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(s2, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(s2), "the collateral fingerprint is reproduced by the re-fold");
let st = Core.status(s);
check(st.agreements == 2 and st.cashRows == 2 and st.securitiesRows == 3 and st.calls == 2 and st.openCalls == 0 and st.exposures == 1, "the status counts: " # debug_show st);
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Collateral: all checks passed");
