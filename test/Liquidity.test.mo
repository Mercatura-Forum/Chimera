/// Liquidity.test.mo: the liquidity arithmetic on the pure layer: the factors refused incomplete or out of
/// range, the buckets of the ladder, the stock of liquid assets under the haircuts and the level 2 caps of
/// BCBS 238 Annex 1, the coverage ratio with the inflow cap, the stable funding ratio by maturity band and
/// counterparty type, the large exposures against the capital, a missing factor or class named; the fold and
/// the fingerprint reproduced by a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";

import LQ "../src/LiquidityTypes";
import Core "../src/LiquidityCore";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : LQ.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func refused<X>(r : { #ok : X; #err : LQ.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };
let D0 = 20_700;
let F = S.liquidityFactors;

// ─── the factors ───
var refusals = 0;
ignore ok(Core.planFactors(F, D0), "the sample factors");
if (refused(Core.planFactors({ F with runoff = [] }, D0), "run-off rates missing")) refusals += 1;
if (refused(Core.planFactors({ F with inflow = [{ counterpartyType = #financial; bps = 10_001 }] }, D0), "a rate over 100 percent")) refusals += 1;
if (refused(Core.planFactors({ F with level2BCapBps = 5_000 }, D0), "the 2B cap over the level 2 cap")) refusals += 1;
if (refused(Core.planFactors({ F with largeExposureReportBps = 3_000 }, D0), "a reporting threshold over the bound")) refusals += 1;
if (refused(Core.planFactors({ F with rsfHqla = [{ level = #level1; bps = 500 }] }, D0), "a level table missing levels")) refusals += 1;
if (refused(Core.planClassifyInstrument("SHORT", #level1, D0), "an ISIN of five characters")) refusals += 1;
if (refused(Core.planCapital("EGP", 0, D0), "capital of zero")) refusals += 1;
Debug.print("count: factor refusals = " # Nat.toText(refusals));
check(LQ.bucketOf(0) == "overnight" and LQ.bucketOf(1) == "overnight" and LQ.bucketOf(2) == "2-7" and LQ.bucketOf(30) == "8-30" and LQ.bucketOf(31) == "31-90" and LQ.bucketOf(365) == "181-365" and LQ.bucketOf(366) == "over-365", "the buckets by days");
Debug.print("count: bucket checks = 7");

// ─── the stock and the coverage ratio ───
var lcr = 0;
// no cap binds: 100 of level 1, 60 of 2A (51 after 15 percent), 30 of 2B (15 after 50 percent)
let s1 = ok(Core.liquidStock(F, [{ level = #level1; value = 100 }, { level = #level2A; value = 60 }, { level = #level2B; value = 30 }, { level = #none; value = 1_000 }]), "stock 1");
check(s1.level1 == 100 and s1.level2A == 51 and s1.level2B == 15 and s1.capAdjustment == 0 and s1.hqla == 166, "the haircuts applied, no cap binding, a level of none excluded: " # debug_show s1); lcr += 1;
// both caps bind: 100 of level 1, 100 of 2A (85), 100 of 2B (50): the 2B adjustment 25, the level 2 adjustment 43, the stock 167
let s2 = ok(Core.liquidStock(F, [{ level = #level1; value = 100 }, { level = #level2A; value = 100 }, { level = #level2B; value = 100 }]), "stock 2");
check(s2.capAdjustment == 68 and s2.hqla == 167, "the Annex 1 caps: 2B to 15 percent of the stock, level 2 to 40 percent: " # debug_show s2); lcr += 1;
// the ratio: outflows 1,000 to a financial at 100 percent and 1,000 to retail stable at 5 percent, inflows 2,000 from a financial at 100 percent capped at 75 percent of the outflows
let r1 = ok(Core.lcr(F, [{ level = #level1; value = 1_000 }], [{ counterpartyType = ?#financial; amount = -1_000 }, { counterpartyType = ?#retailStable; amount = -1_000 }, { counterpartyType = ?#financial; amount = 2_000 }, { counterpartyType = null; amount = -100 }]), "lcr 1");
check(r1.outflows == 1_150 and r1.inflows == 2_000 and r1.inflowsCounted == 862 and r1.netOutflows == 288 and r1.ratioBps == 34_722, "the run-off rates, the derivative payable at 100 percent, the inflow cap, the ratio: " # debug_show r1); lcr += 1;
let r2 = ok(Core.lcr(F, [{ level = #level1; value = 5 }], []), "lcr with no flows");
check(r2.ratioBps == 1_000_000 and r2.netOutflows == 0, "no net outflow with a stock reads as the ceiling"); lcr += 1;
switch (Core.lcr({ F with runoff = [{ counterpartyType = #financial; bps = 10_000 }] }, [], [{ counterpartyType = ?#sovereign; amount = -1 }])) { case (#err(#MissingFactor(x))) { check(Text.startsWith(x.what, #text "runoff"), "the missing run-off rate is named: " # x.what); refusals += 1 }; case (_) check(false, "a missing rate is a refusal") };
Debug.print("count: liquid stock and coverage checks = " # Nat.toText(lcr));

// ─── the stable funding ratio ───
// capital 1,000; a taking from a financial for 90 days (0 percent), a retail stable deposit for 90 days (95), a corporate taking for 200 days (50), a taking for two years (100);
// a level 1 bond 500 (5 percent), a placement to a financial for 30 days 400 (10), a corporate loan for two years 300 (85), a derivative asset 100 (100), a derivative payable
let n1 = ok(Core.nsfr(F, 1_000, [
  { asset = false; value = 1_000; residualDays = ?90; level = null; counterpartyType = ?#financial; derivative = false },
  { asset = false; value = 1_000; residualDays = ?90; level = null; counterpartyType = ?#retailStable; derivative = false },
  { asset = false; value = 1_000; residualDays = ?200; level = null; counterpartyType = ?#nonFinancialCorporate; derivative = false },
  { asset = false; value = 1_000; residualDays = ?730; level = null; counterpartyType = null; derivative = false },
  { asset = true; value = 500; residualDays = ?1_000; level = ?#level1; counterpartyType = null; derivative = false },
  { asset = true; value = 400; residualDays = ?30; level = null; counterpartyType = ?#financial; derivative = false },
  { asset = true; value = 300; residualDays = ?730; level = null; counterpartyType = ?#nonFinancialCorporate; derivative = false },
  { asset = true; value = 100; residualDays = null; level = null; counterpartyType = null; derivative = true },
  { asset = false; value = 100; residualDays = null; level = null; counterpartyType = null; derivative = true },
]), "nsfr");
check(n1.asf == 1_000 + 0 + 950 + 500 + 1_000 and n1.rsf == 25 + 40 + 255 + 100 and n1.ratioBps == 82_143, "the funding factors by band and type, a derivative payable providing nothing: " # debug_show n1);
switch (Core.nsfr(F, 0, [{ asset = true; value = 1; residualDays = ?1; level = null; counterpartyType = null; derivative = false }])) { case (#err(#MissingFactor(_))) refusals += 1; case (_) check(false, "an asset without a type is a refusal") };
Debug.print("count: stable funding checks = 2");

// ─── the large exposures ───
let le = Core.largeExposures(F, 10_000, [("CITI", "CITIGROUP", 2_600), ("HSBC", "HSBC", 1_000), ("CIB", "CIB", 900), ("NBE", "EG-STATE", 2_500)]);
check(le.rows.size() == 3 and le.breaches == 1 and le.rows[0].breach and le.rows[0].shareBps == 2_600 and not le.rows[3 - 1].breach, "those at or over the reporting threshold listed, the one over the bound a breach: " # debug_show le);
Debug.print("count: large exposure checks = 1");
Debug.print("count: refusals = " # Nat.toText(refusals));

// ─── the fold ───
let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, LQ.Event)>();
var block = 500;
func apply(ev : LQ.Event) { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)) };
apply(ok(Core.planFactors(F, D0), "factors"));
apply(ok(Core.planClassifyInstrument("EG0000012345", #level1, D0), "classify"));
apply(ok(Core.planClassifyInstrument("EG0000012345", #level2A, D0 + 1), "reclassify"));
apply(ok(Core.planClassifyCounterparty("CITI", #financial, D0), "CITI"));
apply(ok(Core.planCapital("EGP", 50_000_000_00, D0), "capital"));
check(Core.instrumentLevel(s, "EG0000012345") == ?#level2A and Core.instrumentLevel(s, "EG0000000000") == null and Core.counterpartyType(s, "CITI") == ?#financial and Core.counterpartyType(s, "NOBODY") == null and Core.capital(s) == ?("EGP", 50_000_000_00), "the classes and the capital on the fold");
let st = Core.status(s);
check(st.factors and st.instruments == 1 and st.counterparties == 1, "the status counts: " # debug_show st);
let again = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(again, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(again), "the liquidity fingerprint is reproduced by the re-fold");
Debug.print("count: fold and fingerprint checks = 3");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Liquidity: all checks passed");
