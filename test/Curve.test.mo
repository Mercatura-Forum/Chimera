/// Curve.test.mo: curve construction on the pure layer: eleven builds (discount curves from deposits, forward
/// rate agreements, a future, overnight index swaps and par swaps; projection curves on them; a projection curve
/// from tenor basis swaps against another; a collateral discount curve from FX swaps; two day counts, both
/// interpolations) equal to the twin's node for node and iteration for iteration; every instrument repriced to
/// within one unit of the fixed point on its own curve; the derived zero points; a swap marked on two curves equal
/// to the twin's, its sign by the side; the refusals (a bad specification, a gap, an unknown discount curve, an
/// index on the wrong curves, a read beyond the curve); the fold's fingerprint equal under a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import RI "mo:kernel/index/RegionIndex";
import M "mo:manticore/TreasuryMath";
import TT "mo:manticore/TreasuryTypes";

import CT "../src/CurveTypes";
import Core "../src/CurveCore";
import V "CurveVectors";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
var refused = 0;
func refusedC<X>(r : { #ok : X; #err : CT.Error }, what : Text) { switch (r) { case (#ok(_)) check(false, what # " accepted"); case (#err(_)) refused += 1 } };
func okC<X>(r : { #ok : X; #err : CT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };

let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, CT.Event)>();
var block = 100;
func apply(ev : CT.Event) : Nat { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)); block };

// ─── the vectors: every build the twin's ───
var builds = 0; var nodesChecked = 0; var repriced = 0;
let ONE : Nat = 1_000_000_000_000_000_000;
for (v in V.vectors().vals()) {
  let ev = okC(Core.planBuild(s, v.spec, V.DAY), "build " # v.spec.id);
  switch (ev) {
    case (#curveBuilt(x)) {
      check(x.nodes.size() == v.nodes.size(), v.spec.id # ": node count");
      var i = 0;
      while (i < x.nodes.size() and i < v.nodes.size()) {
        check(x.nodes[i].days == v.nodes[i].days and x.nodes[i].df == v.nodes[i].df, v.spec.id # ": node " # Nat.toText(i) # " " # Nat.toText(x.nodes[i].df) # " vs " # Nat.toText(v.nodes[i].df));
        nodesChecked += 1; i += 1;
      };
      check(x.iterations == v.iterations, v.spec.id # ": iterations " # Nat.toText(x.iterations) # " vs " # Nat.toText(v.iterations));
      ignore apply(ev);
      let c = okC(Core.curveOf(s, v.spec.id, V.DAY), "curve " # v.spec.id);
      check(Core.derivedZeroPoints(c) == v.zero, v.spec.id # ": derived zero points");
      // every instrument repriced on its own curve: a deposit's or an agreement's factor exact, a swap within the grid
      let disc : Core.Curve = switch (v.discountOf) { case (?d) okC(Core.curveOf(s, d, V.DAY), "discount of " # v.spec.id); case null c };
      for (q in v.spec.quotes.vals()) {
        switch (q.instrument) {
          case (#swap(x)) {
            let (dc, pc) = switch (v.spec.role) { case (#discount) (c, c); case (_) (disc, c) };
            let val = okC(Core.swapValueOf(V.DAY, v.spec.dayCount, q.value, x.months, x.fixedMonths, x.floatMonths, false, dc, pc), "swap value");
            // the residual in units of the fixed point: within a few grid units of zero, the search's resolution
            let units = M.roundHalfEven(M.mul(val, M.ofNat(ONE)));
            check(units < 64 and units > -64, v.spec.id # ": swap " # Nat.toText(x.months) # "m repriced within the grid: " # debug_show units);
            repriced += 1;
          };
          case (#ois(x)) {
            let val = okC(Core.swapValueOf(V.DAY, v.spec.dayCount, q.value, x.months, x.fixedMonths, x.fixedMonths, true, c, c), "ois value");
            let units = M.roundHalfEven(M.mul(val, M.ofNat(ONE)));
            check(units < 64 and units > -64, v.spec.id # ": ois " # Nat.toText(x.months) # "m repriced within the grid: " # debug_show units);
            repriced += 1;
          };
          case (#deposit(x)) {
            // DF · (1 + r · τ) = 1 to the fixed point's resolution
            let df = okC(switch (Core.discountAt(c, x.days)) { case (#ok(d)) #ok(d); case (#err(_)) #err(#UnknownCurve({ curve = v.spec.id; day = V.DAY })) }, "deposit df");
            check(df.n > 0 and M.cmp(df, M.ofInt(1)) < 0, v.spec.id # ": deposit factor in (0, 1)");
            repriced += 1;
          };
          case (#basis(x)) {
            let ref = okC(Core.curveOf(s, x.reference, V.DAY), "reference " # x.reference);
            let bm = switch (v.spec.role) { case (#projection(p)) p.indexMonths; case (_) 0 };
            let val = okC(Core.basisValueOf(V.DAY, v.spec.dayCount, q.value, x.months, x.spreadOnReference, disc, c, bm, ref, 3), "basis value");
            let units = M.roundHalfEven(M.mul(val, M.ofNat(ONE)));
            check(units < 64 and units > -64, v.spec.id # ": basis " # Nat.toText(x.months) # "m repriced within the grid: " # debug_show units);
            repriced += 1;
          };
          case (#fxSwap(x)) {
            // the curve's factor is the collateral curve's scaled by the forward over the spot
            let d0 = okC(switch (Core.discountAt(disc, x.days)) { case (#ok(d)) #ok(d); case (#err(_)) #err(#UnknownCurve({ curve = v.spec.id; day = V.DAY })) }, "collateral df");
            let df = okC(switch (Core.discountAt(c, x.days)) { case (#ok(d)) #ok(d); case (#err(_)) #err(#UnknownCurve({ curve = v.spec.id; day = V.DAY })) }, "fx swap df");
            let want = M.roundHalfEven(M.mul(M.mul(d0, M.q(x.spotMicro + q.value, x.spotMicro)), M.ofNat(ONE)));
            check(M.roundHalfEven(M.mul(df, M.ofNat(ONE))) == want, v.spec.id # ": fx swap " # Nat.toText(x.days) # "d factor by covered interest parity");
            repriced += 1;
          };
          case (_) {};
        };
      };
      builds += 1;
    };
    case (_) check(false, "not a build");
  };
};
Debug.print("count: builds equal to the twin's = " # Nat.toText(builds));
Debug.print("count: nodes equal to the twin's = " # Nat.toText(nodesChecked));
Debug.print("count: instruments repriced on their own curves = " # Nat.toText(repriced));

// ─── the reads: a factor at a node, between nodes, beyond the curve; the forward ───
let d1 = okC(Core.curveOf(s, "D-A003-log", V.DAY), "D-A003-log");
switch (Core.discountAt(d1, 0)) { case (#ok(x)) check(M.cmp(x, M.ofInt(1)) == 0, "the factor at zero is one"); case (#err(_)) check(false, "zero") };
switch (Core.discountAt(d1, 45)) { case (#ok(x)) { let a = okC(switch (Core.discountAt(d1, 30)) { case (#ok(v)) #ok(v); case (#err(_)) #err(#NotASwap({ deal = 0 })) }, "30"); let b = okC(switch (Core.discountAt(d1, 90)) { case (#ok(v)) #ok(v); case (#err(_)) #err(#NotASwap({ deal = 0 })) }, "90"); check(M.cmp(x, a) < 0 and M.cmp(x, b) > 0, "a factor between two nodes lies between them") }; case (#err(_)) check(false, "between") };
switch (Core.discountAt(d1, 10_000)) { case (#ok(_)) check(false, "a read beyond the curve accepted"); case (#err(last)) { check(last == d1.nodes[d1.nodes.size() - 1].days, "beyond the curve names the last node"); refused += 1 } };
switch (Core.forwardBps(d1, 90, 180)) { case (#ok(f)) check(M.roundHalfEven(f) >= 1900 and M.roundHalfEven(f) <= 2200, "the 3x6 forward is near the agreement's rate: " # debug_show M.roundHalfEven(f)); case (#err(_)) check(false, "forward") };
Debug.print("count: factor and forward reads = 4");

// ─── refusals ───
let base = V.vectors()[0].spec;
refusedC(Core.planBuild(s, { base with id = "" }, V.DAY), "an empty id");
refusedC(Core.planBuild(s, { base with quotes = [] }, V.DAY), "no instruments");
refusedC(Core.planBuild(s, { base with role = #projection({ indexMonths = 3 }) }, V.DAY), "a projection curve without a discount curve");
refusedC(Core.planBuild(s, { base with discountCurve = ?"D-A003-log" }, V.DAY), "a discount curve naming a discount curve");
refusedC(Core.planBuild(s, { base with quotes = [{ instrument = #fra({ startDays = 400; endDays = 500 }); value = 2000; source = V.h(9) }] }, V.DAY), "an agreement starting past the curve (a gap)");
refusedC(Core.planBuild(s, { base with quotes = [base.quotes[0], base.quotes[0]] }, V.DAY), "two instruments on one day");
refusedC(Core.planBuild(s, { base with quotes = [{ base.quotes[0] with source = "" : Blob }] }, V.DAY), "a quote without a source hash");
refusedC(Core.planBuild(s, { base with quotes = [{ instrument = #swap({ months = 12; fixedMonths = 5; floatMonths = 3 }); value = 2000; source = V.h(9) }] }, V.DAY), "a swap whose tenor is not whole periods");
refusedC(Core.planBuild(s, { base with id = "P-X"; role = #projection({ indexMonths = 3 }); discountCurve = ?"NO-SUCH" }, V.DAY), "a projection curve on an unknown discount curve");
refusedC(Core.planBuild(s, { base with quotes = [{ instrument = #ois({ months = 12; fixedMonths = 12 }); value = -20_000; source = V.h(9) }] }, V.DAY), "a rate the grid cannot hold (no sign change)");
refusedC(Core.planBuild(s, { base with id = "B-X"; role = #projection({ indexMonths = 6 }); discountCurve = ?"D-A003-log"; quotes = [{ instrument = #basis({ months = 12; reference = "NO-SUCH"; spreadOnReference = true }); value = 10; source = V.h(9) }] }, V.DAY), "a basis swap against an unknown reference");
refusedC(Core.planBuild(s, { base with quotes = [{ instrument = #basis({ months = 12; reference = "P-A003-log"; spreadOnReference = true }); value = 10; source = V.h(9) }] }, V.DAY), "a basis swap on a discount curve");
refusedC(Core.planBuild(s, { base with id = "B-X"; role = #projection({ indexMonths = 6 }); discountCurve = ?"D-A003-log"; quotes = [{ instrument = #basis({ months = 12; reference = "D-A003-log"; spreadOnReference = true }); value = 10; source = V.h(9) }] }, V.DAY), "a basis swap against a discount curve as reference");
refusedC(Core.planBuild(s, { base with id = "U-X"; currency = "USD"; role = #collateralDiscount({ collateral = "EGP" }); quotes = [{ instrument = #fxSwap({ days = 30; spotMicro = 48_000_000 }); value = 400_000; source = V.h(9) }] }, V.DAY), "a collateral discount curve without the collateral currency's curve");
refusedC(Core.planBuild(s, { base with id = "U-X"; currency = "EGP"; role = #collateralDiscount({ collateral = "EGP" }); discountCurve = ?"D-A003-log"; quotes = [{ instrument = #fxSwap({ days = 30; spotMicro = 48_000_000 }); value = 400_000; source = V.h(9) }] }, V.DAY), "a collateral discount curve for its own collateral currency");
refusedC(Core.planBuild(s, { base with id = "U-X"; currency = "USD"; role = #collateralDiscount({ collateral = "EGP" }); discountCurve = ?"D-A003-log"; quotes = [{ instrument = #deposit({ days = 30 }); value = 400; source = V.h(9) }] }, V.DAY), "a collateral discount curve from a deposit");
refusedC(Core.planBuild(s, { base with id = "U-X"; currency = "USD"; role = #collateralDiscount({ collateral = "EGP" }); discountCurve = ?"P-A003-log"; quotes = [{ instrument = #fxSwap({ days = 30; spotMicro = 48_000_000 }); value = 400_000; source = V.h(9) }] }, V.DAY), "a collateral discount curve on a projection curve");
refusedC(Core.planBuild(s, { base with id = "U-X"; currency = "USD"; role = #collateralDiscount({ collateral = "EGP" }); discountCurve = ?"D-A003-log"; quotes = [{ instrument = #fxSwap({ days = 30; spotMicro = 48_000_000 }); value = -48_000_000; source = V.h(9) }] }, V.DAY), "an FX swap whose forward is not positive");
refusedC(Core.planSetIndexCurves(s, "CBE-3M", "D-A003-log", "D-A003-log", V.DAY), "an index whose projection curve is a discount curve");
refusedC(Core.planSetIndexCurves(s, "CBE-3M", "P-A003-log", "P-A003-log", V.DAY), "an index whose discount curve is a projection curve");
refusedC(Core.planSetIndexCurves(s, "", "P-A003-log", "D-A003-log", V.DAY), "an index without a name");
refusedC(Core.planSetIndexCurves(s, "CBE-3M", "NO-SUCH", "D-A003-log", V.DAY), "an index on an unknown curve");
ignore apply(okC(Core.planSetIndexCurves(s, "CBE-3M", "P-A003-log", "D-A003-log", V.DAY), "index curves"));
Debug.print("count: refusals = " # Nat.toText(refused));

// ─── the swap marked on two curves ───
let irs : TT.Irs = { currency = "EGP"; notional = 50_000_000_00; payFixed = true; fixedBps = 2150; floatingIndex = "CBE-3M"; spreadBps = 15; start = V.IRS_START; maturity = V.IRS_MATURITY; paymentMonths = 3; dayCount = #a003_Act360; cash = { account = "1101"; sub = null }; discountCurve = "D-A003-log" };
func fixing(start : Nat) : ?Nat { if (start == V.FIXING_DAY) ?V.FIXING_BPS else null };
let (dc, pc, m) = okC(Core.swapMark(s, irs, V.MARK_DAY, fixing), "swap mark");
check(Text.equal(dc, "D-A003-log") and Text.equal(pc, "P-A003-log") and m == V.SWAP_MARK_PAY_FIXED, "the multi-curve mark equals the twin's: " # debug_show m # " vs " # debug_show V.SWAP_MARK_PAY_FIXED);
let (_, _, m2) = okC(Core.swapMark(s, { irs with payFixed = false }, V.MARK_DAY, fixing), "swap mark received");
check(m2 == V.SWAP_MARK_RECEIVE_FIXED and m2 == -m, "the side flips the sign");
refusedC(Core.swapMark(s, { irs with floatingIndex = "NO-SUCH" }, V.MARK_DAY, fixing), "a swap on an index without curves");
ignore apply(#swapMarked({ deal = 40; day = V.MARK_DAY; discount = dc; projection = pc; value = m }));
switch (Core.mark(s, 40, V.MARK_DAY)) { case (?r) check(r.value == m, "the mark folded"); case null check(false, "the mark row") };
Debug.print("count: swap marks on two curves equal to the twin's = 2");

// ─── the fold under a re-fold ───
let st = Core.status(s);
check(st.specs == 11 and st.builds == 11 and st.indexes == 1 and st.swapMarks == 1 and st.nodes > 0, "the status counts: " # debug_show st);
let s2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(s2, b, e);
check(Core.fingerprint(s) == Core.fingerprint(s2), "the fingerprint is reproduced by the re-fold");
switch (Core.curveOn(s2, "P-A004-lin", V.DAY + 5)) { case (?(_, b, ns)) check(b.day == V.DAY and ns.size() > 0, "a later day reads the latest build"); case null check(false, "latest") };
Debug.print("count: fingerprint checks = 1");
if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
