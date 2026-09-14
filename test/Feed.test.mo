/// Feed.test.mo: the price feed on the pure layer: a declaration refused with fewer than three sources or a
/// band or a bound out of range, a source's signature verified against the reference vector and refused when a
/// figure is changed, the assessment (three fresh figures within the band accepted at their median, a split a
/// halt, fewer than three nothing, a stale figure left out), a replayed or future or stale submission refused,
/// the reference price refused while halted and lifted only after the sources agree again; the fold and the
/// fingerprint reproduced by a re-fold.
// engine: wasi-only

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat64 "mo:core/Nat64";
import Runtime "mo:core/Runtime";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";

import FT "../src/FeedTypes";
import Core "../src/FeedCore";
import V "support/FeedVectors";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : FT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func refused<X>(r : { #ok : X; #err : FT.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };
let D0 = 20_700;
let NS : Nat64 = 1_000_000_000;
let zeroKey : Blob = Blob.fromArray(Array.repeat<Nat8>(0, Core.PK_BYTES));
func src(id : Text) : FT.Source { { id; publicKey = zeroKey } };
let feed : FT.Feed = { isin = V.isin; sources = [{ id = V.source; publicKey = V.publicKey }, src("REUTERS"), src("CBE"), src("EGX")]; bandBps = 100; staleSeconds = 600 };

// ─── the declaration ───
var refusals = 0;
ignore ok(Core.planDeclare(feed, D0), "the feed");
if (refused(Core.planDeclare({ feed with sources = [src("A"), src("B")] }, D0), "two sources")) refusals += 1;
if (refused(Core.planDeclare({ feed with bandBps = 0 }, D0), "no band")) refusals += 1;
if (refused(Core.planDeclare({ feed with staleSeconds = 0 }, D0), "no bound")) refusals += 1;
if (refused(Core.planDeclare({ feed with sources = [src("A"), src("A"), src("B")] }, D0), "a source twice")) refusals += 1;
if (refused(Core.planDeclare({ feed with sources = [{ id = "A"; publicKey = "\01\02" : Blob }, src("B"), src("C")] }, D0), "a key of two bytes")) refusals += 1;
Debug.print("count: declaration refusals = " # Nat.toText(refusals));

// ─── the signature against the reference vector ───
check(Core.verify(V.publicKey, V.isin, V.source, V.priceMicro, V.asOf, V.signature), "the reference signature verifies over the canonical message");
check(not Core.verify(V.publicKey, V.isin, V.source, V.priceMicro + 1, V.asOf, V.signature), "a changed figure does not verify");
check(not Core.verify(V.publicKey, V.isin, "OTHER", V.priceMicro, V.asOf, V.signature), "another source's name does not verify");
check(not Core.verify(zeroKey, V.isin, V.source, V.priceMicro, V.asOf, V.signature), "another key does not verify");
Debug.print("count: signature checks = 4");

// ─── the assessment ───
func row(source : Text, price : Nat, asOf : Nat64) : Core.SourceRow { { isin = V.isin; source; publicKey = zeroKey; latestMicro = price; latestAsOf = asOf; latestBlock = if (price == 0) 0 else 1; submissions = 1 } };
let f0 : Core.FeedRow = { isin = V.isin; sources = 4; bandBps = 100; staleSeconds = 600; halted = 0; acceptedMicro = 0; acceptedAsOf = 0; acceptedBlock = 0; acceptances = 0; haltBlock = 0; lastBlock = 1 };
let t0 : Nat64 = 1_000 * NS;
var assessments = 0;
switch (Core.assess(f0, [row("A", 98_500_000, t0), row("B", 98_400_000, t0 - 10 * NS), row("C", 98_600_000, t0 - 20 * NS)], t0)) {
  case (#accepted(a)) { check(a.priceMicro == 98_500_000 and a.asOf == t0 and a.figures.size() == 3, "three fresh figures within the band: the median, the latest time: " # debug_show a); assessments += 1 };
  case (other) check(false, "three agreeing figures accepted: " # debug_show other);
};
switch (Core.assess(f0, [row("A", 98_500_000, t0), row("B", 98_400_000, t0), row("C", 98_600_000, t0), row("D", 98_450_000, t0)], t0)) {
  case (#accepted(a)) { check(a.priceMicro == 98_475_000, "four figures: the mean of the middle two: " # debug_show a.priceMicro); assessments += 1 };
  case (other) check(false, "four agreeing figures accepted: " # debug_show other);
};
switch (Core.assess(f0, [row("A", 98_500_000, t0), row("B", 99_600_000, t0), row("C", 98_600_000, t0)], t0)) {
  case (#split(x)) { check(x.figures.size() == 3, "a figure 1.1 percent above the lowest is a split"); assessments += 1 };
  case (other) check(false, "a split is a split: " # debug_show other);
};
switch (Core.assess(f0, [row("A", 98_500_000, t0), row("B", 98_400_000, t0 - 700 * NS), row("C", 98_600_000, t0)], t0)) {
  case (#none) assessments += 1;
  case (other) check(false, "two fresh figures and a stale one are nothing: " # debug_show other);
};
switch (Core.assess(f0, [row("A", 98_500_000, t0), row("B", 0, 0), row("C", 98_600_000, t0), row("D", 98_550_000, t0 - 599 * NS)], t0)) {
  case (#accepted(a)) { check(a.figures.size() == 3 and a.priceMicro == 98_550_000, "a source with no figure is not counted; one at the bound is: " # debug_show a); assessments += 1 };
  case (other) check(false, "three of four accepted: " # debug_show other);
};
Debug.print("count: assessments = " # Nat.toText(assessments));

// ─── the fold and the submissions ───
let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, FT.Event)>();
var block = 100;
func apply(ev : FT.Event) { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)) };
apply(ok(Core.planDeclare(feed, D0), "declare"));
let now : Nat64 = V.asOf + 30 * NS;
let sub : FT.Submission = { isin = V.isin; source = V.source; priceMicro = V.priceMicro; asOf = V.asOf; signature = V.signature };
if (refused(Core.planSubmit(s, { sub with source = "NOBODY" }, now, D0), "an unknown source")) refusals += 1;
if (refused(Core.planSubmit(s, { sub with priceMicro = V.priceMicro + 1 }, now, D0), "a changed figure")) refusals += 1;
if (refused(Core.planSubmit(s, sub, V.asOf + 700 * NS, D0), "a figure older than the bound")) refusals += 1;
if (refused(Core.planSubmit(s, sub, V.asOf - 200 * NS, D0), "a figure from the future")) refusals += 1;
let evs = ok(Core.planSubmit(s, sub, now, D0), "the signed figure");
check(evs.size() == 1, "one fresh figure is recorded and nothing accepted");
for (ev in evs.vals()) apply(ev);
if (refused(Core.planSubmit(s, sub, now, D0), "the same figure again")) refusals += 1;
// the other sources' figures land through the fold directly (their keys are not the vector's)
apply(#priceSubmitted({ isin = V.isin; source = "REUTERS"; priceMicro = V.priceMicro + 200_000; asOf = V.asOf + NS; signature = V.signature; day = D0 }));
apply(#priceSubmitted({ isin = V.isin; source = "CBE"; priceMicro = V.priceMicro - 300_000; asOf = V.asOf + 2 * NS; signature = V.signature; day = D0 }));
switch (Core.assess(switch (Core.feed(s, V.isin)) { case (?f) f; case null Runtime.trap("no feed") }, Core.sourcesOf(s, V.isin), now)) {
  case (#accepted(a)) { check(a.priceMicro == V.priceMicro and a.figures.size() == 3, "three sources within the band on the fold"); apply(#priceAccepted({ isin = V.isin; priceMicro = a.priceMicro; asOf = a.asOf; figures = a.figures; day = D0 })) };
  case (other) check(false, "accepted on the fold: " # debug_show other);
};
let ref1 = ok(Core.referencePrice(s, V.isin, now), "the reference price");
check(ref1.priceMicro == V.priceMicro and ref1.block == block, "the reference is the accepted price at its block");
if (refused(Core.referencePrice(s, V.isin, now + 601 * NS), "the reference past the bound")) refusals += 1;
check(Core.staleFeeds(s, now + 601 * NS).size() == 1 and Core.staleFeeds(s, now).size() == 0, "a stale feed is listed for the halt");
if (refused(Core.planLift(s, V.isin, D0), "a lift with no halt")) refusals += 1;
apply(#instrumentHalted({ isin = V.isin; reason = #disagreement; figures = []; day = D0 }));
if (refused(Core.referencePrice(s, V.isin, now), "the reference while halted")) refusals += 1;
if (refused(Core.planLift(s, V.isin, D0), "a lift before the sources agree again")) refusals += 1;
apply(#priceAccepted({ isin = V.isin; priceMicro = V.priceMicro; asOf = now; figures = []; day = D0 }));
apply(ok(Core.planLift(s, V.isin, D0), "the lift after an agreement"));
ignore ok(Core.referencePrice(s, V.isin, now), "the reference after the lift");
let st = Core.status(s);
check(st.feeds == 1 and st.halted == 0 and st.submissions == 3 and st.acceptances == 2, "the status counts: " # debug_show st);
Debug.print("count: refusals = " # Nat.toText(refusals));
let again = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(again, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(again), "the feed fingerprint is reproduced by the re-fold");
Debug.print("count: fold and fingerprint checks = 4");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Feed: all checks passed");
