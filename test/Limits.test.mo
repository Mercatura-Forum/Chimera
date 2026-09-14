/// Limits.test.mo: the limit tree on the pure layer: nodes set under the parent rules and refused outside them,
/// the counterparty's recorded group and country agreeing with the tree, every predicate matching the row it
/// should and no other, the breaches an amount makes at every level of the tree, the counters per node per day,
/// the sweep's fold (the bound rolled at the opening, the slices summed, the publication replacing the figure and
/// clearing what the fold caught up with), and the fingerprint reproduced by a re-fold.
// engine: wasi-only

import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";
import Array "mo:core/Array";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import Treasury "mo:manticore/TreasuryCore";

import LT "../src/LimitTypes";
import Core "../src/LimitCore";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : LT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func refused(r : { #ok : LT.Event; #err : LT.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };

let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, LT.Event)>();
var block = 100;
func apply(ev : LT.Event) : Nat { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)); block };
let D0 = 20_700;
let EGP = "EGP"; let USD = "USD";
// the books: HQ over two desks, each over one book
let books : [(Text, ?Text)] = [("HQ", null), ("DESK-A", ?"HQ"), ("DESK-B", ?"HQ"), ("BR01", ?"DESK-A"), ("BR02", ?"DESK-B")];
func parentOf(b : Text) : ?Text { for ((id, p) in books.vals()) { if (Text.equal(id, b)) return p }; null };
func exists(b : Text) : Bool { for ((id, _) in books.vals()) { if (Text.equal(id, b)) return true }; false };
func isUnder(book : Text, desk : Text) : Bool { var cur : ?Text = ?book; var n = 0; while (n < 6) { switch (cur) { case (?b) { if (Text.equal(b, desk)) return true; cur := parentOf(b) }; case null return false }; n += 1 }; false };
func node(id : Text, kind : LT.NodeKind, parent : ?Text, currency : Text, limit : Nat) : LT.Node { { id; kind; parent; currency; limit } };
func set(n : LT.Node) : Nat { apply(ok(Core.planSetNode(s, n, isUnder, exists, D0), "set " # n.id)) };
let alice = Principal.fromText("2vxsx-fae");

// ─── counterparties and the tree ───
var acts = 0;
ignore apply(ok(Core.planAmendCounterparty(s, { name = "CITI"; group = "CITIGROUP"; country = "US" }, D0), "CITI"));
ignore apply(ok(Core.planAmendCounterparty(s, { name = "CIB"; group = "CIB"; country = "EG" }, D0), "CIB"));
ignore apply(ok(Core.planAmendCounterparty(s, { name = "HSBC"; group = "HSBC"; country = "GB" }, D0), "HSBC"));
acts += 3;
ignore set(node("CTY-US", #country("US"), null, EGP, 100_000_000_00));
ignore set(node("GRP-CITI", #group("CITIGROUP"), ?"CTY-US", EGP, 60_000_000_00));
ignore set(node("CP-CITI", #counterparty("CITI"), ?"GRP-CITI", EGP, 40_000_000_00));
ignore set(node("CP-CIB", #counterparty("CIB"), null, EGP, 30_000_000_00));
ignore set(node("CP-CITI-USD", #counterparty("CITI"), null, USD, 5_000_000_00));
ignore set(node("CLS-SOV", #instrumentClass(#sovereign), null, EGP, 80_000_000_00));
ignore set(node("ISS-EGY", #issuer("Arab Republic of Egypt"), ?"CLS-SOV", EGP, 50_000_000_00));
ignore set(node("TNR-1Y", #tenor({ fromDays = 0; toDays = 365 }), null, EGP, 70_000_000_00));
ignore set(node("DESK-A", #desk("DESK-A"), null, EGP, 90_000_000_00));
ignore set(node("BOOK-BR01", #book("BR01"), ?"DESK-A", EGP, 44_000_000_00));
acts += 10;
Debug.print("count: counterparties and nodes recorded = " # Nat.toText(acts));

// the refusals: a wrong parent kind, a currency mismatch, a counterparty under the wrong group, a book under a
// desk it is not in, an unknown parent, a node as its own parent, a limit of zero, a bucket upside down, a
// counterparty that contradicts its node's parent, a removal of a parent with children
var refusals = 0;
if (refused(Core.planSetNode(s, node("X1", #issuer("X"), ?"CP-CITI", EGP, 1), isUnder, exists, D0), "issuer under a counterparty")) refusals += 1;
if (refused(Core.planSetNode(s, node("X2", #counterparty("CIB"), ?"CTY-US", USD, 1), isUnder, exists, D0), "a child in another currency")) refusals += 1;
if (refused(Core.planSetNode(s, node("X3", #counterparty("CIB"), ?"GRP-CITI", EGP, 1), isUnder, exists, D0), "CIB under CITIGROUP")) refusals += 1;
if (refused(Core.planSetNode(s, node("X4", #book("BR02"), ?"DESK-A", EGP, 1), isUnder, exists, D0), "BR02 under DESK-A")) refusals += 1;
if (refused(Core.planSetNode(s, node("X5", #counterparty("HSBC"), ?"GRP-NONE", EGP, 1), isUnder, exists, D0), "an unknown parent")) refusals += 1;
if (refused(Core.planSetNode(s, node("X6", #group("G"), ?"X6", EGP, 1), isUnder, exists, D0), "a node as its own parent")) refusals += 1;
if (refused(Core.planSetNode(s, node("X7", #group("G"), null, EGP, 0), isUnder, exists, D0), "a limit of zero")) refusals += 1;
if (refused(Core.planSetNode(s, node("X8", #tenor({ fromDays = 30; toDays = 10 }), null, EGP, 1), isUnder, exists, D0), "a bucket upside down")) refusals += 1;
if (refused(Core.planSetNode(s, node("X9", #book("BR09"), null, EGP, 1), isUnder, exists, D0), "an unknown book")) refusals += 1;
if (refused(Core.planAmendCounterparty(s, { name = "CITI"; group = "OTHER"; country = "US" }, D0), "CITI moved out of the group its node hangs under")) refusals += 1;
if (refused(Core.planRemoveNode(s, "GRP-CITI", D0), "a parent with children removed")) refusals += 1;
if (refused(Core.planSetNode(s, node("CP-CITI", #group("CITI"), null, EGP, 1), isUnder, exists, D0), "a node's kind changed in place")) refusals += 1;
Debug.print("count: tree refusals = " # Nat.toText(refusals));
check(Core.liveNodes(s).size() == 10, "ten live nodes");

// ─── the predicates ───
let egyHash = Treasury.hash8("Arab Republic of Egypt");
func facts(family : LT.Family, id : Nat, book : Text, cp : Text, ccy : Text, amount : Nat, issuer : Bool, cls : Bool, days : ?Nat) : Core.RowFacts {
  { family; id; book; cpHash = Treasury.hash8(cp); currency = ccy; amount; isin = if (issuer) "EG0000012345" else ""; issuerHash = if (issuer) egyHash else 0; classification = if (cls) ?#sovereign else null; remainingDays = days }
};
func has(xs : [Text], x : Text) : Bool { for (y in xs.vals()) { if (Text.equal(x, y)) return true }; false };
func same(xs : [Text], ys : [Text]) : Bool { xs.size() == ys.size() and Array.foldLeft<Text, Bool>(ys, true, func(acc, y) { acc and has(xs, y) }) };
let prepared = Core.prepare(s);
var predicates = 0;
// a sovereign bond bought from CITI in BR01, 200 days to run: every node but CIB's and the USD one
let n1 = Core.nodesFor(s, prepared, facts(#treasury, 1, "BR01", "CITI", EGP, 10_000_000_00, true, true, ?200), isUnder);
check(same(n1, ["CTY-US", "GRP-CITI", "CP-CITI", "CLS-SOV", "ISS-EGY", "TNR-1Y", "DESK-A", "BOOK-BR01"]), "the bond falls under eight nodes: " # debug_show n1); predicates += 1;
// the same bond sold: no issuer, no class (a sale is not a holding)
let n2 = Core.nodesFor(s, prepared, facts(#treasury, 2, "BR01", "CITI", EGP, 10_000_000_00, false, false, ?200), isUnder);
check(same(n2, ["CTY-US", "GRP-CITI", "CP-CITI", "TNR-1Y", "DESK-A", "BOOK-BR01"]), "the sale falls under six: " # debug_show n2); predicates += 1;
// a USD placement with CITI in BR02: only the USD counterparty node
let n3 = Core.nodesFor(s, prepared, facts(#call, 3, "BR02", "CITI", USD, 1_000_000_00, false, false, null), isUnder);
check(same(n3, ["CP-CITI-USD"]), "the USD call falls under the USD node only: " # debug_show n3); predicates += 1;
// a two-year deposit from CIB in BR02: CIB's node, no tenor bucket, no desk
let n4 = Core.nodesFor(s, prepared, facts(#treasury, 4, "BR02", "CIB", EGP, 5_000_000_00, false, false, ?730), isUnder);
check(same(n4, ["CP-CIB"]), "the CIB deposit falls under CIB's node only: " # debug_show n4); predicates += 1;
// a repo with HSBC (no node, no group node) in the desk's own book, 30 days: the tenor bucket and the desk
let n5 = Core.nodesFor(s, prepared, facts(#repo, 5, "DESK-A", "HSBC", EGP, 5_000_000_00, false, false, ?30), isUnder);
check(same(n5, ["TNR-1Y", "DESK-A"]), "the HSBC repo falls under the structural nodes: " # debug_show n5); predicates += 1;
// an unknown counterparty with no record: no group or country
let n6 = Core.nodesFor(s, prepared, facts(#loan, 6, "HQ", "NOBODY", EGP, 1, false, false, null), isUnder);
check(n6.size() == 0, "an unrecorded counterparty in HQ falls under nothing: " # debug_show n6); predicates += 1;
Debug.print("count: predicate checks = " # Nat.toText(predicates));

// ─── utilisation and breaches at every level ───
var breaches = 0;
// 35m of the bond captured within limits: recorded under its nodes
let f1 = facts(#treasury, 1, "BR01", "CITI", EGP, 35_000_000_00, true, true, ?200);
check(Core.breachesOf(s, n1, f1.amount).size() == 0, "35m within every limit");
ignore apply(#utilised({ family = #treasury; id = 1; currency = EGP; amount = f1.amount; nodes = n1; day = D0 }));
check(Core.utilisation(switch (Core.node(s, "CP-CITI")) { case (?n) n; case null Runtime.trap("no node") }) == 35_000_000_00, "CITI's node carries 35m");
// 10m more breaches the counterparty (40m) at the leaf, and the book (44m)
let b2 = Core.breachesOf(s, n1, 10_000_000_00);
check(b2.size() == 2 and has(Array.map<(Text, Nat, Nat), Text>(b2, func((n, _, _)) { n }), "CP-CITI") and has(Array.map<(Text, Nat, Nat), Text>(b2, func((n, _, _)) { n }), "BOOK-BR01"), "10m more breaches the leaf and the book: " # debug_show b2); breaches += b2.size();
// with an approver the row is recorded and the breaches beside it
ignore apply(#utilised({ family = #treasury; id = 2; currency = EGP; amount = 10_000_000_00; nodes = n1; day = D0 }));
for ((n, m, l) in b2.vals()) ignore apply(#breached({ node = n; family = #treasury; id = 2; measured = m; limit = l; approver = alice; day = D0 }));
// 20m from HSBC in the desk's book: the desk (90m) holds 45m, the tenor bucket (70m) 45m; nothing of CITI's is touched
check(Core.breachesOf(s, n5, 20_000_000_00).size() == 0, "HSBC's 20m is within the structural nodes");
ignore apply(#utilised({ family = #repo; id = 5; currency = EGP; amount = 20_000_000_00; nodes = n5; day = D0 }));
// 30m more from CITI: the leaf (75m of 40m), the group (75m of 60m), the issuer (75m of 50m), the bucket (95m of
// 70m), the desk (95m of 90m) and the book (75m of 44m) breach; the country (75m of 100m) and the class (75m of
// 80m) hold
let b3 = Core.breachesOf(s, n1, 30_000_000_00);
let b3n = Array.map<(Text, Nat, Nat), Text>(b3, func((n, _, _)) { n });
check(same(b3n, ["CP-CITI", "GRP-CITI", "BOOK-BR01", "DESK-A", "TNR-1Y", "ISS-EGY"]), "30m more breaches the leaf, its group, the book, the desk, the bucket and the issuer: " # debug_show b3n); breaches += b3.size();
for ((n, m, l) in b3.vals()) ignore apply(#breachRefused({ node = n; family = #treasury; subject = alice; amount = 30_000_000_00; measured = m; limit = l; day = D0 }));
// the country (100m) is caught at the child's capture when the children's sum passes it
ignore apply(#utilised({ family = #treasury; id = 7; currency = EGP; amount = 50_000_000_00; nodes = ["CTY-US"]; day = D0 }));
let b4 = Core.breachesOf(s, ["CTY-US"], 6_000_000_00);
check(b4.size() == 1 and b4[0].0 == "CTY-US" and b4[0].1 == 101_000_000_00, "the country is breached by the sum of its children: " # debug_show b4); breaches += 1;
Debug.print("count: breaches at every level = " # Nat.toText(breaches));
let (rec, ref) = Core.counter(s, "CP-CITI", D0);
check(rec == 1 and ref == 1, "CITI's leaf counts one recorded and one refused on the day: " # debug_show (rec, ref));
check(Core.counter(s, "GRP-CITI", D0) == (0, 1) and Core.counter(s, "CTY-US", D0) == (0, 0), "the group counts one refused, the country none");
let cs = Core.countersOf(s, "CP-CITI", D0 - 5, D0 + 5);
check(cs.size() == 1 and cs[0].day == D0, "the counters over a range");
Debug.print("count: counter checks = 3");

// ─── the sweep's fold ───
var sweepChecks = 0;
let citi = switch (Core.node(s, "CP-CITI")) { case (?n) n; case null Runtime.trap("no node") };
check(citi.sinceBound == 45_000_000_00 and citi.beforeBound == 0 and citi.published == 0, "before any sweep everything captured is since the bound"); sweepChecks += 1;
if (refused(Core.planOpenSweep(s, D0, block + 1, 0), "a slice of zero")) refusals += 1;
ignore apply(ok(Core.planOpenSweep(s, D0, block + 1, 256), "open the sweep"));
if (refused(Core.planOpenSweep(s, D0, block + 1, 256), "a second sweep while one is open")) refusals += 1;
if (refused(Core.planSetNode(s, node("X10", #group("G"), null, EGP, 1), isUnder, exists, D0), "a node set while a sweep is open")) refusals += 1;
let citi1 = switch (Core.node(s, "CP-CITI")) { case (?n) n; case null Runtime.trap("no node") };
check(citi1.beforeBound == 45_000_000_00 and citi1.sinceBound == 0 and Core.utilisation(citi1) == 45_000_000_00, "the opening rolls the captures under the bound"); sweepChecks += 1;
// a capture during the sweep: since the bound
ignore apply(#utilised({ family = #call; id = 300; currency = EGP; amount = 1_000_000_00; nodes = ["CP-CITI"]; day = D0 }));
// the slices: the treasury family in two, the calls in one, the repos and the loans empty
ignore apply(#sweepSliced({ day = D0; slice = 0; family = #treasury; visited = 256; nextCursor = ?("\01" : Blob); familyDone = false; nodes = [("CP-CITI", 30_000_000_00), ("GRP-CITI", 30_000_000_00)]; agreements = [] }));
ignore apply(#sweepSliced({ day = D0; slice = 1; family = #treasury; visited = 12; nextCursor = null; familyDone = true; nodes = [("CP-CITI", 14_000_000_00), ("GRP-CITI", 14_000_000_00)]; agreements = [] }));
ignore apply(#sweepSliced({ day = D0; slice = 2; family = #call; visited = 3; nextCursor = null; familyDone = true; nodes = [("CP-CITI", 500_000_00)]; agreements = [] }));
ignore apply(#sweepSliced({ day = D0; slice = 3; family = #repo; visited = 0; nextCursor = null; familyDone = true; nodes = []; agreements = [] }));
ignore apply(#sweepSliced({ day = D0; slice = 4; family = #loan; visited = 0; nextCursor = null; familyDone = true; nodes = []; agreements = [] }));
switch (Core.sweepView(s)) { case (?v) { check(v.complete and v.slices == 5 and v.rows == 271, "five slices, 271 rows, complete: " # debug_show v); sweepChecks += 1 }; case null check(false, "the sweep vanished") };
let citi2 = switch (Core.node(s, "CP-CITI")) { case (?n) n; case null Runtime.trap("no node") };
check(citi2.pending == 44_500_000_00 and Core.utilisation(citi2) == 46_000_000_00, "the pending figure sums the slices; the utilisation still reads the old figure and the captures"); sweepChecks += 1;
ignore apply(#sweepPublished({ day = D0; slices = 5; rows = 271; nodes = [("CP-CITI", 44_500_000_00), ("GRP-CITI", 44_000_000_00)] }));
let citi3 = switch (Core.node(s, "CP-CITI")) { case (?n) n; case null Runtime.trap("no node") };
check(citi3.published == 44_500_000_00 and citi3.publishedDay == D0 and citi3.beforeBound == 0 and citi3.sinceBound == 1_000_000_00 and citi3.pending == 0, "the publication replaces the figure, clears what the fold caught up with and keeps the capture past the bound: " # debug_show citi3); sweepChecks += 1;
check(Core.utilisation(citi3) == 45_500_000_00, "the utilisation is the published figure plus the capture since");
check(Core.sweepView(s) == null and Core.status(s).sweeps == 1 and Core.status(s).lastPublishedDay == D0, "the sweep closed"); sweepChecks += 1;
if (refused(Core.planOpenSweep(s, D0, block + 1, 256), "a sweep for a day already published")) refusals += 1;
Debug.print("count: sweep fold checks = " # Nat.toText(sweepChecks));
Debug.print("count: refusals = " # Nat.toText(refusals));

// ─── removal and the re-fold ───
ignore apply(ok(Core.planRemoveNode(s, "TNR-1Y", D0 + 1), "remove the bucket"));
check(Core.liveNodes(s).size() == 9 and Core.status(s).removed == 1, "nine live nodes after the removal");
ignore set(node("TNR-1Y", #tenor({ fromDays = 0; toDays = 180 }), null, EGP, 1_000_000_00));
check(Core.liveNodes(s).size() == 10 and Core.status(s).removed == 0, "a removed node set again is live with its new bounds");
let s2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(s2, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(s2), "the limits fingerprint is reproduced by the re-fold");
let st = Core.status(s);
check(st.nodes == 10 and st.counterparties == 3 and st.breachesRecorded == 2 and st.breachesRefused == 6, "the status counts: " # debug_show st);
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Limits: all checks passed");
