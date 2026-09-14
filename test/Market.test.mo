/// Market.test.mo: the market cycle on the pure layer: a market refused without an instrument, a unit face, a
/// deadline or with a participant twice; an order refused for no units or an unknown market; a cycle refused
/// with nothing staged or one already open; the engine's price per unit rounded against the desk (a bid down, an
/// offer up); the clean price a fill's cash implies with the accrued taken out; the driver's next step through the
/// fold from the opening to the close (submit each order, clear, read the fills, capture and instruct each, set the
/// trade, close), a fill against an unnamed principal held until the participant is declared, the settlement
/// observed; the fingerprint reproduced by a re-fold.
// engine: wasi-only

import Blob "mo:core/Blob";
import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Runtime "mo:core/Runtime";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";

import MT "../src/MarketTypes";
import Core "../src/MarketCore";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : MT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func refused<X>(r : { #ok : X; #err : MT.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };
let D0 = 20_700;
func p(n : Nat8) : Principal { Principal.fromBlob(Blob.fromArray([n, 1, 2, 3, 4])) };
let engine = p(9); let shares = p(10); let cash = p(11); let desk = p(1); let citi = p(20); let hsbc = p(21); let stranger = p(22);
let market : MT.Market = { isin = "EG0000012345"; engine; sharesLedger = shares; cashLedger = cash; currency = "EGP"; unitNominal = 10_000; deadlineSecs = 86_400; participants = [{ principal = citi; counterparty = { party = null; name = "CITI"; bic = "CITIUS33"; lei = "" } }, { principal = hsbc; counterparty = { party = null; name = "HSBC"; bic = "MIDLGB22"; lei = "" } }] };
let arena = RI.newArena();
let s = Core.newState(arena);
type Step = { #event : MT.Event; #settled : (Nat, Nat) };
let folded = List.empty<(Nat, Step)>();
var block = 100;
func apply(ev : MT.Event) : Nat { block += 1; Core.fold(s, block, ev); List.add(folded, (block, #event(ev))); block };
func settled(instruction : Nat, tradeId : Nat) { block += 1; Core.observeSettlement(s, block, #settled({ instruction; tradeId; day = D0 })); List.add(folded, (block, #settled((instruction, tradeId)))) };

// ─── the declaration ───
var refusals = 0;
if (refused(Core.planDeclare({ market with isin = "SHORT" }, D0), "a short ISIN")) refusals += 1;
if (refused(Core.planDeclare({ market with unitNominal = 0 }, D0), "no face per unit")) refusals += 1;
if (refused(Core.planDeclare({ market with deadlineSecs = 0 }, D0), "no deadline")) refusals += 1;
if (refused(Core.planDeclare({ market with participants = [market.participants[0], market.participants[0]] }, D0), "a participant twice")) refusals += 1;
if (refused(Core.planDeclare({ market with participants = [{ principal = citi; counterparty = { party = null; name = ""; bic = ""; lei = "" } }] }, D0), "a participant without a name")) refusals += 1;
let terms : MT.OrderTerms = { book = "BR01"; isin = market.isin; side = #buy; units = 500; classification = #fvoci; cash = { account = "1101"; sub = null }; reference = "order-1" };
if (refused(Core.planStage(s, terms, desk, true, null, D0), "an order before the market")) refusals += 1;
ignore apply(ok(Core.planDeclare(market, D0), "the market"));
if (refused(Core.planStage(s, { terms with units = 0 }, desk, true, null, D0), "an order for nothing")) refusals += 1;
if (refused(Core.planStage(s, { terms with reference = "" }, desk, true, null, D0), "an order without a reference")) refusals += 1;
if (refused(Core.planOpenCycle(s, market.isin, 98_500_000, 50, D0), "a cycle with nothing staged")) refusals += 1;
let o1 = apply(ok(Core.planStage(s, terms, desk, true, null, D0), "a bid"));
let o2 = apply(ok(Core.planStage(s, { terms with side = #sell; units = 300; reference = "order-2" }, desk, true, null, D0), "an offer"));
let o3 = apply(ok(Core.planStage(s, { terms with units = 100; reference = "order-3" }, desk, true, null, D0), "a second bid"));
ignore apply(ok(Core.planCancel(s, o3, "withdrawn by the trader", D0), "a cancellation"));
if (refused(Core.planCancel(s, o3, "again", D0), "a cancellation of a cancelled order")) refusals += 1;
check(Core.stagedOf(s, market.isin).size() == 2 and Core.status(s).staged == 2, "two orders staged after the cancellation");
Debug.print("count: declaration and order refusals = " # Nat.toText(refusals));

// ─── the arithmetic ───
check(Core.unitPrice(98_500_000, 10_000, #buy) == 9_850 and Core.unitPrice(98_500_000, 10_000, #sell) == 9_850, "an exact price per unit is the same on both sides");
check(Core.unitPrice(98_500_001, 10_000, #buy) == 9_850 and Core.unitPrice(98_500_001, 10_000, #sell) == 9_851, "an inexact price rounds a bid down and an offer up");
switch (Core.impliedPrice(9_850 * 500, 1_234, 500 * 10_000)) { case (?(price, clean)) check(clean == 4_923_766 and price == 98_475_320, "the clean price implied by the fill's cash less the accrued: " # debug_show (price, clean)); case null check(false, "implied") };
check(Core.impliedPrice(100, 200, 10_000) == null, "cash below the accrued implies nothing");
Debug.print("count: arithmetic checks = 4");

// ─── the cycle through the fold ───
let m = switch (Core.market(s, market.isin)) { case (?x) x; case null Runtime.trap("no market") };
let cid = apply(ok(Core.planOpenCycle(s, market.isin, 98_500_000, 50, D0), "the cycle"));
if (refused(Core.planOpenCycle(s, market.isin, 98_500_000, 50, D0), "a second cycle while one is open")) refusals += 1;
func cycle() : Core.CycleRow { switch (Core.cycle(s, cid)) { case (?c) c; case null Runtime.trap("no cycle") } };
var steps = 0;
switch (Core.nextStep(s, cycle(), m)) { case (#submit(x)) { check(x.order.id == o1 and x.limit == 9_850, "the first staged order is submitted at the reference per unit"); steps += 1 }; case (other) check(false, "submit: " # debug_show other) };
ignore apply(#orderSubmitted({ cycle = cid; order = o1; engineOrder = 7; window = 3; limit = 9_850; day = D0 }));
switch (Core.nextStep(s, cycle(), m)) { case (#submit(x)) { check(x.order.id == o2, "the second order next"); steps += 1 }; case (other) check(false, "submit 2: " # debug_show other) };
ignore apply(#orderSubmitted({ cycle = cid; order = o2; engineOrder = 8; window = 3; limit = 9_850; day = D0 }));
check(cycle().state == #clearing and cycle().submitted == 2, "every order submitted: the cycle clears");
switch (Core.nextStep(s, cycle(), m)) { case (#clear(x)) { check(x.window == null, "the first clear is the engine's open window"); steps += 1 }; case (other) check(false, "clear: " # debug_show other) };
ignore apply(#clearAdvanced({ cycle = cid; window = 3; clearingPrice = ?9_850; targetVolume = 800; filled = 400; chunks = 1; complete = false; day = D0 }));
check(cycle().state == #clearing, "an incomplete chunk keeps clearing");
switch (Core.nextStep(s, cycle(), m)) { case (#clear(x)) { check(x.window == ?3, "the next chunk names the window the engine cleared"); steps += 1 }; case (other) check(false, "clear 2: " # debug_show other) };
ignore apply(#clearAdvanced({ cycle = cid; window = 3; clearingPrice = ?9_850; targetVolume = 800; filled = 800; chunks = 2; complete = true; day = D0 }));
check(cycle().state == #settling and cycle().hasPrice and cycle().clearingPrice == 9_850, "the clear complete: the cycle settles at the price");
switch (Core.nextStep(s, cycle(), m)) { case (#readFills(x)) { check(x.window == 3 and x.from == 0, "the fills are read from the start"); steps += 1 }; case (other) check(false, "read: " # debug_show other) };
ignore apply(#filled({ cycle = cid; order = o1; seq = 0; price = 9_850; units = 500; counterparty = citi; day = D0 }));
ignore apply(#filled({ cycle = cid; order = o2; seq = 1; price = 9_850; units = 200; counterparty = stranger; day = D0 }));
ignore apply(#fillUnattributed({ cycle = cid; seq = 1; counterparty = stranger; day = D0 }));
ignore apply(#fillsRead({ cycle = cid; through = 2; fills = 2; complete = true; day = D0 }));
check(cycle().readDone and cycle().fills == 2 and cycle().unattributed == 1 and cycle().nextSeq == 2, "two fills read, one unattributed: " # debug_show (Core.cycleView(cycle())));
switch (Core.order(s, o2)) { case (?o) check(o.state == #partlyFilled and o.filled == 200, "the offer partly filled"); case null check(false, "o2") };
switch (Core.nextStep(s, cycle(), m)) { case (#captureFill(x)) { check(x.fill.seq == 0, "the attributed fill is captured first"); steps += 1 }; case (other) check(false, "capture: " # debug_show other) };
ignore apply(#fillCaptured({ cycle = cid; seq = 0; deal = 900; day = D0 }));
switch (Core.nextStep(s, cycle(), m)) { case (#instructFill(x)) { check(x.fill.seq == 0, "a captured fill not yet instructed is instructed next"); steps += 1 }; case (other) check(false, "instruct: " # debug_show other) };
ignore apply(#fillInstructed({ cycle = cid; seq = 0; deal = 900; instruction = 901; day = D0 }));
switch (Core.nextStep(s, cycle(), m)) { case (#settleFill(x)) { check(x.fill.seq == 0 and x.fill.instruction == 901, "the instructed fill waits for its trade"); steps += 1 }; case (other) check(false, "settle: " # debug_show other) };
ignore apply(#fillTradeSet({ cycle = cid; seq = 0; tradeId = 77; day = D0 }));
switch (Core.nextStep(s, cycle(), m)) { case (#withdraw(x)) { check(x.order.id == o2, "the partly filled offer still resting on the engine is withdrawn"); steps += 1 }; case (other) check(false, "withdraw: " # debug_show other) };
ignore apply(#orderWithdrawn({ cycle = cid; order = o2; engineOrder = 8; day = D0 }));
switch (Core.order(s, o2)) { case (?o) check(o.state == #partlyFilled and o.withdrawn, "the withdrawn offer is partly filled"); case null check(false, "o2 withdrawn") };
switch (Core.nextStep(s, cycle(), m)) { case (#nothing(x)) { check(x.reason != "", "the unattributed fill holds the close: " # x.reason); steps += 1 }; case (other) check(false, "hold: " # debug_show other) };
// the participant declared: the market redeclared with the stranger named
ignore apply(#marketDeclared({ market = { market with participants = [market.participants[0], market.participants[1], { principal = stranger; counterparty = { party = null; name = "NEWCO"; bic = ""; lei = "" } }] }; day = D0 }));
switch (Core.nextStep(s, cycle(), m)) { case (#captureFill(x)) { check(x.fill.seq == 1, "the fill is captured once its participant is named"); steps += 1 }; case (other) check(false, "capture 2: " # debug_show other) };
ignore apply(#fillCaptured({ cycle = cid; seq = 1; deal = 910; day = D0 }));
ignore apply(#fillInstructed({ cycle = cid; seq = 1; deal = 910; instruction = 911; day = D0 }));
ignore apply(#fillTradeSet({ cycle = cid; seq = 1; tradeId = 78; day = D0 }));
check(cycle().unattributed == 0 and cycle().instructed == 2, "both fills instructed");
switch (Core.nextStep(s, cycle(), m)) { case (#close) steps += 1; case (other) check(false, "close: " # debug_show other) };
settled(901, 77);
check(cycle().settled == 1 and Core.status(s).settled == 1, "a fill's instruction settled marks the fill");
switch (Core.fillOfInstruction(s, 901)) { case (?f) check(f.state == #settled and f.tradeId == 77, "the fill by its instruction"); case null check(false, "fill of 901") };
ignore apply(#cycleClosed({ cycle = cid; fills = 2; unfilled = [o2]; day = D0 }));
check(cycle().state == #closed and Core.status(s).openCycles == 0, "the cycle closed");
switch (Core.order(s, o2)) { case (?o) check(o.state == #partlyFilled, "the offer stays partly filled at the close"); case null check(false, "o2 close") };
switch (Core.nextStep(s, cycle(), m)) { case (#nothing(_)) steps += 1; case (other) check(false, "closed: " # debug_show other) };
if (refused(Core.planOpenCycle(s, market.isin, 98_600_000, 60, D0 + 1), "a cycle after the close with nothing staged")) refusals += 1;
Debug.print("count: driver steps = " # Nat.toText(steps));
Debug.print("count: refusals = " # Nat.toText(refusals));

// ─── the fold reproduced ───
let again = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) { switch (e) { case (#event(ev)) Core.fold(again, b, ev); case (#settled((i, t))) Core.observeSettlement(again, b, #settled({ instruction = i; tradeId = t; day = D0 })) } };
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(again), "the market fingerprint is reproduced by the re-fold");
Debug.print("count: fold and fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Market: all checks passed");
