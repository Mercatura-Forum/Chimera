/// Settlement.test.mo: settlement through Tachyon on the pure layer: the venue, the ledgers and a cycle declared,
/// a purchase and a sale instructed against Manticore's rows with the amounts read from the deal, the next step
/// of every state, a purchase's trade checked leg for leg against what Tachyon would return, the mirror of
/// Tachyon's audit log fed from an enumeration and refusing a tampered leaf or a wrong root, the settlement
/// receipt found in the mirror with the payouts read out of it, the fold through settlement, fail, recycle,
/// reclaim and reset, the status advice and the confirmation rendered and the advice parsed back; the fold's
/// fingerprint equal under a re-fold.
// engine: wasi-only

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import TT "mo:manticore/TreasuryTypes";
import Treasury "mo:manticore/TreasuryCore";
import Fx "mo:manticore/Fx";
import MMR "mo:tachyon/MerkleMMR";
import DT "mo:tachyon/DvpTypes";

import ST "../src/SettlementTypes";
import Core "../src/SettlementCore";
import Msg "../src/SettlementMessages";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func fp(c : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, c); w.toBlob() };
func okC<X>(r : { #ok : X; #err : ST.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func okT<X>(r : { #ok : X; #err : TT.TreasuryError }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
var refused = 0;
func refusedC<X>(r : { #ok : X; #err : ST.Error }, what : Text) { switch (r) { case (#ok(_)) check(false, what # " accepted"); case (#err(_)) refused += 1 } };

let arena = RI.newArena();
let t = Treasury.newState(arena);
let c = Core.newState(arena);
let folded = List.empty<(Nat, ST.Event)>();
var block = 400;
func next() : Nat { block += 1; block };
func applyT(ev : TT.TreasuryEvent) : Nat { let b = next(); Treasury.fold(t, b, ev); b };
func applyS(ev : ST.Event) : Nat { let b = next(); Core.fold(c, b, ev); List.add(folded, (b, ev)); b };

let D0 = 20_710;
let EGP = "EGP";
let desk = Principal.fromText("aaaaa-aa");
let cp = Principal.fromText("2ibo7-dia");
let coreP = Principal.fromText("rrkah-fqaaa-aaaaa-aaaaq-cai");
let cashL = Principal.fromText("ryjl3-tyaaa-aaaaa-aaaba-cai");
let secL = Principal.fromText("renrk-eyaaa-aaaaa-aaada-cai");
let ctx : Treasury.Ctx = { functional = EGP; spot = func(_ : Text, _ : Nat) : ?Fx.Rate { null }; pair = func(_ : Text) : ?Fx.PositionPair { null }; fixing = func(_ : Text, _ : Nat) : ?Nat { null }; isShariaBook = func(_ : Text) : Bool { false } };
var termsTable : [(Nat, TT.DealKind)] = [];
func terms(b : Nat) : ?TT.DealKind { for ((k, v) in termsTable.vals()) { if (k == b) return ?v }; null };
ignore applyT(okT(Treasury.planPolicy(S.policy), "policy"));
let bond : TT.SecurityTerms = { isin = "EG0000012345"; issuer = "ARE"; currency = EGP; couponBps = 1200; couponsPerYear = 2; dayCount = #a001_ActActIcma({ couponsPerYear = 2 }); issue = 20_500; maturity = 21_596 };
ignore applyT(okT(Treasury.planRegisterSecurity(t, bond, D0), "security"));
func capture(kind : TT.DealKind) : Nat {
  let id = block + 1;
  let r = okT(Treasury.planCapture(t, id, "BR01", S.citi, kind, "REF-" # Nat.toText(id), desk, D0, null, ctx, terms), "capture");
  let b = applyT(r.ev); termsTable := Array.concat(termsTable, [(b, kind)]);
  for (e in r.extras.vals()) ignore applyT(e);
  b
};
let buy : TT.SecurityTrade = { isin = bond.isin; direction = #buy; nominal = 10_000_000_00; priceMicro = 98_000_000; settlement = D0 + 2; classification = #fvoci; cash = S.cash; priceCurve = bond.isin; venue = null };
let buyId = capture(#security(buy));
let ?buyRow = Treasury.row(t, buyId) else Runtime.trap("FAIL: a row is missing");

// ─── the venue, the ledgers, a cycle ───
refusedC(Core.planInstruct(c, t, buyRow, buy, 1, 1, cp, ?1, "REF", S.h(1), D0), "instruct before the venue");
refusedC(Core.planVenue({ core = coreP; deadlineSecs = 0; recycleLimit = 3; claimsAccount = "1540" }), "a zero deadline");
ignore applyS(okC(Core.planVenue({ core = coreP; deadlineSecs = 3600; recycleLimit = 2; claimsAccount = "1540" }), "venue"));
refusedC(Core.planLedger({ role = #cash({ currency = "EG" }); ledger = cashL; partial = false }), "a two-letter currency");
refusedC(Core.planLedger({ role = #cash({ currency = EGP }); ledger = cashL; partial = true }), "a partial cash ledger");
ignore applyS(okC(Core.planLedger({ role = #cash({ currency = EGP }); ledger = cashL; partial = false }), "cash ledger"));
refusedC(Core.planInstruct(c, t, buyRow, buy, 1, 1, cp, ?1, "REF", S.h(1), D0), "instruct without the instrument's ledger");
ignore applyS(okC(Core.planLedger({ role = #security({ isin = bond.isin }); ledger = secL; partial = true }), "security ledger"));
refusedC(Core.planInstruct(c, t, buyRow, buy, 1, 1, cp, ?1, "REF", S.h(1), D0), "instruct without a cycle");
refusedC(Core.planOpenCycle(c, { businessDate = D0 - 1; market = "EGX"; priceSource = "EGX closing" }, D0), "a cycle in the past");
ignore applyS(okC(Core.planOpenCycle(c, { businessDate = D0 + 2; market = "EGX"; priceSource = "EGX closing" }, D0), "cycle"));
refusedC(Core.planOpenCycle(c, { businessDate = D0 + 2; market = "EGX"; priceSource = "EGX closing" }, D0), "a cycle twice");
check(Core.ledgers(c).size() == 2, "two ledgers declared");
Debug.print("count: venue, ledgers and cycle declared = 4");

// ─── a purchase instructed: the desk is the taker of the counterparty's trade ───
refusedC(Core.planInstruct(c, t, buyRow, buy, 9_850_000_00, 1, cp, null, "REF", S.h(1), D0), "a purchase without the counterparty's trade id");
refusedC(Core.planInstruct(c, t, buyRow, buy, 9_850_000_00, 1, cp, ?7, "", S.h(1), D0), "an empty reference");
let ins1 = applyS(okC(Core.planInstruct(c, t, buyRow, buy, 9_850_000_00, 1, cp, ?7, "REF-buy", S.h(1), D0), "instruct the purchase"));
refusedC(Core.planInstruct(c, t, buyRow, buy, 9_850_000_00, 1, cp, ?7, "REF-buy", S.h(1), D0), "instruct twice");
let ?r1 = Core.instruction(c, ins1) else Runtime.trap("FAIL: a row is missing");
check(r1.role == #taker and r1.tradeId == 7 and r1.assetAmount == buy.nominal and r1.cashAmount == 9_850_000_00 and r1.cycle == D0 + 2, "the instruction's legs are the deal's");
check(Core.openInstructionOf(c, buyId, 0) != null, "the leg is in Tachyon's hands");
switch (Core.cycle(c, D0 + 2)) { case (?cy) check(cy.instructions == 1 and cy.pending == 1, "the cycle counts the instruction"); case null check(false, "cycle") };
check(Core.nextStep(r1) == #verifyTrade({ tradeId = 7 }), "a purchase's first step reads the trade back");
// the trade as Tachyon would return it, checked leg for leg
func view(status : DT.TradeStatus, assetAmount : Nat, cashAmount : Nat, maker : Principal, taker : ?Principal) : DT.TradeView {
  let leg : DT.LegStateView = { escrowed = false; escrowBlock = null; escrowedAmount = 0; payout = null; payoutAmount = 0; refund = null; refundAmount = 0 };
  { id = 7; maker; taker; legA = { ledger = secL; kind = #icrc1({ amount = assetAmount }) }; legB = { ledger = cashL; kind = #icrc1({ amount = cashAmount }) }; legAState = leg; legBState = leg; deadline = 0; status; createdAt = 0 }
};
check(Core.checkTrade(r1, desk, view(#Open, buy.nominal, 9_850_000_00, cp, ?desk)) == null, "the matching trade passes");
check(Core.checkTrade(r1, desk, view(#Open, buy.nominal, 9_850_000_00, cp, null)) == null, "an open-taker trade passes");
check(Core.checkTrade(r1, desk, view(#Open, buy.nominal - 1, 9_850_000_00, cp, ?desk)) != null, "a different nominal is refused");
check(Core.checkTrade(r1, desk, view(#Open, buy.nominal, 9_850_000_01, cp, ?desk)) != null, "a different amount is refused");
check(Core.checkTrade(r1, desk, view(#Open, buy.nominal, 9_850_000_00, desk, ?cp)) != null, "a trade whose maker is not the counterparty is refused");
check(Core.checkTrade(r1, desk, view(#Aborted, buy.nominal, 9_850_000_00, cp, ?desk)) != null, "an aborted trade is refused");
Debug.print("count: trade checks = 6");
ignore applyS(#tradeVerified({ instruction = ins1; tradeId = 7; day = D0 }));
let ?r1v = Core.instruction(c, ins1) else Runtime.trap("FAIL: a row is missing");
check(Core.nextStep(r1v) == #fundTaker({ tradeId = 7 }), "a verified purchase funds the cash leg");
ignore applyS(#fundingRecorded({ instruction = ins1; tradeId = 7; escrowed = true; bothEscrowed = false; note = ""; day = D0 }));
let ?r1f = Core.instruction(c, ins1) else Runtime.trap("FAIL: a row is missing");
check(r1f.state == #funded and r1f.escrowed and Core.nextStep(r1f) == #observe({ tradeId = 7 }), "funded, the desk observes the trade");

// ─── the mirror of Tachyon's audit log, and the receipt ───
func ev(seq : Nat, tradeId : Nat, encoded : Text) : DT.AuditEvent { { seq; tradeId; encoded; leafHex = Core.hex(MMR.hashLeaf(Text.encodeUtf8(encoded))) } };
let log1 : [DT.AuditEvent] = [ev(0, 7, "ORDER|id=7|maker=x"), ev(1, 7, "FUND_A|id=7|block=3|amount=1000000000"), ev(2, 7, "FUND_B|id=7|block=9|amount=985000000")];
func rootOf(events : [DT.AuditEvent]) : ?Blob { let m = MMR.newState(); for (e in events.vals()) ignore MMR.append(m, MMR.hashLeaf(Text.encodeUtf8(e.encoded))); MMR.rootHash(m) };
switch (Core.checkEnumeration(c, log1, rootOf(log1))) {
  case (#ok(sync)) { check(sync.from == 0 and sync.leaves.size() == 3, "three leaves to append"); ignore applyS(#auditSynced({ from = sync.from; leaves = sync.leaves; root = sync.root; day = D0 })) };
  case (#err(e)) check(false, "enumeration refused: " # e);
};
check(Core.mirrorLeaves(c) == 3 and Core.mirrorRoot(c) == rootOf(log1), "the mirror holds the three leaves under Tachyon's root");
switch (Core.checkEnumeration(c, log1, ?S.h(9))) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "a wrong root accepted") };
let tampered = Array.tabulate<DT.AuditEvent>(3, func(i) { if (i == 1) ({ log1[i] with leafHex = Core.hex(S.h(2)) }) else log1[i] });
switch (Core.checkEnumeration(c, tampered, rootOf(log1))) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "a tampered leaf accepted") };
let swapped : [DT.AuditEvent] = [log1[0], ev(1, 7, "FUND_A|id=7|block=4|amount=1000000000"), log1[2]];
switch (Core.checkEnumeration(c, swapped, rootOf(swapped))) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "a leaf differing from the mirrored one accepted") };
switch (Core.checkEnumeration(c, [log1[0]], rootOf([log1[0]]))) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "an enumeration shorter than the mirror accepted") };
switch (Core.findReceipt(c, 7, log1)) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "a receipt before the settlement") };
let log2 = Array.concat<DT.AuditEvent>(log1, [ev(3, 7, "FUNDED|id=7"), ev(4, 7, "SETTLED|id=7|legA_to_taker=1000000000|legB_to_maker=985000000"), ev(5, 8, "ORDER|id=8|maker=y")]);
switch (Core.checkEnumeration(c, log2, rootOf(log2))) {
  case (#ok(sync)) { check(sync.from == 3 and sync.leaves.size() == 3, "three more leaves"); ignore applyS(#auditSynced({ from = sync.from; leaves = sync.leaves; root = sync.root; day = D0 + 2 })) };
  case (#err(e)) check(false, "second enumeration refused: " # e);
};
let receipt = okC(switch (Core.findReceipt(c, 7, log2)) { case (#ok(r)) #ok(r); case (#err(e)) #err(#ReceiptNotVerified({ instruction = ins1; reason = e })) }, "receipt");
check(receipt.seq == 4 and receipt.assetPaid == 1_000_000_000 and receipt.cashPaid == 985_000_000 and ?receipt.root == Core.mirrorRoot(c), "the settlement receipt: sequence, payouts and root");
Debug.print("count: audit enumerations mirrored = 2");
Debug.print("count: receipts verified = 1");
ignore applyS(#receiptVerified({ instruction = ins1; tradeId = 7; seq = receipt.seq; leaf = receipt.leaf; root = receipt.root; assetPaid = receipt.assetPaid; cashPaid = receipt.cashPaid; day = D0 + 2 }));
ignore applyS(#settled({ instruction = ins1; tradeId = 7; day = D0 + 2 }));
let ?r1s = Core.instruction(c, ins1) else Runtime.trap("FAIL: a row is missing");
check(r1s.state == #settled and Core.openInstructionOf(c, buyId, 0) == null, "settled, the leg is the desk's again");
switch (Core.cycle(c, D0 + 2)) { case (?cy) check(cy.settled == 1 and cy.pending == 0, "the cycle counts the settlement"); case null check(false, "cycle") };
check(Core.nextStep(r1s) == #nothing({ reason = "the instruction is settled" }), "nothing to drive once settled");

// ─── a sale instructed: the desk is the maker; it fails, is recycled, reclaimed and reset ───
ignore applyT(okT(Treasury.planSettleLeg(t, buyRow, #security(buy), 0, D0 + 2, ctx), "settle the purchase's leg").ev);
let sell : TT.SecurityTrade = { buy with direction = #sell; nominal = 4_000_000_00; priceMicro = 99_000_000; settlement = D0 + 3 };
let sellId = capture(#security(sell));
let ?sellRow = Treasury.row(t, sellId) else Runtime.trap("FAIL: a row is missing");
refusedC(Core.planInstruct(c, t, sellRow, sell, 3_960_000_00, 1, cp, ?9, "REF-sell", S.h(1), D0), "a sale with a trade id");
refusedC(Core.planInstruct(c, t, sellRow, sell, 3_960_000_00, 1, cp, null, "REF-sell", S.h(1), D0), "a sale whose settlement date has no cycle");
ignore applyS(okC(Core.planOpenCycle(c, { businessDate = D0 + 3; market = "EGX"; priceSource = "EGX closing" }, D0), "cycle 2"));
let ins2 = applyS(okC(Core.planInstruct(c, t, sellRow, sell, 3_960_000_00, 1, cp, null, "REF-sell", S.h(1), D0), "instruct the sale"));
let ?r2 = Core.instruction(c, ins2) else Runtime.trap("FAIL: a row is missing");
check(r2.role == #maker and Core.nextStep(r2) == #openTrade, "a sale's first step opens the trade");
ignore applyS(#tradeOpened({ instruction = ins2; tradeId = 11; escrowed = false; note = "asset escrow not yet in"; day = D0 }));
let ?r2o = Core.instruction(c, ins2) else Runtime.trap("FAIL: a row is missing");
check(r2o.state == #opened and Core.nextStep(r2o) == #fundMaker({ tradeId = 11 }), "opened without escrow, the desk funds its leg");
ignore applyS(#fundingRecorded({ instruction = ins2; tradeId = 11; escrowed = true; bothEscrowed = false; note = ""; day = D0 }));
let ?r2f = Core.instruction(c, ins2) else Runtime.trap("FAIL: a row is missing");
check(r2f.state == #funded and Core.nextStep(r2f) == #observe({ tradeId = 11 }), "funded, observing");
// the cycle closes with the sale unsettled: failed, recycled into the next cycle
ignore applyS(#failed({ instruction = ins2; cause = "not settled by the close of cycle"; fails = 1; day = D0 + 3 }));
ignore applyS(#cycleOpened({ cycle = { businessDate = D0 + 4; market = "EGX"; priceSource = "EGX closing" }; day = D0 + 3 }));
ignore applyS(#recycled({ instruction = ins2; cycle = D0 + 4; fails = 1; day = D0 + 3 }));
ignore applyS(#cycleClosed({ businessDate = D0 + 3; settled = 0; failed = 1; pending = 0; day = D0 + 3 }));
let ?r2r = Core.instruction(c, ins2) else Runtime.trap("FAIL: a row is missing");
check(r2r.state == #instructed and r2r.cycle == D0 + 4 and r2r.fails == 1 and r2r.escrowed and Core.nextStep(r2r) == #resetTrade({ previous = 11 }), "recycled with its escrow still on the aborted trade: reset first");
switch (Core.cycle(c, D0 + 3), Core.cycle(c, D0 + 4)) { case (?a, ?b) check(a.state == #closed and a.failed == 1 and b.instructions == 1 and b.pending == 1, "the cycles' counts"); case (_) check(false, "cycles") };
ignore applyS(#reclaimed({ instruction = ins2; tradeId = 11; note = "legA refund ok"; day = D0 + 4 }));
ignore applyS(#tradeReset({ instruction = ins2; previous = 11; day = D0 + 4 }));
let ?r2x = Core.instruction(c, ins2) else Runtime.trap("FAIL: a row is missing");
check(r2x.tradeId == 0 and not r2x.escrowed and Core.nextStep(r2x) == #openTrade, "reset, the sale opens a new trade");
check(Core.instructionsOfCycle(c, D0 + 4).size() == 1 and Core.instructionsInState(c, #instructed).size() == 1 and Core.instructionsInState(c, #settled).size() == 1, "the instructions by cycle and by state");
Debug.print("count: instructions through their states = 2");
Debug.print("count: refusals = " # Nat.toText(refused));

// ─── the messages ───
let advice = Msg.sese024Xml("REF-sell", ?11, Msg.statusOf(#failed, #maker, 1), "SAFE-001", bond.isin, sell.nominal, EGP, 3_960_000_00, 2, D0 + 3, false);
check(Text.contains(advice, #text "<Flng>") and Text.contains(advice, #text "<Cd>AWMO</Cd>") and Text.contains(advice, #text "<MktInfrstrctrTxId>11</MktInfrstrctrTxId>"), "a failed sale advises failing, awaiting money");
switch (Msg.parseSese024(Text.encodeUtf8(advice), 2)) {
  case (#ok(inc)) check(Text.equal(inc.reference, "REF-sell") and inc.tradeId == ?11 and Text.equal(inc.status, "SttlmSts/Flng") and inc.quantity == sell.nominal and inc.amount == 3_960_000_00 and Text.equal(inc.currency, EGP), "the advice parses back to its fields");
  case (#err(e)) check(false, "parse: " # e);
};
let pending = Msg.sese024Xml("REF-buy", ?7, Msg.statusOf(#funded, #taker, 0), "SAFE-001", bond.isin, buy.nominal, EGP, 9_850_000_00, 2, D0 + 2, true);
check(Text.contains(pending, #text "<Pdg>") and Text.contains(pending, #text "<Cd>AWSH</Cd>") and Text.contains(pending, #text "<SctiesMvmntTp>RECE</SctiesMvmntTp>"), "a funded purchase advises pending, awaiting shares");
let conf = Msg.sese025Xml("REF-buy", 7, "SAFE-001", bond.isin, buy.nominal, EGP, 9_850_000_00, 2, D0 + 2, D0 + 2, true);
check(Text.contains(conf, #text "<FctvSttlmDt><Dt><Dt>") and Text.contains(conf, #text "<SttldQty><Qty><FaceAmt>10000000.00</FaceAmt>") and Text.contains(conf, #text "<CdtDbtInd>DBIT</CdtDbtInd>"), "the confirmation carries the settled quantity and the effective day");
switch (Msg.parseSese024(Text.encodeUtf8("<Document><SctiesSttlmTxStsAdvc><TxId><AcctOwnrTxId>x</AcctOwnrTxId></TxId></SctiesSttlmTxStsAdvc></Document>"), 2)) { case (#err(_)) refused += 1; case (#ok(_)) check(false, "an advice without a status parsed") };
Debug.print("count: messages rendered and parsed = 4");

// ─── the fold under a re-fold ───
let c2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(c2, b, e);
check(fp(c) == fp(c2), "the settlement fingerprint is reproduced by the re-fold");
let st = Core.status(c);
check(st.instructions == 2 and st.cycles == 3 and st.ledgers == 2 and st.mirroredLeaves == 6 and st.settled == 1 and st.failed == 1, "the status counts: " # debug_show st);
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Settlement: all checks passed");
