/// Reconciliation.test.mo: the reconciliation on the pure layer: a camt.054 parsed to the statement's entry shape,
/// a semt.002 and a semt.017 parsed to holdings and postings and refused outside their paths; a notification's
/// entries matched by reference then by amount and date within the tolerance, each leg once, closed legs never;
/// the custodian's holdings against the depot's positions and its postings against the desk's instructions,
/// explained fails never an item; the policy's refusals; the fold of breaks, their ageing, their resolution, the
/// cash rows and the clearing of a cash break; the fingerprint reproduced by a re-fold.
// engine: wasi-only

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

import RT "../src/ReconciliationTypes";
import Core "../src/ReconciliationCore";
import Msg "../src/ReconciliationMessages";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };
func ok<X>(r : { #ok : X; #err : RT.Error }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # debug_show e) } };
func okT<X>(r : { #ok : X; #err : Text }, what : Text) : X { switch (r) { case (#ok(x)) x; case (#err(e)) Runtime.trap("FAIL: " # what # " refused: " # e) } };
func refusedT<X>(r : { #ok : X; #err : Text }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };
func refused(r : { #ok : RT.Event; #err : RT.Error }, what : Text) : Bool { switch (r) { case (#ok(_)) { failures += 1; Debug.print("FAIL: " # what # " was accepted"); false }; case (#err(_)) true } };
func h(n : Nat8) : Blob { S.h(n) };
let D0 = 20_700;

// ─── the documents ───
func camt054(entries : [(Text, Text, Text, Text)]) : Text {
  var ntry = "";
  for ((ref, amt, cd, date) in entries.vals()) ntry #= "<Ntry><NtryRef>" # ref # "</NtryRef><Amt Ccy=\"USD\">" # amt # "</Amt><CdtDbtInd>" # cd # "</CdtDbtInd><Sts><Cd>BOOK</Cd></Sts><BookgDt><Dt>" # date # "</Dt></BookgDt><ValDt><Dt>" # date # "</Dt></ValDt></Ntry>";
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:camt.054.001.08\"><BkToCstmrDbtCdtNtfctn><GrpHdr><MsgId>N1</MsgId><CreDtTm>2026-09-01T10:00:00Z</CreDtTm></GrpHdr><Ntfctn><Id>N1</Id><CreDtTm>2026-09-01T10:00:00Z</CreDtTm><Acct><Id><Othr><Id>NOSTRO-USD</Id></Othr></Id><Ccy>USD</Ccy></Acct>" # ntry # "</Ntfctn></BkToCstmrDbtCdtNtfctn></Document>"
};
func semt002(account : Text, date : Text, basis : Text, balances : [(Text, Text)]) : Text {
  var bal = "";
  for ((isin, face) in balances.vals()) bal #= "<BalForAcct><FinInstrmId><ISIN>" # isin # "</ISIN></FinInstrmId><AggtBal><ShrtLngInd>LONG</ShrtLngInd><Qty><Qty><Qty><FaceAmt>" # face # "</FaceAmt></Qty></Qty></Qty></AggtBal></BalForAcct>";
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:semt.002.001.11\"><SctiesBalCtdyRpt><Pgntn><PgNb>1</PgNb><LastPgInd>true</LastPgInd></Pgntn><StmtGnlDtls><StmtId>H1</StmtId><StmtDtTm><Dt>" # date # "</Dt></StmtDtTm><Frqcy><Cd>DAIL</Cd></Frqcy><UpdTp><Cd>COMP</Cd></UpdTp><StmtBsis><Cd>" # basis # "</Cd></StmtBsis><ActvtyInd>true</ActvtyInd><SubAcctInd>false</SubAcctInd></StmtGnlDtls><SfkpgAcct><Id>" # account # "</Id></SfkpgAcct>" # bal # "</SctiesBalCtdyRpt></Document>"
};
func semt017(account : Text, from : Text, to : Text, txs : [(Text, Text, Text, Text, Text)]) : Text {
  var fi = "";
  for ((isin, ref, mv, face, date) in txs.vals()) fi #= "<FinInstrmDtls><FinInstrmId><ISIN>" # isin # "</ISIN></FinInstrmId><Tx><AcctOwnrTxId>" # ref # "</AcctOwnrTxId><TxDtls><TxActvty><Cd>SETT</Cd></TxActvty><SctiesMvmntTp>" # mv # "</SctiesMvmntTp><Pmt>APMT</Pmt><PstngQty><Qty><FaceAmt>" # face # "</FaceAmt></Qty></PstngQty><FctvSttlmDt><Dt>" # date # "</Dt></FctvSttlmDt></TxDtls></Tx></FinInstrmDtls>";
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:semt.017.001.12\"><SctiesTxPstngRpt><Pgntn><PgNb>1</PgNb><LastPgInd>true</LastPgInd></Pgntn><StmtGnlDtls><StmtId>T1</StmtId><StmtPrd><FrDtToDt><FrDt>" # from # "</FrDt><ToDt>" # to # "</ToDt></FrDtToDt></StmtPrd><StmtBsis><Cd>SETT</Cd></StmtBsis><ActvtyInd>true</ActvtyInd><SubAcctInd>false</SubAcctInd></StmtGnlDtls><SfkpgAcct><Id>" # account # "</Id></SfkpgAcct>" # fi # "</SctiesTxPstngRpt></Document>"
};

var parsed = 0;
let n1 = okT(Msg.parseCamt054(Text.encodeUtf8(camt054([("security-1", "9850000.00", "DBIT", "2026-09-01"), ("mm-7", "1000.50", "CRDT", "2026-08-31")])), "USD", 2), "camt.054");
check(n1.entries.size() == 2 and n1.entries[0].amount == 9_850_000_00 and not n1.entries[0].credit and n1.entries[1].credit and n1.entries[1].amount == 1_000_50 and n1.id == "N1" and n1.account == "NOSTRO-USD", "the notification's entries, amounts and sides"); parsed += 1;
check(n1.entries[0].valueDay == n1.entries[1].valueDay + 1, "the value days from the ISO dates"); parsed += 1;
if (refusedT(Msg.parseCamt054(Text.encodeUtf8(camt054([("x", "1.00", "DBIT", "2026-09-01")])), "EGP", 2), "a notification in another currency")) parsed += 1;
if (refusedT(Msg.parseCamt054(Text.encodeUtf8("<Document><BkToCstmrStmt/></Document>"), "USD", 2), "a statement where a notification is expected")) parsed += 1;
let h1 = okT(Msg.parseSemt002(Text.encodeUtf8(semt002("SAFE-001", "2026-09-01", "SETT", [("EG0000012345", "10000000.00"), ("EG0000023456", "2500000.00")])), 2), "semt.002");
check(h1.account == "SAFE-001" and h1.holdings.size() == 2 and h1.holdings[0].nominal == 10_000_000_00 and h1.holdings[1].isin == "EG0000023456" and h1.id == "H1", "the holdings report's account and balances"); parsed += 1;
if (refusedT(Msg.parseSemt002(Text.encodeUtf8(semt002("SAFE-001", "2026-09-01", "TRAD", [])), 2), "a report on the trade-date basis")) parsed += 1;
let t1 = okT(Msg.parseSemt017(Text.encodeUtf8(semt017("SAFE-001", "2026-08-30", "2026-09-01", [("EG0000012345", "security-1", "RECE", "10000000.00", "2026-09-01"), ("EG0000012345", "security-2", "DELI", "1000000.00", "2026-08-31")])), 2), "semt.017");
check(t1.transactions.size() == 2 and not t1.transactions[0].delivered and t1.transactions[1].delivered and t1.transactions[1].nominal == 1_000_000_00 and t1.to - t1.from == 2, "the posting report's transactions"); parsed += 1;
if (refusedT(Msg.parseSemt017(Text.encodeUtf8(semt017("SAFE-001", "2026-08-30", "2026-09-01", [("EG0000012345", "x", "RECE", "1.00", "2026-09-05")])), 2), "a posting outside the period")) parsed += 1;
check(Msg.depotDocumentKind(Text.encodeUtf8(semt002("A", "2026-09-01", "SETT", []))) == ?#holdings and Msg.depotDocumentKind(Text.encodeUtf8(semt017("A", "2026-09-01", "2026-09-01", []))) == ?#transactions and Msg.depotDocumentKind(Text.encodeUtf8("<Document/>")) == null, "the kind by the root"); parsed += 1;
Debug.print("count: documents parsed and refused = " # Nat.toText(parsed));

// ─── the notification's matching ───
func leg(posting : Nat, amount : Nat, debit : Bool, day : Nat, reference : Text, status : Nat8) : Treasury.NostroLegRow {
  { nostroHash = 1; valueDay = day; posting; amount; debit; status; refHash = Treasury.hash8(reference); statement8 = 0 }
};
func entry(reference : Text, amount : Nat, credit : Bool, day : Nat) : TT.StatementEntry { { reference; amount; credit; valueDay = day; bookingDay = day; counterparty = "" } };
let legs = [leg(10, 500_00, true, D0, "a", Treasury.LEG_OPEN), leg(11, 500_00, true, D0 + 1, "b", Treasury.LEG_OPEN), leg(12, 700_00, false, D0, "c", Treasury.LEG_OPEN), leg(13, 900_00, true, D0, "d", Treasury.LEG_MATCHED)];
// a statement credit is our debit; by reference first: "b" at 500 on D0 matches leg 11 although leg 10 is closer by date
let m1 = Core.matchNotification([entry("b", 500_00, true, D0), entry("zzz", 500_00, true, D0), entry("d", 900_00, true, D0), entry("c", 700_00, false, D0 + 5)], legs, 2);
check(m1.matches.size() == 2 and m1.matches[0] == (0, 11) and m1.matches[1] == (1, 10), "by reference first, then by amount and date, each leg once: " # debug_show m1.matches);
check(m1.unmatched == [2, 3], "a matched leg is never taken again; a date outside the tolerance does not match: " # debug_show m1.unmatched);
Debug.print("count: notification matching checks = 2");

// ─── the custodian's holdings and postings ───
let mh = Core.matchHoldings([{ isin = "A"; nominal = 100 }, { isin = "B"; nominal = 50 }, { isin = "D"; nominal = 7 }], [("A", 100), ("B", 60), ("C", 30)]);
check(mh.matched == 1 and mh.breaks.size() == 3, "one holding agrees, three differ");
check(mh.breaks[0] == ("B", 60, 50, #inOurBooksOnly) and mh.breaks[1] == ("D", 0, 7, #onStatementOnly) and mh.breaks[2] == ("C", 30, 0, #inOurBooksOnly), "the sides: " # debug_show mh.breaks);
func ins(id : Nat, reference : Text, isin : Text, nominal : Nat, delivered : Bool, settled : Bool, failed : Bool) : Core.DeskInstruction { { id; referenceHash = Treasury.hash8(reference); isin; nominal; delivered; settled; failed; day = D0 } };
let ours = [ins(1, "r1", "A", 100, false, true, false), ins(2, "r2", "A", 200, true, true, false), ins(3, "r3", "B", 300, false, false, true), ins(4, "r4", "B", 400, false, false, false), ins(5, "r5", "A", 500, true, true, false)];
let mt = Core.matchTransactions([{ reference = "r1"; isin = "A"; nominal = 100; delivered = false; effectiveDay = D0 }, { reference = "r2"; isin = "A"; nominal = 250; delivered = true; effectiveDay = D0 }, { reference = "r9"; isin = "B"; nominal = 1; delivered = false; effectiveDay = D0 }], ours);
check(mt.matched == [(0, 1)], "the posting with the settled instruction of the same reference, instrument, quantity and direction matches: " # debug_show mt.matched);
check(mt.explained == [3], "the failed instruction the custodian did not post is explained by the fail, the pending one is nothing: " # debug_show mt.explained);
check(mt.breaks.size() == 4, "the quantity mismatch and the unknown reference are the statement's breaks, the settled instructions not posted are the desk's: " # debug_show mt.breaks);
check(mt.breaks[0] == (#onStatementOnly, "A", 0, 250, "r2", ?2) and mt.breaks[1] == (#onStatementOnly, "B", 0, 1, "r9", null) and mt.breaks[2] == (#inOurBooksOnly, "A", 200, 0, "", ?2) and mt.breaks[3] == (#inOurBooksOnly, "A", 500, 0, "", ?5), "each break names its side, instrument, quantities and instruction");
Debug.print("count: custodian matching checks = 6");

// ─── the policy and the fold ───
let arena = RI.newArena();
let s = Core.newState(arena);
let folded = List.empty<(Nat, RT.Event)>();
var block = 300;
func apply(ev : RT.Event) : Nat { block += 1; Core.fold(s, block, ev); List.add(folded, (block, ev)); block };
var refusals = 0;
if (refused(Core.planPolicy({ cashAccounts = []; breakAgeAlertDays = 3 }), "a policy with no cash account")) refusals += 1;
if (refused(Core.planPolicy({ cashAccounts = [{ currency = "EG"; cash = S.cash }]; breakAgeAlertDays = 3 }), "a currency of two letters")) refusals += 1;
if (refused(Core.planPolicy({ cashAccounts = [{ currency = "EGP"; cash = S.cash }]; breakAgeAlertDays = 0 }), "an age of zero")) refusals += 1;
ignore apply(ok(Core.planPolicy(S.reconciliationPolicy), "policy"));
check(Core.cashAccountOf(S.reconciliationPolicy, "USD") == ?S.nostro and Core.cashAccountOf(S.reconciliationPolicy, "KWD") == null, "the policy's account per currency");
ignore apply(#notificationRecorded({ nostro = "N"; notification = h(5); entries = 2; matched = [(entry("a", 500_00, true, D0), 10)]; unmatched = 1; day = D0 }));
check(Core.notifiedPosting(s, "N", entry("a", 500_00, true, D0)) == ?10 and Core.notifiedPosting(s, "N", entry("a", 500_00, false, D0)) == null and Core.notifiedPosting(s, "M", entry("a", 500_00, true, D0)) == null, "the matched entry is keyed by nostro, reference, amount, side and day");
ignore apply(#depotStatementRecorded({ depot = "D"; statement = h(6); kind = #holdings; statementDate = D0; from = D0; to = D0; reported = 2; matched = 1; explained = 0; breaks = 1; day = D0 }));
let b1 = apply(#depotBreak({ depot = "D"; statement = h(6); kind = #position; side = #onStatementOnly; isin = "A"; ours = 100; theirs = 110; reference = ""; instruction = null; day = D0 }));
let b2 = apply(#depotBreak({ depot = "D"; statement = h(7); kind = #transaction; side = #inOurBooksOnly; isin = "A"; ours = 200; theirs = 0; reference = "r2"; instruction = ?2; day = D0 + 1 }));
ignore apply(#breakExplainedByFail({ depot = "D"; statement = h(7); isin = "B"; reference = "r3"; instruction = 3; nominal = 300; day = D0 + 1 }));
check(Core.status(s).depotBreaks == 2 and Core.status(s).depotBreaksOpen == 2 and Core.status(s).explainedFails == 1 and Core.statement(s, h(6)) != null and Core.statement(s, h(8)) == null, "two breaks open, one fail explained, the statement known");
// ageing: at D0 + 3 the first break is three days old and alerted once
let aged = Core.agedBreaks(s, D0 + 3, 3);
check(aged.size() == 1, "one break past the age: " # debug_show aged);
for (ev in aged.vals()) ignore apply(ev);
check(Core.agedBreaks(s, D0 + 3, 3).size() == 0 and Core.agedBreaks(s, D0 + 4, 3).size() == 1, "alerted once; the second ages the day after");
if (refused(Core.planResolveDepotBreak(s, 999, "x", null, D0 + 3), "an unknown break")) refusals += 1;
if (refused(Core.planResolveDepotBreak(s, b1, "", null, D0 + 3), "a resolution without a reason")) refusals += 1;
ignore apply(ok(Core.planResolveDepotBreak(s, b1, "the custodian corrected its report", ?b2, D0 + 3), "resolve"));
if (refused(Core.planResolveDepotBreak(s, b1, "again", null, D0 + 3), "a break resolved twice")) refusals += 1;
let v1 = Core.depotBreakView(switch (Core.depotBreak(s, b1)) { case (?b) b; case null Runtime.trap("no break") }, D0 + 5);
check(v1.state == "resolved" and v1.resolvedDay == ?(D0 + 3) and v1.correction == ?b2 and v1.ageDays == 5 and Core.status(s).depotBreaksOpen == 1, "the resolution on the row: " # debug_show v1);
// cash: an intent, a reply with a difference, a break; the next agreement clears it
let ledger = Principal.fromText("aaaaa-aa");
ignore apply(#cashReconciliationIntended({ currency = "EGP"; ledger; day = D0 }));
check((switch (Core.cashRow(s, "EGP")) { case (?r) r.inFlight and r.ledger == ?ledger; case null false }), "the intent marks the currency in flight with its ledger");
ignore apply(#cashReconciled({ currency = "EGP"; ledger; ledgerBalance = 1_000_00; bookBalance = 990_00; difference = 10_00; tipHeight = ?77; day = D0 }));
let cb = apply(#cashBreak({ currency = "EGP"; ledger; ledgerBalance = 1_000_00; bookBalance = 990_00; difference = 10_00; day = D0 }));
let cr = switch (Core.cashRow(s, "EGP")) { case (?r) r; case null Runtime.trap("no cash row") };
check(not cr.inFlight and cr.difference == 10_00 and cr.openBreak == cb and cr.tipHeight == ?77 and Core.status(s).cashBreaksOpen == 1, "the reply on the row with the ledger's height and the break open: " # debug_show cr);
check(Core.agedCashBreaks(s, D0 + 3, 3).size() == 1 and Core.agedCashBreaks(s, D0 + 2, 3).size() == 0, "the cash break ages on the third day");
ignore apply(#cashBreakAged({ break_ = cb; ageDays = 3; day = D0 + 3 }));
check(Core.agedCashBreaks(s, D0 + 3, 3).size() == 0, "alerted once");
ignore apply(#cashReconciled({ currency = "EGP"; ledger; ledgerBalance = 1_000_00; bookBalance = 1_000_00; difference = 0; tipHeight = null; day = D0 + 4 }));
ignore apply(#cashBreakCleared({ break_ = cb; day = D0 + 4 }));
check(Core.cashBreakView(switch (Core.cashBreak(s, cb)) { case (?b) b; case null Runtime.trap("no break") }, D0 + 4).state == "cleared" and Core.status(s).cashBreaksOpen == 0 and (switch (Core.cashRow(s, "EGP")) { case (?r) r.openBreak == 0; case null false }), "the agreement clears the break");
if (refused(Core.planResolveCashBreak(s, cb, "x", null, D0 + 4), "a cleared break resolved")) refusals += 1;
ignore apply(#cashReconciliationIntended({ currency = "USD"; ledger; day = D0 + 4 }));
ignore apply(#cashReconciliationFailed({ currency = "USD"; ledger; reason = "unreachable"; day = D0 + 4 }));
check((switch (Core.cashRow(s, "USD")) { case (?r) not r.inFlight; case null false }), "a failed call leaves the currency free for the next");
Debug.print("count: fold checks = 12");
Debug.print("count: refusals = " # Nat.toText(refusals));

// ─── the re-fold ───
let s2 = Core.newState(RI.newArena());
for ((b, e) in List.values(folded)) Core.fold(s2, b, e);
func fp(x : Core.State) : Blob { let w = C.Writer(); Core.fingerprintInto(w, x); w.toBlob() };
check(fp(s) == fp(s2), "the reconciliation fingerprint is reproduced by the re-fold");
Debug.print("count: fingerprint checks = 1");

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("Reconciliation: all checks passed");
