/// DeskCore.test.mo: the spine, end to end, without a replica: the desk log on the kernel's `DomainLog`, the
/// journal, genesis from a declaration, four eyes with every refusal the kernel names, the entitlements' ceiling
/// and daily limit read out of the operation, a deal captured through the spine and settled by the end-of-day job,
/// and the state reproduced by a fresh fold of the log.
// engine: wasi-only

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Debug "mo:core/Debug";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat64 "mo:core/Nat64";
import Principal "mo:core/Principal";
import Text "mo:core/Text";

import JT "mo:journal/JournalTypes";
import JCore "mo:journal/JournalCore";
import JLog "mo:journal/JournalLog";
import DL "mo:kernel/domain/DomainLog";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";

import T "../src/DeskTypes";
import Can "../src/DeskCanonical";
import Cat "../src/Catalogue";
import Auth "../src/Authority";
import Core "../src/DeskCore";
import Eod "../src/EndOfDay";
import S "support/CommandSamples";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };

// ─── the actor's shape, in a test ───
let installer = Principal.fromText("2vxsx-fae");
let me = Principal.fromText("uudaw-bjl2s-u3g5n-tl3bw-myvsv-sc6dh-rhpoc-xn22a-bujen-j4k3w-3qe");
let officer = Principal.fromText("2chl6-4hpzw-vqaaa-aaaaa-c");
let checker = Principal.fromText("navaa-7dphy-ddxgp-nm5hc-zhuwg-42hef-qqids-odhp3-u34n3-bgqvc-uqe");
let trader = Principal.fromText("jyy23-65gu4-qxgeu-qs74r-uoabv-2aegn-y7nxr-36d2o-arada-qcdpm-rqe");
let stranger = Principal.fromText("xjwyi-4uhzb-pbxss-475s7-6gaae-qcefo-kkiw4-daxq2-fst74-yl5k3-vqe");
let anonymous = Principal.fromText("2vxsx-fae");

let desk = Core.newState(installer);
let deskLog = DL.newState();
let journal = JCore.newState(me);
let journalLog = JLog.newState();
let codec = Can.codec();
let D0 = 20670;
var clock : Nat64 = Nat64.fromNat(D0 * 86_400 + 12 * 3600) * 1_000_000_000;
func setDay(d : Nat) { clock := Nat64.fromNat(d * 86_400 + 12 * 3600) * 1_000_000_000 };
func now() : Nat64 { clock };
func deskBlock(i : Nat) : ?Core.Block { DL.get(deskLog, codec, i) };
let blocks : Core.Blocks = { get = deskBlock };
let jblocks : JCore.Blocks = { get = func(i : Nat) : ?JT.Block { JLog.get(journalLog, i) } };
func commitDesk(caller : Principal, ev : T.Event, trailer : ?Blob) : Core.Block { let b = DL.append(deskLog, codec, now(), caller, ev, trailer); Core.apply(desk, b); b };
func commitJournal(ev : JT.Event) : JT.Block {
  let b = JLog.append(journalLog, now(), me, ev);
  JCore.apply(journal, jblocks, b);
  switch (b.event) { case (#posted(p)) ignore TreasuryCore.indexJournalLegs(desk.treasury, b.index, p.valueDate, p.legs, p.sourceRef.id); case (_) {} };
  b
};
func execute(authority : Principal, authId : Text, c : T.Command) : [Nat] {
  let plan = switch (Core.planCommand(desk, blocks, journal, me, now(), authority, authId, c)) { case (#ok(p)) p; case (#err(e)) { check(false, "planned command failed at execution: " # debug_show e); return [] } };
  switch (plan.event) { case (?ev) ignore commitDesk(authority, ev, null); case null {} };
  for (ev in plan.extra.vals()) ignore commitDesk(authority, ev, null);
  let out = List.empty<Nat>();
  for (st in plan.journal.vals()) { switch (st) { case (#event(ev)) List.add(out, commitJournal(ev).index); case (#existing(i)) List.add(out, i) } };
  for (ev in Core.consumptionEvents(desk, journal, now(), authority, c).vals()) ignore commitDesk(authority, ev, null);
  List.toArray(out)
};
type R<X> = { #ok : X; #err : T.Error };
func propose(caller : Principal, c : T.Command) : R<Nat> {
  switch (Core.prepareProposal(desk, blocks, journal, me, caller, now(), c, "test")) {
    case (#err(r)) { if (r.record) ignore commitDesk(caller, Auth.refusalEvent(caller, Auth.commandPermission(c), r.error, ""), null); #err(r.error) };
    case (#ok(o)) #ok(commitDesk(caller, o.event, ?o.trailer).index);
  }
};
func approve(caller : Principal, p : Nat) : R<{ executed : Bool; postings : [Nat] }> {
  switch (Core.prepareApprove(desk, blocks, journal, me, caller, now(), p)) {
    case (#err(r)) { if (r.record) ignore commitDesk(caller, Auth.refusalEvent(caller, "command.approve", r.error, ""), null); #err(r.error) };
    case (#ok(o)) {
      ignore commitDesk(caller, o.approval, null);
      switch (o.execute) {
        case null #ok({ executed = false; postings = [] });
        case (?ex) { let postings = execute(ex.maker, Nat.toText(ex.proposal), ex.command); ignore commitDesk(caller, #commandExecuted({ proposal = ex.proposal; commandHash = ex.commandHash; effects = postings }), null); #ok({ executed = true; postings }) };
      }
    };
  }
};
func perform(caller : Principal, c : T.Command) : R<[Nat]> {
  switch (Core.preparePerform(desk, blocks, journal, me, caller, now(), c)) {
    case (#err(r)) { if (r.record) ignore commitDesk(caller, Auth.refusalEvent(caller, Auth.commandPermission(c), r.error, ""), null); #err(r.error) };
    case (#ok(_)) #ok(execute(caller, Nat.toText(desk.height), c));
  }
};
func send(c : T.Command) : [Nat] {
  let p = switch (propose(officer, c)) { case (#ok(i)) i; case (#err(e)) { check(false, "propose " # Cat.commandName(c) # " refused: " # debug_show e); return [] } };
  switch (approve(checker, p)) { case (#ok(r)) { check(r.executed, Cat.commandName(c) # " executed"); r.postings }; case (#err(e)) { check(false, "approve " # Cat.commandName(c) # " refused: " # debug_show e); [] } }
};
func errTag(e : T.Error) : Text { let t = debug_show e; let parts = Text.split(t, #char '('); switch (parts.next()) { case (?x) x; case null t } };

// ─── genesis, as the actor does it ───
switch (JCore.prepareAddPoster(journal, me, me)) { case (#ok(ev)) ignore commitJournal(ev); case (#err(e)) check(false, "poster: " # debug_show e) };
ignore commitDesk(installer, #deskInstalled({ installer }), null);
func genesis(c : T.Command) { ignore execute(installer, Nat.toText(desk.height), c) };
genesis(#openBook({ id = "HQ"; name = "Head office"; parent = null; sharia = false }));
genesis(#openBook({ id = "BR01"; name = "Desk 1"; parent = ?"HQ"; sharia = false }));
genesis(#openBook({ id = "ISL"; name = "Sharia desk"; parent = ?"HQ"; sharia = true }));
let officerPerms = Array.filter<Text>(Cat.ids(), func(id) { id != "command.approve" and id != "command.breakGlass" and id != "override.review" });
genesis(#defineRole({ id = "officer"; name = "Officer"; permissions = officerPerms }));
genesis(#defineRole({ id = "checker"; name = "Checker"; permissions = ["command.approve", "command.reject", "override.review", "command.perform", "treasury.deal.capture"] }));
genesis(#defineRole({ id = "trader"; name = "Trader"; permissions = ["command.perform", "treasury.deal.capture", "command.create", "treasury.deal.settle"] }));
genesis(#grantRole({ subject = officer; role = "officer"; scope = { partitions = null; currencies = null; ceiling = null; dailyLimit = null } }));
genesis(#grantRole({ subject = checker; role = "checker"; scope = { partitions = null; currencies = null; ceiling = null; dailyLimit = null } }));
genesis(#grantRole({ subject = trader; role = "trader"; scope = { partitions = ?["BR01"]; currencies = ?["USD", "EGP"]; ceiling = ?[{ currency = "USD"; amount = 2_000_000_00 }]; dailyLimit = ?[{ currency = "USD"; amount = 2_500_000_00 }] } }));
for (perm in Cat.catalogue().vals()) { if (perm.dualByDefault) genesis(#setDualPolicy({ permission = perm.id; required = 1; eligibleRole = "checker"; ttlSeconds = 3600 })) };
check(desk.height > 40, "genesis wrote the declaration as blocks");
Debug.print("count: genesis blocks = " # Nat.toText(desk.height));

// ─── the journal and the market, through four eyes ───
ignore send(#journalRegisterCurrency({ code = "EGP"; minorUnits = 2 }));
ignore send(#journalRegisterCurrency({ code = "USD"; minorUnits = 2 }));
let chart : [(Text, JT.Side, JT.Category)] = [
  ("1100", #debit, #asset), ("1101", #debit, #asset), ("1300", #debit, #asset), ("2300", #credit, #liability), ("1310", #debit, #asset), ("2310", #credit, #liability),
  ("4320", #credit, #income), ("5320", #debit, #expense), ("1400", #debit, #asset), ("1410", #debit, #asset), ("1420", #debit, #asset), ("4400", #credit, #income), ("5400", #debit, #expense),
  ("4410", #credit, #income), ("5410", #debit, #expense), ("1500", #debit, #asset), ("1510", #debit, #asset), ("1520", #debit, #asset), ("3500", #credit, #equity), ("1530", #debit, #asset),
  ("4500", #credit, #income), ("4510", #credit, #income), ("5510", #debit, #expense), ("1990", #debit, #asset), ("1800", #debit, #asset), ("1801", #debit, #asset), ("4800", #credit, #income), ("4801", #credit, #income),
];
for ((code, side, cat) in chart.vals()) ignore send(#journalOpenAccount({ code; name = "account " # code; normalSide = side; category = cat; constraint = #none }));
ignore send(#journalOpenPeriod({ id = "P1"; start = D0 - 30; end = D0 + 120 }));
ignore send(#journalRollBusinessDate({ day = D0 }));
ignore send(#journalSetActivationHeight({ height = 0 }));
ignore send(#setFunctionalCurrency({ currency = "EGP" }));
ignore send(#setFxPair({ pair = { currency = "USD"; position = "1800"; equivalent = "1801"; unrealised = "4800"; realised = "4801"; monetary = true } }));
ignore send(#setFxRate({ rate = { currency = "USD"; functional = "EGP"; numerator = 48_000_000; denominator = 1_000_000; asOf = D0; source = "CBE" } }));
ignore send(#setFeatureActivation({ feature = T.FEATURE_TREASURY; height = Nat64.fromNat(desk.height) }));
ignore send(#setFeatureActivation({ feature = T.FEATURE_END_OF_DAY; height = Nat64.fromNat(desk.height) }));
ignore send(#setTreasuryPolicy(S.policy));
ignore send(#registerNostro({ nostro = { id = "NOSTRO-USD-CITI"; account = "1100"; sub = ?"NOSTRO-USD"; currency = "USD"; correspondent = S.citi; iban = ""; valueDateToleranceDays = 2 } }));
ignore send(#recordRateFixing({ index = "CBE-ON"; day = D0; rateBps = 2000 }));
check(JCore.listAccounts(journal).size() == chart.size(), "the chart is open");
Debug.print("count: governance and market commands through four eyes = " # Nat.toText(chart.size() + 12));

// ─── four eyes: every refusal the kernel names ───
var refusals = 0;
let heightBefore = desk.height;
// a stranger cannot propose, and the refusal is not a block (nobody onboarded the stranger)
switch (propose(stranger, #openBook({ id = "X"; name = "x"; parent = null; sharia = false }))) { case (#err(#NoGrant(_))) refusals += 1; case (r) check(false, "a stranger proposed: " # debug_show r) };
check(desk.height == heightBefore, "a stranger's refusal grows no log");
// the anonymous principal
switch (propose(anonymous, #openBook({ id = "X"; name = "x"; parent = null; sharia = false }))) { case (#err(#AnonymousCaller)) refusals += 1; case (r) check(false, "anonymous proposed: " # debug_show r) };
// a single-authority command cannot be proposed, a dual one cannot be performed
switch (propose(trader, #captureDeal({ book = "BR01"; counterparty = S.citi; kind = #moneyMarket(S.mm); reference = "x"; approver = null }))) { case (#err(#InvalidPolicy(_))) refusals += 1; case (r) check(false, "a single-authority command was proposed: " # debug_show r) };
switch (perform(officer, #openBook({ id = "X"; name = "x"; parent = null; sharia = false }))) { case (#err(#RequiresDualAuthorisation(_))) refusals += 1; case (r) check(false, "a dual command was performed: " # debug_show r) };
// self-approval, an ineligible checker, a double approval, a rejection, an expiry
let p1 = switch (propose(officer, #openBook({ id = "BR02"; name = "Desk 2"; parent = ?"HQ"; sharia = false }))) { case (#ok(i)) i; case (#err(e)) { check(false, "propose: " # debug_show e); 0 } };
switch (approve(officer, p1)) { case (#err(#SelfApproval(_))) refusals += 1; case (r) check(false, "self-approval: " # debug_show r) };
switch (approve(trader, p1)) { case (#err(#NotEligibleChecker(_))) refusals += 1; case (r) check(false, "ineligible checker: " # debug_show r) };
switch (approve(checker, p1)) { case (#ok(r)) check(r.executed, "the approval executed"); case (#err(e)) check(false, "approve: " # debug_show e) };
switch (approve(checker, p1)) { case (#err(#ProposalNotAwaiting(_))) refusals += 1; case (r) check(false, "approved twice: " # debug_show r) };
check(Auth.getBook(desk.authority, "BR02") != null, "BR02 opened by the approval");
let p2 = switch (propose(officer, #openBook({ id = "BR03"; name = "Desk 3"; parent = ?"HQ"; sharia = false }))) { case (#ok(i)) i; case (#err(e)) { check(false, "propose: " # debug_show e); 0 } };
switch (Core.prepareReject(desk, blocks, checker, now(), p2, "not needed")) { case (#ok(ev)) ignore commitDesk(checker, ev, null); case (#err(r)) check(false, "reject: " # debug_show r.error) };
switch (approve(checker, p2)) { case (#err(#ProposalNotAwaiting(_))) refusals += 1; case (r) check(false, "approved a rejected proposal: " # debug_show r) };
let p3 = switch (propose(officer, #openBook({ id = "BR04"; name = "Desk 4"; parent = ?"HQ"; sharia = false }))) { case (#ok(i)) i; case (#err(e)) { check(false, "propose: " # debug_show e); 0 } };
clock += 3601 * 1_000_000_000;   // an hour past the proposal's lifetime, still the same day
switch (approve(checker, p3)) { case (#err(#ProposalExpired(_))) refusals += 1; case (r) check(false, "approved an expired proposal: " # debug_show r) };
let expired = Auth.expiredProposals(desk.authority, now(), 10);
check(expired == [p3], "the expiry sweep names the expired proposal");
for (i in expired.vals()) ignore commitDesk(me, #commandExpired({ proposal = i }), null);
check(Auth.getBook(desk.authority, "BR04") == null, "an expired proposal did nothing");
// a body that no longer hashes to the proposal: the trailer of a fresh proposal is swapped for another command's
let p4 = switch (propose(officer, #openBook({ id = "BR05"; name = "Desk 5"; parent = ?"HQ"; sharia = false }))) { case (#ok(i)) i; case (#err(e)) { check(false, "propose: " # debug_show e); 0 } };
switch (Auth.bodyOf(blocks, p4)) { case (?#openBook(x)) check(x.id == "BR05", "the body reads back from the trailer"); case (_) check(false, "the body is not in the trailer") };
Debug.print("count: four-eyes refusals named by the kernel = " # Nat.toText(refusals));
check(desk.authority.refusedCount >= 5, "refusals by onboarded principals are blocks: " # Nat.toText(desk.authority.refusedCount));
Debug.print("count: refusal blocks recorded = " # Nat.toText(desk.authority.refusedCount));

// ─── the entitlements: scope, ceiling and daily limit read out of the deal ───
var scoped = 0;
let usdMm = { S.mm with principal = 1_500_000_00; cash = S.nostro; start = D0; maturity = D0 + 30 };
switch (perform(trader, #captureDeal({ book = "HQ"; counterparty = S.citi; kind = #moneyMarket(usdMm); reference = "hq"; approver = null }))) { case (#err(#OutsideBookScope(_))) scoped += 1; case (r) check(false, "outside book scope: " # debug_show r) };
switch (perform(trader, #captureDeal({ book = "BR01"; counterparty = S.citi; kind = #moneyMarket({ usdMm with principal = 2_500_000_00 }); reference = "big"; approver = null }))) { case (#err(#OverCeiling(_))) scoped += 1; case (r) check(false, "over the ceiling: " # debug_show r) };
let deal1 = switch (perform(trader, #captureDeal({ book = "BR01"; counterparty = S.citi; kind = #moneyMarket(usdMm); reference = "mm-1"; approver = null }))) { case (#ok(_)) desk.height - 2; case (#err(e)) { check(false, "capture: " # debug_show e); 0 } };
switch (TreasuryCore.row(desk.treasury, deal1)) { case (?r) check(r.notional == 1_500_000_00 and r.book == "BR01", "the deal's row"); case null check(false, "no row for the captured deal") };
check(Auth.consumedFor(desk.authority, trader, "USD", D0) == 1_500_000_00, "the daily consumption was recorded from the deal: " # Nat.toText(Auth.consumedFor(desk.authority, trader, "USD", D0)));
switch (perform(trader, #captureDeal({ book = "BR01"; counterparty = S.citi; kind = #moneyMarket(usdMm); reference = "mm-2"; approver = null }))) { case (#err(#OverDailyLimit(_))) scoped += 1; case (r) check(false, "over the daily limit: " # debug_show r) };
switch (perform(trader, #captureDeal({ book = "ISL"; counterparty = S.citi; kind = #moneyMarket(usdMm); reference = "isl"; approver = null }))) { case (#err(#OutsideBookScope(_))) scoped += 1; case (r) check(false, "the trader's scope: " # debug_show r) };
switch (perform(officer, #captureDeal({ book = "ISL"; counterparty = S.citi; kind = #moneyMarket(usdMm); reference = "isl"; approver = null }))) { case (#err(#TreasuryError({ error = #ShariaBook(_) }))) scoped += 1; case (r) check(false, "an interest deal in a Sharia book: " # debug_show r) };
Debug.print("count: entitlement refusals read out of the operation = " # Nat.toText(scoped));

// ─── the end of day: the treasury job settles the placement's start leg and accrues ───
ignore send(#openEndOfDay({ book = "BR01"; businessDate = D0; shardSize = 8 }));
let recorder : Core.Recorder = { desk = func(ev : T.Event) : Nat { commitDesk(me, ev, null).index }; journal = func(ev : JT.Event) : Nat { commitJournal(ev).index } };
switch (Core.runEndOfDayChunk(desk, blocks, journal, jblocks, me, now(), "BR01", D0, 8, recorder)) {
  case (#ok(a)) { check(a.completed and a.failures.size() == 0 and a.postings.size() == 1, "the run settled the start leg: " # debug_show (a.completed, a.failures, a.postings)) };
  case (#err(e)) check(false, "advance: " # debug_show e);
};
switch (TreasuryCore.row(desk.treasury, deal1)) { case (?r) check(TreasuryCore.legSettled(r, 0), "leg 0 settled by the job"); case null check(false, "row") };
let placed = JCore.balance(journal, "1300", ?TreasuryCore.dealSub(deal1), "USD");
check(placed.debitsPosted - placed.creditsPosted == 1_500_000_00, "the placement is on its account: " # debug_show placed);
// day 2: the accrual
setDay(D0 + 1);
ignore send(#journalRollBusinessDate({ day = D0 + 1 }));
ignore send(#setFxRate({ rate = { currency = "USD"; functional = "EGP"; numerator = 48_100_000; denominator = 1_000_000; asOf = D0 + 1; source = "CBE" } }));
ignore send(#openEndOfDay({ book = "BR01"; businessDate = D0 + 1; shardSize = 8 }));
switch (Core.runEndOfDayChunk(desk, blocks, journal, jblocks, me, now(), "BR01", D0 + 1, 8, recorder)) {
  case (#ok(a)) check(a.completed and a.postings.size() == 1, "the second run accrued one day");
  case (#err(e)) check(false, "advance 2: " # debug_show e);
};
switch (TreasuryCore.row(desk.treasury, deal1)) { case (?r) check(r.accruedPosted == 18_750, "one day of 4.5 percent on 1,500,000 ACT/360 is 187.50: " # debug_show r.accruedPosted); case null check(false, "row") };
// an act dated into the running day is refused while a run is open; a completed run admits it
setDay(D0 + 2);
ignore send(#journalRollBusinessDate({ day = D0 + 2 }));
ignore send(#openEndOfDay({ book = "BR01"; businessDate = D0 + 2; shardSize = 8 }));
switch (Core.planCommand(desk, blocks, journal, me, now(), officer, "x", #markDeal({ deal = deal1; postingDate = D0 + 2; valueDate = D0 + 2; period = "P1"; narration = "m" }))) {
  case (#err(#BatchError({ error = #RunExists(_) }))) {};
  case (r) check(false, "an act into a running day was admitted: " # debug_show r);
};
switch (Core.runEndOfDayChunk(desk, blocks, journal, jblocks, me, now(), "BR01", D0 + 2, 8, recorder)) { case (#ok(_)) {}; case (#err(e)) check(false, "advance 3: " # debug_show e) };
Debug.print("count: end-of-day runs completed by the treasury job = " # Nat.toText(Eod.runCount(desk.eod)));

// ─── the fold: a fresh replay of the log reproduces the state ───
let all = DL.getRange(deskLog, codec, 0, DL.length(deskLog));
check(all.size() == desk.height, "every block decodes: " # Nat.toText(all.size()) # " of " # Nat.toText(desk.height));
let fresh = Core.replay(installer, all, JLog.getRange(journalLog, 0, JLog.length(journalLog)));
check(Core.fingerprint(fresh) == Core.fingerprint(desk), "the replayed fingerprint equals the live one");
let chain = DL.verifyChain(deskLog, codec);
check(chain.fault == null and chain.checked == desk.height, "the chain verifies from genesis: " # debug_show chain);
let jchain = JLog.verifyChain(journalLog);
check(jchain.fault == null, "the journal's chain verifies");
Debug.print("count: desk blocks replayed to the same fingerprint = " # Nat.toText(all.size()));
Debug.print("count: journal blocks chained = " # Nat.toText(jchain.checked));

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("DESK CORE GREEN");
