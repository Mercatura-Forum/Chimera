/// Desk.mo: the desk contract, with the journal embedded.
///
///   JCore / JLog        the double-entry journal: postings, balances, periods, proofs: the record of what moved
///   DeskCore / DL       the desk's log on the kernel's `DomainLog`: authority, market data, deals, the end of day:
///                       the record of why it was allowed to move
///   DomainCert          one certified tip carrying both Merkle roots
///
/// Every update method is validate, append, apply, certify, with no `await` on the path, so a call is atomic: it
/// commits the authority block, the postings it caused and both certified roots together, or it changes nothing.
/// That is the reason the desk embeds the journal instead of calling one: a four-eyes approval and the posting it
/// authorises cannot end up on opposite sides of a failed message.
///
/// Genesis: the books, roles, grants and policies arrive as install arguments and are written to the desk log as
/// ordinary blocks in the install message, each validated by the same planner that validates a run-time command.
/// There is no superuser; from block 0 the log is complete and dual control holds without exception.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import CertifiedData "mo:core/CertifiedData";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Nat64 "mo:core/Nat64";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";
import Time "mo:core/Time";
import Timer "mo:core/Timer";

import JT "mo:journal/JournalTypes";
import JCore "mo:journal/JournalCore";
import JLog "mo:journal/JournalLog";
import AT "mo:kernel/auth/AuthTypes";
import MC "mo:kernel/auth/MakerChecker";
import P "mo:kernel/auth/Permissions";
import DL "mo:kernel/domain/DomainLog";
import DomainCert "mo:kernel/domain/DomainCert";
import Batch "mo:kernel/batch/Batch";
import RI "mo:kernel/index/RegionIndex";

import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import TreasuryMessages "mo:manticore/TreasuryMessages";
import TreasuryMath "mo:manticore/TreasuryMath";
import CloseCore "mo:manticore/CloseCore";
import Fx "mo:manticore/Fx";
import AlertCore "mo:manticore/AlertCore";
import AlT "mo:manticore/AlertTypes";

import T "DeskTypes";
import Cat "Catalogue";
import Can "DeskCanonical";
import Auth "Authority";
import Core "DeskCore";
import Fixings "Fixings";
import Eod "EndOfDay";
import CallT "CallTypes";
import CallCore "CallCore";
import CuT "CustodyTypes";
import CustodyCore "CustodyCore";

shared (initMsg) persistent actor class Desk(init : {
  /// Books to open at genesis, parents before children.
  books : [{ id : T.BookId; name : Text; parent : ?T.BookId; sharia : Bool }];
  roles : [{ id : T.RoleId; name : Text; permissions : [T.PermissionId] }];
  /// Without at least one grant the desk has no authority at all and can do nothing, which is the safe failure.
  grants : [{ subject : Principal; role : T.RoleId; scope : T.Scope }];
  policies : [T.DualPolicy];
  /// Applied to every dual permission the explicit list does not cover, so the policy table is complete from block 0.
  defaultDual : ?{ eligibleRole : T.RoleId; required : Nat; ttlSeconds : Nat };
  /// The journal's calendar authority at genesis: on a substrate whose clock is not wall time the business date
  /// comes with the authority and the clock is never the desk's calendar.
  calendar : ?{ authority : JT.CalendarAuthority; maxRollDays : Nat; businessDate : ?JT.Day };
  identity : ?T.Identity;
}) = self {

  // ─── persisted state ──────────────────────────────────────────────────────

  let installer : Principal = initMsg.caller;
  let desk : Core.State = Core.newState(installer);
  let deskLog : DL.State = DL.newState();
  /// The replay's own arena, reused by every replay in steps: its pages are handed out again from the start when a
  /// replay begins, so a verification never grows the contract's memory beyond one replay's worth. The template
  /// arena is never allocated from; it only says what a fresh arena's free list looks like.
  let replayArena : RI.Arena = RI.newArena();
  let arenaTemplate : RI.Arena = RI.newArena();
  transient var replay : ?Core.Replay = null;
  let cert : DomainCert.State = DomainCert.newState();
  let journal : JCore.State = JCore.newState(Principal.fromActor(self));
  let journalLog : JLog.State = JLog.newState();

  let LABEL_DESK = "thebes_desk";
  let LABEL_JOURNAL = "thebes_journal";

  func now() : Nat64 { Nat64.fromNat(Int.abs(Time.now())) };
  func me() : Principal { Principal.fromActor(self) };
  transient let codec = Can.codec();

  func tipOf(label_ : Text, n : Nat, hash : ?Blob, root : ?Blob) : DomainCert.Tip {
    { label_; index = if (n == 0) 0 else n - 1; hash = switch (hash) { case (?h) h; case null Blob.fromArray([]) }; root = switch (root) { case (?r) r; case null Blob.fromArray([]) } }
  };
  /// Re-certify both tips together, so the two roots always describe the same message.
  func recertify() {
    DomainCert.update(cert, [
      tipOf(LABEL_DESK, DL.length(deskLog), DL.tipHash(deskLog), DL.mmrRoot(deskLog)),
      tipOf(LABEL_JOURNAL, JLog.length(journalLog), JLog.tipHash(journalLog), JLog.mmrRoot(journalLog)),
    ], CertifiedData.set);
  };

  func deskBlock(i : Nat) : ?Core.Block { DL.get(deskLog, codec, i) };
  func deskBlocks() : Core.Blocks { { get = deskBlock } };
  func journalBlocks() : JCore.Blocks { { get = func(i : Nat) : ?JT.Block { JLog.get(journalLog, i) } } };

  func commitDesk(caller : Principal, event : T.Event, trailer : ?Blob) : Core.Block {
    let b = DL.append(deskLog, codec, now(), caller, event, trailer);
    Core.apply(desk, b);
    recertify();
    b
  };
  func commitJournal(event : JT.Event) : JT.Block {
    let b = JLog.append(journalLog, now(), me(), event);
    JCore.apply(journal, journalBlocks(), b);
    // the treasury's nostro index: the legs of a posted movement on a registered nostro, written in the same
    // message so a statement can be matched against what the journal committed and nothing less
    switch (b.event) {
      case (#posted(p)) ignore TreasuryCore.indexJournalLegs(desk.treasury, b.index, p.valueDate, p.legs, p.sourceRef.id);
      case (_) {};
    };
    recertify();
    b
  };

  // ─── refusals and the method gate ─────────────────────────────────────────

  func recordRefusal(caller : Principal, permission : T.PermissionId, e : T.Error) {
    if (Auth.recordableRefusal(desk.authority, caller)) ignore commitDesk(caller, Auth.refusalEvent(caller, permission, e, debug_show (e)), null);
  };
  func fail<X>(caller : Principal, permission : T.PermissionId, r : { error : T.Error; record : Bool }) : Result.Result<X, T.Error> {
    if (r.record) recordRefusal(caller, permission, r.error);
    #err(r.error)
  };
  /// Every write method names its permission from the catalogue; the identifier is a required argument.
  func requireMethodPermission(caller : Principal, method : Text) : Result.Result<T.PermissionId, T.Error> {
    let ?perm = Cat.byMethod(method) else Runtime.trap("Desk: method " # method # " has no catalogue entry");
    switch (Auth.authorise(desk.authority, caller, { permission = perm.id; partition = null; totals = [] }, JCore.effectiveToday(journal, now()))) {
      case (#ok(_)) #ok(perm.id);
      case (#err(e)) { recordRefusal(caller, perm.id, e); #err(e) };
    }
  };

  var lastExecutedEvent : ?Nat = null;

  /// Commit a command's effects; called only after the command was planned successfully in this message.
  func executeCommand(authority : Principal, authId : Text, command : T.Command) : [Nat] {
    let plan = switch (Core.planCommand(desk, deskBlocks(), journal, me(), now(), authority, authId, command)) {
      case (#ok(p)) p;
      case (#err(e)) Runtime.trap("Desk: planned command failed at execution: " # debug_show (e));
    };
    lastExecutedEvent := switch (plan.event) { case (?ev) ?commitDesk(authority, ev, null).index; case null null };
    for (ev in plan.extra.vals()) ignore commitDesk(authority, ev, null);
    let indices = List.empty<Nat>();
    for (step in plan.journal.vals()) {
      switch (step) { case (#event(ev)) List.add(indices, commitJournal(ev).index); case (#existing(idx)) List.add(indices, idx) };
    };
    for (ev in Core.consumptionEvents(desk, journal, now(), authority, command).vals()) ignore commitDesk(authority, ev, null);
    List.toArray(indices)
  };

  // ─── genesis ──────────────────────────────────────────────────────────────

  func genesisCommand(command : T.Command) {
    switch (Core.planCommand(desk, deskBlocks(), journal, me(), now(), installer, Nat.toText(desk.height), command)) {
      case (#err(e)) Runtime.trap("Desk genesis: " # Cat.commandName(command) # " rejected: " # debug_show (e));
      case (#ok(plan)) {
        switch (plan.event) { case (?ev) ignore commitDesk(installer, ev, null); case null {} };
        for (ev in plan.extra.vals()) ignore commitDesk(installer, ev, null);
        for (step in plan.journal.vals()) { switch (step) { case (#event(ev)) ignore commitJournal(ev); case (#existing(_)) {} } };
      };
    };
  };

  func genesis() {
    switch (JCore.prepareAddPoster(journal, me(), me())) {
      case (#ok(ev)) ignore commitJournal(ev);
      case (#err(e)) Runtime.trap("Desk genesis: cannot register the desk as a journal poster: " # debug_show (e));
    };
    ignore commitDesk(installer, #deskInstalled({ installer }), null);
    switch (init.calendar) { case (?c) genesisCommand(#journalSetCalendarAuthority({ authority = c.authority; maxRollDays = c.maxRollDays; businessDate = c.businessDate })); case null {} };
    for (b in init.books.vals()) genesisCommand(#openBook({ id = b.id; name = b.name; parent = b.parent; sharia = b.sharia }));
    for (r in init.roles.vals()) genesisCommand(#defineRole({ id = r.id; name = r.name; permissions = r.permissions }));
    for (g in init.grants.vals()) genesisCommand(#grantRole({ subject = g.subject; role = g.role; scope = g.scope }));
    for (p in init.policies.vals()) genesisCommand(#setDualPolicy(p));
    switch (init.defaultDual) {
      case (?d) {
        for (perm in Cat.catalogue().vals()) {
          if (perm.dualByDefault and Auth.policyFor(desk.authority, perm.id) == null) {
            genesisCommand(#setDualPolicy({ permission = perm.id; required = d.required; eligibleRole = d.eligibleRole; ttlSeconds = d.ttlSeconds }));
          };
        };
      };
      case null {};
    };
    switch (init.identity) { case (?i) genesisCommand(#setDeskIdentity(i)); case null {} };
  };

  // ═══════════════════════════════════════════════════════
  //  MAKER-CHECKER
  // ═══════════════════════════════════════════════════════

  public type ProposeResult = { proposal : Nat; commandHash : Blob; required : Nat; expiresAt : Nat64 };

  public shared ({ caller }) func propose(command : T.Command, justification : Text) : async Result.Result<ProposeResult, T.Error> {
    switch (requireMethodPermission(caller, "propose")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.prepareProposal(desk, deskBlocks(), journal, me(), caller, now(), command, justification)) {
      case (#err(r)) fail<ProposeResult>(caller, Auth.commandPermission(command), r);
      case (#ok(out)) {
        let b = commitDesk(caller, out.event, ?out.trailer);
        switch (b.event) {
          case (#commandProposed(x)) #ok({ proposal = b.index; commandHash = x.commandHash; required = x.required; expiresAt = x.expiresAt });
          case (_) Runtime.trap("Desk: proposal block is not a proposal");
        }
      };
    }
  };

  public type ApproveResult = { proposal : Nat; approvals : Nat; required : Nat; executed : Bool; postings : [Nat]; event : ?Nat };

  /// An approval that completes the policy executes the command in this same message, so there is no state in
  /// which a command is approved and unexecuted.
  public shared ({ caller }) func approve(proposal : Nat) : async Result.Result<ApproveResult, T.Error> {
    switch (requireMethodPermission(caller, "approve")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.prepareApprove(desk, deskBlocks(), journal, me(), caller, now(), proposal)) {
      case (#err(r)) {
        let perm = switch (Auth.getProposal(desk.authority, deskBlocks(), proposal)) { case (?v) v.permission; case null "command.approve" };
        fail<ApproveResult>(caller, perm, r)
      };
      case (#ok(out)) {
        ignore commitDesk(caller, out.approval, null);
        switch (out.execute) {
          case null {
            let ?v = Auth.getProposal(desk.authority, deskBlocks(), proposal) else Runtime.trap("Desk: approved proposal vanished");
            #ok({ proposal; approvals = v.approvals.size(); required = v.required; executed = false; postings = []; event = null })
          };
          case (?ex) {
            let postings = executeCommand(ex.maker, Nat.toText(ex.proposal), ex.command);
            ignore commitDesk(caller, #commandExecuted({ proposal = ex.proposal; commandHash = ex.commandHash; effects = postings }), null);
            #ok({ proposal; approvals = ex.proposal; required = 0; executed = true; postings; event = lastExecutedEvent })
          };
        }
      };
    }
  };

  public shared ({ caller }) func reject(proposal : Nat, reason : Text) : async Result.Result<{ block : Nat }, T.Error> {
    switch (requireMethodPermission(caller, "reject")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.prepareReject(desk, deskBlocks(), caller, now(), proposal, reason)) {
      case (#err(r)) fail<{ block : Nat }>(caller, "command.reject", r);
      case (#ok(ev)) #ok({ block = commitDesk(caller, ev, null).index });
    }
  };

  /// Expiry is a fact of the clock: open to any caller, the block attributed to the contract.
  public shared func expireProposals(limit : Nat) : async Nat {
    let expired = Auth.expiredProposals(desk.authority, now(), Nat.min(limit, 100));
    for (idx in expired.vals()) ignore commitDesk(me(), #commandExpired({ proposal = idx }), null);
    expired.size()
  };

  public type PerformResult = { block : Nat; postings : [Nat] };

  public shared ({ caller }) func perform(command : T.Command) : async Result.Result<PerformResult, T.Error> {
    switch (requireMethodPermission(caller, "perform")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.preparePerform(desk, deskBlocks(), journal, me(), caller, now(), command)) {
      case (#err(r)) fail<PerformResult>(caller, Auth.commandPermission(command), r);
      case (#ok(_)) {
        let at = desk.height;
        let postings = executeCommand(caller, Nat.toText(at), command);
        #ok({ block = at; postings })
      };
    }
  };

  public type OverrideResult = { override_ : Nat; postings : [Nat] };

  /// The emergency path: a distinct permission, a witness who could have approved, a review that stays open.
  public shared ({ caller }) func emergencyOverride(command : T.Command, witness : Principal, justification : Text) : async Result.Result<OverrideResult, T.Error> {
    switch (requireMethodPermission(caller, "emergencyOverride")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.prepareOverride(desk, deskBlocks(), journal, me(), caller, now(), command, witness, justification)) {
      case (#err(r)) fail<OverrideResult>(caller, "command.breakGlass", r);
      case (#ok(out)) {
        let b = commitDesk(caller, out.event, ?out.trailer);
        let postings = executeCommand(caller, Nat.toText(b.index), command);
        let hash = switch (b.event) { case (#emergencyOverride(o)) o.commandHash; case (_) Blob.fromArray([]) };
        ignore commitDesk(caller, #commandExecuted({ proposal = b.index; commandHash = hash; effects = postings }), null);
        #ok({ override_ = b.index; postings })
      };
    }
  };

  public shared ({ caller }) func reviewOverride(index : Nat, disposition : Text) : async Result.Result<{ block : Nat }, T.Error> {
    switch (requireMethodPermission(caller, "reviewOverride")) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    switch (Core.prepareReviewOverride(desk, deskBlocks(), caller, index, disposition)) {
      case (#err(e)) #err(e);
      case (#ok(ev)) #ok({ block = commitDesk(caller, ev, null).index });
    }
  };

  // ═══════════════════════════════════════════════════════
  //  THE END OF DAY
  // ═══════════════════════════════════════════════════════

  public type AdvanceResult = { book : T.BookId; businessDate : Nat; cursor : Nat; items : Nat; posted : Nat; examined : Nat; zeroMovement : Nat; failures : Nat; completed : Bool; blocks : [Nat]; postings : [Nat] };

  /// Anyone may advance an open run: the plan was fixed when the run opened and is re-derived and checked here, so
  /// a caller cannot choose what is posted, only that progress happens; the blocks are attributed to the contract.
  public shared func advanceEndOfDay(book : T.BookId, businessDate : Nat, limit : Nat) : async Result.Result<AdvanceResult, T.Error> {
    let recorder : Core.Recorder = { desk = func(ev : T.Event) : Nat { commitDesk(me(), ev, null).index }; journal = func(ev : JT.Event) : Nat { commitJournal(ev).index } };
    switch (Core.runEndOfDayChunk(desk, deskBlocks(), journal, journalBlocks(), me(), now(), book, businessDate, limit, recorder)) {
      case (#err(e)) #err(e);
      case (#ok(a)) {
        let ?run = Eod.getRun(desk.eod, book, businessDate) else Runtime.trap("Desk: the run vanished while it was being advanced");
        let v = Eod.view(run);
        #ok({ book; businessDate; cursor = v.cursor; items = v.items; posted = v.posted; examined = v.examined; zeroMovement = v.zeroMovement; failures = v.failures.size(); completed = a.completed; blocks = a.blocks; postings = a.postings })
      };
    }
  };
  public query func endOfDayRun(book : T.BookId, businessDate : Nat) : async ?T.RunView { switch (Eod.getRun(desk.eod, book, businessDate)) { case (?r) ?Eod.view(r); case null null } };
  public query func listEndOfDayRuns() : async [T.RunView] { Array.map<Eod.Run, T.RunView>(Eod.listRuns(desk.eod), Eod.view) };
  public query func endOfDayPlan(book : T.BookId, shardSize : Nat) : async Result.Result<{ items : [Batch.PlanItem]; hash : Blob }, Text> {
    switch (Batch.plan(Eod.planInput(book, if (shardSize == 0) Batch.DEFAULT_SHARD_SIZE else shardSize))) { case (#ok(items)) #ok({ items; hash = Batch.planHash(items) }); case (#err(f)) #err(Batch.faultText(f)) }
  };
  public query func retryPolicy(book : T.BookId) : async Nat { Eod.retryLimit(desk.eod, book) };

  // ═══════════════════════════════════════════════════════
  //  THE DESK'S OWN READS
  // ═══════════════════════════════════════════════════════

  public type Page<X> = { rows : [X]; withheld : Nat; scope : ?[T.BookId] };
  func readScope(caller : Principal) : ?[T.BookId] { Auth.readableBooks(desk.authority, caller) };
  func page<X>(caller : Principal, all : [X], bookOf : X -> ?T.BookId) : Page<X> {
    let sc = readScope(caller);
    let rows = List.empty<X>();
    var withheld = 0;
    for (x in all.vals()) { let ok = switch (bookOf(x)) { case null true; case (?b) Auth.mayReadBook(sc, b) }; if (ok) List.add(rows, x) else withheld += 1 };
    { rows = List.toArray(rows); withheld; scope = sc }
  };

  public query func deskInstaller() : async Principal { desk.authority.installer };
  public query func deskHeight() : async Nat { desk.height };
  public query func deskStatus() : async T.Status { Auth.status(desk.authority, desk.height) };
  public query func deskIdentity() : async ?T.Identity { desk.authority.identity };
  public shared query ({ caller }) func listBooks() : async Page<T.Book> { page<T.Book>(caller, Auth.listBooks(desk.authority), func(b) { ?b.id }) };
  public query func getBook(id : T.BookId) : async ?T.Book { Auth.getBook(desk.authority, id) };
  public query func listRoles() : async [AT.Role] { Auth.listRoles(desk.authority) };
  public query func listGrants() : async [T.GrantView] { Auth.listGrants(desk.authority) };
  public query func listPolicies() : async [T.DualPolicy] { Auth.listPolicies(desk.authority) };
  public query func listFeatures() : async [(T.FeatureId, Nat64)] { Auth.listFeatures(desk.authority) };
  public query func featureActivation(feature : T.FeatureId) : async Nat64 { Auth.featureActivation(desk.authority, feature) };
  public query func featureActive(feature : T.FeatureId) : async Bool { Auth.featureActive(desk.authority, feature, desk.height) };
  public query func getProposal(index : Nat) : async ?T.ProposalView { Auth.getProposal(desk.authority, deskBlocks(), index) };
  public query func proposalCommand(index : Nat) : async ?T.Command { Auth.bodyOf(deskBlocks(), index) };
  public query func listProposals(cursor : ?Nat, limit : Nat) : async { rows : [T.ProposalView]; cursor : ?Nat; total : Nat } { Auth.listProposals(desk.authority, deskBlocks(), cursor, limit) };
  public query func auditTrail(cursor : ?Nat, limit : Nat) : async { rows : [MC.AuditRow]; cursor : ?Nat; total : Nat } { Auth.auditTrail(desk.authority, deskBlocks(), cursor, limit) };
  public query func listOverrides(cursor : ?Nat, limit : Nat) : async { rows : [T.OverrideView]; cursor : ?Nat; total : Nat } { Auth.listOverrides(desk.authority, deskBlocks(), cursor, limit) };
  public query func listConsumed() : async [T.ConsumedView] { Auth.listConsumed(desk.authority) };
  public query func consumedBy(subject : Principal, currency : Text, day : Nat) : async Nat { Auth.consumedFor(desk.authority, subject, currency, day) };
  /// The catalogue as built, for the audit that holds it against the interface.
  public query func permissions() : async [AT.Permission] { Cat.catalogue() };
  public query func permissionCatalogueReport() : async P.Report { Cat.validate() };
  public query func permissionCounts() : async P.Summary { P.summarise(Cat.catalogue()) };
  public query func openMethods() : async [(Text, Text)] { Cat.openMethods() };
  public query func permissionForCommand(command : T.Command) : async ?AT.Permission { Cat.forCommand(command) };
  public query func commandHashOf(command : T.Command) : async Blob { Can.commandHash(command) };
  public query func commandBytesOf(command : T.Command) : async Blob { Can.commandBytes(command) };

  // the log
  public query func deskBlockCount() : async Nat { DL.length(deskLog) };
  public query func getDeskBlock(index : Nat) : async ?Core.Block { deskBlock(index) };
  public query func getDeskBlocks(start : Nat, length : Nat) : async [Core.Block] { DL.getRange(deskLog, codec, start, Nat.min(length, 1000)) };
  public query func getRawDeskBlock(index : Nat) : async ?Blob { DL.rawBlock(deskLog, index) };
  public query func deskMmrRoot() : async ?Blob { DL.mmrRoot(deskLog) };
  public query func deskProof(index : Nat) : async ?DL.Proof { DL.proof(deskLog, index) };
  public query func verifyDeskChain() : async { checked : Nat; fault : ?Text } { DL.verifyChain(deskLog, codec) };
  public query func tipCertificate() : async ?DomainCert.Certificate { DomainCert.certificate(cert, CertifiedData.getCertificate) };
  /// Every block of a log, paged to exhaustion: a range read is bounded, and a fold that stopped at the bound would
  /// compare a prefix and call it the state.
  func allDeskBlocks() : [Core.Block] {
    let out = List.empty<Core.Block>();
    var start = 0;
    label walk loop {
      let pg = DL.page(deskLog, codec, start, DL.MAX_RANGE);
      for (b in pg.blocks.vals()) List.add(out, b);
      switch (pg.next) { case (?n) start := n; case null break walk };
    };
    List.toArray(out)
  };
  func allJournalBlocks() : [JT.Block] {
    let out = List.empty<JT.Block>();
    var start = 0;
    let n = JLog.length(journalLog);
    while (start < n) {
      let page = JLog.getRange(journalLog, start, 1000);
      if (page.size() == 0) Runtime.trap("Desk: the journal log returned an empty page inside its length");
      for (b in page.vals()) List.add(out, b);
      start += page.size();
    };
    List.toArray(out)
  };
  /// The live fingerprints and the fingerprints of a fresh fold of each log, so "the state is the fold of the log"
  /// is a comparison a battery makes rather than a claim.
  public query func fingerprints() : async { deskLive : Blob; deskReplayed : Blob; journalLive : Blob; journalReplayed : Blob; deskHeight : Nat; journalHeight : Nat } {
    let journalRange = allJournalBlocks();
    let freshDesk = Core.replay(installer, allDeskBlocks(), journalRange);
    let freshJournal = JCore.replay(me(), journalRange);
    { deskLive = Core.fingerprint(desk); deskReplayed = Core.fingerprint(freshDesk); journalLive = JCore.fingerprint(journal); journalReplayed = JCore.fingerprint(freshJournal); deskHeight = desk.height; journalHeight = JCore.height(journal) }
  };

  /// The live fingerprints alone, for a comparison across an upgrade or around a refusal.
  public query func liveFingerprints() : async { desk : Blob; journal : Blob; deskHeight : Nat; journalHeight : Nat } {
    { desk = Core.fingerprint(desk); journal = JCore.fingerprint(journal); deskHeight = desk.height; journalHeight = JCore.height(journal) }
  };
  /// The fold of both logs in steps, for a log longer than one message can fold: `beginReplay` fixes the targets
  /// at the live heights and resets the replay arena; `advanceReplay` applies up to a page of desk blocks and the
  /// journal to each mark; `replaySections` compares the replayed state with the live one section by section
  /// once complete, and says so when the live state has moved past the targets since. Open methods: a caller
  /// chooses nothing and the replay writes nothing but its own scratch state.
  public shared func beginReplay() : async { deskTarget : Nat; journalTarget : Nat } {
    replayArena.pageCount := 0; replayArena.freeHead := arenaTemplate.freeHead; replayArena.freeCount := 0;
    let r = Core.replayBegin(installer, replayArena, desk.height, JLog.length(journalLog));
    replay := ?r;
    { deskTarget = r.deskTarget; journalTarget = r.journalTarget }
  };
  public shared func advanceReplay(blocks : Nat) : async { applied : Nat; next : Nat; deskTarget : Nat; complete : Bool } {
    let ?r = replay else Runtime.trap("Desk: no replay has begun");
    let n = Nat.min(Nat.max(blocks, 1), DL.MAX_RANGE);
    let blocksToApply : [Core.Block] = if (r.next < r.deskTarget) DL.page(deskLog, codec, r.next, Nat.min(n, r.deskTarget - r.next)).blocks else [];
    let applied = Core.replayStep(r, blocksToApply, func(from : Nat, want : Nat) : [JT.Block] { JLog.getRange(journalLog, from, Nat.min(want, 1000)) });
    { applied; next = r.next; deskTarget = r.deskTarget; complete = r.complete }
  };
  public query func replaySections() : async { complete : Bool; current : Bool; deskTarget : Nat; deskHeight : Nat; sections : [(Text, Blob, Blob)] } {
    let ?r = replay else return { complete = false; current = false; deskTarget = 0; deskHeight = desk.height; sections = [] };
    let live = Core.fingerprintSections(desk);
    let replayed = Core.fingerprintSections(r.state);
    { complete = r.complete; current = r.complete and r.deskTarget == desk.height and r.journalTarget == JLog.length(journalLog); deskTarget = r.deskTarget; deskHeight = desk.height;
      sections = Array.tabulate<(Text, Blob, Blob)>(live.size(), func(i) { (live[i].0, live[i].1, replayed[i].1) }) }
  };

  /// The fingerprint by section, live and replayed in one message, naming the sub-state that diverges when one
  /// does; a log longer than one message folds is compared through the replay in steps instead.
  public query func fingerprintSections() : async [(Text, Blob, Blob)] {
    let fresh = Core.replay(installer, allDeskBlocks(), allJournalBlocks());
    let live = Core.fingerprintSections(desk);
    let replayed = Core.fingerprintSections(fresh);
    Array.tabulate<(Text, Blob, Blob)>(live.size(), func(i) { (live[i].0, live[i].1, replayed[i].1) })
  };

  // ═══════════════════════════════════════════════════════
  //  THE JOURNAL'S READS
  // ═══════════════════════════════════════════════════════

  public query func journalBlockCount() : async Nat { JLog.length(journalLog) };
  public query func getJournalBlock(index : Nat) : async ?JT.Block { JLog.get(journalLog, index) };
  public query func getJournalBlocks(start : Nat, length : Nat) : async [JT.Block] { JLog.getRange(journalLog, start, Nat.min(length, 1000)) };
  public query func getRawJournalBlock(index : Nat) : async ?Blob { JLog.rawBlock(journalLog, index) };
  public query func journalMmrRoot() : async ?Blob { JLog.mmrRoot(journalLog) };
  public query func journalProof(index : Nat) : async ?JLog.Proof { JLog.proof(journalLog, index) };
  public query func verifyJournalChain() : async { checked : Nat; fault : ?Text } { JLog.verifyChain(journalLog) };
  public query func listAccounts(cursor : ?JT.AccountCode, limit : Nat) : async JCore.AccountPage { JCore.listAccountsPaged(journal, cursor, limit) };
  public query func getAccount(code : JT.AccountCode) : async ?JT.Account { JCore.getAccount(journal, code) };
  public query func listPeriods() : async [JT.Period] { JCore.listPeriods(journal) };
  public query func listCurrencies() : async [JT.CurrencyInfo] { JCore.listCurrencies(journal) };
  public query func trialBalance(period : JT.PeriodId) : async ?JT.TrialBalance { JCore.trialBalance(journal, period) };
  public query func generalLedger(period : JT.PeriodId, account : ?JT.AccountCode) : async ?JT.GeneralLedger { JCore.generalLedger(journal, journalBlocks(), period, account) };
  public query func balance(account : JT.AccountCode, subledger : ?JT.SubledgerKey, currency : JT.Currency) : async JT.Balance { JCore.balance(journal, account, subledger, currency) };
  public query func subledgerBalances(account : JT.AccountCode) : async [JT.Balance] { JCore.subledgerBalances(journal, account) };
  public query func valueDatedBalance(account : JT.AccountCode, subledger : ?JT.SubledgerKey, currency : JT.Currency, asOf : JT.Day) : async { debits : Nat; credits : Nat } { JCore.valueDatedBalance(journal, account, subledger, currency, asOf) };
  public query func getPosting(index : Nat) : async ?JT.PostingView { JCore.postingView(journal, journalBlocks(), index) };
  public query func periodPostingIndices(period : JT.PeriodId) : async [Nat] { JCore.periodPostingIndices(journal, period) };
  public query func businessDate() : async ?JT.Day { JCore.businessDate(journal) };
  public query func accountingToday() : async JT.Day { JCore.effectiveToday(journal, now()) };
  public query func calendar() : async ?JT.CalendarConfig { JCore.calendar(journal) };
  public query func journalStatus() : async { height : Nat; posted : Nat; pending : Nat; voided : Nat; active : Bool; accounts : Nat; periods : Nat; currencies : Nat; fingerprint : Blob } {
    { height = JCore.height(journal); posted = JCore.postedCount(journal); pending = JCore.pendingCount(journal); voided = JCore.voidedCount(journal); active = JCore.isActive(journal);
      accounts = JCore.listAccounts(journal).size(); periods = JCore.listPeriods(journal).size(); currencies = JCore.listCurrencies(journal).size(); fingerprint = JCore.fingerprint(journal) }
  };

  // ═══════════════════════════════════════════════════════
  //  MARKET DATA
  // ═══════════════════════════════════════════════════════

  public query func functionalCurrency() : async ?JT.Currency { CloseCore.functional(desk.close) };
  public query func listFxPairs() : async [Fx.PositionPair] { CloseCore.listPairs(desk.close) };
  public query func listFxRates() : async [Fx.Rate] { CloseCore.listRates(desk.close) };
  public query func fxRate(currency : Text, day : Nat) : async ?Fx.Rate { CloseCore.rateOn(desk.close, currency, day) };
  public query func rateFixing(index : Text, day : Nat) : async ?Nat { Fixings.fixingOn(desk.fixings, index, day) };

  // ═══════════════════════════════════════════════════════
  //  ALERTS
  // ═══════════════════════════════════════════════════════

  func alertBlocks() : AlertCore.Blocks { Core.alertBlocks(deskBlocks()) };
  public query func getAlert(id : Nat) : async ?AlT.Alert { AlertCore.get(desk.alerts, alertBlocks(), id) };
  public query func listAlerts(cursor : ?Nat, limit : Nat) : async AlertCore.Page { AlertCore.listPaged(desk.alerts, alertBlocks(), cursor, limit) };
  public query func listOpenAlerts(cursor : ?Nat, limit : Nat) : async AlertCore.Page { AlertCore.listOpenPaged(desk.alerts, alertBlocks(), cursor, limit) };
  public query func alertStatus() : async { opened : Nat; cleared : Nat; escalated : Nat; open : Nat } { AlertCore.counts(desk.alerts) };

  // ═══════════════════════════════════════════════════════
  //  TREASURY (Manticore's reads, verbatim in shape)
  // ═══════════════════════════════════════════════════════

  func treasuryView(caller : Principal, id : Nat) : Result.Result<?TT.DealView, T.Error> {
    switch (TreasuryCore.row(desk.treasury, id)) {
      case null #ok(null);
      case (?r) { if (not Auth.mayReadBook(readScope(caller), r.book)) return #err(#OutsideBookScope({ book = r.book })); #ok(?Core.dealView(desk, deskBlocks(), r)) };
    }
  };
  public shared query ({ caller }) func treasuryDeal(id : Nat) : async Result.Result<?TT.DealView, T.Error> { treasuryView(caller, id) };
  public shared query ({ caller }) func treasuryDealTerms(id : Nat) : async Result.Result<?TT.DealKind, T.Error> {
    switch (treasuryView(caller, id)) { case (#err(e)) #err(e); case (#ok(null)) #ok(null); case (#ok(?_)) { switch (TreasuryCore.row(desk.treasury, id)) { case (?r) #ok(Core.treasuryKindOf(deskBlocks(), r)); case null #ok(null) } } }
  };
  public shared query ({ caller }) func treasuryDealsOfBook(book : Text) : async Result.Result<[TT.DealView], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Array.map<TreasuryCore.DealRow, TT.DealView>(TreasuryCore.dealsOfBook(desk.treasury, book), func(r) { Core.dealView(desk, deskBlocks(), r) }))
  };
  public shared query ({ caller }) func treasuryDealsByState(state : TT.DealState, cursor : ?Blob, limit : Nat) : async { entries : [TT.DealView]; cursor : ?Blob } {
    let pg = TreasuryCore.listByState(desk.treasury, state, cursor, limit);
    let out = List.empty<TT.DealView>();
    for (id in pg.ids.vals()) { switch (treasuryView(caller, id)) { case (#ok(?v)) List.add(out, v); case (_) {} } };
    { entries = List.toArray(out); cursor = pg.cursor }
  };
  public shared query ({ caller }) func treasuryDealsOfCounterparty(name : Text, cursor : ?Blob, limit : Nat) : async { entries : [TT.DealView]; cursor : ?Blob } {
    let pg = TreasuryCore.listByCounterparty(desk.treasury, name, cursor, limit);
    let out = List.empty<TT.DealView>();
    for (id in pg.ids.vals()) { switch (treasuryView(caller, id)) { case (#ok(?v)) List.add(out, v); case (_) {} } };
    { entries = List.toArray(out); cursor = pg.cursor }
  };
  public shared query ({ caller }) func treasuryPositions(book : Text) : async Result.Result<[TT.PositionView], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(TreasuryCore.positions(desk.treasury, book, Core.treasuryTerms(deskBlocks())))
  };
  /// The positions of a book across both families: the treasury's aggregation and the call money.
  public shared query ({ caller }) func positions(book : Text) : async Result.Result<[TT.PositionView], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Core.positions(desk, deskBlocks(), book))
  };
  /// A counterparty's exposure in a currency across a book, with the call balances counted.
  public shared query ({ caller }) func counterpartyExposure(book : Text, counterparty : Text, currency : Text) : async Result.Result<Nat, T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Core.counterpartyExposure(desk, book, counterparty, currency))
  };

  // ── call and notice money ──
  func callViewOf(caller : Principal, id : Nat) : Result.Result<?CallT.View, T.Error> {
    switch (CallCore.row(desk.calls, id)) {
      case null #ok(null);
      case (?r) { if (not Auth.mayReadBook(readScope(caller), r.book)) return #err(#OutsideBookScope({ book = r.book })); #ok(?Core.callView(deskBlocks(), r)) };
    }
  };
  public shared query ({ caller }) func call(id : Nat) : async Result.Result<?CallT.View, T.Error> { callViewOf(caller, id) };
  public shared query ({ caller }) func callTerms(id : Nat) : async Result.Result<?CallT.Terms, T.Error> {
    switch (callViewOf(caller, id)) {
      case (#err(e)) #err(e);
      case (#ok(null)) #ok(null);
      case (#ok(?_)) { switch (deskBlock(id)) { case (?b) { switch (b.event) { case (#call(#opened(x))) #ok(?x.terms); case (_) #ok(null) } }; case null #ok(null) } };
    }
  };
  public shared query ({ caller }) func callsOfBook(book : Text) : async Result.Result<[CallT.View], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Array.map<CallCore.Row, CallT.View>(CallCore.callsOfBook(desk.calls, book), func(r) { Core.callView(deskBlocks(), r) }))
  };
  public shared query ({ caller }) func callsOfCounterparty(name : Text, cursor : ?Blob, limit : Nat) : async { entries : [CallT.View]; cursor : ?Blob } {
    let pg = CallCore.listByCounterparty(desk.calls, name, cursor, limit);
    let out = List.empty<CallT.View>();
    for (id in pg.ids.vals()) { switch (callViewOf(caller, id)) { case (#ok(?v)) List.add(out, v); case (_) {} } };
    { entries = List.toArray(out); cursor = pg.cursor }
  };
  /// The interest a call has earned or owed to a day, by the row's base and rate: what the next accrual will post.
  public query func callAccrualTo(id : Nat, day : Nat) : async ?Int { switch (CallCore.row(desk.calls, id)) { case (?r) ?CallCore.accrualTarget(r, day); case null null } };
  public query func callStatus() : async { opened : Nat; open : Nat; interestTotal : Int } { CallCore.status(desk.calls) };

  // ── securities services ──
  public query func custodyStatus() : async { policy : ?CuT.Policy; status : CuT.Status } { { policy = CustodyCore.policy(desk.custody); status = CustodyCore.status(desk.custody) } };
  public query func instrumentExtension(isin : Text) : async ?CustodyCore.InstrumentRow { CustodyCore.instrument(desk.custody, isin) };
  public query func depot(id : Text) : async ?CustodyCore.DepotRow { CustodyCore.depot(desk.custody, id) };
  public query func bookDepot(book : T.BookId) : async ?Text { CustodyCore.bookDepotOf(desk.custody, book) };
  public query func dealDepot(deal : Nat) : async ?Text { CustodyCore.dealDepotOf(desk.custody, deal) };
  /// The settled position of a depot in an instrument: the holdings of every lot in it.
  public query func depotPosition(depotId : Text, isin : Text) : async CuT.PositionView { CustodyCore.position(desk.custody, depotId, isin) };
  public query func depotHoldings(depotId : Text) : async [CuT.HoldingView] {
    CustodyCore.holdingsOf(desk.custody, depotId, func(lot : Nat) : (Text, Text) { switch (TreasuryCore.row(desk.treasury, lot)) { case (?r) (r.isin, r.book); case null ("", "") } })
  };
  /// The trade-date position of a book in an instrument less what has settled into its depots: the purchase lots
  /// captured whose first leg has not settled, and the sales captured whose first leg has not settled.
  public shared query ({ caller }) func pendingSettlement(book : T.BookId, isin : Text) : async Result.Result<{ purchases : Nat; sales : Nat }, T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    var purchases = 0; var sales = 0;
    for (r in TreasuryCore.dealsOfBook(desk.treasury, book).vals()) {
      if (r.kind == 4 and Text.equal(r.isin, isin) and TreasuryCore.isOpen(r) and not TreasuryCore.legSettled(r, 0)) {
        if ((r.flags & TreasuryCore.F_BUY) != 0) purchases += r.notional else sales += r.notional;
      };
    };
    #ok({ purchases; sales })
  };
  public query func corporateAction(id : Nat) : async ?CuT.ActionView { switch (CustodyCore.action(desk.custody, id)) { case (?r) ?CustodyCore.actionView(r); case null null } };
  public query func corporateActionsOf(isin : Text) : async [CuT.ActionView] { Array.map<CustodyCore.ActionRow, CuT.ActionView>(CustodyCore.actionsOf(desk.custody, isin), CustodyCore.actionView) };
  public query func entitlements(action : Nat) : async [CuT.EntitlementView] {
    Array.map<CustodyCore.EntitlementRow, CuT.EntitlementView>(CustodyCore.entitlementsOf(desk.custody, action), func(e) { CustodyCore.entitlementView(desk.custody, e) })
  };
  public shared query ({ caller }) func treasuryLots(book : Text, isin : Text) : async Result.Result<[TT.DealView], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Array.map<TreasuryCore.DealRow, TT.DealView>(TreasuryCore.lotsOf(desk.treasury, book, isin), func(r) { Core.dealView(desk, deskBlocks(), r) }))
  };
  public query func treasurySecurity(isin : Text) : async ?TreasuryCore.SecurityRow { TreasuryCore.security(desk.treasury, isin) };
  public query func treasuryCurve(id : Text, day : Nat) : async ?TreasuryCore.CurveRow { TreasuryCore.curveOn(desk.treasury, id, day) };
  public shared query ({ caller }) func treasuryLimits(book : Text) : async Result.Result<[TT.Limit], T.Error> {
    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(TreasuryCore.limitsOf(desk.treasury, book))
  };
  public query func treasuryNostro(id : Text) : async ?TreasuryCore.NostroRow { TreasuryCore.nostro(desk.treasury, id) };
  public query func nostroPostings(nostro : Text, from : Nat, to : Nat) : async [TT.NostroPostingView] {
    switch (TreasuryCore.nostro(desk.treasury, nostro)) {
      case null [];
      case (?n) Array.map<TreasuryCore.NostroLegRow, TT.NostroPostingView>(TreasuryCore.nostroLegsIn(desk.treasury, n.accountHash, from, to), func(l) { { posting = l.posting; valueDay = l.valueDay; amount = l.amount; debit = l.debit; matched = l.status == TreasuryCore.LEG_MATCHED; statement = null } });
    }
  };
  public query func nostroBreaks(nostro : ?Text, includeResolved : Bool) : async [TT.BreakView] {
    let rows = switch (nostro) { case (?n) TreasuryCore.breaksOfNostro(desk.treasury, n, includeResolved); case null TreasuryCore.openBreaks(desk.treasury) };
    let today = JCore.effectiveToday(journal, now());
    Array.map<TreasuryCore.BreakRow, TT.BreakView>(rows, func(b) { Core.breakView(desk, deskBlocks(), b, today) })
  };
  public query func nostroBreak(id : Nat) : async ?TT.BreakView { switch (TreasuryCore.breakRow(desk.treasury, id)) { case (?b) ?Core.breakView(desk, deskBlocks(), b, JCore.effectiveToday(journal, now())); case null null } };
  public query func treasuryStatus() : async { policy : ?TT.Policy; status : TT.Status } { { policy = TreasuryCore.policy(desk.treasury); status = TreasuryCore.status(desk.treasury) } };
  func minorUnits(ccy : Text) : Nat8 { switch (JCore.currencyMinorUnits(journal, ccy)) { case (?m) m; case null 2 } };
  /// An FX deal's confirmation as fxtr.014.001.04 from its terms; `leg` selects the near or far leg of a swap.
  public shared query ({ caller }) func treasuryConfirmation(id : Nat, leg : Nat) : async Result.Result<?Text, T.Error> {
    switch (treasuryView(caller, id)) {
      case (#err(e)) #err(e);
      case (#ok(null)) #ok(null);
      case (#ok(?v)) {
        let ?r = TreasuryCore.row(desk.treasury, id) else return #ok(null);
        let cp = Core.treasuryCounterpartyOf(deskBlocks(), id);
        let f : ?TT.FxForward = switch (Core.treasuryKindOf(deskBlocks(), r)) { case (?#fxForward(f)) ?f; case (?#fxSwap(x)) (if (leg == 0) ?x.near else ?x.far); case (_) null };
        switch (f) {
          case null #ok(null);
          case (?fwd) {
            let q = if (leg == 0) r.secondAmount else TreasuryMath.quoteAmount(fwd.baseAmount, fwd.rateMicro);
            // the desk's own identity is the trading side; the book's name when none is declared
            let (ownBic, ownName) = switch (desk.authority.identity) {
              case (?i) (i.bic, i.name);
              case null ("", switch (Auth.getBook(desk.authority, r.book)) { case (?b) b.name; case null r.book });
            };
            #ok(?TreasuryMessages.fxtr014Xml(v.reference, r.day, ownBic, ownName, cp, fwd, q, minorUnits(fwd.base), minorUnits(fwd.quote), r.kind == 3))
          };
        }
      };
    }
  };
  /// A security deal's settlement instruction as sese.023.001.09.
  public shared query ({ caller }) func treasurySettlementInstruction(id : Nat, safekeepingAccount : Text) : async Result.Result<?Text, T.Error> {
    switch (treasuryView(caller, id)) {
      case (#err(e)) #err(e);
      case (#ok(null)) #ok(null);
      case (#ok(?v)) {
        let ?r = TreasuryCore.row(desk.treasury, id) else return #ok(null);
        switch (Core.treasuryKindOf(deskBlocks(), r), TreasuryCore.security(desk.treasury, r.isin)) {
          case (?#security(t), ?sec) {
            let terms : TT.SecurityTerms = { isin = sec.isin; issuer = sec.issuer; currency = sec.currency; couponBps = sec.couponBps; couponsPerYear = sec.couponsPerYear; dayCount = TreasuryCore.conventionOf(sec); issue = sec.issue; maturity = sec.maturity };
            let accrued = TreasuryMath.accruedCoupon(t.nominal, sec.couponBps, TreasuryCore.conventionOf(sec), TreasuryCore.couponPeriodsOf(sec, t.nominal), t.settlement);
            #ok(?TreasuryMessages.sese023Xml(v.reference, t, terms, r.secondAmount + accrued, minorUnits(sec.currency), safekeepingAccount, r.day))
          };
          case (_) #ok(null);
        }
      };
    }
  };

  // ═══════════════════════════════════════════════════════
  //  LIFECYCLE
  // ═══════════════════════════════════════════════════════

  // Genesis runs once, in the install message; on an upgrade the logs are already populated.
  if (DL.length(deskLog) == 0) { genesis() };
  // Certified data does not survive an upgrade; the tips do.
  DomainCert.recertify(cert, CertifiedData.set);
  // Proposals past their lifetime are recorded expired, never silently dropped.
  ignore Timer.recurringTimer<system>(#seconds 60, func() : async () {
    for (idx in Auth.expiredProposals(desk.authority, now(), 50).vals()) ignore commitDesk(me(), #commandExpired({ proposal = idx }), null);
  });
};
