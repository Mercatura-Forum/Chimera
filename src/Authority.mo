/// Authority.mo: who may do what on the desk, as the fold of the desk log.
///
/// Books, roles, grants, dual-authorisation policies, feature activation heights, the desk's identity, one row
/// per proposal and per override in stable memory, and the consumed-today figures the daily limits are measured
/// against. No money: a balance is a journal balance. The shape is Manticore's authority layer over the kernel's
/// catalogue, four-eyes rows and scope evaluator.
///
/// Two rules, stated here because they are decisions. Fail closed on dual control: a permission the catalogue
/// marks dual with no policy recorded cannot be performed and cannot be approved. What a refusal records: a refusal
/// to exceed authority is a block, because a desk proves what it prevented; a refusal for malformed input is a
/// typed error and records nothing; only a principal the desk has onboarded can cause a refusal block.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import List "mo:core/List";
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Nat64 "mo:core/Nat64";
import Order "mo:core/Order";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import JC "mo:journal/Canonical";
import AT "mo:kernel/auth/AuthTypes";
import MC "mo:kernel/auth/MakerChecker";
import E "mo:kernel/auth/Entitlements";
import Cmd "mo:kernel/domain/Command";
import DL "mo:kernel/domain/DomainLog";
import RI "mo:kernel/index/RegionIndex";

import T "DeskTypes";
import Cat "Catalogue";
import Can "DeskCanonical";

module {

  public type Block = DL.Block<T.Event>;
  public type Blocks = { get : Nat -> ?Block };

  public type BookEntry = { id : T.BookId; name : Text; parent : ?T.BookId; sharia : Bool; var open : Bool; openedAtBlock : Nat; var closedAtBlock : ?Nat };
  public type GrantEntry = { subject : Principal; role : T.RoleId; scope : T.Scope; grantedAtBlock : Nat };

  public type State = {
    var installer : Principal;
    var identity : ?T.Identity;
    books : Map.Map<T.BookId, BookEntry>;
    roles : Map.Map<T.RoleId, AT.Role>;
    grants : Map.Map<(Principal, T.RoleId), GrantEntry>;
    policies : Map.Map<T.PermissionId, T.DualPolicy>;
    features : Map.Map<T.FeatureId, Nat64>;
    proposalRows : RI.State;
    openProposals : Map.Map<Nat, ()>;
    overrideRows : RI.State;
    openOverrides : Map.Map<Nat, ()>;
    consumed : Map.Map<(Principal, Text, Nat), Nat>;
    var refusedCount : Nat;
    var executedCount : Nat;
  };

  func cmpPR(a : (Principal, Text), b : (Principal, Text)) : Order.Order {
    switch (Principal.compare(a.0, b.0)) { case (#equal) Text.compare(a.1, b.1); case (o) o }
  };
  func cmpPCD(a : (Principal, Text, Nat), b : (Principal, Text, Nat)) : Order.Order {
    switch (Principal.compare(a.0, b.0)) { case (#equal) { switch (Text.compare(a.1, b.1)) { case (#equal) Nat.compare(a.2, b.2); case (o) o } }; case (o) o }
  };

  public func newState(arena : RI.Arena, installer : Principal) : State {
    {
      var installer; var identity = null;
      books = Map.empty<T.BookId, BookEntry>(); roles = Map.empty<T.RoleId, AT.Role>(); grants = Map.empty<(Principal, T.RoleId), GrantEntry>();
      policies = Map.empty<T.PermissionId, T.DualPolicy>(); features = Map.empty<T.FeatureId, Nat64>();
      proposalRows = RI.newStateIn(arena, { keyBytes = 8; valBytes = MC.PROPOSAL_ROW_BYTES }); openProposals = Map.empty<Nat, ()>();
      overrideRows = RI.newStateIn(arena, { keyBytes = 8; valBytes = MC.OVERRIDE_ROW_BYTES }); openOverrides = Map.empty<Nat, ()>();
      consumed = Map.empty<(Principal, Text, Nat), Nat>();
      var refusedCount = 0; var executedCount = 0;
    }
  };

  func rowKey(index : Nat) : Blob { RI.key([RI.beBytes(index, 8)], 8) };
  func rowIndex(k : Blob) : Nat { var v = 0; for (b in Blob.toArray(k).vals()) { v := v * 256 + Nat8.toNat(b) }; v };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func getBook(s : State, id : T.BookId) : ?T.Book {
    switch (Map.get(s.books, Text.compare, id)) {
      case (?b) ?{ id = b.id; name = b.name; parent = b.parent; sharia = b.sharia; open = b.open; openedAtBlock = b.openedAtBlock; closedAtBlock = b.closedAtBlock };
      case null null;
    }
  };
  public func listBooks(s : State) : [T.Book] { Array.filterMap<(Text, BookEntry), T.Book>(Map.toArray(s.books), func((id, _)) { getBook(s, id) }) };
  public func isShariaBook(s : State, id : T.BookId) : Bool { switch (Map.get(s.books, Text.compare, id)) { case (?b) b.sharia; case null false } };
  public func requireOpenBook(s : State, book : T.BookId) : ?T.Error {
    switch (Map.get(s.books, Text.compare, book)) { case null ?#UnknownBook({ book }); case (?b) { if (not b.open) ?#BookClosed({ book }) else null } }
  };
  public func getRole(s : State, id : T.RoleId) : ?AT.Role { Map.get(s.roles, Text.compare, id) };
  public func listRoles(s : State) : [AT.Role] { Array.map<(Text, AT.Role), AT.Role>(Map.toArray(s.roles), func((_, r)) { r }) };
  public func holdsRole(s : State, subject : Principal, role : T.RoleId) : Bool { Map.containsKey(s.grants, cmpPR, (subject, role)) };
  public func roleHolderCount(s : State, role : T.RoleId) : Nat { var n = 0; for (((_, r), _) in Map.entries(s.grants)) { if (Text.equal(r, role)) n += 1 }; n };
  public func listGrants(s : State) : [T.GrantView] {
    Array.map<((Principal, Text), GrantEntry), T.GrantView>(Map.toArray(s.grants), func((_, g)) {
      { subject = g.subject; role = g.role; scope = g.scope; permissions = switch (Map.get(s.roles, Text.compare, g.role)) { case (?r) r.permissions; case null [] } }
    })
  };
  public func listPolicies(s : State) : [T.DualPolicy] { Array.map<(Text, T.DualPolicy), T.DualPolicy>(Map.toArray(s.policies), func((_, p)) { p }) };
  public func policyFor(s : State, permission : T.PermissionId) : ?T.DualPolicy { Map.get(s.policies, Text.compare, permission) };
  public func listFeatures(s : State) : [(T.FeatureId, Nat64)] { Map.toArray(s.features) };
  public func featureActivation(s : State, f : T.FeatureId) : Nat64 { switch (Map.get(s.features, Text.compare, f)) { case (?h) h; case null T.ACTIVATION_OFF } };
  public func featureActive(s : State, f : T.FeatureId, height : Nat) : Bool { featureActivation(s, f) <= Nat64.fromNat(height) };
  public func requireFeature(s : State, f : T.FeatureId, height : Nat) : ?T.Error {
    if (featureActive(s, f, height)) null else ?#FeatureInactive({ feature = f; activationHeight = featureActivation(s, f); height = Nat64.fromNat(height) })
  };
  public func consumedFor(s : State, subject : Principal, currency : Text, day : Nat) : Nat {
    switch (Map.get(s.consumed, cmpPCD, (subject, currency, day))) { case (?n) n; case null 0 }
  };
  public func listConsumed(s : State) : [T.ConsumedView] {
    Array.map<((Principal, Text, Nat), Nat), T.ConsumedView>(Map.toArray(s.consumed), func(((subject, currency, day), amount)) { { subject; currency; day; amount } })
  };

  /// The subject's grants, resolved to their roles' permissions, in a deterministic order.
  public func grantsOf(s : State, subject : Principal) : [E.ResolvedGrant] {
    let out = List.empty<E.ResolvedGrant>();
    for (((p, role), g) in Map.entries(s.grants)) {
      if (Principal.equal(p, subject)) {
        List.add(out, { role; permissions = switch (Map.get(s.roles, Text.compare, role)) { case (?r) r.permissions; case null [] }; scope = g.scope });
      };
    };
    E.sortGrants(List.toArray(out))
  };

  /// The books a subject may read, or null for unrestricted; a subject with no grant reads nothing.
  public func readableBooks(s : State, subject : Principal) : ?[T.BookId] {
    let gs = grantsOf(s, subject);
    if (gs.size() == 0) return ?[];
    let out = List.empty<T.BookId>();
    for (g in gs.vals()) {
      switch (g.scope.partitions) {
        case null return null;
        case (?books) { for (b in books.vals()) { var seen = false; for (x in List.values(out)) { if (Text.equal(x, b)) seen := true }; if (not seen) List.add(out, b) } };
      };
    };
    ?List.toArray(out)
  };
  public func mayReadBook(scope : ?[T.BookId], book : T.BookId) : Bool {
    switch (scope) { case null true; case (?books) { for (b in books.vals()) { if (Text.equal(b, book)) return true }; false } }
  };

  // ─── authorisation ─────────────────────────────────────────────────────────

  public func authorise(s : State, subject : Principal, op : E.Operation, day : Nat) : Result.Result<{ role : T.RoleId }, T.Error> {
    if (Principal.isAnonymous(subject)) return #err(#AnonymousCaller);
    switch (E.evaluate(grantsOf(s, subject), op, func(ccy) { consumedFor(s, subject, ccy, day) })) {
      case (#allow(x)) #ok(x);
      case (#deny(e)) #err(T.ofAuth(e));
    }
  };
  /// Only a principal the desk has onboarded can grow the log with a refusal.
  public func recordableRefusal(s : State, subject : Principal) : Bool { not Principal.isAnonymous(subject) and grantsOf(s, subject).size() > 0 };
  public func refusalEvent(subject : Principal, permission : T.PermissionId, e : T.Error, detail : Text) : T.Event {
    #operationRefused({ subject; permission; reason = T.refusalReason(e); detail })
  };
  public func commandPermissionRecord(c : T.Command) : AT.Permission {
    switch (Cat.forCommand(c)) { case (?p) p; case null Runtime.trap("Authority: no permission for command " # Cat.commandName(c)) }
  };
  public func commandPermission(c : T.Command) : T.PermissionId { commandPermissionRecord(c).id };
  public func requireUsable(s : State, perm : AT.Permission) : ?T.Error {
    switch (MC.resolvePolicy(perm, policyFor(s, perm.id))) { case (#refuse(e)) ?T.ofAuth(e); case (_) null }
  };

  // ─── the rows and the entries they point at ────────────────────────────────

  public func proposalRow(s : State, index : Nat) : ?MC.ProposalRow { switch (RI.get(s.proposalRows, rowKey(index))) { case (?v) ?MC.decodeProposalRow(v); case null null } };
  public func overrideRow(s : State, index : Nat) : ?MC.OverrideRow { switch (RI.get(s.overrideRows, rowKey(index))) { case (?v) ?MC.decodeOverrideRow(v); case null null } };
  func blockAt(bb : Blocks, index : Nat, what : Text) : Block {
    let ?b = bb.get(index) else Runtime.trap("Authority: the log has no block " # Nat.toText(index) # " for " # what);
    b
  };

  /// A proposal, rebuilt from its row and its blocks.
  public func proposalEntry(s : State, bb : Blocks, index : Nat) : ?MC.Entry {
    let ?row = proposalRow(s, index) else return null;
    let proposed = blockAt(bb, index, "a proposal");
    let #commandProposed(x) = proposed.event else Runtime.trap("Authority: block " # Nat.toText(index) # " has a proposal row but is not a proposal");
    let approvals = Array.map<Nat, Principal>(row.approvalBlocks, func(i) {
      let #commandApproved(a) = blockAt(bb, i, "an approval").event else Runtime.trap("Authority: block " # Nat.toText(i) # " is named as an approval but is not one");
      a.checker
    });
    let status : MC.Status = switch (row.status) {
      case (#awaiting) #awaiting;
      case (#executed(at)) {
        let #commandExecuted(e) = blockAt(bb, at, "an execution").event else Runtime.trap("Authority: block " # Nat.toText(at) # " is named as an execution but is not one");
        #executed({ at; effects = e.effects })
      };
      case (#rejected(at)) {
        let #commandRejected(r) = blockAt(bb, at, "a rejection").event else Runtime.trap("Authority: block " # Nat.toText(at) # " is named as a rejection but is not one");
        #rejected({ by = r.checker; reason = r.reason })
      };
      case (#expired(_)) #expired;
    };
    ?{
      index; commandHash = x.commandHash; commandEncoding = x.commandEncoding; permission = x.permission; partition = x.partition; maker = x.maker;
      required = x.required; eligibleRole = x.eligibleRole; expiresAt = x.expiresAt; justification = x.justification; approvals; status;
    }
  };
  /// The body a proposal or an override carries in its trailer, required to hash to what the block recorded.
  public func bodyOf(bb : Blocks, index : Nat) : ?T.Command {
    let ?b = bb.get(index) else return null;
    switch (b.event) {
      case (#commandProposed(p)) Cmd.bodyOf(Can.registry(), p, b.trailer);
      case (#emergencyOverride(o)) {
        let p : Cmd.Proposed = { permission = ""; partition = null; maker = o.actor_; required = 0; eligibleRole = ""; expiresAt = 0; justification = o.justification; commandHash = o.commandHash; commandEncoding = o.commandEncoding };
        Cmd.bodyOf(Can.registry(), p, b.trailer)
      };
      case (_) null;
    }
  };
  public func proposalView(e : MC.Entry) : T.ProposalView {
    {
      index = e.index; commandHash = e.commandHash; commandEncoding = e.commandEncoding; permission = e.permission; book = e.partition; maker = e.maker;
      required = e.required; eligibleRole = e.eligibleRole; expiresAt = e.expiresAt; justification = e.justification; approvals = e.approvals; status = MC.resultText(e);
      executedAt = switch (e.status) { case (#executed(x)) ?x.at; case (_) null }; effects = switch (e.status) { case (#executed(x)) x.effects; case (_) [] };
    }
  };
  public func getProposal(s : State, bb : Blocks, index : Nat) : ?T.ProposalView { switch (proposalEntry(s, bb, index)) { case (?e) ?proposalView(e); case null null } };
  public func overrideView(s : State, bb : Blocks, index : Nat) : ?T.OverrideView {
    let ?row = overrideRow(s, index) else return null;
    let #emergencyOverride(x) = blockAt(bb, index, "an override").event else Runtime.trap("Authority: block " # Nat.toText(index) # " has an override row but is not an override");
    let (reviewedBy, disposition) : (?Principal, ?Text) = if (row.reviewedAt == 0) (null, null) else {
      let #overrideReviewed(r) = blockAt(bb, row.reviewedAt, "a review").event else Runtime.trap("Authority: block " # Nat.toText(row.reviewedAt) # " is named as a review but is not one");
      (?r.reviewer, ?r.disposition)
    };
    ?{ index; actor_ = x.actor_; witness = x.witness; justification = x.justification; commandHash = x.commandHash; reviewedBy; disposition }
  };
  public let MAX_PAGE : Nat = 500;
  func rowIndices(idx : RI.State, cursor : ?Nat, limit : Nat) : { indices : [Nat]; next : ?Nat } {
    let (lo, hi) = RI.rangeEnds([], 8);
    let page = RI.range(idx, lo, hi, switch (cursor) { case (?c) ?rowKey(c); case null null }, if (limit == 0 or limit > MAX_PAGE) MAX_PAGE else limit);
    { indices = Array.map<(Blob, Blob), Nat>(page.entries, func((k, _)) { rowIndex(k) }); next = switch (page.cursor) { case (?k) ?rowIndex(k); case null null } }
  };
  public func listProposals(s : State, bb : Blocks, cursor : ?Nat, limit : Nat) : { rows : [T.ProposalView]; cursor : ?Nat; total : Nat } {
    let pg = rowIndices(s.proposalRows, cursor, limit);
    { rows = Array.filterMap<Nat, T.ProposalView>(pg.indices, func(i) { getProposal(s, bb, i) }); cursor = pg.next; total = RI.size(s.proposalRows) }
  };
  public func auditTrail(s : State, bb : Blocks, cursor : ?Nat, limit : Nat) : { rows : [MC.AuditRow]; cursor : ?Nat; total : Nat } {
    let pg = rowIndices(s.proposalRows, cursor, limit);
    { rows = Array.filterMap<Nat, MC.AuditRow>(pg.indices, func(i) { switch (proposalEntry(s, bb, i)) { case (?e) ?MC.auditRow(e); case null null } }); cursor = pg.next; total = RI.size(s.proposalRows) }
  };
  public func listOverrides(s : State, bb : Blocks, cursor : ?Nat, limit : Nat) : { rows : [T.OverrideView]; cursor : ?Nat; total : Nat } {
    let pg = rowIndices(s.overrideRows, cursor, limit);
    { rows = Array.filterMap<Nat, T.OverrideView>(pg.indices, func(i) { overrideView(s, bb, i) }); cursor = pg.next; total = RI.size(s.overrideRows) }
  };
  /// Proposals past their expiry, oldest first, at most `limit`; the row carries the expiry, so no block is read.
  public func expiredProposals(s : State, now : Nat64, limit : Nat) : [Nat] {
    let out = List.empty<Nat>();
    label scan for ((idx, _) in Map.entries(s.openProposals)) {
      if (List.size(out) >= limit) break scan;
      switch (proposalRow(s, idx)) { case (?r) { switch (r.status) { case (#awaiting) { if (r.expiresAt <= now) List.add(out, idx) }; case (_) {} } }; case null {} };
    };
    List.toArray(out)
  };

  // ─── the planners of the authority commands ────────────────────────────────

  public type Planned = Result.Result<?T.Event, T.Error>;

  func validIdentifier(t : Text, maxBytes : Nat) : Bool {
    let bytes = Text.encodeUtf8(t).size();
    if (bytes == 0 or bytes > maxBytes) return false;
    for (c in t.chars()) {
      let ok = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '-' or c == '_' or c == '.';
      if (not ok) return false;
    };
    true
  };
  func bookDepthOk(s : State, parent : ?T.BookId) : Bool {
    var depth = 1;
    var cur = parent;
    label walk loop {
      switch (cur) {
        case null break walk;
        case (?p) { depth += 1; if (depth > T.MAX_BOOK_DEPTH) return false; cur := switch (Map.get(s.books, Text.compare, p)) { case (?b) b.parent; case null null } };
      };
    };
    true
  };

  public func planOpenBook(s : State, x : { id : T.BookId; name : Text; parent : ?T.BookId; sharia : Bool }) : Planned {
    if (not validIdentifier(x.id, T.MAX_BOOK_ID_BYTES)) return #err(#InvalidBook({ reason = "book id must be 1.." # Nat.toText(T.MAX_BOOK_ID_BYTES) # " characters of [A-Za-z0-9-_.]" }));
    if (Text.encodeUtf8(x.name).size() > AT.MAX_NAME_BYTES) return #err(#InvalidBook({ reason = "name exceeds the bound" }));
    if (Map.containsKey(s.books, Text.compare, x.id)) return #err(#BookExists({ book = x.id }));
    switch (x.parent) {
      case (?p) {
        switch (Map.get(s.books, Text.compare, p)) { case null return #err(#UnknownBook({ book = p })); case (?b) { if (not b.open) return #err(#BookClosed({ book = p })) } };
        if (not bookDepthOk(s, x.parent)) return #err(#InvalidBook({ reason = "book tree deeper than " # Nat.toText(T.MAX_BOOK_DEPTH) }));
      };
      case null {};
    };
    #ok(?#bookOpened({ id = x.id; name = x.name; parent = x.parent; sharia = x.sharia }))
  };
  public func planCloseBook(s : State, id : T.BookId) : Planned {
    switch (Map.get(s.books, Text.compare, id)) {
      case null #err(#UnknownBook({ book = id }));
      case (?b) {
        if (not b.open) return #err(#BookClosed({ book = id }));
        for ((_, c) in Map.entries(s.books)) { if (c.open and c.parent == ?id) return #err(#InvalidBook({ reason = "book " # id # " has an open child " # c.id })) };
        #ok(?#bookClosed({ id }))
      };
    }
  };
  public func planDefineRole(s : State, x : { id : T.RoleId; name : Text; permissions : [T.PermissionId] }) : Planned {
    if (not validIdentifier(x.id, AT.MAX_ROLE_ID_BYTES)) return #err(#InvalidRole({ reason = "role id must be 1.." # Nat.toText(AT.MAX_ROLE_ID_BYTES) # " characters of [A-Za-z0-9-_.]" }));
    if (Text.encodeUtf8(x.name).size() > AT.MAX_NAME_BYTES) return #err(#InvalidRole({ reason = "name exceeds the bound" }));
    if (Map.containsKey(s.roles, Text.compare, x.id)) return #err(#RoleExists({ role = x.id }));
    if (x.permissions.size() == 0) return #err(#InvalidRole({ reason = "a role with no permissions cannot do anything; define it with at least one" }));
    if (x.permissions.size() > AT.MAX_PERMISSIONS_PER_ROLE) return #err(#InvalidRole({ reason = "permission list exceeds the bound" }));
    var i = 0;
    while (i < x.permissions.size()) {
      if (not Cat.exists(x.permissions[i])) return #err(#UnknownPermission({ permission = x.permissions[i] }));
      var j = i + 1;
      while (j < x.permissions.size()) { if (Text.equal(x.permissions[i], x.permissions[j])) return #err(#InvalidRole({ reason = "duplicate permission " # x.permissions[i] })); j += 1 };
      i += 1;
    };
    #ok(?#roleDefined({ id = x.id; name = x.name; permissions = x.permissions }))
  };
  public func planGrantRole(s : State, x : { subject : Principal; role : T.RoleId; scope : T.Scope }) : Planned {
    if (Principal.isAnonymous(x.subject)) return #err(#InvalidScope({ reason = "the anonymous principal cannot hold a grant" }));
    if (not Map.containsKey(s.roles, Text.compare, x.role)) return #err(#UnknownRole({ role = x.role }));
    if (Map.containsKey(s.grants, cmpPR, (x.subject, x.role))) return #err(#GrantExists({ subject = x.subject; role = x.role }));
    switch (E.validateScope(x.scope)) { case (?r) return #err(#InvalidScope({ reason = r })); case null {} };
    switch (x.scope.partitions) { case (?books) { for (b in books.vals()) { if (not Map.containsKey(s.books, Text.compare, b)) return #err(#UnknownBook({ book = b })) } }; case null {} };
    #ok(?#roleGranted({ subject = x.subject; role = x.role; scope = x.scope }))
  };
  public func planRevokeRole(s : State, x : { subject : Principal; role : T.RoleId }) : Planned {
    if (not Map.containsKey(s.grants, cmpPR, (x.subject, x.role))) return #err(#NoSuchGrant({ subject = x.subject; role = x.role }));
    // a policy that would be left unsatisfiable by the revocation is refused: the checker set must stay complete
    for ((_, p) in Map.entries(s.policies)) {
      if (Text.equal(p.eligibleRole, x.role) and roleHolderCount(s, x.role) <= p.required) {
        return #err(#InvalidPolicy({ reason = "revoking " # x.role # " would leave policy " # p.permission # " with fewer holders than it requires" }));
      };
    };
    #ok(?#roleRevoked({ subject = x.subject; role = x.role }))
  };
  public func planSetDualPolicy(s : State, p : T.DualPolicy) : Planned {
    switch (MC.validatePolicy(p, Cat.exists(p.permission), Map.containsKey(s.roles, Text.compare, p.eligibleRole), roleHolderCount(s, p.eligibleRole))) {
      case (?r) #err(#InvalidPolicy({ reason = r }));
      case null #ok(?#dualPolicySet(p));
    }
  };
  public func planClearDualPolicy(s : State, permission : T.PermissionId) : Planned {
    let ?perm = Cat.byId(permission) else return #err(#UnknownPermission({ permission }));
    if (policyFor(s, permission) == null) return #err(#InvalidPolicy({ reason = "no policy recorded for " # permission }));
    if (perm.dualByDefault) return #err(#InvalidPolicy({ reason = permission # " is dual by default; its policy can be replaced, never cleared" }));
    #ok(?#dualPolicyCleared({ permission }))
  };
  public func planSetFeatureActivation(x : { feature : T.FeatureId; height : Nat64 }, journalActive : Bool, height : Nat) : Planned {
    if (not validIdentifier(x.feature, AT.MAX_ROLE_ID_BYTES)) return #err(#InvalidFeature({ reason = "feature id must be 1.." # Nat.toText(AT.MAX_ROLE_ID_BYTES) # " characters of [A-Za-z0-9-_.]" }));
    if (x.height != T.ACTIVATION_OFF and not journalActive) return #err(#FeatureInactive({ feature = x.feature; activationHeight = x.height; height = Nat64.fromNat(height) }));
    #ok(?#featureActivationSet({ feature = x.feature; height = x.height }))
  };
  public func planSetIdentity(i : T.Identity) : Planned {
    let n = Text.encodeUtf8(i.name).size();
    if (n == 0 or n > T.MAX_IDENTITY_BYTES) return #err(#InvalidIdentity({ reason = "the desk is named in 1.." # Nat.toText(T.MAX_IDENTITY_BYTES) # " bytes" }));
    let b = Text.encodeUtf8(i.bic).size();
    if (b != 8 and b != 11) return #err(#InvalidIdentity({ reason = "the BIC has 8 or 11 characters" }));
    let l = Text.encodeUtf8(i.lei).size();
    if (l != 0 and l != 20) return #err(#InvalidIdentity({ reason = "the LEI has 20 characters, or is absent" }));
    #ok(?#identitySet(i))
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  public func apply(s : State, block : Block) {
    switch (block.event) {
      case (#deskInstalled(x)) s.installer := x.installer;
      case (#bookOpened(x)) {
        let entry : BookEntry = { id = x.id; name = x.name; parent = x.parent; sharia = x.sharia; var open = true; openedAtBlock = block.index; var closedAtBlock = null };
        Map.add(s.books, Text.compare, x.id, entry);
      };
      case (#bookClosed(x)) { switch (Map.get(s.books, Text.compare, x.id)) { case (?b) { b.open := false; b.closedAtBlock := ?block.index }; case null Runtime.trap("Authority: close of unknown book") } };
      case (#roleDefined(x)) Map.add(s.roles, Text.compare, x.id, { id = x.id; name = x.name; permissions = x.permissions; definedAtBlock = block.index });
      case (#roleGranted(x)) Map.add(s.grants, cmpPR, (x.subject, x.role), { subject = x.subject; role = x.role; scope = x.scope; grantedAtBlock = block.index });
      case (#roleRevoked(x)) ignore Map.delete(s.grants, cmpPR, (x.subject, x.role));
      case (#dualPolicySet(p)) Map.add(s.policies, Text.compare, p.permission, p);
      case (#dualPolicyCleared(x)) ignore Map.delete(s.policies, Text.compare, x.permission);
      case (#featureActivationSet(x)) Map.add(s.features, Text.compare, x.feature, x.height);
      case (#identitySet(i)) s.identity := ?i;
      case (#commandProposed(x)) {
        ignore RI.put(s.proposalRows, rowKey(block.index), MC.encodeProposalRow({ expiresAt = x.expiresAt; status = #awaiting; approvalBlocks = [] }));
        Map.add(s.openProposals, Nat.compare, block.index, ());
      };
      case (#commandApproved(x)) {
        let ?r = proposalRow(s, x.proposal) else Runtime.trap("Authority: approval of unknown proposal");
        if (r.approvalBlocks.size() >= AT.MAX_REQUIRED_APPROVALS) Runtime.trap("Authority: more approvals than any policy can require");
        ignore RI.put(s.proposalRows, rowKey(x.proposal), MC.encodeProposalRow({ r with approvalBlocks = Array.concat<Nat>(r.approvalBlocks, [block.index]) }));
      };
      case (#commandRejected(x)) {
        let ?r = proposalRow(s, x.proposal) else Runtime.trap("Authority: rejection of unknown proposal");
        ignore RI.put(s.proposalRows, rowKey(x.proposal), MC.encodeProposalRow({ r with status = #rejected(block.index) }));
        ignore Map.delete(s.openProposals, Nat.compare, x.proposal);
      };
      case (#commandExecuted(x)) {
        switch (proposalRow(s, x.proposal)) {
          case (?r) {
            ignore RI.put(s.proposalRows, rowKey(x.proposal), MC.encodeProposalRow({ r with status = #executed(block.index) }));
            ignore Map.delete(s.openProposals, Nat.compare, x.proposal);
          };
          case null {
            let ?o = overrideRow(s, x.proposal) else Runtime.trap("Authority: execution names neither a proposal nor an override");
            ignore RI.put(s.overrideRows, rowKey(x.proposal), MC.encodeOverrideRow({ o with executedAt = block.index }));
          };
        };
        s.executedCount += 1;
      };
      case (#commandExpired(x)) {
        let ?r = proposalRow(s, x.proposal) else Runtime.trap("Authority: expiry of unknown proposal");
        ignore RI.put(s.proposalRows, rowKey(x.proposal), MC.encodeProposalRow({ r with status = #expired(block.index) }));
        ignore Map.delete(s.openProposals, Nat.compare, x.proposal);
      };
      case (#operationRefused(_)) s.refusedCount += 1;
      case (#emergencyOverride(_)) {
        ignore RI.put(s.overrideRows, rowKey(block.index), MC.encodeOverrideRow({ executedAt = 0; reviewedAt = 0 }));
        Map.add(s.openOverrides, Nat.compare, block.index, ());
      };
      case (#overrideReviewed(x)) {
        let ?o = overrideRow(s, x.override_) else Runtime.trap("Authority: review of unknown override");
        ignore RI.put(s.overrideRows, rowKey(x.override_), MC.encodeOverrideRow({ o with reviewedAt = block.index }));
        ignore Map.delete(s.openOverrides, Nat.compare, x.override_);
      };
      case (#dailyConsumed(x)) {
        let prev = consumedFor(s, x.subject, x.currency, x.day);
        Map.add(s.consumed, cmpPCD, (x.subject, x.currency, x.day), prev + x.amount);
      };
      case (_) {};
    }
  };

  public func status(s : State, height : Nat) : T.Status {
    {
      height; books = Map.size(s.books); roles = Map.size(s.roles); grants = Map.size(s.grants); policies = Map.size(s.policies);
      proposals = RI.size(s.proposalRows); openProposals = Map.size(s.openProposals); overrides = RI.size(s.overrideRows);
      refused = s.refusedCount; executed = s.executedCount; identity = s.identity;
    }
  };

  public func fingerprintInto(w : JC.Writer, s : State) {
    w.principal(s.installer);
    switch (s.identity) { case null w.byte(0); case (?i) { w.byte(1); w.text(i.name); w.text(i.bic); w.text(i.lei) } };
    w.nat(Map.size(s.books));
    for ((_, b) in Map.entries(s.books)) { w.text(b.id); w.text(b.name); switch (b.parent) { case null w.byte(0); case (?p) { w.byte(1); w.text(p) } }; w.bool(b.sharia); w.bool(b.open); w.nat(b.openedAtBlock); w.optNat(b.closedAtBlock) };
    w.nat(Map.size(s.roles));
    for ((_, r) in Map.entries(s.roles)) { w.text(r.id); w.text(r.name); w.len16(r.permissions.size()); for (p in r.permissions.vals()) w.text(p); w.nat(r.definedAtBlock) };
    w.nat(Map.size(s.grants));
    for ((_, g) in Map.entries(s.grants)) { w.principal(g.subject); w.text(g.role); Can.writeScope(w, g.scope); w.nat(g.grantedAtBlock) };
    w.nat(Map.size(s.policies));
    for ((_, p) in Map.entries(s.policies)) { w.text(p.permission); w.nat(p.required); w.text(p.eligibleRole); w.nat(p.ttlSeconds) };
    w.nat(Map.size(s.features));
    for ((f, h) in Map.entries(s.features)) { w.text(f); w.nat64(h) };
    w.nat(RI.size(s.proposalRows)); w.nat(Map.size(s.openProposals)); w.nat(RI.size(s.overrideRows)); w.nat(Map.size(s.openOverrides));
    var cursor : ?Nat = null;
    label walk loop {
      let pg = rowIndices(s.proposalRows, cursor, MAX_PAGE);
      for (i in pg.indices.vals()) { switch (proposalRow(s, i)) { case (?r) { w.nat(i); w.blob(MC.encodeProposalRow(r)) }; case null {} } };
      switch (pg.next) { case null break walk; case (?n) cursor := ?n };
    };
    cursor := null;
    label walk2 loop {
      let pg = rowIndices(s.overrideRows, cursor, MAX_PAGE);
      for (i in pg.indices.vals()) { switch (overrideRow(s, i)) { case (?r) { w.nat(i); w.blob(MC.encodeOverrideRow(r)) }; case null {} } };
      switch (pg.next) { case null break walk2; case (?n) cursor := ?n };
    };
    w.nat(Map.size(s.consumed));
    for (((p, c, d), n) in Map.entries(s.consumed)) { w.principal(p); w.text(c); w.nat(d); w.nat(n) };
    w.nat(s.refusedCount); w.nat(s.executedCount);
  };
}
