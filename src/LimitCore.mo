/// LimitCore.mo: the limit tree and the risk sweep folded from the desk log in stable memory.
///
/// A node's utilisation is three figures: what the last sweep published, what was captured after that sweep's
/// bound and before the next opened (folded at the next publication), and what was captured since the open
/// sweep's bound (carried into the sweep after). The check at capture adds the three and the act's own amount,
/// so it reads a bounded number of rows however many are open. The sweep is the kernel's sharded walk: one family
/// at a time, one bounded slice a message, the cursor and the partial sums recorded per slice so a restart resumes
/// from the log.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TreasuryCore "mo:manticore/TreasuryCore";

import CuT "CustodyTypes";
import LT "LimitTypes";

module {

  public let NODE_ROW_BYTES : Nat = 135;
  public let COUNTER_ROW_BYTES : Nat = 8;
  public let COUNTERPARTY_ROW_BYTES : Nat = 80;
  public let MAX_NODES : Nat = 256;
  public let MAX_SLICE : Nat = 1_024;
  let MAX_PAGE = 512;

  public type NodeRow = {
    id : Text; kind : Nat8; subject : Text; fromDays : Nat; toDays : Nat; parent : ?Text; currency : Text; limit : Nat;
    published : Nat; publishedDay : Nat; beforeBound : Nat; sinceBound : Nat; pending : Nat; removed : Bool; lastBlock : Nat;
  };
  public type CounterpartyRow = { name : Text; group : Text; country : Text; block : Nat };
  public type Sweep = { day : Nat; bound : Nat; sliceSize : Nat; var family : Nat; var cursor : ?Blob; var slices : Nat; var rows : Nat };

  public func kindCode(k : LT.NodeKind) : Nat8 {
    switch (k) { case (#counterparty(_)) 1; case (#group(_)) 2; case (#country(_)) 3; case (#issuer(_)) 4; case (#instrumentClass(_)) 5; case (#tenor(_)) 6; case (#book(_)) 7; case (#desk(_)) 8 }
  };
  public func kindTextOf(c : Nat8) : Text {
    switch (c) { case 1 "counterparty"; case 2 "group"; case 3 "country"; case 4 "issuer"; case 5 "instrumentClass"; case 6 "tenor"; case 7 "book"; case _ "desk" }
  };
  public func subjectOf(k : LT.NodeKind) : Text {
    switch (k) {
      case (#counterparty(t) or #group(t) or #country(t) or #issuer(t) or #book(t) or #desk(t)) t;
      case (#instrumentClass(c)) CuT.classificationText(c);
      case (#tenor(b)) Nat.toText(b.fromDays) # "-" # Nat.toText(b.toDays);
    }
  };
  public func familyCode(f : LT.Family) : Nat { switch (f) { case (#treasury) 0; case (#call) 1; case (#repo) 2; case (#loan) 3 } };
  public func familyOf(c : Nat) : LT.Family { switch (c) { case 0 #treasury; case 1 #call; case 2 #repo; case _ #loan } };

  func encodeNode(r : NodeRow) : Blob {
    let b = R.buf();
    R.putByte(b, r.kind); R.putText(b, r.subject, 32); R.putNat(b, r.fromDays, 4); R.putNat(b, r.toDays, 4);
    switch (r.parent) { case null { R.putByte(b, 0); R.putText(b, "", 32) }; case (?p) { R.putByte(b, 1); R.putText(b, p, 32) } };
    R.putText(b, r.currency, 8); R.putNat(b, r.limit, 8); R.putNat(b, r.published, 8); R.putNat(b, r.publishedDay, 4);
    R.putNat(b, r.beforeBound, 8); R.putNat(b, r.sinceBound, 8); R.putNat(b, r.pending, 8); R.putBool(b, r.removed); R.putNat(b, r.lastBlock, 8);
    R.done(b, NODE_ROW_BYTES)
  };
  func decodeNode(id : Text, v : Blob) : NodeRow {
    let a = Blob.toArray(v);
    { id; kind = a[0]; subject = R.getText(a, 1, 32); fromDays = R.getNat(a, 33, 4); toDays = R.getNat(a, 37, 4); parent = if (a[41] == 1) ?R.getText(a, 42, 32) else null;
      currency = R.getText(a, 74, 8); limit = R.getNat(a, 82, 8); published = R.getNat(a, 90, 8); publishedDay = R.getNat(a, 98, 4);
      beforeBound = R.getNat(a, 102, 8); sinceBound = R.getNat(a, 110, 8); pending = R.getNat(a, 118, 8); removed = R.getBool(a, 126); lastBlock = R.getNat(a, 127, 8) }
  };
  func encodeCounterparty(r : CounterpartyRow) : Blob { let b = R.buf(); R.putText(b, r.name, 32); R.putText(b, r.group, 32); R.putText(b, r.country, 8); R.putNat(b, r.block, 8); R.done(b, COUNTERPARTY_ROW_BYTES) };
  func decodeCounterparty(v : Blob) : CounterpartyRow { let a = Blob.toArray(v); { name = R.getText(a, 0, 32); group = R.getText(a, 32, 32); country = R.getText(a, 64, 8); block = R.getNat(a, 72, 8) } };
  public func hash8(t : Text) : Nat { TreasuryCore.hash8(t) };

  public type State = {
    nodes : RI.State;           // id(32) -> row
    counters : RI.State;        // id(32) ‖ day(4) -> recorded(4) ‖ refused(4)
    counterparties : RI.State;  // hash8(name) -> name(32) ‖ group(32) ‖ country(8) ‖ block(8)
    var sweep : ?Sweep;
    var nodeCount : Nat;
    var removedCount : Nat;
    var counterpartyCount : Nat;
    var recorded : Nat;
    var refused : Nat;
    var sweeps : Nat;
    var lastPublishedDay : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      nodes = RI.newStateIn(arena, { keyBytes = 32; valBytes = NODE_ROW_BYTES });
      counters = RI.newStateIn(arena, { keyBytes = 36; valBytes = COUNTER_ROW_BYTES });
      counterparties = RI.newStateIn(arena, { keyBytes = 8; valBytes = COUNTERPARTY_ROW_BYTES });
      var sweep = null; var nodeCount = 0; var removedCount = 0; var counterpartyCount = 0; var recorded = 0; var refused = 0; var sweeps = 0; var lastPublishedDay = 0;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func node(s : State, id : Text) : ?NodeRow { switch (RI.get(s.nodes, R.textKey(id, 32))) { case (?v) ?decodeNode(id, v); case null null } };
  func putNode(s : State, r : NodeRow) { ignore RI.put(s.nodes, R.textKey(r.id, 32), encodeNode(r)) };
  public func counterpartyByHash(s : State, h : Nat) : ?CounterpartyRow { switch (RI.get(s.counterparties, R.key(h, 8))) { case (?v) ?decodeCounterparty(v); case null null } };
  public func counterparty(s : State, name : Text) : ?CounterpartyRow { counterpartyByHash(s, hash8(name)) };
  public func counterparties(s : State) : [CounterpartyRow] {
    let out = List.empty<CounterpartyRow>();
    let (lo, hi) = R.fullRange(8);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.counterparties, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) List.add(out, decodeCounterparty(v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  /// Every node, removed ones included, in id order: the tree is bounded at MAX_NODES.
  public func nodes(s : State) : [NodeRow] {
    let out = List.empty<NodeRow>();
    let (lo, hi) = R.fullRange(32);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.nodes, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, decodeNode(R.getText(Blob.toArray(k), 0, 32), v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func liveNodes(s : State) : [NodeRow] { Array.filter<NodeRow>(nodes(s), func(r) { not r.removed }) };
  public func counter(s : State, id : Text, day : Nat) : (Nat, Nat) {
    switch (RI.get(s.counters, counterKey(id, day))) { case (?v) { let a = Blob.toArray(v); (R.getNat(a, 0, 4), R.getNat(a, 4, 4)) }; case null (0, 0) }
  };
  func counterKey(id : Text, day : Nat) : Blob { Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(id, 32)), Blob.toArray(R.key(day, 4)))) };
  func putCounter(s : State, id : Text, day : Nat, recorded : Nat, refused : Nat) {
    let b = R.buf(); R.putNat(b, recorded, 4); R.putNat(b, refused, 4);
    ignore RI.put(s.counters, counterKey(id, day), R.done(b, COUNTER_ROW_BYTES));
  };
  /// The counters of a node over a range of days.
  public func countersOf(s : State, id : Text, from : Nat, to : Nat) : [LT.CounterView] {
    let out = List.empty<LT.CounterView>();
    let p = Blob.toArray(R.textKey(id, 32));
    let lo = Blob.fromArray(Array.concat<Nat8>(p, Blob.toArray(R.key(from, 4))));
    let hi = Blob.fromArray(Array.concat<Nat8>(p, Blob.toArray(R.key(to, 4))));
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.counters, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) { let a = Blob.toArray(v); List.add(out, { node = id; day = R.getNat(Blob.toArray(k), 32, 4); recorded = R.getNat(a, 0, 4); refused = R.getNat(a, 4, 4) }) };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func utilisation(r : NodeRow) : Nat { r.published + r.beforeBound + r.sinceBound };
  public func view(r : NodeRow) : LT.NodeView {
    { id = r.id; kind = kindTextOf(r.kind); subject = r.subject; fromDays = r.fromDays; toDays = r.toDays; parent = r.parent; currency = r.currency; limit = r.limit;
      published = r.published; publishedDay = r.publishedDay; sinceFold = r.beforeBound + r.sinceBound; utilisation = utilisation(r); removed = r.removed; lastBlock = r.lastBlock }
  };
  public func sweepView(s : State) : ?LT.SweepView {
    switch (s.sweep) {
      case null null;
      case (?w) ?{ day = w.day; bound = w.bound; sliceSize = w.sliceSize; family = if (w.family > 3) "done" else LT.familyText(familyOf(w.family)); cursor = w.cursor; slices = w.slices; rows = w.rows; complete = w.family > 3 };
    }
  };
  public func status(s : State) : LT.Status {
    { nodes = s.nodeCount; removed = s.removedCount; counterparties = s.counterpartyCount; breachesRecorded = s.recorded; breachesRefused = s.refused; sweeps = s.sweeps; lastPublishedDay = s.lastPublishedDay; sweepOpen = switch (s.sweep) { case null false; case (?_) true } }
  };

  // ─── the predicates ────────────────────────────────────────────────────────

  /// What the tree reads of a row: its family and id, the book, the counterparty's hash, the currency and the
  /// measure in it, the instrument's issuer hash and class where the row holds one, and the days it has left
  /// where it has a term.
  public type RowFacts = { family : LT.Family; id : Nat; book : Text; cpHash : Nat; currency : Text; amount : Nat; isin : Text; issuerHash : Nat; classification : ?CuT.Classification; remainingDays : ?Nat };

  /// The live nodes with their subjects hashed once, so a row is matched by hash comparison and never by a hash
  /// computed per node per row.
  public type Prepared = { row : NodeRow; subjectHash : Nat };
  public func prepare(s : State) : [Prepared] {
    Array.map<NodeRow, Prepared>(liveNodes(s), func(n) { { row = n; subjectHash = if (n.kind == 1 or n.kind == 4) hash8(n.subject) else 0 } })
  };
  /// The nodes a row falls under: every live node in the row's currency whose subject the row carries. A parent
  /// is not implied by a child: it is matched on its own subject, so a parent limit binds whether or not the
  /// child node exists.
  public func nodesFor(s : State, prepared : [Prepared], f : RowFacts, isUnder : (Text, Text) -> Bool) : [Text] {
    let cp = counterpartyByHash(s, f.cpHash);
    let out = List.empty<Text>();
    for (p in prepared.vals()) {
      let n = p.row;
      if (not Text.equal(n.currency, f.currency)) continue;
      let hit = switch (n.kind) {
        case 1 p.subjectHash == f.cpHash;
        case 2 { switch (cp) { case (?c) Text.equal(c.group, n.subject); case null false } };
        case 3 { switch (cp) { case (?c) Text.equal(c.country, n.subject); case null false } };
        case 4 f.issuerHash != 0 and p.subjectHash == f.issuerHash;
        case 5 { switch (f.classification) { case (?c) Text.equal(CuT.classificationText(c), n.subject); case null false } };
        case 6 { switch (f.remainingDays) { case (?d) d >= n.fromDays and d <= n.toDays; case null false } };
        case 7 Text.equal(n.subject, f.book);
        case _ isUnder(f.book, n.subject);
      };
      if (hit) List.add(out, n.id);
    };
    List.toArray(out)
  };
  /// The breaches an amount would make: each node whose utilisation plus the amount passes its limit.
  public func breachesOf(s : State, nodeIds : [Text], amount : Nat) : [(Text, Nat, Nat)] {
    let out = List.empty<(Text, Nat, Nat)>();
    for (id in nodeIds.vals()) {
      switch (node(s, id)) { case (?n) { let m = utilisation(n) + amount; if (m > n.limit) List.add(out, (id, m, n.limit)) }; case null {} };
    };
    List.toArray(out)
  };

  // ─── planners ─────────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, LT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidNode({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  func parentKindAllowed(child : Nat8, parent : Nat8) : Bool {
    switch (child, parent) {
      case (1, 2) true; case (1, 3) true; case (2, 3) true; case (4, 5) true; case (7, 8) true; case (8, 8) true;
      case (_) false;
    }
  };
  /// A node set: its id and subject bounded, its currency a code, a parent that exists and is of a kind that
  /// covers the child's, a counterparty node whose recorded group or country agrees with the parent it hangs
  /// under, a book node whose desk is an ancestor book. Setting an existing node keeps its utilisation.
  public func planSetNode(s : State, n : LT.Node, isUnder : (Text, Text) -> Bool, bookExists : Text -> Bool, day : Nat) : Res<LT.Event> {
    switch (s.sweep) { case (?w) return #err(#SweepOpen({ day = w.day })); case null {} };
    if (bytesOf(n.id) == 0 or bytesOf(n.id) > 32) return bad("a node is named in 1..32 bytes");
    let subject = subjectOf(n.kind);
    if (bytesOf(subject) == 0 or bytesOf(subject) > 32) return bad("a node's subject is named in 1..32 bytes");
    if (bytesOf(n.currency) != 3) return bad("a currency code has three letters");
    if (n.limit == 0) return bad("a limit is positive");
    switch (n.kind) {
      case (#tenor(b)) { if (b.toDays < b.fromDays) return bad("a tenor bucket runs from its lower bound to its upper") };
      case (#country(c)) { if (bytesOf(c) > 8) return bad("a country is named in at most 8 bytes") };
      case (#book(b)) { if (not bookExists(b)) return bad("a book node names an open book") };
      case (#desk(d)) { if (not bookExists(d)) return bad("a desk node names an open book") };
      case (_) {};
    };
    let existing = node(s, n.id);
    switch (existing) { case (?e) { if (not e.removed and e.kind != kindCode(n.kind)) return bad("a node keeps its kind; remove it to change it") }; case null { if (s.nodeCount >= s.removedCount + MAX_NODES) return bad("the tree holds at most " # Nat.toText(MAX_NODES) # " nodes") } };
    switch (n.parent) {
      case (?p) {
        if (Text.equal(p, n.id)) return bad("a node is not its own parent");
        let ?pr = node(s, p) else return #err(#UnknownNode({ node = p }));
        if (pr.removed) return #err(#UnknownNode({ node = p }));
        if (not parentKindAllowed(kindCode(n.kind), pr.kind)) return bad("a " # LT.kindText(n.kind) # " node does not hang under a " # kindTextOf(pr.kind) # " node");
        if (not Text.equal(pr.currency, n.currency)) return bad("a child shares its parent's currency");
        switch (n.kind) {
          case (#counterparty(name)) {
            let ?c = counterparty(s, name) else return bad("the counterparty's group and country are recorded before it hangs under a parent");
            if (pr.kind == 2 and not Text.equal(c.group, pr.subject)) return bad("the counterparty's recorded group is " # c.group # ", not " # pr.subject);
            if (pr.kind == 3 and not Text.equal(c.country, pr.subject)) return bad("the counterparty's recorded country is " # c.country # ", not " # pr.subject);
          };
          case (#book(b)) { if (not isUnder(b, pr.subject)) return bad("the book is not under the desk " # pr.subject) };
          case (#desk(d)) { if (not isUnder(d, pr.subject)) return bad("the desk is not under the desk " # pr.subject) };
          case (_) {};
        };
      };
      case null {};
    };
    #ok(#nodeSet({ node = n; day }))
  };
  public func planRemoveNode(s : State, id : Text, day : Nat) : Res<LT.Event> {
    switch (s.sweep) { case (?w) return #err(#SweepOpen({ day = w.day })); case null {} };
    let ?r = node(s, id) else return #err(#UnknownNode({ node = id }));
    if (r.removed) return #err(#UnknownNode({ node = id }));
    for (n in nodes(s).vals()) { if (not n.removed and n.parent == ?id) return #err(#NodeHasChildren({ node = id })) };
    #ok(#nodeRemoved({ node = id; day }))
  };
  /// A counterparty's group and country recorded; a change that contradicts the parent a node of the
  /// counterparty hangs under is refused, so the tree and the records never disagree.
  public func planAmendCounterparty(s : State, c : LT.Counterparty, day : Nat) : Res<LT.Event> {
    func b(reason : Text) : Res<LT.Event> { #err(#InvalidCounterparty({ reason })) };
    switch (s.sweep) { case (?w) return #err(#SweepOpen({ day = w.day })); case null {} };
    if (bytesOf(c.name) == 0 or bytesOf(c.name) > 32) return b("a counterparty is named in 1..32 bytes");
    if (bytesOf(c.group) > 32) return b("a group is named in at most 32 bytes");
    if (bytesOf(c.country) > 8) return b("a country is named in at most 8 bytes");
    for (n in nodes(s).vals()) {
      if (n.removed or n.kind != 1 or not Text.equal(n.subject, c.name)) continue;
      switch (n.parent) {
        case (?p) {
          switch (node(s, p)) {
            case (?pr) {
              if (pr.kind == 2 and not Text.equal(pr.subject, c.group)) return b("node " # n.id # " hangs under group " # pr.subject);
              if (pr.kind == 3 and not Text.equal(pr.subject, c.country)) return b("node " # n.id # " hangs under country " # pr.subject);
            };
            case null {};
          };
        };
        case null {};
      };
    };
    #ok(#counterpartyAmended({ counterparty = c; day }))
  };
  public func planOpenSweep(s : State, day : Nat, bound : Nat, sliceSize : Nat) : Res<LT.Event> {
    switch (s.sweep) { case (?w) return #err(#SweepOpen({ day = w.day })); case null {} };
    if (day <= s.lastPublishedDay and s.sweeps > 0) return #err(#SweepExists({ day = s.lastPublishedDay }));
    if (sliceSize == 0 or sliceSize > MAX_SLICE) return #err(#InvalidSweep({ reason = "a slice holds 1.." # Nat.toText(MAX_SLICE) # " rows" }));
    #ok(#sweepOpened({ day; bound; sliceSize }))
  };

  // ─── the fold ─────────────────────────────────────────────────────────────

  public func fold(s : State, block : Nat, ev : LT.Event) {
    switch (ev) {
      case (#nodeSet(x)) {
        let n = x.node;
        let (fromDays, toDays) = switch (n.kind) { case (#tenor(b)) (b.fromDays, b.toDays); case (_) (0, 0) };
        switch (node(s, n.id)) {
          case (?e) {
            if (e.removed) { s.removedCount -= 1 };
            putNode(s, { e with kind = kindCode(n.kind); subject = subjectOf(n.kind); fromDays; toDays; parent = n.parent; currency = n.currency; limit = n.limit; removed = false; lastBlock = block });
          };
          case null {
            s.nodeCount += 1;
            putNode(s, { id = n.id; kind = kindCode(n.kind); subject = subjectOf(n.kind); fromDays; toDays; parent = n.parent; currency = n.currency; limit = n.limit; published = 0; publishedDay = 0; beforeBound = 0; sinceBound = 0; pending = 0; removed = false; lastBlock = block });
          };
        };
      };
      case (#nodeRemoved(x)) { switch (node(s, x.node)) { case (?e) { if (not e.removed) { s.removedCount += 1; putNode(s, { e with removed = true; published = 0; beforeBound = 0; sinceBound = 0; pending = 0; lastBlock = block }) } }; case null {} } };
      case (#counterpartyAmended(x)) {
        if (counterparty(s, x.counterparty.name) == null) s.counterpartyCount += 1;
        ignore RI.put(s.counterparties, R.key(hash8(x.counterparty.name), 8), encodeCounterparty({ name = x.counterparty.name; group = x.counterparty.group; country = x.counterparty.country; block }));
      };
      case (#utilised(x)) {
        for (id in x.nodes.vals()) { switch (node(s, id)) { case (?n) putNode(s, { n with sinceBound = n.sinceBound + x.amount; lastBlock = block }); case null {} } };
      };
      case (#breached(x)) { let (a, b) = counter(s, x.node, x.day); putCounter(s, x.node, x.day, a + 1, b); s.recorded += 1 };
      case (#breachRefused(x)) { let (a, b) = counter(s, x.node, x.day); putCounter(s, x.node, x.day, a, b + 1); s.refused += 1 };
      case (#sweepOpened(x)) {
        s.sweep := ?{ day = x.day; bound = x.bound; sliceSize = x.sliceSize; var family = 0; var cursor = null; var slices = 0; var rows = 0 };
        for (n in nodes(s).vals()) { if (not n.removed) putNode(s, { n with beforeBound = n.beforeBound + n.sinceBound; sinceBound = 0; pending = 0 }) };
      };
      case (#sweepSliced(x)) {
        switch (s.sweep) {
          case (?w) {
            w.slices += 1; w.rows += x.visited;
            if (x.familyDone) { w.family := familyCode(x.family) + 1; w.cursor := null } else { w.family := familyCode(x.family); w.cursor := x.nextCursor };
          };
          case null {};
        };
        for ((id, amount) in x.nodes.vals()) { switch (node(s, id)) { case (?n) putNode(s, { n with pending = n.pending + amount }); case null {} } };
      };
      case (#sweepPublished(x)) {
        for (n in nodes(s).vals()) { if (not n.removed) putNode(s, { n with published = n.pending; pending = 0; beforeBound = 0; publishedDay = x.day; lastBlock = block }) };
        s.sweep := null; s.sweeps += 1; s.lastPublishedDay := x.day;
      };
    }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    w.nat(s.nodeCount); w.nat(s.removedCount); w.nat(s.counterpartyCount); w.nat(s.recorded); w.nat(s.refused); w.nat(s.sweeps); w.nat(s.lastPublishedDay);
    switch (s.sweep) { case null w.byte(0); case (?x) { w.byte(1); w.nat(x.day); w.nat(x.bound); w.nat(x.sliceSize); w.nat(x.family); switch (x.cursor) { case null w.byte(0); case (?c) { w.byte(1); w.blob(c) } }; w.nat(x.slices); w.nat(x.rows) } };
    for ((idx, width) in [(s.nodes, 32), (s.counters, 36), (s.counterparties, 8)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var cursor : ?Blob = null;
      var n = 0;
      label walk loop {
        let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
        for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v); n += 1 };
        switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
      };
      w.nat(n);
    };
  };
}
