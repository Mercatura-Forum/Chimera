/// MarketCore.mo: the market rows folded from the desk log in stable memory: the markets and their participants,
/// the orders, the cycles and the fills; the planners of the staging and the opening, and the next step a cycle's
/// driver takes, pure over the rows; the fold observes the settlement family for the fills settled.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";
import TreasuryMath "mo:manticore/TreasuryMath";

import MT "MarketTypes";
import ST "SettlementTypes";

module {

  public let MARKET_ROW_BYTES : Nat = 120;
  public let PARTICIPANT_ROW_BYTES : Nat = 103;
  public let ORDER_ROW_BYTES : Nat = 83;
  public let CYCLE_ROW_BYTES : Nat = 108;
  public let FILL_ROW_BYTES : Nat = 95;
  public let MAX_PARTICIPANTS : Nat = 1_024;
  /// Fills read from the engine per driver call: bounded so the message that records them is bounded.
  public let FILLS_PER_READ : Nat = 25;
  let MAX_PAGE = 512;

  public func hash8(t : Text) : Nat { TreasuryCore.hash8(t) };
  public func principalHash(p : Principal) : Nat { hash8(Principal.toText(p)) };

  public type MarketRow = { isin : Text; engine : Principal; sharesLedger : Principal; cashLedger : Principal; currency : Text; unitNominal : Nat; deadlineSecs : Nat; participants : Nat; lastBlock : Nat };
  public type ParticipantRow = { isin : Text; principal : Principal; counterparty : TT.Counterparty };
  public type OrderRow = { id : Nat; isin : Text; book : Text; side : MT.Side; units : Nat; filled : Nat; state : MT.OrderState; cycle : Nat; engineOrder : Nat; traderHash : Nat; day : Nat; lastBlock : Nat; withdrawn : Bool };
  public type CycleRow = {
    id : Nat; isin : Text; day : Nat; state : MT.CycleState; referenceMicro : Nat; referenceBlock : Nat; window : Nat; hasWindow : Bool;
    orders : Nat; submitted : Nat; refused : Nat; fills : Nat; instructed : Nat; settled : Nat; unattributed : Nat;
    clearingPrice : Nat; hasPrice : Bool; volume : Nat; chunks : Nat; nextSeq : Nat; readDone : Bool; lastBlock : Nat;
  };
  public type FillRow = { cycle : Nat; seq : Nat; order : Nat; price : Nat; units : Nat; cpHash : Nat; counterparty : Principal; deal : Nat; instruction : Nat; tradeId : Nat; state : MT.FillState; block : Nat };

  // ─── rows ──────────────────────────────────────────────────────────────────

  func putPrincipal(b : R.Buf, p : Principal) { let bytes = Principal.toBlob(p); R.putNat(b, bytes.size(), 1); b.addBlob(bytes); var i = bytes.size(); while (i < 29) { R.putByte(b, 0); i += 1 } };
  func getPrincipal(a : [Nat8], off : Nat) : Principal { let n = R.getNat(a, off, 1); Principal.fromBlob(Blob.fromArray(Array.tabulate<Nat8>(n, func(i) { a[off + 1 + i] }))) };
  func sideCode(x : MT.Side) : Nat8 { switch (x) { case (#buy) 1; case (#sell) 2 } };
  func sideOf(c : Nat8) : MT.Side { if (c == 2) #sell else #buy };
  func orderStateCode(x : MT.OrderState) : Nat8 { switch (x) { case (#staged) 1; case (#submitted) 2; case (#filled) 3; case (#partlyFilled) 4; case (#unfilled) 5; case (#refused) 6; case (#cancelled) 7 } };
  func orderStateOf(c : Nat8) : MT.OrderState { switch (c) { case 1 #staged; case 2 #submitted; case 3 #filled; case 4 #partlyFilled; case 5 #unfilled; case 6 #refused; case _ #cancelled } };
  func cycleStateCode(x : MT.CycleState) : Nat8 { switch (x) { case (#open) 1; case (#clearing) 2; case (#settling) 3; case (#closed) 4 } };
  func cycleStateOf(c : Nat8) : MT.CycleState { switch (c) { case 1 #open; case 2 #clearing; case 3 #settling; case _ #closed } };
  func fillStateCode(x : MT.FillState) : Nat8 { switch (x) { case (#recorded) 1; case (#unattributed) 2; case (#captured) 3; case (#instructed) 4; case (#settled) 5 } };
  func fillStateOf(c : Nat8) : MT.FillState { switch (c) { case 1 #recorded; case 2 #unattributed; case 3 #captured; case 4 #instructed; case _ #settled } };

  func encodeMarket(r : MarketRow) : Blob {
    let b = R.buf();
    putPrincipal(b, r.engine); putPrincipal(b, r.sharesLedger); putPrincipal(b, r.cashLedger); R.putText(b, r.currency, 8); R.putNat(b, r.unitNominal, 8); R.putNat(b, r.deadlineSecs, 4); R.putNat(b, r.participants, 2); R.putNat(b, r.lastBlock, 8);
    R.done(b, MARKET_ROW_BYTES)
  };
  func decodeMarket(isin : Text, v : Blob) : MarketRow {
    let a = Blob.toArray(v);
    { isin; engine = getPrincipal(a, 0); sharesLedger = getPrincipal(a, 30); cashLedger = getPrincipal(a, 60); currency = R.getText(a, 90, 8); unitNominal = R.getNat(a, 98, 8); deadlineSecs = R.getNat(a, 106, 4); participants = R.getNat(a, 110, 2); lastBlock = R.getNat(a, 112, 8) }
  };
  func encodeParticipant(r : ParticipantRow) : Blob {
    let b = R.buf();
    putPrincipal(b, r.principal); R.putText(b, r.counterparty.name, 32); R.putText(b, r.counterparty.bic, 12); R.putText(b, r.counterparty.lei, 20);
    switch (r.counterparty.party) { case (?id) { R.putBool(b, true); R.putNat(b, id, 8) }; case null { R.putBool(b, false); R.putNat(b, 0, 8) } };
    R.done(b, PARTICIPANT_ROW_BYTES)
  };
  func decodeParticipant(isin : Text, v : Blob) : ParticipantRow {
    let a = Blob.toArray(v);
    let principal = getPrincipal(a, 0);
    { isin; principal; counterparty = { party = if (R.getBool(a, 94)) ?R.getNat(a, 95, 8) else null; name = R.getText(a, 30, 32); bic = R.getText(a, 62, 12); lei = R.getText(a, 74, 20) } }
  };
  func encodeOrder(r : OrderRow) : Blob {
    let b = R.buf();
    R.putText(b, r.isin, 12); R.putText(b, r.book, 16); R.putByte(b, sideCode(r.side)); R.putNat(b, r.units, 8); R.putNat(b, r.filled, 8); R.putByte(b, orderStateCode(r.state));
    R.putNat(b, r.cycle, 8); R.putNat(b, r.engineOrder, 8); R.putNat(b, r.traderHash, 8); R.putNat(b, r.day, 4); R.putNat(b, r.lastBlock, 8); R.putBool(b, r.withdrawn);
    R.done(b, ORDER_ROW_BYTES)
  };
  func decodeOrder(id : Nat, v : Blob) : OrderRow {
    let a = Blob.toArray(v);
    { id; isin = R.getText(a, 0, 12); book = R.getText(a, 12, 16); side = sideOf(a[28]); units = R.getNat(a, 29, 8); filled = R.getNat(a, 37, 8); state = orderStateOf(a[45]);
      cycle = R.getNat(a, 46, 8); engineOrder = R.getNat(a, 54, 8); traderHash = R.getNat(a, 62, 8); day = R.getNat(a, 70, 4); lastBlock = R.getNat(a, 74, 8); withdrawn = R.getBool(a, 82) }
  };
  func encodeCycle(r : CycleRow) : Blob {
    let b = R.buf();
    R.putText(b, r.isin, 12); R.putNat(b, r.day, 4); R.putByte(b, cycleStateCode(r.state)); R.putNat(b, r.referenceMicro, 8); R.putNat(b, r.referenceBlock, 8); R.putNat(b, r.window, 8); R.putBool(b, r.hasWindow);
    R.putNat(b, r.orders, 4); R.putNat(b, r.submitted, 4); R.putNat(b, r.refused, 4); R.putNat(b, r.fills, 4); R.putNat(b, r.instructed, 4); R.putNat(b, r.settled, 4); R.putNat(b, r.unattributed, 4);
    R.putNat(b, r.clearingPrice, 8); R.putBool(b, r.hasPrice); R.putNat(b, r.volume, 8); R.putNat(b, r.chunks, 4); R.putNat(b, r.nextSeq, 8); R.putBool(b, r.readDone); R.putNat(b, r.lastBlock, 8);
    R.done(b, CYCLE_ROW_BYTES)
  };
  func decodeCycle(id : Nat, v : Blob) : CycleRow {
    let a = Blob.toArray(v);
    { id; isin = R.getText(a, 0, 12); day = R.getNat(a, 12, 4); state = cycleStateOf(a[16]); referenceMicro = R.getNat(a, 17, 8); referenceBlock = R.getNat(a, 25, 8); window = R.getNat(a, 33, 8); hasWindow = R.getBool(a, 41);
      orders = R.getNat(a, 42, 4); submitted = R.getNat(a, 46, 4); refused = R.getNat(a, 50, 4); fills = R.getNat(a, 54, 4); instructed = R.getNat(a, 58, 4); settled = R.getNat(a, 62, 4); unattributed = R.getNat(a, 66, 4);
      clearingPrice = R.getNat(a, 70, 8); hasPrice = R.getBool(a, 78); volume = R.getNat(a, 79, 8); chunks = R.getNat(a, 87, 4); nextSeq = R.getNat(a, 91, 8); readDone = R.getBool(a, 99); lastBlock = R.getNat(a, 100, 8) }
  };
  func encodeFill(r : FillRow) : Blob {
    let b = R.buf();
    R.putNat(b, r.order, 8); R.putNat(b, r.price, 8); R.putNat(b, r.units, 8); R.putNat(b, r.cpHash, 8); putPrincipal(b, r.counterparty); R.putNat(b, r.deal, 8); R.putNat(b, r.instruction, 8); R.putNat(b, r.tradeId, 8); R.putByte(b, fillStateCode(r.state)); R.putNat(b, r.block, 8);
    R.done(b, FILL_ROW_BYTES)
  };
  func decodeFill(cycle : Nat, seq : Nat, v : Blob) : FillRow {
    let a = Blob.toArray(v);
    { cycle; seq; order = R.getNat(a, 0, 8); price = R.getNat(a, 8, 8); units = R.getNat(a, 16, 8); cpHash = R.getNat(a, 24, 8); counterparty = getPrincipal(a, 32); deal = R.getNat(a, 62, 8); instruction = R.getNat(a, 70, 8); tradeId = R.getNat(a, 78, 8); state = fillStateOf(a[86]); block = R.getNat(a, 87, 8) }
  };

  public type State = {
    markets : RI.State;            // isin(12) -> market row
    participants : RI.State;       // hash8(isin)(8) ‖ hash8(principal)(8) -> participant row
    orders : RI.State;             // id(8) -> order row
    ordersByIsin : RI.State;       // hash8(isin)(8) ‖ id(8) -> state(1)
    ordersByCycle : RI.State;      // cycle(8) ‖ id(8) -> state(1)
    cycles : RI.State;             // id(8) -> cycle row
    cyclesByIsin : RI.State;       // hash8(isin)(8) ‖ id(8) -> state(1)
    fills : RI.State;              // cycle(8) ‖ seq(8) -> fill row
    fillByInstruction : RI.State;  // instruction(8) -> cycle(8) ‖ seq(8)
    var marketCount : Nat;
    var orderCount : Nat;
    var stagedCount : Nat;
    var cycleCount : Nat;
    var openCycles : Nat;
    var fillCount : Nat;
    var settledFills : Nat;
    var unattributedCount : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      markets = RI.newStateIn(arena, { keyBytes = 12; valBytes = MARKET_ROW_BYTES });
      participants = RI.newStateIn(arena, { keyBytes = 16; valBytes = PARTICIPANT_ROW_BYTES });
      orders = RI.newStateIn(arena, { keyBytes = 8; valBytes = ORDER_ROW_BYTES });
      ordersByIsin = RI.newStateIn(arena, { keyBytes = 16; valBytes = 1 });
      ordersByCycle = RI.newStateIn(arena, { keyBytes = 16; valBytes = 1 });
      cycles = RI.newStateIn(arena, { keyBytes = 8; valBytes = CYCLE_ROW_BYTES });
      cyclesByIsin = RI.newStateIn(arena, { keyBytes = 16; valBytes = 1 });
      fills = RI.newStateIn(arena, { keyBytes = 16; valBytes = FILL_ROW_BYTES });
      fillByInstruction = RI.newStateIn(arena, { keyBytes = 8; valBytes = 16 });
      var marketCount = 0; var orderCount = 0; var stagedCount = 0; var cycleCount = 0; var openCycles = 0; var fillCount = 0; var settledFills = 0; var unattributedCount = 0;
    }
  };

  // ─── reads ─────────────────────────────────────────────────────────────────

  public func market(s : State, isin : Text) : ?MarketRow { switch (RI.get(s.markets, R.textKey(isin, 12))) { case (?v) ?decodeMarket(isin, v); case null null } };
  func participantKey(isin : Text, p : Principal) : Blob { R.key2(hash8(isin), 8, principalHash(p), 8) };
  public func participant(s : State, isin : Text, p : Principal) : ?ParticipantRow { switch (RI.get(s.participants, participantKey(isin, p))) { case (?v) ?decodeParticipant(isin, v); case null null } };
  public func participantsOf(s : State, isin : Text) : [ParticipantRow] {
    let (lo, hi) = R.prefixRange(hash8(isin), 8, 8);
    let out = List.empty<ParticipantRow>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.participants, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) List.add(out, decodeParticipant(isin, v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func markets(s : State) : [MarketRow] {
    let (lo, hi) = R.fullRange(12);
    let out = List.empty<MarketRow>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.markets, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, decodeMarket(R.getText(Blob.toArray(k), 0, 12), v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func order(s : State, id : Nat) : ?OrderRow { switch (RI.get(s.orders, R.key(id, 8))) { case (?v) ?decodeOrder(id, v); case null null } };
  func idsUnder(idx : RI.State, prefix : Nat) : [Nat] {
    let (lo, hi) = R.prefixRange(prefix, 8, 8);
    let out = List.empty<Nat>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, _) in page.entries.vals()) List.add(out, R.getNat(Blob.toArray(k), 8, 8));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func ordersOf(s : State, isin : Text) : [OrderRow] { Array.filterMap<Nat, OrderRow>(idsUnder(s.ordersByIsin, hash8(isin)), func(id) { order(s, id) }) };
  public func stagedOf(s : State, isin : Text) : [OrderRow] { Array.filter<OrderRow>(ordersOf(s, isin), func(o) { o.state == #staged }) };
  public func ordersOfCycle(s : State, cycle : Nat) : [OrderRow] { Array.filterMap<Nat, OrderRow>(idsUnder(s.ordersByCycle, cycle), func(id) { order(s, id) }) };
  public func cycle(s : State, id : Nat) : ?CycleRow { switch (RI.get(s.cycles, R.key(id, 8))) { case (?v) ?decodeCycle(id, v); case null null } };
  public func cyclesOf(s : State, isin : Text) : [CycleRow] { Array.filterMap<Nat, CycleRow>(idsUnder(s.cyclesByIsin, hash8(isin)), func(id) { cycle(s, id) }) };
  public func openCycleOf(s : State, isin : Text) : ?CycleRow { Array.find<CycleRow>(cyclesOf(s, isin), func(c) { c.state != #closed }) };
  public func fill(s : State, cycle_ : Nat, seq : Nat) : ?FillRow { switch (RI.get(s.fills, R.key2(cycle_, 8, seq, 8))) { case (?v) ?decodeFill(cycle_, seq, v); case null null } };
  public func fillsOf(s : State, cycle_ : Nat) : [FillRow] {
    let (lo, hi) = R.prefixRange(cycle_, 8, 8);
    let out = List.empty<FillRow>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.fills, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, decodeFill(cycle_, R.getNat(Blob.toArray(k), 8, 8), v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func fillOfInstruction(s : State, instruction : Nat) : ?FillRow {
    switch (RI.get(s.fillByInstruction, R.key(instruction, 8))) { case (?v) { let a = Blob.toArray(v); fill(s, R.getNat(a, 0, 8), R.getNat(a, 8, 8)) }; case null null }
  };
  public func orderView(r : OrderRow, trader : Principal, reference : Text) : MT.OrderView {
    { id = r.id; book = r.book; isin = r.isin; side = r.side; units = r.units; filled = r.filled; state = MT.orderStateText(r.state); cycle = if (r.cycle == 0) null else ?r.cycle; engineOrder = if (r.engineOrder == 0) null else ?r.engineOrder; trader; reference; day = r.day; lastBlock = r.lastBlock }
  };
  public func cycleView(r : CycleRow) : MT.CycleView {
    { id = r.id; isin = r.isin; day = r.day; state = MT.cycleStateText(r.state); referenceMicro = r.referenceMicro; window = if (r.hasWindow) ?r.window else null; orders = r.orders; submitted = r.submitted; refused = r.refused; fills = r.fills; instructed = r.instructed; settled = r.settled; unattributed = r.unattributed;
      clearingPrice = if (r.hasPrice) ?r.clearingPrice else null; volume = r.volume; chunks = r.chunks; nextSeq = r.nextSeq; lastBlock = r.lastBlock }
  };
  public func fillView(r : FillRow) : MT.FillView {
    { cycle = r.cycle; seq = r.seq; order = r.order; price = r.price; units = r.units; counterparty = r.counterparty; deal = if (r.deal == 0) null else ?r.deal; instruction = if (r.instruction == 0) null else ?r.instruction; tradeId = if (r.tradeId == 0) null else ?r.tradeId; state = MT.fillStateText(r.state); block = r.block }
  };
  public func status(s : State) : MT.Status { { markets = s.marketCount; orders = s.orderCount; staged = s.stagedCount; cycles = s.cycleCount; openCycles = s.openCycles; fills = s.fillCount; settled = s.settledFills; unattributed = s.unattributedCount } };

  // ─── the planners ──────────────────────────────────────────────────────────

  public type Res<X> = Result.Result<X, MT.Error>;
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planDeclare(m : MT.Market, day : Nat) : Res<MT.Event> {
    func bad<X>(reason : Text) : Res<X> { #err(#InvalidMarket({ reason })) };
    if (bytesOf(m.isin) != 12) return bad("an ISIN has twelve characters");
    if (Principal.isAnonymous(m.engine) or Principal.isAnonymous(m.sharesLedger) or Principal.isAnonymous(m.cashLedger)) return bad("the engine and the ledgers are principals");
    if (bytesOf(m.currency) != 3) return bad("a currency code has three letters");
    if (m.unitNominal == 0) return bad("a share unit carries a positive face");
    if (m.deadlineSecs == 0) return bad("the funding deadline is positive");
    if (m.participants.size() > MAX_PARTICIPANTS) return bad("at most " # Nat.toText(MAX_PARTICIPANTS) # " participants");
    var i = 0;
    while (i < m.participants.size()) {
      let p = m.participants[i];
      if (Principal.isAnonymous(p.principal)) return bad("a participant is a principal");
      let n = bytesOf(p.counterparty.name);
      if (n == 0 or n > 32) return bad("a participant's counterparty is named in 1..32 bytes");
      if (bytesOf(p.counterparty.bic) > 12 or bytesOf(p.counterparty.lei) > 20) return bad("a BIC has at most twelve characters and an LEI twenty");
      var j = 0;
      while (j < i) { if (Principal.equal(m.participants[j].principal, p.principal)) return bad("a participant is named once"); j += 1 };
      i += 1;
    };
    #ok(#marketDeclared({ market = m; day }))
  };

  public func planStage(s : State, t : MT.OrderTerms, trader : Principal, withinLimits : Bool, approver : ?Principal, day : Nat) : Res<MT.Event> {
    func bad<X>(reason : Text) : Res<X> { #err(#InvalidOrder({ reason })) };
    let ?_ = market(s, t.isin) else return #err(#NoMarket({ isin = t.isin }));
    if (t.units == 0) return bad("an order is for a positive number of units");
    if (bytesOf(t.book) == 0 or bytesOf(t.book) > 16) return bad("a book is named in 1..16 bytes");
    if (bytesOf(t.reference) == 0 or bytesOf(t.reference) > 35) return bad("the reference is 1..35 bytes");
    #ok(#orderStaged({ terms = t; trader; withinLimits; approver; day }))
  };

  public func planCancel(s : State, id : Nat, reason : Text, day : Nat) : Res<MT.Event> {
    let ?o = order(s, id) else return #err(#UnknownOrder({ order = id }));
    if (o.state != #staged) return #err(#OrderNotIn({ order = id; state = MT.orderStateText(o.state); wanted = "staged" }));
    if (bytesOf(reason) == 0 or bytesOf(reason) > 140) return #err(#InvalidOrder({ reason = "a cancellation states its reason in 1..140 bytes" }));
    #ok(#orderCancelled({ order = id; reason; day }))
  };

  /// A cycle opened over every staged order of the instrument at the reference the caller read from the feed.
  public func planOpenCycle(s : State, isin : Text, referenceMicro : Nat, referenceBlock : Nat, day : Nat) : Res<MT.Event> {
    let ?_ = market(s, isin) else return #err(#NoMarket({ isin }));
    switch (openCycleOf(s, isin)) { case (?c) return #err(#CycleOpen({ isin; cycle = c.id })); case null {} };
    let staged = stagedOf(s, isin);
    if (staged.size() == 0) return #err(#NothingStaged({ isin }));
    #ok(#cycleOpened({ isin; referenceMicro; referenceBlock; orders = Array.map<OrderRow, Nat>(staged, func(o) { o.id }); day }))
  };

  /// The engine's price per unit from a price per 100 in micro: the desk's limit, rounded against itself (a bid
  /// down, an offer up).
  public func unitPrice(referenceMicro : Nat, unitNominal : Nat, side : MT.Side) : Nat {
    let n = referenceMicro * unitNominal;
    let d = 100 * 1_000_000;
    let floor = n / d;
    switch (side) { case (#buy) floor; case (#sell) { if (floor * d == n) floor else floor + 1 } }
  };
  /// The clean price per 100 in micro the fill's cash implies, once the coupon accrued to the settlement day is
  /// taken out: the deal captured carries this price and the exact clean cost, so the settlement leg's cash is the
  /// engine's to the minor unit.
  public func impliedPrice(cashAmount : Nat, accrued : Nat, nominal : Nat) : ?(Nat, Nat) {
    if (cashAmount < accrued or nominal == 0) return null;
    let clean : Nat = cashAmount - accrued;
    ?(TreasuryMath.roundNat(TreasuryMath.q(clean * 100 * 1_000_000, nominal)), clean)
  };

  /// What the cycle's driver does next, from the rows alone.
  public type NextStep = {
    #submit : { order : OrderRow; limit : Nat };
    #clear : { window : ?Nat };
    #readFills : { window : Nat; from : Nat };
    #captureFill : { fill : FillRow };
    #instructFill : { fill : FillRow };
    #settleFill : { fill : FillRow };
    #withdraw : { order : OrderRow };
    #close;
    #nothing : { reason : Text };
  };
  public func nextStep(s : State, c : CycleRow, m : MarketRow) : NextStep {
    switch (c.state) {
      case (#open) {
        for (o in ordersOfCycle(s, c.id).vals()) { if (o.state == #staged) return #submit({ order = o; limit = unitPrice(c.referenceMicro, m.unitNominal, o.side) }) };
        #clear({ window = null })
      };
      // the first call clears the engine's open window; the later ones resume the chunks of the window it named
      case (#clearing) #clear({ window = if (c.chunks > 0) ?c.window else null });
      case (#settling) {
        if (not c.readDone) return #readFills({ window = c.window; from = c.nextSeq });
        for (f in fillsOf(s, c.id).vals()) {
          switch (f.state) {
            case (#recorded) return #captureFill({ fill = f });
            case (#unattributed) { if (participant(s, c.isin, f.counterparty) != null) return #captureFill({ fill = f }) };
            // a fill captured but not instructed (the settlement cycle was not open, or the venue not declared) is instructed once it can be
            case (#captured) return #instructFill({ fill = f });
            case (#instructed) { if (f.tradeId == 0) return #settleFill({ fill = f }) };
            case (_) {};
          };
        };
        for (o in ordersOfCycle(s, c.id).vals()) { if ((o.state == #submitted or o.state == #partlyFilled) and not o.withdrawn and o.engineOrder != 0) return #withdraw({ order = o }) };
        if (c.unattributed > 0) return #nothing({ reason = Nat.toText(c.unattributed) # " fills against principals the market does not name; declare the participants" });
        #close
      };
      case (#closed) #nothing({ reason = "the cycle is closed" });
    }
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  func putMarket(s : State, r : MarketRow) { ignore RI.put(s.markets, R.textKey(r.isin, 12), encodeMarket(r)) };
  func putOrder(s : State, r : OrderRow) {
    ignore RI.put(s.orders, R.key(r.id, 8), encodeOrder(r));
    ignore RI.put(s.ordersByIsin, R.key2(hash8(r.isin), 8, r.id, 8), Blob.fromArray([orderStateCode(r.state)]));
    if (r.cycle != 0) ignore RI.put(s.ordersByCycle, R.key2(r.cycle, 8, r.id, 8), Blob.fromArray([orderStateCode(r.state)]));
  };
  func putCycle(s : State, r : CycleRow) {
    ignore RI.put(s.cycles, R.key(r.id, 8), encodeCycle(r));
    ignore RI.put(s.cyclesByIsin, R.key2(hash8(r.isin), 8, r.id, 8), Blob.fromArray([cycleStateCode(r.state)]));
  };
  func putFill(s : State, r : FillRow) { ignore RI.put(s.fills, R.key2(r.cycle, 8, r.seq, 8), encodeFill(r)) };
  func withOrder(s : State, id : Nat, block : Nat, f : OrderRow -> OrderRow) { switch (order(s, id)) { case (?o) { let before = o.state; let after = f({ o with lastBlock = block }); if (before == #staged and after.state != #staged and s.stagedCount > 0) s.stagedCount -= 1; putOrder(s, after) }; case null {} } };
  func withCycle(s : State, id : Nat, block : Nat, f : CycleRow -> CycleRow) { switch (cycle(s, id)) { case (?c) { let after = f({ c with lastBlock = block }); if (c.state != #closed and after.state == #closed and s.openCycles > 0) s.openCycles -= 1; putCycle(s, after) }; case null {} } };
  func withFill(s : State, cycle_ : Nat, seq : Nat, f : FillRow -> FillRow) { switch (fill(s, cycle_, seq)) { case (?x) putFill(s, f(x)); case null {} } };

  public func fold(s : State, block : Nat, ev : MT.Event) {
    switch (ev) {
      case (#marketDeclared(x)) {
        let m = x.market;
        if (market(s, m.isin) == null) s.marketCount += 1;
        putMarket(s, { isin = m.isin; engine = m.engine; sharesLedger = m.sharesLedger; cashLedger = m.cashLedger; currency = m.currency; unitNominal = m.unitNominal; deadlineSecs = m.deadlineSecs; participants = m.participants.size(); lastBlock = block });
        for (p in m.participants.vals()) ignore RI.put(s.participants, participantKey(m.isin, p.principal), encodeParticipant({ isin = m.isin; principal = p.principal; counterparty = p.counterparty }));
      };
      case (#orderStaged(x)) {
        putOrder(s, { id = block; isin = x.terms.isin; book = x.terms.book; side = x.terms.side; units = x.terms.units; filled = 0; state = #staged; cycle = 0; engineOrder = 0; traderHash = principalHash(x.trader); day = x.day; lastBlock = block; withdrawn = false });
        s.orderCount += 1; s.stagedCount += 1;
      };
      case (#orderCancelled(x)) withOrder(s, x.order, block, func(o) { { o with state = #cancelled } });
      case (#cycleOpened(x)) {
        putCycle(s, { id = block; isin = x.isin; day = x.day; state = #open; referenceMicro = x.referenceMicro; referenceBlock = x.referenceBlock; window = 0; hasWindow = false;
                      orders = x.orders.size(); submitted = 0; refused = 0; fills = 0; instructed = 0; settled = 0; unattributed = 0; clearingPrice = 0; hasPrice = false; volume = 0; chunks = 0; nextSeq = 0; readDone = false; lastBlock = block });
        for (id in x.orders.vals()) { switch (order(s, id)) { case (?o) putOrder(s, { o with cycle = block; lastBlock = block }); case null {} } };
        s.cycleCount += 1; s.openCycles += 1;
      };
      case (#orderSubmitted(x)) {
        withOrder(s, x.order, block, func(o) { { o with state = #submitted; engineOrder = x.engineOrder } });
        withCycle(s, x.cycle, block, func(c) { let n = c.submitted + 1; { c with submitted = n; window = x.window; hasWindow = true; state = if (n + c.refused >= c.orders) #clearing else c.state } });
      };
      case (#orderRefused(x)) {
        withOrder(s, x.order, block, func(o) { { o with state = #refused } });
        withCycle(s, x.cycle, block, func(c) { let n = c.refused + 1; { c with refused = n; state = if (n + c.submitted >= c.orders) #clearing else c.state } });
      };
      case (#clearAdvanced(x)) {
        withCycle(s, x.cycle, block, func(c) {
          { c with window = x.window; hasWindow = true; chunks = x.chunks; volume = x.targetVolume; clearingPrice = switch (x.clearingPrice) { case (?p) p; case null 0 }; hasPrice = x.clearingPrice != null; state = if (x.complete) #settling else #clearing }
        });
      };
      case (#filled(x)) {
        putFill(s, { cycle = x.cycle; seq = x.seq; order = x.order; price = x.price; units = x.units; cpHash = principalHash(x.counterparty); counterparty = x.counterparty; deal = 0; instruction = 0; tradeId = 0; state = #recorded; block });
        withOrder(s, x.order, block, func(o) { let f = o.filled + x.units; { o with filled = f; state = if (f >= o.units) #filled else #partlyFilled } });
        withCycle(s, x.cycle, block, func(c) { { c with fills = c.fills + 1; nextSeq = Nat.max(c.nextSeq, x.seq + 1) } });
        s.fillCount += 1;
      };
      case (#fillUnattributed(x)) {
        withFill(s, x.cycle, x.seq, func(f) { { f with state = #unattributed } });
        withCycle(s, x.cycle, block, func(c) { { c with unattributed = c.unattributed + 1 } });
        s.unattributedCount += 1;
      };
      case (#fillCaptured(x)) {
        switch (fill(s, x.cycle, x.seq)) {
          case (?f) {
            if (f.state == #unattributed) { withCycle(s, x.cycle, block, func(c) { { c with unattributed = if (c.unattributed > 0) c.unattributed - 1 else 0 } }); if (s.unattributedCount > 0) s.unattributedCount -= 1 };
            putFill(s, { f with deal = x.deal; state = #captured });
          };
          case null {};
        };
      };
      case (#fillInstructed(x)) {
        withFill(s, x.cycle, x.seq, func(f) { { f with instruction = x.instruction; state = #instructed } });
        ignore RI.put(s.fillByInstruction, R.key(x.instruction, 8), R.key2(x.cycle, 8, x.seq, 8));
        withCycle(s, x.cycle, block, func(c) { { c with instructed = c.instructed + 1 } });
      };
      case (#fillTradeSet(x)) withFill(s, x.cycle, x.seq, func(f) { { f with tradeId = x.tradeId } });
      case (#fillsRead(x)) withCycle(s, x.cycle, block, func(c) { { c with nextSeq = Nat.max(c.nextSeq, x.through); readDone = x.complete } });
      case (#orderWithdrawn(x)) withOrder(s, x.order, block, func(o) { { o with state = if (o.filled > 0) #partlyFilled else #unfilled; withdrawn = true } });
      case (#callRefused(x)) withCycle(s, x.cycle, block, func(c) { c });
      case (#cycleClosed(x)) withCycle(s, x.cycle, block, func(c) { { c with state = #closed } });
    }
  };

  /// The settlement family's events the fills follow: a fill's instruction settled marks the fill.
  public func observeSettlement(s : State, block : Nat, ev : ST.Event) {
    switch (ev) {
      case (#settled(x)) {
        switch (fillOfInstruction(s, x.instruction)) {
          case (?f) { if (f.state != #settled) { putFill(s, { f with state = #settled; tradeId = x.tradeId }); withCycle(s, f.cycle, block, func(c) { { c with settled = c.settled + 1 } }); s.settledFills += 1 } };
          case null {};
        };
      };
      case (_) {};
    }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    for ((idx, width) in [(s.markets, 12), (s.participants, 16), (s.orders, 8), (s.ordersByIsin, 16), (s.ordersByCycle, 16), (s.cycles, 8), (s.cyclesByIsin, 16), (s.fills, 16), (s.fillByInstruction, 8)].vals()) {
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
    w.nat(s.marketCount); w.nat(s.orderCount); w.nat(s.stagedCount); w.nat(s.cycleCount); w.nat(s.openCycles); w.nat(s.fillCount); w.nat(s.settledFills); w.nat(s.unattributedCount);
  };
}
