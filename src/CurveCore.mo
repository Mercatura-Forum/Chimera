/// CurveCore.mo: the bootstrap and the fold of built curves. Every discount factor is an eighteen-decimal fixed
/// point on the kernel's arithmetic (Manticore's `TreasuryMath`: rationals for the present values, the fixed
/// point with its exp and ln for the log-linear interpolation), the instruments are solved in maturity order,
/// and a swap's factor is found by a bracketed secant search on the integer grid of the fixed point, so the result
/// is the one integer at which the instrument's value changes sign: the same for every run and for the twin.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";
import Sha256 "mo:sha2/Sha256";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import DC "mo:manticore/DayCount";
import M "mo:manticore/TreasuryMath";
import Products "mo:manticore/Products";
import TT "mo:manticore/TreasuryTypes";
import TreasuryCore "mo:manticore/TreasuryCore";

import CT "CurveTypes";

module {

  public let SPEC_ROW_BYTES : Nat = 105;
  public let BUILD_ROW_BYTES : Nat = 24;
  public let NODE_ROW_BYTES : Nat = 20;
  public let INDEX_ROW_BYTES : Nat = 72;
  public let MARK_ROW_BYTES : Nat = 72;
  public let MAX_QUOTES : Nat = 64;
  public let MAX_ITERATIONS : Nat = 64;
  /// The largest discount factor the search admits: two, so a negative rate has room.
  let DF_MAX : Nat = 2_000_000_000_000_000_000;
  let ONE : Nat = 1_000_000_000_000_000_000;

  type Res<X> = Result.Result<X, CT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidSpec({ reason })) };

  // ─── rows ──────────────────────────────────────────────────────────────────

  public type SpecRow = { id : Text; currency : Text; role : CT.Role; dayCount : DC.Convention; interpolation : CT.Interpolation; discountCurve : ?Text; quotes : Nat; block : Nat };
  public type BuildRow = { id : Text; day : Nat; nodes : Nat; iterations : Nat; block : Nat };
  public type IndexRow = { index : Text; projection : Text; discount : Text; day : Nat; block : Nat };
  public type MarkRow = { deal : Nat; day : Nat; discount : Text; projection : Text; value : Int; block : Nat };

  func roleCode(r : CT.Role) : (Nat8, Nat, Text) { switch (r) { case (#discount) (1, 0, ""); case (#projection(p)) (2, p.indexMonths, ""); case (#collateralDiscount(c)) (3, 0, c.collateral) } };
  func roleOf(c : Nat8, months : Nat, collateral : Text) : CT.Role { if (c == 2) #projection({ indexMonths = months }) else if (c == 3) #collateralDiscount({ collateral }) else #discount };
  func convCode(c : DC.Convention) : (Nat8, Nat) {
    switch (c) { case (#a001_ActActIcma(x)) (1, x.couponsPerYear); case (#a003_Act360) (3, 0); case (#a004_Act365Fixed) (4, 0); case (#a005_ActActIsda) (5, 0); case (#a006_Thirty360Isda) (6, 0); case (#a007_ThirtyE360) (7, 0); case (#a011_Thirty365) (11, 0) }
  };
  func convOf(c : Nat8, n : Nat) : DC.Convention {
    switch (c) { case 1 #a001_ActActIcma({ couponsPerYear = n }); case 4 #a004_Act365Fixed; case 5 #a005_ActActIsda; case 6 #a006_Thirty360Isda; case 7 #a007_ThirtyE360; case 11 #a011_Thirty365; case _ #a003_Act360 }
  };
  func interpCode(i : CT.Interpolation) : Nat8 { switch (i) { case (#logLinearDiscount) 1; case (#linearZero) 2 } };
  func interpOf(c : Nat8) : CT.Interpolation { if (c == 2) #linearZero else #logLinearDiscount };
  func putInt(b : R.Buf, v : Int, width : Nat) { R.putBool(b, v < 0); R.putNat(b, Int.abs(v), width) };
  func getInt(a : [Nat8], off : Nat, width : Nat) : Int { let neg = R.getBool(a, off); let m = R.getNat(a, off + 1, width); if (neg) -m else m };

  func encodeSpec(r : SpecRow) : Blob {
    let b = R.buf();
    let (rc, rm, rcol) = roleCode(r.role); let (cc, cn) = convCode(r.dayCount);
    R.putText(b, r.currency, 3); R.putByte(b, rc); R.putNat(b, rm, 2); R.putByte(b, cc); R.putNat(b, cn, 2); R.putByte(b, interpCode(r.interpolation));
    R.putBool(b, r.discountCurve != null); R.putText(b, switch (r.discountCurve) { case (?d) d; case null "" }, 32); R.putNat(b, r.quotes, 2); R.putNat(b, r.block, 8);
    R.putText(b, rcol, 3);
    var pad = 0; while (pad < 49) { R.putByte(b, 0); pad += 1 };
    R.done(b, SPEC_ROW_BYTES)
  };
  func decodeSpec(id : Text, v : Blob) : SpecRow {
    let a = Blob.toArray(v);
    { id; currency = R.getText(a, 0, 3); role = roleOf(a[3], R.getNat(a, 4, 2), R.getText(a, 53, 3)); dayCount = convOf(a[6], R.getNat(a, 7, 2)); interpolation = interpOf(a[9]);
      discountCurve = if (R.getBool(a, 10)) ?R.getText(a, 11, 32) else null; quotes = R.getNat(a, 43, 2); block = R.getNat(a, 45, 8) }
  };
  func encodeBuild(r : BuildRow) : Blob { let b = R.buf(); R.putNat(b, r.nodes, 4); R.putNat(b, r.iterations, 4); R.putNat(b, r.block, 8); R.putNat(b, r.day, 4); R.putNat(b, 0, 4); R.done(b, BUILD_ROW_BYTES) };
  func decodeBuild(id : Text, day : Nat, v : Blob) : BuildRow { let a = Blob.toArray(v); { id; day; nodes = R.getNat(a, 0, 4); iterations = R.getNat(a, 4, 4); block = R.getNat(a, 8, 8) } };
  func encodeNode(n : CT.Node) : Blob { let b = R.buf(); R.putNat(b, n.days, 4); R.putNat(b, n.df, 16); R.done(b, NODE_ROW_BYTES) };
  func decodeNode(v : Blob) : CT.Node { let a = Blob.toArray(v); { days = R.getNat(a, 0, 4); df = R.getNat(a, 4, 16) } };
  func encodeIndex(r : IndexRow) : Blob { let b = R.buf(); R.putText(b, r.projection, 32); R.putText(b, r.discount, 32); R.putNat(b, r.day, 4); R.putNat(b, r.block, 4); R.done(b, INDEX_ROW_BYTES) };
  func decodeIndex(index : Text, v : Blob) : IndexRow { let a = Blob.toArray(v); { index; projection = R.getText(a, 0, 32); discount = R.getText(a, 32, 32); day = R.getNat(a, 64, 4); block = R.getNat(a, 68, 4) } };
  func encodeMark(r : MarkRow) : Blob { let b = R.buf(); R.putText(b, r.discount, 24); R.putText(b, r.projection, 24); putInt(b, r.value, 15); R.putNat(b, r.block, 8); R.done(b, MARK_ROW_BYTES) };
  func decodeMark(deal : Nat, day : Nat, v : Blob) : MarkRow { let a = Blob.toArray(v); { deal; day; discount = R.getText(a, 0, 24); projection = R.getText(a, 24, 24); value = getInt(a, 48, 15); block = R.getNat(a, 64, 8) } };

  public type State = {
    specs : RI.State;      // id(32) -> spec row
    builds : RI.State;     // id(32) ‖ day(4) -> build row
    nodes : RI.State;      // id(32) ‖ day(4) ‖ i(2) -> node
    latest : RI.State;     // id(32) -> day(4): the latest build of a curve
    indexes : RI.State;    // index(32) -> index row
    marks : RI.State;      // deal(8) ‖ day(4) -> mark row
    var specCount : Nat;
    var buildCount : Nat;
    var nodeCount : Nat;
    var indexCount : Nat;
    var markCount : Nat;
  };

  public func newState(arena : RI.Arena) : State {
    {
      specs = RI.newStateIn(arena, { keyBytes = 32; valBytes = SPEC_ROW_BYTES });
      builds = RI.newStateIn(arena, { keyBytes = 36; valBytes = BUILD_ROW_BYTES });
      nodes = RI.newStateIn(arena, { keyBytes = 38; valBytes = NODE_ROW_BYTES });
      latest = RI.newStateIn(arena, { keyBytes = 32; valBytes = 4 });
      indexes = RI.newStateIn(arena, { keyBytes = 32; valBytes = INDEX_ROW_BYTES });
      marks = RI.newStateIn(arena, { keyBytes = 12; valBytes = MARK_ROW_BYTES });
      var specCount = 0; var buildCount = 0; var nodeCount = 0; var indexCount = 0; var markCount = 0;
    }
  };

  func buildKey(id : Text, day : Nat) : Blob { Blob.fromArray(Array.concat(Blob.toArray(R.textKey(id, 32)), Blob.toArray(R.key(day, 4)))) };
  func nodeKey(id : Text, day : Nat, i : Nat) : Blob { Blob.fromArray(Array.concat(Blob.toArray(buildKey(id, day)), Blob.toArray(R.key(i, 2)))) };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func spec(s : State, id : Text) : ?SpecRow { switch (RI.get(s.specs, R.textKey(id, 32))) { case (?v) ?decodeSpec(id, v); case null null } };
  public func build(s : State, id : Text, day : Nat) : ?BuildRow { switch (RI.get(s.builds, buildKey(id, day))) { case (?v) ?decodeBuild(id, day, v); case null null } };
  public func latestDay(s : State, id : Text) : ?Nat { switch (RI.get(s.latest, R.textKey(id, 32))) { case (?v) ?R.getNat(Blob.toArray(v), 0, 4); case null null } };
  /// The nodes of a build, in tenor order.
  public func nodesOf(s : State, id : Text, day : Nat) : [CT.Node] {
    let ?b = build(s, id, day) else return [];
    Array.tabulate<CT.Node>(b.nodes, func(i) { switch (RI.get(s.nodes, nodeKey(id, day, i))) { case (?v) decodeNode(v); case null ({ days = 0; df = 0 }) } })
  };
  /// The curve as it stands on a day: the build of that day, else the latest build on or before it.
  public func curveOn(s : State, id : Text, day : Nat) : ?(SpecRow, BuildRow, [CT.Node]) {
    let ?sp = spec(s, id) else return null;
    let ?ld = latestDay(s, id) else return null;
    let d = if (build(s, id, day) != null) day else if (ld <= day) ld else return null;
    let ?b = build(s, id, d) else return null;
    ?(sp, b, nodesOf(s, id, d))
  };
  public func index(s : State, name : Text) : ?IndexRow { switch (RI.get(s.indexes, R.textKey(name, 32))) { case (?v) ?decodeIndex(name, v); case null null } };
  public func mark(s : State, deal : Nat, day : Nat) : ?MarkRow { switch (RI.get(s.marks, R.key2(deal, 8, day, 4))) { case (?v) ?decodeMark(deal, day, v); case null null } };
  public func status(s : State) : CT.Status { { specs = s.specCount; builds = s.buildCount; nodes = s.nodeCount; indexes = s.indexCount; swapMarks = s.markCount } };
  public func specView(s : State, sp : SpecRow, day : Nat) : ?CT.CurveView {
    let ?(_, b, ns) = curveOn(s, sp.id, day) else return null;
    ?{ id = sp.id; currency = sp.currency; role = CT.roleText(sp.role); dayCount = DC.isoCode(sp.dayCount); interpolation = CT.interpolationText(sp.interpolation); discountCurve = sp.discountCurve; day = b.day; quotes = sp.quotes; nodes = ns; iterations = b.iterations; block = b.block }
  };
  public func indexView(r : IndexRow) : CT.IndexView { { index = r.index; projection = r.projection; discount = r.discount; day = r.day } };

  // ─── interpolation ─────────────────────────────────────────────────────────

  /// A built curve as a function of the tenor in days: exact rationals from the fixed-point nodes.
  public type Curve = { day : Nat; dayCount : DC.Convention; interpolation : CT.Interpolation; nodes : [CT.Node] };

  func qOfFixed(df : Nat) : M.Q { M.q(df, ONE) };
  /// The discount factor at a tenor: one at zero, the node's at a node, interpolated between nodes by the
  /// curve's scheme, refused beyond the last node.
  public func discountAt(c : Curve, days : Nat) : Result.Result<M.Q, Nat> {
    if (days == 0) return #ok(M.ofInt(1));
    let n = c.nodes.size();
    if (n == 0 or days > c.nodes[n - 1].days) return #err(if (n == 0) 0 else c.nodes[n - 1].days);
    var i = 0;
    while (i < n) {
      let nd = c.nodes[i];
      if (days == nd.days) return #ok(qOfFixed(nd.df));
      if (days < nd.days) {
        let (t0, df0) : (Nat, Nat) = if (i == 0) (0, ONE) else (c.nodes[i - 1].days, c.nodes[i - 1].df);
        return #ok(between(c, t0, df0, nd.days, nd.df, days));
      };
      i += 1;
    };
    #ok(qOfFixed(c.nodes[n - 1].df))
  };
  /// A node's simple zero rate on the fixed grid: `(1/DF - 1) / fraction` in eighteen decimals, rounded half-even.
  func zeroFixed(c : Curve, days : Nat, df : Nat) : Int {
    let frac = M.ofFraction(DC.fraction(c.dayCount, c.day, c.day + days));
    if (frac.n == 0) return 0;
    M.roundHalfEven(M.mul(M.div(M.sub(M.div(M.ofInt(1), qOfFixed(df)), M.ofInt(1)), frac), M.ofNat(ONE)))
  };
  /// Between two nodes: log-linear on the factors (the fixed point's ln and exp), or linear on the simple zero
  /// rates with the factor recomputed from the interpolated rate. Both schemes interpolate on the fixed grid and
  /// return a factor on it, so a bootstrap's sums keep bounded denominators whichever scheme the curve declares.
  func between(c : Curve, t0 : Nat, df0 : Nat, t1 : Nat, df1 : Nat, t : Nat) : M.Q {
    switch (c.interpolation) {
      case (#logLinearDiscount) {
        let l0 = M.fln(df0); let l1 = M.fln(df1);
        let l = (l0 * (t1 - t : Int) + l1 * (t - t0 : Int)) / (t1 - t0 : Int);
        qOfFixed(Int.abs(M.fexp(l)))
      };
      case (#linearZero) {
        let z0 = if (t0 == 0) zeroFixed(c, t1, df1) else zeroFixed(c, t0, df0);
        let z1 = zeroFixed(c, t1, df1);
        let z = M.roundHalfEven(M.q(z0 * (t1 - t : Int) + z1 * (t - t0 : Int), t1 - t0));
        let frac = M.ofFraction(DC.fraction(c.dayCount, c.day, c.day + t));
        let df = M.roundHalfEven(M.div(M.ofNat(ONE), M.add(M.ofInt(1), M.mul(M.q(z, ONE), frac))));
        qOfFixed(Int.abs(df))
      };
    }
  };
  /// The simple forward rate in basis points between two tenors: `(DF(t₀)/DF(t₁) − 1) / fraction × 10⁴`.
  public func forwardBps(c : Curve, days0 : Nat, days1 : Nat) : Result.Result<M.Q, Nat> {
    let d0 = switch (discountAt(c, days0)) { case (#ok(x)) x; case (#err(e)) return #err(e) };
    let d1 = switch (discountAt(c, days1)) { case (#ok(x)) x; case (#err(e)) return #err(e) };
    let frac = M.ofFraction(DC.fraction(c.dayCount, c.day + days0, c.day + days1));
    if (frac.n == 0) return #ok(M.zero());
    #ok(M.mul(M.div(M.sub(M.div(d0, d1), M.ofInt(1)), frac), M.ofNat(M.BPS)))
  };
  /// The treasury domain's zero curve derived from a built curve: at every node the simple rate on the
  /// treasury's own convention (actual over 360, the one its `discountFactor` applies) that reproduces the node's
  /// factor, rounded half-even to the whole basis point the published curves carry. The build's day count and
  /// interpolation are the curve's; the treasury's `#zeroRates` form has one convention and interpolates the rate
  /// linearly, so the two read the same factor at every node and each its own between nodes.
  public func derivedZeroPoints(c : Curve) : [(Nat, Int)] {
    Array.map<CT.Node, (Nat, Int)>(c.nodes, func(n) {
      if (n.days == 0) return (0, 0);
      let r = M.mul(M.div(M.sub(M.div(M.ofInt(1), qOfFixed(n.df)), M.ofInt(1)), M.q(n.days, 360)), M.ofNat(M.BPS));
      (n.days, M.roundHalfEven(r))
    })
  };

  // ─── the bootstrap ─────────────────────────────────────────────────────────

  /// The tenor an instrument's maturity is at, and its text for a refusal.
  func maturityDays(day : Nat, i : CT.Instrument) : Nat {
    switch (i) {
      case (#deposit(x)) x.days;
      case (#fra(x)) x.endDays;
      case (#future(x)) x.endDays;
      case (#swap(x)) Products.addMonths(day, x.months) - day;
      case (#ois(x)) Products.addMonths(day, x.months) - day;
      case (#basis(x)) Products.addMonths(day, x.months) - day;
      case (#fxSwap(x)) x.days;
    }
  };

  public func validateSpec(sp : CT.Spec) : Res<()> {
    let n = Text.encodeUtf8(sp.id).size();
    if (n == 0 or n > 32) return bad("a curve id is 1..32 bytes");
    if (Text.encodeUtf8(sp.currency).size() != 3) return bad("a currency code has three letters");
    if (sp.quotes.size() == 0) return bad("a curve is built from at least one instrument");
    if (sp.quotes.size() > MAX_QUOTES) return bad("a curve is built from at most " # Nat.toText(MAX_QUOTES) # " instruments");
    switch (sp.role, sp.discountCurve) {
      case (#projection(_), null) return bad("a projection curve is built with a discount curve");
      case (#projection(p), _) { if (p.indexMonths == 0 or p.indexMonths > 12) return bad("an index tenor is 1..12 months") };
      case (#discount, ?_) return bad("a discount curve discounts itself");
      case (#collateralDiscount(_), null) return bad("a collateral discount curve is built with the collateral currency's discount curve");
      case (#collateralDiscount(c), _) { if (Text.encodeUtf8(c.collateral).size() != 3) return bad("a collateral currency code has three letters"); if (Text.equal(c.collateral, sp.currency)) return bad("a collateral discount curve is for a currency other than its collateral") };
      case (_) {};
    };
    for (q in sp.quotes.vals()) {
      if (q.source.size() != 32) return bad("a quote's source is a sha256");
      switch (q.instrument) {
        case (#deposit(x)) { if (x.days == 0) return bad("a deposit has a positive tenor") };
        case (#fra(x)) { if (x.endDays <= x.startDays) return bad("a forward rate agreement ends after it starts") };
        case (#future(x)) { if (x.endDays <= x.startDays) return bad("a future's period ends after it starts") };
        case (#swap(x)) { if (x.months == 0 or x.fixedMonths == 0 or x.floatMonths == 0 or x.months % x.fixedMonths != 0 or x.months % x.floatMonths != 0) return bad("a swap's tenor is a whole number of its fixed and floating periods"); switch (sp.role) { case (#projection(p)) { if (x.floatMonths != p.indexMonths) return bad("a swap on a projection curve floats at the index's tenor") }; case (#discount) {}; case (#collateralDiscount(_)) return bad("a collateral discount curve is built from FX swaps") } };
        case (#ois(x)) { if (x.months == 0 or x.fixedMonths == 0 or x.months % x.fixedMonths != 0) return bad("an overnight index swap's tenor is a whole number of its fixed periods"); if (sp.role != #discount) return bad("an overnight index swap builds a discount curve") };
        case (#basis(x)) { if (x.months == 0) return bad("a basis swap has a tenor"); if (Text.equal(x.reference, sp.id)) return bad("a basis swap's reference is another curve"); switch (sp.role) { case (#projection(p)) { if (x.months % p.indexMonths != 0) return bad("a basis swap's tenor is a whole number of the index's periods") }; case (_) return bad("a basis swap builds a projection curve") } };
        case (#fxSwap(x)) { if (x.days == 0) return bad("an FX swap has a positive tenor"); if (x.spotMicro == 0) return bad("an FX swap is quoted against a spot"); if (x.spotMicro + q.value <= 0) return bad("an FX swap's forward is positive"); switch (sp.role) { case (#collateralDiscount(_)) {}; case (_) return bad("an FX swap builds a collateral discount curve") } };
      };
      switch (sp.role, q.instrument) {
        case (#collateralDiscount(_), #fxSwap(_)) {};
        case (#collateralDiscount(_), _) return bad("a collateral discount curve is built from FX swaps");
        case (_) {};
      };
    };
    #ok(())
  };

  /// The quotes in maturity order, strictly increasing.
  func ordered(day : Nat, quotes : [CT.Quote]) : Res<[CT.Quote]> {
    let sorted = Array.sort<CT.Quote>(quotes, func(a, b) { Nat.compare(maturityDays(day, a.instrument), maturityDays(day, b.instrument)) });
    var i = 1;
    while (i < sorted.size()) {
      if (maturityDays(day, sorted[i].instrument) == maturityDays(day, sorted[i - 1].instrument)) return bad("two instruments mature at " # Nat.toText(maturityDays(day, sorted[i].instrument)) # " days");
      i += 1;
    };
    #ok(sorted)
  };

  /// The value of a swap instrument with the maturity's factor `x` tried: fixed leg less floating leg for a
  /// notional of one, on the discount curve `disc` and the projection `proj` (each the curve so far extended by
  /// the node tried where it is the one being built). Positive when the rate quoted is below par for the factor.
  public func swapValueOf(day : Nat, conv : DC.Convention, rateBps : Int, months : Nat, fixedMonths : Nat, floatMonths : Nat, ois : Bool, disc : Curve, proj : Curve) : Result.Result<M.Q, CT.Error> { swapValue(day, conv, rateBps, months, fixedMonths, floatMonths, ois, disc, proj) };
  func swapValue(day : Nat, conv : DC.Convention, rateBps : Int, months : Nat, fixedMonths : Nat, floatMonths : Nat, ois : Bool, disc : Curve, proj : Curve) : Result.Result<M.Q, CT.Error> {
    let maturity = Products.addMonths(day, months);
    var fixed = M.zero();
    for (p in M.swapPeriods(day, maturity, fixedMonths).vals()) {
      let df = switch (discountAt(disc, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "discount"; days = p.end - day; last })) };
      fixed := M.add(fixed, M.mul(M.mul(df, M.ofFraction(DC.fraction(conv, p.start, p.end))), M.q(rateBps, M.BPS)));
    };
    var floating = M.zero();
    if (ois) {
      for (p in M.swapPeriods(day, maturity, fixedMonths).vals()) {
        let d0 = switch (discountAt(disc, p.start - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "discount"; days = p.start - day; last })) };
        let d1 = switch (discountAt(disc, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "discount"; days = p.end - day; last })) };
        floating := M.add(floating, M.sub(d0, d1));
      };
    } else {
      for (p in M.swapPeriods(day, maturity, floatMonths).vals()) {
        let df = switch (discountAt(disc, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "discount"; days = p.end - day; last })) };
        let fwd = switch (forwardBps(proj, p.start - day, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "projection"; days = p.end - day; last })) };
        floating := M.add(floating, M.mul(M.mul(df, M.ofFraction(DC.fraction(conv, p.start, p.end))), M.div(fwd, M.ofNat(M.BPS))));
      };
    };
    #ok(M.sub(fixed, floating))
  };

  /// The value of a tenor basis swap with the maturity's factor tried in the curve being built: the reference
  /// index's leg less the built index's leg for a notional of one, both on the discount curve, the spread on the
  /// leg the instrument names. Positive when the spread quoted is above what the factor implies, and increasing
  /// with the factor, since a higher factor at the maturity is a lower forward for the built index's last period.
  public func basisValueOf(day : Nat, conv : DC.Convention, spreadBps : Int, months : Nat, spreadOnReference : Bool, disc : Curve, built : Curve, builtMonths : Nat, reference : Curve, referenceMonths : Nat) : Result.Result<M.Q, CT.Error> {
    basisValue(day, conv, spreadBps, months, spreadOnReference, disc, built, builtMonths, reference, referenceMonths)
  };
  func basisValue(day : Nat, conv : DC.Convention, spreadBps : Int, months : Nat, spreadOnReference : Bool, disc : Curve, built : Curve, builtMonths : Nat, reference : Curve, referenceMonths : Nat) : Result.Result<M.Q, CT.Error> {
    let maturity = Products.addMonths(day, months);
    func leg(proj : Curve, tenorMonths : Nat, spread : Int, which : Text) : Result.Result<M.Q, CT.Error> {
      var pv = M.zero();
      for (p in M.swapPeriods(day, maturity, tenorMonths).vals()) {
        let df = switch (discountAt(disc, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = "discount"; days = p.end - day; last })) };
        let fwd = switch (forwardBps(proj, p.start - day, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = which; days = p.end - day; last })) };
        pv := M.add(pv, M.mul(M.mul(df, M.ofFraction(DC.fraction(conv, p.start, p.end))), M.div(M.add(fwd, M.ofInt(spread)), M.ofNat(M.BPS))));
      };
      #ok(pv)
    };
    let ref = switch (leg(reference, referenceMonths, if (spreadOnReference) spreadBps else 0, "reference")) { case (#ok(v)) v; case (#err(e)) return #err(e) };
    let own = switch (leg(built, builtMonths, if (spreadOnReference) 0 else spreadBps, "projection")) { case (#ok(v)) v; case (#err(e)) return #err(e) };
    #ok(M.sub(ref, own))
  };

  /// The build: every instrument in maturity order, its factor exact where the instrument is linear in it and
  /// found by a bracketed search on the integer grid where it is not; the discount curve given for a projection
  /// build is read as it stands. Returns the nodes and the residual evaluations the searches took.
  public func bootstrap(sp : CT.Spec, day : Nat, discount : ?Curve, referenceOf : CT.CurveId -> ?(Curve, Nat)) : Res<([CT.Node], Nat)> {
    switch (validateSpec(sp)) { case (#err(e)) return #err(e); case (#ok(_)) {} };
    let quotes = switch (ordered(day, sp.quotes)) { case (#err(e)) return #err(e); case (#ok(q)) q };
    let built = List.empty<CT.Node>();
    var iterations = 0;
    func soFar(extra : ?CT.Node) : Curve {
      let ns = List.toArray(built);
      { day; dayCount = sp.dayCount; interpolation = sp.interpolation; nodes = switch (extra) { case (?n) Array.concat(ns, [n]); case null ns } }
    };
    func fixedOf(q : M.Q) : Nat { let v = M.roundHalfEven(M.mul(q, M.ofNat(ONE))); if (v < 0) 0 else Int.abs(v) };
    for (q in quotes.vals()) {
      let t = maturityDays(day, q.instrument);
      let name = CT.instrumentText(q.instrument);
      let last = switch (List.last(built)) { case (?n) n.days; case null 0 };
      let lastDf = switch (List.last(built)) { case (?n) n.df; case null ONE };
      // the solver's first guess: the previous node's factor carried to this maturity at the quoted rate (at
      // the reference curve's forward for a basis swap, whose quote is a spread)
      let carry : Int = switch (q.instrument) {
        case (#basis(x)) { switch (referenceOf(x.reference)) { case (?(rc, _)) { switch (forwardBps(rc, last, t)) { case (#ok(f)) M.roundHalfEven(f); case (#err(_)) 0 } }; case null 0 } };
        case (_) q.value;
      };
      let guess = fixedOf(M.div(qOfFixed(lastDf), M.add(M.ofInt(1), M.div(M.mul(M.q(carry, 1), M.ofFraction(DC.fraction(sp.dayCount, day + last, day + t))), M.ofNat(M.BPS)))));
      switch (q.instrument) {
        case (#deposit(x)) {
          // DF(t) = 1 / (1 + r · fraction(day, day + t))
          let frac = M.ofFraction(DC.fraction(sp.dayCount, day, day + x.days));
          let df = M.div(M.ofInt(1), M.add(M.ofInt(1), M.div(M.mul(M.q(q.value, 1), frac), M.ofNat(M.BPS))));
          List.add(built, { days = t; df = fixedOf(df) });
        };
        case (#fra(x)) {
          if (x.startDays > last) return #err(#Gap({ instrument = name; needs = x.startDays; last }));
          let d0 = switch (discountAt(soFar(null), x.startDays)) { case (#ok(v)) v; case (#err(l)) return #err(#Gap({ instrument = name; needs = x.startDays; last = l })) };
          let frac = M.ofFraction(DC.fraction(sp.dayCount, day + x.startDays, day + x.endDays));
          let df = M.div(d0, M.add(M.ofInt(1), M.div(M.mul(M.q(q.value, 1), frac), M.ofNat(M.BPS))));
          List.add(built, { days = t; df = fixedOf(df) });
        };
        case (#future(x)) {
          if (x.startDays > last) return #err(#Gap({ instrument = name; needs = x.startDays; last }));
          let d0 = switch (discountAt(soFar(null), x.startDays)) { case (#ok(v)) v; case (#err(l)) return #err(#Gap({ instrument = name; needs = x.startDays; last = l })) };
          let frac = M.ofFraction(DC.fraction(sp.dayCount, day + x.startDays, day + x.endDays));
          // the forward the price implies less the declared convexity adjustment
          let df = M.div(d0, M.add(M.ofInt(1), M.div(M.mul(M.q(q.value - x.convexityBps, 1), frac), M.ofNat(M.BPS))));
          List.add(built, { days = t; df = fixedOf(df) });
        };
        case (#swap(x)) {
          switch (sp.role) {
            case (#discount) {
              // fixed against the curve's own forwards: the value is monotone in the maturity's factor
              let r = bisect(func(df : Nat) : Result.Result<M.Q, CT.Error> { let c = soFar(?{ days = t; df }); swapValue(day, sp.dayCount, q.value, x.months, x.fixedMonths, x.floatMonths, false, c, c) }, name, guess);
              switch (r) { case (#err(e)) return #err(e); case (#ok((df, n))) { List.add(built, { days = t; df }); iterations += n } };
            };
            case (#projection(_)) {
              let ?disc = discount else return #err(#NoDiscountCurve({ curve = sp.id; day }));
              let r = bisect(func(df : Nat) : Result.Result<M.Q, CT.Error> { swapValue(day, sp.dayCount, q.value, x.months, x.fixedMonths, x.floatMonths, false, disc, soFar(?{ days = t; df })) }, name, guess);
              switch (r) { case (#err(e)) return #err(e); case (#ok((df, n))) { List.add(built, { days = t; df }); iterations += n } };
            };
            case (#collateralDiscount(_)) return bad("a collateral discount curve is built from FX swaps");
          };
        };
        case (#ois(x)) {
          let r = bisect(func(df : Nat) : Result.Result<M.Q, CT.Error> { let c = soFar(?{ days = t; df }); swapValue(day, sp.dayCount, q.value, x.months, x.fixedMonths, x.fixedMonths, true, c, c) }, name, guess);
          switch (r) { case (#err(e)) return #err(e); case (#ok((df, n))) { List.add(built, { days = t; df }); iterations += n } };
        };
        case (#basis(x)) {
          let ?disc = discount else return #err(#NoDiscountCurve({ curve = sp.id; day }));
          let ?(rc, rm) = referenceOf(x.reference) else return #err(#UnknownReference({ instrument = name; curve = x.reference; day }));
          let bm = switch (sp.role) { case (#projection(p)) p.indexMonths; case (_) 0 };
          let r = bisect(func(df : Nat) : Result.Result<M.Q, CT.Error> { basisValue(day, sp.dayCount, q.value, x.months, x.spreadOnReference, disc, soFar(?{ days = t; df }), bm, rc, rm) }, name, guess);
          switch (r) { case (#err(e)) return #err(e); case (#ok((df, n))) { List.add(built, { days = t; df }); iterations += n } };
        };
        case (#fxSwap(x)) {
          // covered interest parity: the curve's factor is the collateral currency's factor scaled by the
          // forward over the spot, the forward being the spot plus the points
          let ?disc = discount else return #err(#NoDiscountCurve({ curve = sp.id; day }));
          let d0 = switch (discountAt(disc, x.days)) { case (#ok(v)) v; case (#err(l)) return #err(#BeyondCurve({ curve = "discount"; days = x.days; last = l })) };
          let df = M.mul(d0, M.q(x.spotMicro + q.value, x.spotMicro));
          List.add(built, { days = t; df = fixedOf(df) });
        };
      };
    };
    #ok((List.toArray(built), iterations))
  };

  /// The root of a residual on the integer grid, the residual increasing with the factor: a bracket from the
  /// guess, widened by a sixty-fourth a step until the residual changes sign (negative at the low end, not at
  /// the high end), then secant steps through the last two iterates, the bracket's midpoint whenever the secant
  /// leaves the bracket or two steps running fail to halve the residual, a step that rounds to the same integer
  /// moved one unit toward the root so the bracket closes. Every step is integer arithmetic on exact residuals:
  /// two builds of the same quotes take the same steps and reach the same factor, the negative end of the
  /// closed bracket. Refused when the residual does not change sign within the grid or within the bound.
  func absQ(x : M.Q) : M.Q { if (M.isNeg(x)) M.neg(x) else x };
  func bisect(f : Nat -> Result.Result<M.Q, CT.Error>, name : Text, guess : Nat) : Result.Result<(Nat, Nat), CT.Error> {
    var n = 0;
    let g = if (guess < 1) 1 else if (guess > DF_MAX) DF_MAX else guess;
    let fg = switch (f(g)) { case (#ok(v)) v; case (#err(e)) return #err(e) };
    n += 1;
    var lo : Nat = g; var hi : Nat = g; var flo = fg; var fhi = fg;
    if (M.isNeg(fg)) {
      while (M.isNeg(fhi)) {
        if (hi >= DF_MAX or n >= MAX_ITERATIONS) return #err(#NotConverged({ instrument = name; iterations = n }));
        hi := if (hi + hi / 64 + 1 > DF_MAX) DF_MAX else hi + hi / 64 + 1;
        fhi := switch (f(hi)) { case (#ok(v)) v; case (#err(e)) return #err(e) };
        n += 1;
      };
    } else {
      while (not M.isNeg(flo)) {
        if (lo <= 1 or n >= MAX_ITERATIONS) return #err(#NotConverged({ instrument = name; iterations = n }));
        lo := if (lo / 64 + 1 >= lo) 1 else lo - lo / 64 - 1;
        flo := switch (f(lo)) { case (#ok(v)) v; case (#err(e)) return #err(e) };
        n += 1;
      };
    };
    var a : Nat = lo; var fa = flo; var b : Nat = hi; var fb = fhi;
    if (M.cmp(absQ(flo), absQ(fhi)) < 0) { a := hi; fa := fhi; b := lo; fb := flo };
    var slow = 0;
    while (hi - lo > 1) {
      if (n >= MAX_ITERATIONS) return #err(#NotConverged({ instrument = name; iterations = n }));
      let m = (lo + hi) / 2;
      var s : Nat = m;
      if (slow < 2 and M.cmp(fb, fa) != 0) {
        let step = M.roundHalfEven(M.div(M.mul(fb, M.ofInt(b - a)), M.sub(fb, fa)));
        let sInt : Int = b - step;
        if (sInt == b) s := (if (M.isNeg(fb)) b + 1 else b - 1)
        else if (sInt > lo and sInt < hi) s := Int.abs(sInt)
        else s := m;
      };
      let fs = switch (f(s)) { case (#ok(v)) v; case (#err(e)) return #err(e) };
      n += 1;
      slow := if (M.cmp(M.mul(absQ(fs), M.ofInt(2)), absQ(fb)) <= 0) 0 else slow + 1;
      a := b; fa := fb; b := s; fb := fs;
      if (M.isNeg(fs)) { lo := s; flo := fs } else { hi := s; fhi := fs };
    };
    #ok((lo, n))
  };

  // ─── planners ──────────────────────────────────────────────────────────────

  public func curveOf(s : State, id : Text, day : Nat) : Res<Curve> {
    let ?(sp, b, ns) = curveOn(s, id, day) else return #err(#UnknownCurve({ curve = id; day }));
    #ok({ day = b.day; dayCount = sp.dayCount; interpolation = sp.interpolation; nodes = ns })
  };

  public func planBuild(s : State, sp : CT.Spec, day : Nat) : Res<CT.Event> {
    let discount = switch (sp.discountCurve) {
      case (?d) {
        switch (curveOf(s, d, day)) {
          case (#err(_)) return #err(#NoDiscountCurve({ curve = d; day }));
          case (#ok(c)) {
            // the discount curve named is of the role the build needs: a discount curve of the curve's own
            // currency for a projection curve, of the collateral currency for a collateral discount curve
            let ?dsp = spec(s, d) else return #err(#NoDiscountCurve({ curve = d; day }));
            switch (sp.role, dsp.role) {
              case (#projection(_), #discount) { if (not Text.equal(dsp.currency, sp.currency)) return #err(#InvalidSpec({ reason = "a projection curve discounts on a curve of its own currency" })) };
              case (#projection(_), #collateralDiscount(_)) { if (not Text.equal(dsp.currency, sp.currency)) return #err(#InvalidSpec({ reason = "a projection curve discounts on a curve of its own currency" })) };
              case (#collateralDiscount(c), #discount) { if (not Text.equal(dsp.currency, c.collateral)) return #err(#InvalidSpec({ reason = "a collateral discount curve is built on the collateral currency's discount curve" })) };
              case (_) return #err(#InvalidSpec({ reason = "the discount curve named is a " # CT.roleText(dsp.role) # " curve" }));
            };
            ?c
          };
        }
      };
      case null null;
    };
    // a basis swap's reference: a projection curve of the same currency, built on the day, with its index tenor
    func referenceOf(id : CT.CurveId) : ?(Curve, Nat) {
      let ?rsp = spec(s, id) else return null;
      let #projection(p) = rsp.role else return null;
      if (not Text.equal(rsp.currency, sp.currency)) return null;
      switch (curveOf(s, id, day)) { case (#ok(c)) ?(c, p.indexMonths); case (#err(_)) null }
    };
    switch (bootstrap(sp, day, discount, referenceOf)) {
      case (#err(e)) #err(e);
      case (#ok((nodes, iterations))) #ok(#curveBuilt({ spec = sp; day; nodes; iterations }));
    }
  };

  public func planSetIndexCurves(s : State, index : Text, projection : Text, discount : Text, day : Nat) : Res<CT.Event> {
    let n = Text.encodeUtf8(index).size();
    if (n == 0 or n > 32) return bad("an index is named in 1..32 bytes");
    let ?p = spec(s, projection) else return #err(#UnknownCurve({ curve = projection; day }));
    let ?d = spec(s, discount) else return #err(#UnknownCurve({ curve = discount; day }));
    switch (p.role) { case (#projection(_)) {}; case (_) return bad("the index's projection curve is a projection curve") };
    switch (d.role) { case (#discount or #collateralDiscount(_)) {}; case (#projection(_)) return bad("the index's discount curve is a discount curve") };
    if (not Text.equal(p.currency, d.currency)) return bad("the index's curves are in one currency");
    #ok(#indexCurvesSet({ index; projection; discount; day }))
  };

  /// The multi-curve mark of a swap: the floating leg projected from the index's curve, both legs discounted
  /// on the index's discount curve, a period already fixed at its fixing; the sign from the desk's side. The
  /// desk's own figure beside the treasury's posted single-curve mark.
  public func swapMark(s : State, i : TT.Irs, day : Nat, fixingFor : Nat -> ?Nat) : Res<(Text, Text, Int)> {
    let ?ix = index(s, i.floatingIndex) else return #err(#UnknownIndex({ index = i.floatingIndex }));
    let disc = switch (curveOf(s, ix.discount, day)) { case (#ok(c)) c; case (#err(e)) return #err(e) };
    let proj = switch (curveOf(s, ix.projection, day)) { case (#ok(c)) c; case (#err(e)) return #err(e) };
    var pv = M.zero();
    for (p in TreasuryCore.swapPeriodsOf(i).vals()) {
      if (p.end > day) {
        let frac = M.ofFraction(DC.fraction(i.dayCount, p.start, p.end));
        let fixed = M.ofInt(M.legAmount(i.notional, M.ofNat(i.fixedBps), i.dayCount, p));
        let floating = switch (if (p.start <= day) fixingFor(p.start) else null) {
          case (?f) M.ofInt(M.legAmount(i.notional, M.ofInt(f + i.spreadBps), i.dayCount, p));
          case null {
            let d0 = if (p.start > day) p.start - day else 0;
            let fwd = switch (forwardBps(proj, d0, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = ix.projection; days = p.end - day; last })) };
            M.div(M.mul(M.mul(M.ofNat(i.notional), M.add(fwd, M.ofInt(i.spreadBps))), frac), M.ofNat(M.BPS))
          };
        };
        let df = switch (discountAt(disc, p.end - day)) { case (#ok(v)) v; case (#err(last)) return #err(#BeyondCurve({ curve = ix.discount; days = p.end - day; last })) };
        pv := M.add(pv, M.mul(M.sub(floating, fixed), df));
      };
    };
    let m = M.roundHalfEven(pv);
    #ok((ix.discount, ix.projection, if (i.payFixed) m else -m))
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  public func fold(s : State, block : Nat, e : CT.Event) {
    switch (e) {
      case (#curveBuilt(x)) {
        let sp = x.spec;
        if (RI.put(s.specs, R.textKey(sp.id, 32), encodeSpec({ id = sp.id; currency = sp.currency; role = sp.role; dayCount = sp.dayCount; interpolation = sp.interpolation; discountCurve = sp.discountCurve; quotes = sp.quotes.size(); block })) == null) s.specCount += 1;
        // a rebuild on the same day writes its nodes over the day's; the build row bounds what is read
        switch (build(s, sp.id, x.day)) { case (?old) { s.nodeCount -= Nat.min(old.nodes, s.nodeCount) }; case null s.buildCount += 1 };
        ignore RI.put(s.builds, buildKey(sp.id, x.day), encodeBuild({ id = sp.id; day = x.day; nodes = x.nodes.size(); iterations = x.iterations; block }));
        var i = 0;
        while (i < x.nodes.size()) { ignore RI.put(s.nodes, nodeKey(sp.id, x.day, i), encodeNode(x.nodes[i])); i += 1 };
        s.nodeCount += x.nodes.size();
        switch (latestDay(s, sp.id)) { case (?d) { if (x.day >= d) ignore RI.put(s.latest, R.textKey(sp.id, 32), R.key(x.day, 4)) }; case null ignore RI.put(s.latest, R.textKey(sp.id, 32), R.key(x.day, 4)) };
      };
      case (#indexCurvesSet(x)) { if (RI.put(s.indexes, R.textKey(x.index, 32), encodeIndex({ index = x.index; projection = x.projection; discount = x.discount; day = x.day; block })) == null) s.indexCount += 1 };
      case (#swapMarked(x)) { if (RI.put(s.marks, R.key2(x.deal, 8, x.day, 4), encodeMark({ deal = x.deal; day = x.day; discount = x.discount; projection = x.projection; value = x.value; block })) == null) s.markCount += 1 };
    }
  };

  func walk(idx : RI.State, lo : Blob, hi : Blob, f : (Blob, Blob) -> ()) {
    var cursor : ?Blob = null;
    label paging loop {
      let page = RI.range(idx, lo, hi, cursor, 512);
      for ((k, v) in page.entries.vals()) f(k, v);
      switch (page.cursor) { case (?c) cursor := ?c; case null break paging };
    }
  };
  public func fingerprintInto(w : C.Writer, s : State) {
    w.nat(s.specCount); w.nat(s.buildCount); w.nat(s.nodeCount); w.nat(s.indexCount); w.nat(s.markCount);
    for ((idx, width) in [(s.specs, 32), (s.builds, 36), (s.nodes, 38), (s.latest, 32), (s.indexes, 32), (s.marks, 12)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      walk(idx, lo, hi, func(k, v) { w.blob(k); w.blob(v); n += 1 });
      w.nat(n);
    };
  };
  public func fingerprint(s : State) : Blob { let w = C.Writer(); fingerprintInto(w, s); Sha256.fromBlob(#sha256, w.toBlob()) };
}
