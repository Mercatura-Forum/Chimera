/// LiquidityCore.mo: the recorded factors and classes folded from the desk log in stable memory, and the ratio
/// arithmetic as pure functions over the positions and flows the desk reads out of its rows: the liquidity
/// coverage ratio (BCBS 238), the net stable funding ratio (BCBS 295) and the large exposures (BCBS 283). Every
/// figure is one rounding of an exact rational; a missing class or factor is a refusal that names itself.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Blob "mo:core/Blob";
import Int "mo:core/Int";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import TreasuryCore "mo:manticore/TreasuryCore";
import M "mo:manticore/TreasuryMath";

import LQ "LiquidityTypes";

module {

  let MAX_PAGE = 512;
  public let INSTRUMENT_ROW_BYTES : Nat = 9;
  public let COUNTERPARTY_ROW_BYTES : Nat = 41;

  public func levelCode(l : LQ.HqlaLevel) : Nat8 { switch (l) { case (#level1) 1; case (#level2A) 2; case (#level2B) 3; case (#none) 4 } };
  public func levelOf(c : Nat8) : LQ.HqlaLevel { switch (c) { case 1 #level1; case 2 #level2A; case 3 #level2B; case _ #none } };
  public func typeCode(t : LQ.CounterpartyType) : Nat8 {
    switch (t) { case (#retailStable) 1; case (#retailLessStable) 2; case (#smallBusiness) 3; case (#nonFinancialCorporate) 4; case (#sovereign) 5; case (#centralBank) 6; case (#financial) 7; case (#operational) 8 }
  };
  public func typeOf(c : Nat8) : LQ.CounterpartyType {
    switch (c) { case 1 #retailStable; case 2 #retailLessStable; case 3 #smallBusiness; case 4 #nonFinancialCorporate; case 5 #sovereign; case 6 #centralBank; case 7 #financial; case _ #operational }
  };
  public func hash8(t : Text) : Nat { TreasuryCore.hash8(t) };

  public type State = {
    instruments : RI.State;     // isin(12) -> level(1) ‖ block(8)
    counterparties : RI.State;  // hash8(name) -> name(32) ‖ type(1) ‖ block(8)
    var factors : ?LQ.Factors;
    var capital : ?(Text, Nat);
    var instrumentCount : Nat;
    var counterpartyCount : Nat;
    var factorsSet : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      instruments = RI.newStateIn(arena, { keyBytes = 12; valBytes = INSTRUMENT_ROW_BYTES });
      counterparties = RI.newStateIn(arena, { keyBytes = 8; valBytes = COUNTERPARTY_ROW_BYTES });
      var factors = null; var capital = null; var instrumentCount = 0; var counterpartyCount = 0; var factorsSet = 0;
    }
  };

  public func factors(s : State) : ?LQ.Factors { s.factors };
  public func capital(s : State) : ?(Text, Nat) { s.capital };
  public func instrumentLevel(s : State, isin : Text) : ?LQ.HqlaLevel { switch (RI.get(s.instruments, R.textKey(isin, 12))) { case (?v) ?levelOf(Blob.toArray(v)[0]); case null null } };
  public func counterpartyType(s : State, name : Text) : ?LQ.CounterpartyType { switch (RI.get(s.counterparties, R.key(hash8(name), 8))) { case (?v) ?typeOf(Blob.toArray(v)[32]); case null null } };
  public func counterpartyTypeByHash(s : State, h : Nat) : ?LQ.CounterpartyType { switch (RI.get(s.counterparties, R.key(h, 8))) { case (?v) ?typeOf(Blob.toArray(v)[32]); case null null } };
  public func counterpartyNameByHash(s : State, h : Nat) : ?Text { switch (RI.get(s.counterparties, R.key(h, 8))) { case (?v) ?R.getText(Blob.toArray(v), 0, 32); case null null } };
  public func instruments(s : State) : [(Text, LQ.HqlaLevel)] {
    let out = List.empty<(Text, LQ.HqlaLevel)>();
    let (lo, hi) = R.fullRange(12);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.instruments, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, (R.getText(Blob.toArray(k), 0, 12), levelOf(Blob.toArray(v)[0])));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func counterparties(s : State) : [(Text, LQ.CounterpartyType)] {
    let out = List.empty<(Text, LQ.CounterpartyType)>();
    let (lo, hi) = R.fullRange(8);
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.counterparties, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) { let a = Blob.toArray(v); List.add(out, (R.getText(a, 0, 32), typeOf(a[32]))) };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };

  // ─── the factors ──────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, LQ.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidFactors({ reason })) };
  let ALL_TYPES : [LQ.CounterpartyType] = [#retailStable, #retailLessStable, #smallBusiness, #nonFinancialCorporate, #sovereign, #centralBank, #financial, #operational];
  let ALL_LEVELS : [LQ.HqlaLevel] = [#level1, #level2A, #level2B, #none];

  func rateFor(rates : [LQ.Rate], t : LQ.CounterpartyType) : ?Nat { for (r in rates.vals()) { if (r.counterpartyType == t) return ?r.bps }; null };
  func levelFor(fs : [LQ.LevelFactor], l : LQ.HqlaLevel) : ?Nat { for (f in fs.vals()) { if (f.level == l) return ?f.bps }; null };
  func completeRates(rates : [LQ.Rate], what : Text) : ?Text {
    for (t in ALL_TYPES.vals()) { switch (rateFor(rates, t)) { case null return ?(what # " has no rate for " # LQ.counterpartyTypeText(t)); case (?b) { if (b > M.BPS) return ?(what # " has a rate over 100 percent") } } };
    null
  };
  func completeLevels(fs : [LQ.LevelFactor], what : Text) : ?Text {
    for (l in ALL_LEVELS.vals()) { switch (levelFor(fs, l)) { case null return ?(what # " has no factor for " # LQ.hqlaText(l)); case (?b) { if (b > M.BPS) return ?(what # " has a factor over 100 percent") } } };
    null
  };
  /// Every table complete over its types or levels, every rate at most 100 percent, the caps and bounds within it.
  public func planFactors(f : LQ.Factors, day : Nat) : Res<LQ.Event> {
    for (check in [completeLevels(f.hqlaHaircuts, "hqlaHaircuts"), completeRates(f.runoff, "runoff"), completeRates(f.inflow, "inflow"), completeRates(f.asfUnderSixMonths, "asfUnderSixMonths"), completeRates(f.asfSixToTwelve, "asfSixToTwelve"),
                   completeLevels(f.rsfHqla, "rsfHqla"), completeRates(f.rsfUnderSixMonths, "rsfUnderSixMonths"), completeRates(f.rsfSixToTwelve, "rsfSixToTwelve"), completeRates(f.rsfOverYear, "rsfOverYear")].vals()) {
      switch (check) { case (?r) return bad(r); case null {} };
    };
    if (f.level2CapBps > M.BPS or f.level2BCapBps > f.level2CapBps) return bad("the level 2 caps are within 100 percent and the 2B cap within the level 2 cap");
    if (f.inflowCapBps > M.BPS or f.asfOverYearBps > M.BPS or f.rsfDerivativesBps > M.BPS) return bad("a factor is at most 100 percent");
    if (f.largeExposureBps == 0 or f.largeExposureBps > M.BPS or f.largeExposureReportBps > f.largeExposureBps) return bad("the large exposure bound is positive, within 100 percent, and the reporting threshold within it");
    #ok(#factorsSet({ factors = f; day }))
  };
  public func planClassifyInstrument(isin : Text, level : LQ.HqlaLevel, day : Nat) : Res<LQ.Event> {
    if (Text.encodeUtf8(isin).size() != 12) return bad("an ISIN has twelve characters");
    #ok(#instrumentClassified({ isin; level; day }))
  };
  public func planClassifyCounterparty(name : Text, t : LQ.CounterpartyType, day : Nat) : Res<LQ.Event> {
    if (Text.encodeUtf8(name).size() == 0 or Text.encodeUtf8(name).size() > 32) return bad("a counterparty is named in 1..32 bytes");
    #ok(#counterpartyClassified({ name; counterpartyType = t; day }))
  };
  public func planCapital(currency : Text, amount : Nat, day : Nat) : Res<LQ.Event> {
    if (Text.encodeUtf8(currency).size() != 3) return bad("a currency code has three letters");
    if (amount == 0) return bad("the eligible capital is positive");
    #ok(#capitalDeclared({ currency; amount; day }))
  };

  // ─── the arithmetic ────────────────────────────────────────────────────────

  /// A liquid asset held: its level and its value in the functional currency before the haircut.
  public type LiquidAsset = { level : LQ.HqlaLevel; value : Nat };
  /// A flow within the horizon as the ratios weigh it: the counterparty type and the amount in the functional
  /// currency, negative for an outflow; a derivative payable or receivable carries no counterparty type and is
  /// weighed at 100 percent.
  public type WeighedFlow = { counterpartyType : ?LQ.CounterpartyType; amount : Int };
  func part(amount : Nat, bps : Nat) : Nat { M.roundNat(M.q(amount * bps, M.BPS)) };

  /// The stock of liquid assets after haircuts, with the level 2 caps as BCBS 238 Annex 1 states them: the 2B
  /// adjustment is the excess of 2B over its cap of the stock, taken against the stock with level 2 at its own
  /// cap; the level 2 adjustment is the excess of level 2, after the 2B adjustment, over its cap of the stock.
  /// With caps c2 and c2b: adj2B = max(0, L2B - c2b/(1-c2b)(L1+L2A), L2B - c2b/(1-c2) L1),
  /// adj2 = max(0, (L2A + L2B - adj2B) - c2/(1-c2) L1), stock = L1 + L2A + L2B - adj2B - adj2.
  public func liquidStock(f : LQ.Factors, assets : [LiquidAsset]) : Res<{ level1 : Nat; level2A : Nat; level2B : Nat; capAdjustment : Nat; hqla : Nat }> {
    var l1 = 0; var l2a = 0; var l2b = 0;
    for (a in assets.vals()) {
      let ?cut = levelFor(f.hqlaHaircuts, a.level) else return #err(#MissingFactor({ what = "hqlaHaircuts " # LQ.hqlaText(a.level) }));
      let v = part(a.value, M.BPS - cut);
      switch (a.level) { case (#level1) l1 += v; case (#level2A) l2a += v; case (#level2B) l2b += v; case (#none) {} };
    };
    let c2b = f.level2BCapBps; let c2 = f.level2CapBps;
    func ratioPart(amount : Nat, num : Nat, den : Nat) : Int { if (den == 0) (amount : Int) * 1_000_000 else M.roundHalfEven(M.q(amount * num, den)) };
    let adj2b : Int = Int.max(0, Int.max((l2b : Int) - ratioPart(l1 + l2a, c2b, M.BPS - c2b), (l2b : Int) - ratioPart(l1, c2b, M.BPS - c2)));
    let adj2 : Int = Int.max(0, ((l2a + l2b) : Int) - adj2b - ratioPart(l1, c2, M.BPS - c2));
    let adjustment = Int.abs(adj2b + adj2);
    let gross = l1 + l2a + l2b;
    #ok({ level1 = l1; level2A = l2a; level2B = l2b; capAdjustment = adjustment; hqla = if (adjustment > gross) 0 else gross - adjustment })
  };
  /// The liquidity coverage ratio in basis points: the stock over the net outflows of the horizon, the inflows
  /// counted at most at the cap of the outflows; a zero net outflow with a stock reads as the ratio's ceiling.
  public func lcr(f : LQ.Factors, assets : [LiquidAsset], flows : [WeighedFlow]) : Res<{ level1 : Nat; level2A : Nat; level2B : Nat; capAdjustment : Nat; hqla : Nat; outflows : Nat; inflows : Nat; inflowsCounted : Nat; netOutflows : Nat; ratioBps : Nat }> {
    let stock = switch (liquidStock(f, assets)) { case (#err(e)) return #err(e); case (#ok(s)) s };
    var outflows = 0; var inflows = 0;
    for (x in flows.vals()) {
      if (x.amount < 0) {
        let bps = switch (x.counterpartyType) { case (?t) { switch (rateFor(f.runoff, t)) { case (?b) b; case null return #err(#MissingFactor({ what = "runoff " # LQ.counterpartyTypeText(t) })) } }; case null M.BPS };
        outflows += part(Int.abs(x.amount), bps);
      } else if (x.amount > 0) {
        let bps = switch (x.counterpartyType) { case (?t) { switch (rateFor(f.inflow, t)) { case (?b) b; case null return #err(#MissingFactor({ what = "inflow " # LQ.counterpartyTypeText(t) })) } }; case null M.BPS };
        inflows += part(Int.abs(x.amount), bps);
      };
    };
    let cap = part(outflows, f.inflowCapBps);
    let counted = Nat.min(inflows, cap);
    let net = (outflows - counted : Nat);
    let ratio = if (net == 0) (if (stock.hqla > 0) 1_000_000 else 0) else M.roundNat(M.q(stock.hqla * M.BPS, net));
    #ok({ level1 = stock.level1; level2A = stock.level2A; level2B = stock.level2B; capAdjustment = stock.capAdjustment; hqla = stock.hqla; outflows; inflows; inflowsCounted = counted; netOutflows = net; ratioBps = Nat.min(ratio, 1_000_000) })
  };

  /// A position as the net stable funding ratio weighs it: an asset or a liability, its value in the functional
  /// currency, its residual maturity in days (none for a derivative or an asset at call), its level when it is a
  /// liquid asset, its counterparty type when it has one, whether it is a derivative.
  public type FundingPosition = { asset : Bool; value : Nat; residualDays : ?Nat; level : ?LQ.HqlaLevel; counterpartyType : ?LQ.CounterpartyType; derivative : Bool };
  func maturityBand(days : ?Nat) : Nat { switch (days) { case null 0; case (?d) { if (d < 182) 0 else if (d < 365) 1 else 2 } } };
  public func nsfr(f : LQ.Factors, capital : Nat, positions : [FundingPosition]) : Res<{ asf : Nat; rsf : Nat; ratioBps : Nat }> {
    var asf = capital;
    var rsf = 0;
    for (p in positions.vals()) {
      if (p.asset) {
        if (p.derivative) { rsf += part(p.value, f.rsfDerivativesBps); continue };
        switch (p.level) {
          case (?l) { let ?b = levelFor(f.rsfHqla, l) else return #err(#MissingFactor({ what = "rsfHqla " # LQ.hqlaText(l) })); rsf += part(p.value, b) };
          case null {
            let ?t = p.counterpartyType else return #err(#MissingFactor({ what = "a non-derivative asset without a counterparty type" }));
            let table = switch (maturityBand(p.residualDays)) { case 0 f.rsfUnderSixMonths; case 1 f.rsfSixToTwelve; case _ f.rsfOverYear };
            let ?b = rateFor(table, t) else return #err(#MissingFactor({ what = "rsf " # LQ.counterpartyTypeText(t) }));
            rsf += part(p.value, b);
          };
        };
      } else {
        if (p.derivative) continue;   // a derivative payable provides no stable funding
        switch (maturityBand(p.residualDays)) {
          case 2 asf += part(p.value, f.asfOverYearBps);
          case band {
            let ?t = p.counterpartyType else return #err(#MissingFactor({ what = "a liability without a counterparty type" }));
            let table = if (band == 0) f.asfUnderSixMonths else f.asfSixToTwelve;
            let ?b = rateFor(table, t) else return #err(#MissingFactor({ what = "asf " # LQ.counterpartyTypeText(t) }));
            asf += part(p.value, b);
          };
        };
      };
    };
    let ratio = if (rsf == 0) (if (asf > 0) 1_000_000 else 0) else M.roundNat(M.q(asf * M.BPS, rsf));
    #ok({ asf; rsf; ratioBps = Nat.min(ratio, 1_000_000) })
  };
  /// The large exposures: every counterparty's exposure against the capital, in basis points, those at or over
  /// the reporting threshold listed, those over the bound breaches.
  public func largeExposures(f : LQ.Factors, capital : Nat, exposures : [(Text, Text, Nat)]) : { rows : [LQ.ExposureRow]; breaches : Nat } {
    let out = List.empty<LQ.ExposureRow>();
    var breaches = 0;
    for ((cp, group, e) in exposures.vals()) {
      let share = if (capital == 0) M.BPS else M.roundNat(M.q(e * M.BPS, capital));
      if (share >= f.largeExposureReportBps) {
        let breach = share > f.largeExposureBps;
        if (breach) breaches += 1;
        List.add(out, { counterparty = cp; group; exposure = e; shareBps = share; breach });
      };
    };
    { rows = List.toArray(out); breaches }
  };

  // ─── the fold ─────────────────────────────────────────────────────────────

  public func fold(s : State, block : Nat, ev : LQ.Event) {
    switch (ev) {
      case (#factorsSet(x)) { s.factors := ?x.factors; s.factorsSet += 1 };
      case (#instrumentClassified(x)) {
        if (RI.get(s.instruments, R.textKey(x.isin, 12)) == null) s.instrumentCount += 1;
        let b = R.buf(); R.putByte(b, levelCode(x.level)); R.putNat(b, block, 8);
        ignore RI.put(s.instruments, R.textKey(x.isin, 12), R.done(b, INSTRUMENT_ROW_BYTES));
      };
      case (#counterpartyClassified(x)) {
        let k = R.key(hash8(x.name), 8);
        if (RI.get(s.counterparties, k) == null) s.counterpartyCount += 1;
        let b = R.buf(); R.putText(b, x.name, 32); R.putByte(b, typeCode(x.counterpartyType)); R.putNat(b, block, 8);
        ignore RI.put(s.counterparties, k, R.done(b, COUNTERPARTY_ROW_BYTES));
      };
      case (#capitalDeclared(x)) s.capital := ?(x.currency, x.amount);
    }
  };
  public func status(s : State) : LQ.Status { { factors = switch (s.factors) { case null false; case (?_) true }; instruments = s.instrumentCount; counterparties = s.counterpartyCount; capital = s.capital } };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.factors) {
      case null w.byte(0);
      case (?f) {
        w.byte(1);
        for (x in f.hqlaHaircuts.vals()) { w.byte(levelCode(x.level)); w.nat(x.bps) };
        w.nat(f.level2CapBps); w.nat(f.level2BCapBps);
        for (t in [f.runoff, f.inflow, f.asfUnderSixMonths, f.asfSixToTwelve, f.rsfUnderSixMonths, f.rsfSixToTwelve, f.rsfOverYear].vals()) { w.nat(t.size()); for (r in t.vals()) { w.byte(typeCode(r.counterpartyType)); w.nat(r.bps) } };
        w.nat(f.inflowCapBps); w.nat(f.asfOverYearBps); for (x in f.rsfHqla.vals()) { w.byte(levelCode(x.level)); w.nat(x.bps) }; w.nat(f.rsfDerivativesBps); w.nat(f.largeExposureBps); w.nat(f.largeExposureReportBps);
      };
    };
    switch (s.capital) { case null w.byte(0); case (?(c, n)) { w.byte(1); w.text(c); w.nat(n) } };
    w.nat(s.instrumentCount); w.nat(s.counterpartyCount); w.nat(s.factorsSet);
    for ((idx, width) in [(s.instruments, 12), (s.counterparties, 8)].vals()) {
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
