/// FeedCore.mo: the price feed rows folded from the desk log in stable memory: one row per feed with its
/// acceptance and its halt, one row per source with its key and its latest figure; the assessment rule over the
/// fresh figures (three sources at least within the band, the median accepted, a split a halt), pure.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Blob "mo:core/Blob";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Nat64 "mo:core/Nat64";
import Result "mo:core/Result";
import Text "mo:core/Text";

import C "mo:journal/Canonical";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import MlDsa "mo:kernel/sig/MlDsa44";
import TreasuryCore "mo:manticore/TreasuryCore";

import FT "FeedTypes";

module {

  public let FEED_ROW_BYTES : Nat = 58;
  public let SOURCE_ROW_BYTES : Nat = 1_376;
  public let MIN_SOURCES : Nat = 3;
  public let MAX_SOURCES : Nat = 16;
  public let PK_BYTES : Nat = MlDsa.PK_BYTES;
  public let SIG_BYTES : Nat = MlDsa.SIG_BYTES;
  /// The FIPS 204 context string of every source signature on a desk feed.
  public let CONTEXT : Text = "thebes.desk.feed.v1";
  /// A figure dated after the desk's clock by more than this is refused.
  public let SKEW_NS : Nat64 = 120_000_000_000;
  let MAX_PAGE = 256;
  let NS : Nat64 = 1_000_000_000;

  public func hash8(t : Text) : Nat { TreasuryCore.hash8(t) };

  public type FeedRow = {
    isin : Text; sources : Nat; bandBps : Nat; staleSeconds : Nat; halted : Nat8; acceptedMicro : Nat; acceptedAsOf : Nat64; acceptedBlock : Nat; acceptances : Nat; haltBlock : Nat; lastBlock : Nat;
  };
  public type SourceRow = { isin : Text; source : Text; publicKey : Blob; latestMicro : Nat; latestAsOf : Nat64; latestBlock : Nat; submissions : Nat };

  func encodeFeed(r : FeedRow) : Blob {
    let b = R.buf();
    R.putByte(b, Nat8.fromNat(r.sources)); R.putNat(b, r.bandBps, 4); R.putNat(b, r.staleSeconds, 8); R.putByte(b, r.halted); R.putNat(b, r.acceptedMicro, 8);
    R.putNat(b, Nat64.toNat(r.acceptedAsOf), 8); R.putNat(b, r.acceptedBlock, 8); R.putNat(b, r.acceptances, 4); R.putNat(b, r.haltBlock, 8); R.putNat(b, r.lastBlock, 8);
    R.done(b, FEED_ROW_BYTES)
  };
  func decodeFeed(isin : Text, v : Blob) : FeedRow {
    let a = Blob.toArray(v);
    { isin; sources = Nat8.toNat(a[0]); bandBps = R.getNat(a, 1, 4); staleSeconds = R.getNat(a, 5, 8); halted = a[13]; acceptedMicro = R.getNat(a, 14, 8);
      acceptedAsOf = Nat64.fromNat(R.getNat(a, 22, 8)); acceptedBlock = R.getNat(a, 30, 8); acceptances = R.getNat(a, 38, 4); haltBlock = R.getNat(a, 42, 8); lastBlock = R.getNat(a, 50, 8) }
  };
  func encodeSource(r : SourceRow) : Blob {
    let b = R.buf();
    R.putText(b, r.source, 32); R.putBlob(b, r.publicKey, PK_BYTES); R.putNat(b, r.latestMicro, 8); R.putNat(b, Nat64.toNat(r.latestAsOf), 8); R.putNat(b, r.latestBlock, 8); R.putNat(b, r.submissions, 8);
    R.done(b, SOURCE_ROW_BYTES)
  };
  func decodeSource(isin : Text, v : Blob) : SourceRow {
    let a = Blob.toArray(v);
    { isin; source = R.getText(a, 0, 32); publicKey = R.getBlob(a, 32, PK_BYTES); latestMicro = R.getNat(a, 32 + PK_BYTES, 8); latestAsOf = Nat64.fromNat(R.getNat(a, 40 + PK_BYTES, 8)); latestBlock = R.getNat(a, 48 + PK_BYTES, 8); submissions = R.getNat(a, 56 + PK_BYTES, 8) }
  };
  func haltCode(r : ?FT.HaltReason) : Nat8 { switch (r) { case null 0; case (?#disagreement) 1; case (?#stale) 2 } };
  func haltOf(c : Nat8) : ?FT.HaltReason { switch (c) { case 1 ?#disagreement; case 2 ?#stale; case _ null } };

  public type State = {
    feeds : RI.State;     // isin(12) -> feed row
    sources : RI.State;   // hash8(isin)(8) ‖ hash8(source)(8) -> source row
    var feedCount : Nat;
    var haltedCount : Nat;
    var submissionCount : Nat;
    var acceptanceCount : Nat;
  };
  public func newState(arena : RI.Arena) : State {
    {
      feeds = RI.newStateIn(arena, { keyBytes = 12; valBytes = FEED_ROW_BYTES });
      sources = RI.newStateIn(arena, { keyBytes = 16; valBytes = SOURCE_ROW_BYTES });
      var feedCount = 0; var haltedCount = 0; var submissionCount = 0; var acceptanceCount = 0;
    }
  };

  public func feed(s : State, isin : Text) : ?FeedRow { switch (RI.get(s.feeds, R.textKey(isin, 12))) { case (?v) ?decodeFeed(isin, v); case null null } };
  func sourceKey(isin : Text, source : Text) : Blob { R.key2(hash8(isin), 8, hash8(source), 8) };
  public func source(s : State, isin : Text, name : Text) : ?SourceRow { switch (RI.get(s.sources, sourceKey(isin, name))) { case (?v) ?decodeSource(isin, v); case null null } };
  public func sourcesOf(s : State, isin : Text) : [SourceRow] {
    let (lo, hi) = R.prefixRange(hash8(isin), 8, 8);
    let out = List.empty<SourceRow>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.sources, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) List.add(out, decodeSource(isin, v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func feeds(s : State) : [FeedRow] {
    let (lo, hi) = R.fullRange(12);
    let out = List.empty<FeedRow>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.feeds, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) List.add(out, decodeFeed(R.getText(Blob.toArray(k), 0, 12), v));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func halted(r : FeedRow) : ?FT.HaltReason { haltOf(r.halted) };
  public func view(r : FeedRow) : FT.FeedView {
    { isin = r.isin; sources = r.sources; bandBps = r.bandBps; staleSeconds = r.staleSeconds; halted = haltOf(r.halted);
      accepted = if (r.acceptedBlock == 0) null else ?{ priceMicro = r.acceptedMicro; asOf = r.acceptedAsOf; block = r.acceptedBlock }; acceptances = r.acceptances; haltBlock = r.haltBlock; lastBlock = r.lastBlock }
  };
  public func sourceView(r : SourceRow) : FT.SourceView {
    { isin = r.isin; source = r.source; latest = if (r.latestBlock == 0) null else ?{ priceMicro = r.latestMicro; asOf = r.latestAsOf; block = r.latestBlock }; submissions = r.submissions }
  };
  public func status(s : State) : FT.Status { { feeds = s.feedCount; halted = s.haltedCount; submissions = s.submissionCount; acceptances = s.acceptanceCount } };

  // ─── the planners ─────────────────────────────────────────────────────────

  public type Res<X> = Result.Result<X, FT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidFeed({ reason })) };

  /// The canonical message a source signs: the domain, the instrument, the source, the price and its time.
  public func message(isin : Text, source : Text, priceMicro : Nat, asOf : Nat64) : Blob {
    let w = C.Writer();
    w.text("THEBES-FEED-PRICE-v1"); w.text(isin); w.text(source); w.nat(priceMicro); w.nat(Nat64.toNat(asOf));
    w.toBlob()
  };
  public func verify(publicKey : Blob, isin : Text, source : Text, priceMicro : Nat, asOf : Nat64, signature : Blob) : Bool {
    if (publicKey.size() != PK_BYTES or signature.size() != SIG_BYTES) return false;
    MlDsa.verify(Blob.toArray(publicKey), Blob.toArray(message(isin, source, priceMicro, asOf)), Blob.toArray(Text.encodeUtf8(CONTEXT)), Blob.toArray(signature))
  };

  public func planDeclare(f : FT.Feed, day : Nat) : Res<FT.Event> {
    if (Text.encodeUtf8(f.isin).size() != 12) return bad("an ISIN has twelve characters");
    if (f.sources.size() < MIN_SOURCES) return bad("a feed names at least " # Nat.toText(MIN_SOURCES) # " sources");
    if (f.sources.size() > MAX_SOURCES) return bad("a feed names at most " # Nat.toText(MAX_SOURCES) # " sources");
    if (f.bandBps == 0 or f.bandBps > 5_000) return bad("the band is between one basis point and 50 percent");
    if (f.staleSeconds == 0 or f.staleSeconds > 30 * 86_400) return bad("the staleness bound is between one second and thirty days");
    var i = 0;
    while (i < f.sources.size()) {
      let x = f.sources[i];
      let n = Text.encodeUtf8(x.id).size();
      if (n == 0 or n > 32) return bad("a source is named in 1..32 bytes");
      if (x.publicKey.size() != PK_BYTES) return bad("a source key is an ML-DSA-44 public key of " # Nat.toText(PK_BYTES) # " bytes");
      var j = 0;
      while (j < i) { if (Text.equal(f.sources[j].id, x.id)) return bad("a source is named once"); j += 1 };
      i += 1;
    };
    #ok(#feedDeclared({ feed = f; day }))
  };

  /// What one assessment of the fresh figures gives: an acceptance, a split, or nothing (fewer than three
  /// fresh figures).
  public type Assessment = { #accepted : { priceMicro : Nat; asOf : Nat64; figures : [FT.Figure] }; #split : { figures : [FT.Figure] }; #none };
  public func assess(f : FeedRow, rows : [SourceRow], now : Nat64) : Assessment {
    let stale : Nat64 = Nat64.fromNat(f.staleSeconds) * NS;
    let fresh = List.empty<FT.Figure>();
    for (r in rows.vals()) { if (r.latestBlock > 0 and r.latestAsOf + stale >= now) List.add(fresh, { source = r.source; priceMicro = r.latestMicro; asOf = r.latestAsOf }) };
    let figures = Array.sort<FT.Figure>(List.toArray(fresh), func(a, b) { switch (Nat.compare(a.priceMicro, b.priceMicro)) { case (#equal) Text.compare(a.source, b.source); case o o } });
    let n = figures.size();
    if (n < MIN_SOURCES) return #none;
    let lo = figures[0].priceMicro;
    let hi = figures[n - 1].priceMicro;
    // the band is measured on the lowest figure; every figure of the set is within it or the set is a split
    if ((hi - lo) * 10_000 > f.bandBps * lo) return #split({ figures });
    let median = if (n % 2 == 1) figures[n / 2].priceMicro else { let a = figures[n / 2 - 1].priceMicro; let b = figures[n / 2].priceMicro; (a + b) / 2 + (if ((a + b) % 2 == 1 and ((a + b) / 2) % 2 == 1) 1 else 0) };
    var latest : Nat64 = 0;
    for (x in figures.vals()) { if (x.asOf > latest) latest := x.asOf };
    #accepted({ priceMicro = median; asOf = latest; figures })
  };

  /// A submission: the source known, the figure fresh, later than the source's last, not from the future, and
  /// signed; then the assessment over every source's latest figure with this one in place.
  public func planSubmit(s : State, x : FT.Submission, now : Nat64, day : Nat) : Res<[FT.Event]> {
    let ?f = feed(s, x.isin) else return #err(#NoFeed({ isin = x.isin }));
    let ?src = source(s, x.isin, x.source) else return #err(#UnknownSource({ isin = x.isin; source = x.source }));
    if (x.priceMicro == 0) return bad("a price is positive");
    if (x.asOf > now + SKEW_NS) return #err(#FutureSubmission({ isin = x.isin; source = x.source; asOf = x.asOf; now }));
    if (x.asOf + Nat64.fromNat(f.staleSeconds) * NS < now) return #err(#StaleSubmission({ isin = x.isin; source = x.source; asOf = x.asOf; now }));
    if (src.latestBlock > 0 and x.asOf <= src.latestAsOf) return #err(#Replayed({ isin = x.isin; source = x.source; asOf = x.asOf; latest = src.latestAsOf }));
    if (not verify(src.publicKey, x.isin, x.source, x.priceMicro, x.asOf, x.signature)) return #err(#BadSignature({ isin = x.isin; source = x.source }));
    let out = List.empty<FT.Event>();
    List.add(out, #priceSubmitted({ isin = x.isin; source = x.source; priceMicro = x.priceMicro; asOf = x.asOf; signature = x.signature; day }));
    let rows = Array.map<SourceRow, SourceRow>(sourcesOf(s, x.isin), func(r) { if (Text.equal(r.source, x.source)) { { r with latestMicro = x.priceMicro; latestAsOf = x.asOf; latestBlock = 1 } } else r });
    switch (assess(f, rows, now)) {
      case (#accepted(a)) List.add(out, #priceAccepted({ isin = x.isin; priceMicro = a.priceMicro; asOf = a.asOf; figures = a.figures; day }));
      case (#split(sp)) { if (f.halted == 0) List.add(out, #instrumentHalted({ isin = x.isin; reason = #disagreement; figures = sp.figures; day })) };
      case (#none) {};
    };
    #ok(List.toArray(out))
  };

  /// A halt lifted: the instrument is halted and the sources have agreed since the halt.
  public func planLift(s : State, isin : Text, day : Nat) : Res<FT.Event> {
    let ?f = feed(s, isin) else return #err(#NoFeed({ isin }));
    if (f.halted == 0) return #err(#NotHalted({ isin }));
    if (f.acceptedBlock <= f.haltBlock) return #err(#NotAgreeing({ isin }));
    #ok(#haltLifted({ isin; day }))
  };

  /// The price a cycle clears against: accepted, fresh and not halted.
  public func referencePrice(s : State, isin : Text, now : Nat64) : Res<{ priceMicro : Nat; asOf : Nat64; block : Nat }> {
    let ?f = feed(s, isin) else return #err(#NoFeed({ isin }));
    switch (haltOf(f.halted)) { case (?reason) return #err(#Halted({ isin; reason })); case null {} };
    if (f.acceptedBlock == 0) return #err(#NoPrice({ isin }));
    if (f.acceptedAsOf + Nat64.fromNat(f.staleSeconds) * NS < now) return #err(#Stale({ isin; asOf = f.acceptedAsOf; now }));
    #ok({ priceMicro = f.acceptedMicro; asOf = f.acceptedAsOf; block = f.acceptedBlock })
  };

  /// The feeds whose accepted price has gone stale and are not yet halted: the end of day halts them.
  public func staleFeeds(s : State, now : Nat64) : [FeedRow] {
    Array.filter<FeedRow>(feeds(s), func(f) { f.halted == 0 and f.acceptedBlock > 0 and f.acceptedAsOf + Nat64.fromNat(f.staleSeconds) * NS < now })
  };

  // ─── the fold ─────────────────────────────────────────────────────────────

  func putFeed(s : State, r : FeedRow) { ignore RI.put(s.feeds, R.textKey(r.isin, 12), encodeFeed(r)) };
  func putSource(s : State, r : SourceRow) { ignore RI.put(s.sources, sourceKey(r.isin, r.source), encodeSource(r)) };

  public func fold(s : State, block : Nat, ev : FT.Event) {
    switch (ev) {
      case (#feedDeclared(x)) {
        let f = x.feed;
        let before = feed(s, f.isin);
        switch (before) { case null s.feedCount += 1; case (?b) { if (b.halted != 0 and s.haltedCount > 0) s.haltedCount -= 1 } };
        // a redeclaration keeps nothing of the old sources' figures: every source starts afresh, the halt and the price cleared
        switch (before) { case (?_) { for (r in sourcesOf(s, f.isin).vals()) ignore RI.put(s.sources, sourceKey(r.isin, r.source), encodeSource({ r with latestMicro = 0; latestAsOf = 0; latestBlock = 0; publicKey = Blob.fromArray(Array.repeat<Nat8>(0, PK_BYTES)) })) }; case null {} };
        putFeed(s, { isin = f.isin; sources = f.sources.size(); bandBps = f.bandBps; staleSeconds = f.staleSeconds; halted = 0; acceptedMicro = 0; acceptedAsOf = 0; acceptedBlock = 0; acceptances = 0; haltBlock = 0; lastBlock = block });
        for (src in f.sources.vals()) putSource(s, { isin = f.isin; source = src.id; publicKey = src.publicKey; latestMicro = 0; latestAsOf = 0; latestBlock = 0; submissions = 0 });
      };
      case (#priceSubmitted(x)) {
        switch (source(s, x.isin, x.source)) { case (?r) putSource(s, { r with latestMicro = x.priceMicro; latestAsOf = x.asOf; latestBlock = block; submissions = r.submissions + 1 }); case null {} };
        switch (feed(s, x.isin)) { case (?f) putFeed(s, { f with lastBlock = block }); case null {} };
        s.submissionCount += 1;
      };
      case (#priceAccepted(x)) {
        switch (feed(s, x.isin)) { case (?f) putFeed(s, { f with acceptedMicro = x.priceMicro; acceptedAsOf = x.asOf; acceptedBlock = block; acceptances = f.acceptances + 1; lastBlock = block }); case null {} };
        s.acceptanceCount += 1;
      };
      case (#instrumentHalted(x)) {
        switch (feed(s, x.isin)) { case (?f) { if (f.halted == 0) s.haltedCount += 1; putFeed(s, { f with halted = haltCode(?x.reason); haltBlock = block; lastBlock = block }) }; case null {} };
      };
      case (#haltLifted(x)) {
        switch (feed(s, x.isin)) { case (?f) { if (f.halted != 0 and s.haltedCount > 0) s.haltedCount -= 1; putFeed(s, { f with halted = 0; lastBlock = block }) }; case null {} };
      };
    }
  };

  public func fingerprintInto(w : C.Writer, s : State) {
    for ((idx, width) in [(s.feeds, 12), (s.sources, 16)].vals()) {
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
    w.nat(s.feedCount); w.nat(s.haltedCount); w.nat(s.submissionCount); w.nat(s.acceptanceCount);
  };
}
