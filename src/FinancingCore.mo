/// FinancingCore.mo: the repo and securities-lending book folded from the desk log in stable memory: the rows,
/// the accrual by the day count, the margin against the day's price, the legs every event posts, the fold. The
/// arithmetic is Manticore's `TreasuryMath`, so the Python twin is the same file.
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
import JT "mo:journal/JournalTypes";
import RI "mo:kernel/index/RegionIndex";
import R "mo:kernel/rows/StableRows";
import DC "mo:manticore/DayCount";
import TT "mo:manticore/TreasuryTypes";
import M "mo:manticore/TreasuryMath";
import Posting "mo:manticore/Posting";

import FT "FinancingTypes";

module {

  public let REPO_ROW_BYTES : Nat = 199;
  public let LOAN_ROW_BYTES : Nat = 220;
  let MAX_PAGE = 512;

  public type RepoRow = {
    id : FT.RepoId; state : FT.RepoState; reverse : Bool; book : Text; cpHash : Nat; currency : Text; cash : Nat; rateBps : Nat; dayCount : Nat8; start : Nat; maturity : Nat;
    isin : Text; nominal : Nat; haircutBps : Nat; thresholdBps : Nat; accrualFrom : Nat; accruedBefore : Int; accruedPosted : Int;
    marginCash : Int; marginCalled : Nat; marginPayer : Nat8; marginDue : Nat; lastValue : Nat; depot : Text; lastBlock : Nat; refHash : Nat;
  };
  public type LoanRow = {
    id : FT.LoanId; state : FT.LoanState; book : Text; cpHash : Nat; currency : Text; isin : Text; nominal : Nat; valueMicro : Nat; feeBps : Nat; dayCount : Nat8;
    cashCollateral : Nat; rebateBps : Nat; collateralIsin : Text; collateralNominal : Nat; start : Nat; noticeDays : Nat; returnDay : Nat; accrualFrom : Nat;
    feeBefore : Int; feePosted : Int; rebateBefore : Int; rebatePosted : Int; manufactured : Nat; depot : Text; lastBlock : Nat; refHash : Nat;
  };

  func repoStateCode(s : FT.RepoState) : Nat8 { switch (s) { case (#open) 1; case (#started) 2; case (#closed) 3 } };
  func repoStateOf(c : Nat8) : FT.RepoState { switch (c) { case 1 #open; case 2 #started; case _ #closed } };
  func loanStateCode(s : FT.LoanState) : Nat8 { switch (s) { case (#open) 1; case (#started) 2; case (#recalled) 3; case (#returned) 4 } };
  func loanStateOf(c : Nat8) : FT.LoanState { switch (c) { case 1 #open; case 2 #started; case 3 #recalled; case _ #returned } };
  func convCode(c : DC.Convention) : Nat8 { switch (c) { case (#a001_ActActIcma(_)) 1; case (#a003_Act360) 3; case (#a004_Act365Fixed) 4; case (#a005_ActActIsda) 5; case (#a006_Thirty360Isda) 6; case (#a007_ThirtyE360) 7; case (#a011_Thirty365) 11 } };
  public func convOf(c : Nat8) : DC.Convention { switch (c) { case 1 #a001_ActActIcma({ couponsPerYear = 1 }); case 3 #a003_Act360; case 4 #a004_Act365Fixed; case 5 #a005_ActActIsda; case 6 #a006_Thirty360Isda; case 7 #a007_ThirtyE360; case _ #a011_Thirty365 } };
  func putInt(b : R.Buf, v : Int) { R.putByte(b, if (v < 0) 1 else 0); R.putNat(b, Int.abs(v), 8) };
  func getInt(a : [Nat8], off : Nat) : Int { let m = R.getNat(a, off + 1, 8); if (a[off] == 1) -m else m };
  public func hash8(t : Text) : Nat { R.getNat(Blob.toArray(Sha256.fromBlob(#sha256, Text.encodeUtf8(t))), 0, 8) };
  public func repoSub(id : FT.RepoId) : JT.SubledgerKey { Posting.subledgerOf("repo/" # Nat.toText(id)) };
  public func loanSub(id : FT.LoanId) : JT.SubledgerKey { Posting.subledgerOf("loan/" # Nat.toText(id)) };
  public func payerCode(p : FT.MarginPayer) : Nat8 { switch (p) { case (#desk) 1; case (#counterparty) 2 } };
  public func payerOf(c : Nat8) : ?FT.MarginPayer { switch (c) { case 1 ?#desk; case 2 ?#counterparty; case _ null } };

  func encodeRepo(r : RepoRow) : Blob {
    let b = R.buf();
    R.putByte(b, repoStateCode(r.state)); R.putBool(b, r.reverse); R.putText(b, r.book, 32); R.putNat(b, r.cpHash, 8); R.putText(b, r.currency, 8); R.putNat(b, r.cash, 8); R.putNat(b, r.rateBps, 4); R.putByte(b, r.dayCount);
    R.putNat(b, r.start, 4); R.putNat(b, r.maturity, 4); R.putText(b, r.isin, 12); R.putNat(b, r.nominal, 8); R.putNat(b, r.haircutBps, 4); R.putNat(b, r.thresholdBps, 4); R.putNat(b, r.accrualFrom, 4);
    putInt(b, r.accruedBefore); putInt(b, r.accruedPosted); putInt(b, r.marginCash); R.putNat(b, r.marginCalled, 8); R.putByte(b, r.marginPayer); R.putNat(b, r.marginDue, 4); R.putNat(b, r.lastValue, 8);
    R.putText(b, r.depot, 32); R.putNat(b, r.lastBlock, 8); R.putNat(b, r.refHash, 8);
    R.done(b, REPO_ROW_BYTES)
  };
  func decodeRepo(id : Nat, v : Blob) : RepoRow {
    let a = Blob.toArray(v);
    { id; state = repoStateOf(a[0]); reverse = R.getBool(a, 1); book = R.getText(a, 2, 32); cpHash = R.getNat(a, 34, 8); currency = R.getText(a, 42, 8); cash = R.getNat(a, 50, 8); rateBps = R.getNat(a, 58, 4); dayCount = a[62];
      start = R.getNat(a, 63, 4); maturity = R.getNat(a, 67, 4); isin = R.getText(a, 71, 12); nominal = R.getNat(a, 83, 8); haircutBps = R.getNat(a, 91, 4); thresholdBps = R.getNat(a, 95, 4); accrualFrom = R.getNat(a, 99, 4);
      accruedBefore = getInt(a, 103); accruedPosted = getInt(a, 112); marginCash = getInt(a, 121); marginCalled = R.getNat(a, 130, 8); marginPayer = a[138]; marginDue = R.getNat(a, 139, 4); lastValue = R.getNat(a, 143, 8);
      depot = R.getText(a, 151, 32); lastBlock = R.getNat(a, 183, 8); refHash = R.getNat(a, 191, 8) }
  };
  func encodeLoan(r : LoanRow) : Blob {
    let b = R.buf();
    R.putByte(b, loanStateCode(r.state)); R.putText(b, r.book, 32); R.putNat(b, r.cpHash, 8); R.putText(b, r.currency, 8); R.putText(b, r.isin, 12); R.putNat(b, r.nominal, 8); R.putNat(b, r.valueMicro, 8); R.putNat(b, r.feeBps, 4); R.putByte(b, r.dayCount);
    R.putNat(b, r.cashCollateral, 8); R.putNat(b, r.rebateBps, 4); R.putText(b, r.collateralIsin, 12); R.putNat(b, r.collateralNominal, 8); R.putNat(b, r.start, 4); R.putNat(b, r.noticeDays, 2); R.putNat(b, r.returnDay, 4); R.putNat(b, r.accrualFrom, 4);
    putInt(b, r.feeBefore); putInt(b, r.feePosted); putInt(b, r.rebateBefore); putInt(b, r.rebatePosted); R.putNat(b, r.manufactured, 8); R.putText(b, r.depot, 32); R.putNat(b, r.lastBlock, 8); R.putNat(b, r.refHash, 8);
    R.done(b, LOAN_ROW_BYTES)
  };
  func decodeLoan(id : Nat, v : Blob) : LoanRow {
    let a = Blob.toArray(v);
    { id; state = loanStateOf(a[0]); book = R.getText(a, 1, 32); cpHash = R.getNat(a, 33, 8); currency = R.getText(a, 41, 8); isin = R.getText(a, 49, 12); nominal = R.getNat(a, 61, 8); valueMicro = R.getNat(a, 69, 8); feeBps = R.getNat(a, 77, 4); dayCount = a[81];
      cashCollateral = R.getNat(a, 82, 8); rebateBps = R.getNat(a, 90, 4); collateralIsin = R.getText(a, 94, 12); collateralNominal = R.getNat(a, 106, 8); start = R.getNat(a, 114, 4); noticeDays = R.getNat(a, 118, 2); returnDay = R.getNat(a, 120, 4); accrualFrom = R.getNat(a, 124, 4);
      feeBefore = getInt(a, 128); feePosted = getInt(a, 137); rebateBefore = getInt(a, 146); rebatePosted = getInt(a, 155); manufactured = R.getNat(a, 164, 8); depot = R.getText(a, 172, 32); lastBlock = R.getNat(a, 204, 8); refHash = R.getNat(a, 212, 8) }
  };

  public type State = {
    repos : RI.State;        // id(8) -> row
    loans : RI.State;        // id(8) -> row
    reposByBook : RI.State;  // book(32) ‖ id(8)
    loansByBook : RI.State;  // book(32) ‖ id(8)
    loansByLot : RI.State;   // lot(8) ‖ loan(8) -> nominal(8): what of a lot is out on loan
    reposByLot : RI.State;   // lot(8) ‖ repo(8) -> nominal(8): what of a lot is pledged to a repo
    var policy : ?FT.Policy;
    var repoCount : Nat;
    var loanCount : Nat;
    var openRepos : Nat;
    var openLoans : Nat;
    var marginCallsOpen : Nat;
  };

  public func newState(arena : RI.Arena) : State {
    {
      repos = RI.newStateIn(arena, { keyBytes = 8; valBytes = REPO_ROW_BYTES });
      loans = RI.newStateIn(arena, { keyBytes = 8; valBytes = LOAN_ROW_BYTES });
      reposByBook = RI.newStateIn(arena, { keyBytes = 40; valBytes = 1 });
      loansByBook = RI.newStateIn(arena, { keyBytes = 40; valBytes = 1 });
      loansByLot = RI.newStateIn(arena, { keyBytes = 16; valBytes = 8 });
      reposByLot = RI.newStateIn(arena, { keyBytes = 16; valBytes = 8 });
      var policy = null; var repoCount = 0; var loanCount = 0; var openRepos = 0; var openLoans = 0; var marginCallsOpen = 0;
    }
  };

  // ─── lookups ───────────────────────────────────────────────────────────────

  public func policy(s : State) : ?FT.Policy { s.policy };
  public func repo(s : State, id : Nat) : ?RepoRow { switch (RI.get(s.repos, R.key(id, 8))) { case (?v) ?decodeRepo(id, v); case null null } };
  public func loan(s : State, id : Nat) : ?LoanRow { switch (RI.get(s.loans, R.key(id, 8))) { case (?v) ?decodeLoan(id, v); case null null } };
  func putRepo(s : State, r : RepoRow) { ignore RI.put(s.repos, R.key(r.id, 8), encodeRepo(r)) };
  func putLoan(s : State, r : LoanRow) { ignore RI.put(s.loans, R.key(r.id, 8), encodeLoan(r)) };
  func idsUnder(idx : RI.State, lo : Blob, hi : Blob, offset : Nat) : [Nat] {
    let out = List.empty<Nat>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, _) in page.entries.vals()) List.add(out, R.getNat(Blob.toArray(k), offset, 8));
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  func textPrefixRange(t : Text, width : Nat, rest : Nat) : (Blob, Blob) {
    let p = Blob.toArray(R.textKey(t, width));
    (Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(0, rest))), Blob.fromArray(Array.concat<Nat8>(p, Array.repeat<Nat8>(255, rest))))
  };
  public func reposOfBook(s : State, book : Text) : [RepoRow] { let (lo, hi) = textPrefixRange(book, 32, 8); Array.filterMap<Nat, RepoRow>(idsUnder(s.reposByBook, lo, hi, 32), func(id) { repo(s, id) }) };
  public func loansOfBook(s : State, book : Text) : [LoanRow] { let (lo, hi) = textPrefixRange(book, 32, 8); Array.filterMap<Nat, LoanRow>(idsUnder(s.loansByBook, lo, hi, 32), func(id) { loan(s, id) }) };
  public func openReposInBook(s : State, book : Text) : [RepoRow] { Array.filter<RepoRow>(reposOfBook(s, book), func(r) { r.state != #closed }) };
  public func openLoansInBook(s : State, book : Text) : [LoanRow] { Array.filter<LoanRow>(loansOfBook(s, book), func(r) { r.state != #returned }) };
  /// What of a lot is out on loan, across the open loans.
  public func lentOfLot(s : State, lot : TT.DealId) : Nat {
    let (lo, hi) = R.prefixRange(lot, 8, 8);
    var n = 0;
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.loansByLot, lo, hi, cursor, MAX_PAGE);
      for ((_, v) in page.entries.vals()) n += R.getNat(Blob.toArray(v), 0, 8);
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    n
  };
  /// The loans a lot is out on, with the nominal on each.
  public func loansOfLot(s : State, lot : TT.DealId) : [(FT.LoanId, Nat)] {
    let (lo, hi) = R.prefixRange(lot, 8, 8);
    let out = List.empty<(Nat, Nat)>();
    var cursor : ?Blob = null;
    label walk loop {
      let page = RI.range(s.loansByLot, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) { let n = R.getNat(Blob.toArray(v), 0, 8); if (n > 0) List.add(out, (R.getNat(Blob.toArray(k), 8, 8), n)) };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };

  // ─── the arithmetic ────────────────────────────────────────────────────────

  public func repoInterestTarget(r : RepoRow, day : Nat) : Int {
    let to = if (r.maturity > 0 and day > r.maturity) r.maturity else day;
    if (to <= r.accrualFrom) return r.accruedBefore;
    r.accruedBefore + M.simpleInterestTo(r.cash, r.rateBps, convOf(r.dayCount), r.accrualFrom, to)
  };
  public func loanValue(r : LoanRow) : Nat { M.cleanCost(r.nominal, r.valueMicro) };
  public func loanFeeTarget(r : LoanRow, day : Nat) : Int {
    let to = if (r.returnDay > 0 and day > r.returnDay) r.returnDay else day;
    if (to <= r.accrualFrom) return r.feeBefore;
    r.feeBefore + M.simpleInterestTo(loanValue(r), r.feeBps, convOf(r.dayCount), r.accrualFrom, to)
  };
  public func loanRebateTarget(r : LoanRow, day : Nat) : Int {
    if (r.cashCollateral == 0) return 0;
    let to = if (r.returnDay > 0 and day > r.returnDay) r.returnDay else day;
    if (to <= r.accrualFrom) return r.rebateBefore;
    r.rebateBefore + M.simpleInterestTo(r.cashCollateral, r.rebateBps, convOf(r.dayCount), r.accrualFrom, to)
  };
  /// The collateral's value after the haircut, at a price per 100.
  public func collateralValue(nominal : Nat, priceMicro : Nat, haircutBps : Nat) : Nat { M.roundNat(M.q(M.cleanCost(nominal, priceMicro) * (10_000 - haircutBps), 10_000)) };
  /// The cash lender's exposure: the cash and the interest to the day, less the margin cash it holds.
  public func exposure(r : RepoRow, day : Nat) : Nat {
    let e : Int = (r.cash : Int) + repoInterestTarget(r, day) - (if (r.reverse) r.marginCash else -r.marginCash);
    if (e < 0) 0 else Int.abs(e)
  };
  /// The margin call a mark raises: the shortfall beyond the threshold, payable by the collateral's giver, or the
  /// excess beyond it, payable back by the cash lender. The desk is the giver of a repo, the lender of a reverse.
  public func marginCall(r : RepoRow, value : Nat, exposure_ : Nat) : ?(Nat, FT.MarginPayer) {
    let threshold = M.roundNat(M.q(exposure_ * r.thresholdBps, 10_000));
    let giver : FT.MarginPayer = if (r.reverse) #counterparty else #desk;
    let lender : FT.MarginPayer = if (r.reverse) #desk else #counterparty;
    if (value + threshold < exposure_) return ?(exposure_ - value, giver);
    if (value > exposure_ + threshold) return ?(value - exposure_, lender);
    null
  };

  // ─── planners ─────────────────────────────────────────────────────────────

  type Res<X> = Result.Result<X, FT.Error>;
  func bad<X>(reason : Text) : Res<X> { #err(#InvalidTerms({ reason })) };
  func bytesOf(t : Text) : Nat { Text.encodeUtf8(t).size() };

  public func planPolicy(p : FT.Policy) : Res<FT.Event> {
    for (a in [p.repoPayable, p.reverseRepoReceivable, p.repoInterestPayable, p.repoInterestReceivable, p.repoInterestExpense, p.repoInterestIncome, p.marginCashGiven, p.marginCashReceived, p.lendingFeeReceivable, p.lendingFeeIncome, p.cashCollateralPayable, p.rebateExpense, p.manufacturedPaymentReceivable].vals()) {
      if (bytesOf(a) == 0 or bytesOf(a) > 32) return bad("every account is named in 1..32 bytes");
    };
    if (p.marginGraceDays > 30) return bad("the margin grace is at most 30 days");
    #ok(#policySet(p))
  };
  public func accountsOf(p : FT.Policy) : [Text] { [p.repoPayable, p.reverseRepoReceivable, p.repoInterestPayable, p.repoInterestReceivable, p.repoInterestExpense, p.repoInterestIncome, p.marginCashGiven, p.marginCashReceived, p.lendingFeeReceivable, p.lendingFeeIncome, p.cashCollateralPayable, p.rebateExpense, p.manufacturedPaymentReceivable] };

  public func validateRepo(t : FT.RepoTerms, day : Nat) : ?FT.Error {
    func b(reason : Text) : ?FT.Error { ?#InvalidTerms({ reason }) };
    if (bytesOf(t.currency) != 3) return b("a currency code has three letters");
    if (t.cash == 0) return b("the purchase price is positive");
    if (t.rateBps > 100_000) return b("the rate is at most 1,000 percent");
    if (t.start < day) return b("the start is not in the past");
    switch (t.maturity) { case (?m) { if (m <= t.start) return b("the maturity is after the start") }; case null {} };
    if (bytesOf(t.collateral.isin) != 12 or t.collateral.nominal == 0) return b("the collateral is an ISIN and a positive nominal");
    if (t.haircutBps >= 10_000) return b("the haircut is below 100 percent");
    if (t.thresholdBps > 5_000) return b("the threshold is at most 50 percent");
    if (bytesOf(t.depot) == 0 or bytesOf(t.depot) > 32) return b("the depot is named in 1..32 bytes");
    null
  };
  public func validateLoan(t : FT.LoanTerms, day : Nat) : ?FT.Error {
    func b(reason : Text) : ?FT.Error { ?#InvalidTerms({ reason }) };
    if (bytesOf(t.isin) != 12 or t.nominal == 0) return b("the loan is an ISIN and a positive nominal");
    if (bytesOf(t.currency) != 3) return b("a currency code has three letters");
    if (t.valueMicro == 0) return b("the loan's value is a positive price per 100");
    if (t.feeBps > 100_000) return b("the fee is at most 1,000 percent");
    if (t.start < day) return b("the start is not in the past");
    if (t.noticeDays > 365) return b("the notice is at most a year");
    switch (t.collateral) {
      case (#cash(c)) { if (c.amount == 0) return b("cash collateral is positive"); if (c.rebateBps > 100_000) return b("the rebate is at most 1,000 percent") };
      case (#securities(c)) { if (bytesOf(c.isin) != 12 or c.nominal == 0) return b("securities collateral is an ISIN and a positive nominal") };
    };
    if (bytesOf(t.depot) == 0 or bytesOf(t.depot) > 32) return b("the depot is named in 1..32 bytes");
    null
  };
  /// Lots allocated first in first out from what is available, for a pledge or a loan; refused short.
  public func allocate(available : [(TT.DealId, Nat)], nominal : Nat, isin : Text) : Res<[(TT.DealId, Nat)]> {
    var left = nominal;
    let out = List.empty<(TT.DealId, Nat)>();
    var total = 0;
    for ((lot, n) in available.vals()) { total += n; if (left > 0 and n > 0) { let q = Nat.min(left, n); List.add(out, (lot, q)); left -= q } };
    if (left > 0) return #err(#InsufficientCollateral({ isin; available = total; wanted = nominal }));
    #ok(List.toArray(out))
  };
  public func planRateReset(s : State, id : FT.RepoId, rateBps : Nat, day : Nat) : Res<FT.Event> {
    let ?r = repo(s, id) else return #err(#UnknownRepo({ repo = id }));
    if (r.state != #started) return #err(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "started" }));
    if (r.maturity != 0) return bad("only an open repo is re-priced");
    if (day < r.accrualFrom) return bad("the reset is not before the last base day");
    if (rateBps > 100_000) return bad("the rate is at most 1,000 percent");
    #ok(#repoRateReset({ repo = id; rateBps; day; catchUp = repoInterestTarget(r, day) - r.accruedPosted }))
  };
  /// The mark of a started repo's collateral at the day's price: the value, the exposure, and the call it raises
  /// when a call is not already open.
  public func planMark(s : State, r : RepoRow, priceMicro : Nat, day : Nat, due : Nat) : [FT.Event] {
    let value = collateralValue(r.nominal, priceMicro, r.haircutBps);
    let e = exposure(r, day);
    let out = List.empty<FT.Event>();
    List.add(out, #collateralMarked({ repo = r.id; value; exposure = e; priceMicro; day }));
    if (r.marginCalled == 0) {
      switch (marginCall(r, value, e)) { case (?(amount, payer)) List.add(out, #marginCallRaised({ repo = r.id; amount; payer; day; due })); case null {} };
    };
    List.toArray(out)
  };
  public func planMeetMargin(s : State, id : FT.RepoId, cash : Nat, collateral : ?FT.Collateral, priceMicro : Nat, lots : [(TT.DealId, Nat)], day : Nat) : Res<FT.Event> {
    let ?r = repo(s, id) else return #err(#UnknownRepo({ repo = id }));
    if (r.marginCalled == 0) return #err(#NoMarginCall({ repo = id }));
    let ?payer = payerOf(r.marginPayer) else return #err(#NoMarginCall({ repo = id }));
    var offered = cash;
    switch (collateral) {
      case (?c) {
        if (not Text.equal(c.isin, r.isin)) return bad("margin collateral is the repo's instrument");
        let giver : FT.MarginPayer = if (r.reverse) #counterparty else #desk;
        if (payer != giver) return bad("a call on the cash lender is met in cash");
        offered += collateralValue(c.nominal, priceMicro, r.haircutBps);
      };
      case null {};
    };
    if (offered < r.marginCalled) return #err(#ShortMargin({ repo = id; called = r.marginCalled; offered }));
    #ok(#marginMet({ repo = id; cash; collateral; lots; payer; day }))
  };
  public func planSubstitute(s : State, id : FT.RepoId, out : FT.Collateral, in_ : FT.Collateral, priceOut : Nat, priceIn : Nat, outLots : [(TT.DealId, Nat)], inLots : [(TT.DealId, Nat)], day : Nat) : Res<FT.Event> {
    let ?r = repo(s, id) else return #err(#UnknownRepo({ repo = id }));
    if (r.state != #started) return #err(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "started" }));
    if (not Text.equal(out.isin, r.isin)) return bad("the collateral going out is the repo's instrument");
    if (out.nominal == 0 or out.nominal > r.nominal) return bad("the collateral going out is within what is pledged");
    if (collateralValue(in_.nominal, priceIn, r.haircutBps) < collateralValue(out.nominal, priceOut, r.haircutBps)) return bad("the collateral coming in is worth at least what goes out, after the haircut");
    if (not Text.equal(in_.isin, r.isin)) return bad("a substitution keeps the repo's instrument; another instrument is a new repo");
    #ok(#collateralSubstituted({ repo = id; out; in_; outLots; inLots; day }))
  };
  public func planRecall(s : State, id : FT.LoanId, day : Nat, returnDay : Nat) : Res<FT.Event> {
    let ?r = loan(s, id) else return #err(#UnknownLoan({ loan = id }));
    if (r.state != #started) return #err(#LoanNotIn({ loan = id; state = FT.loanStateText(r.state); wanted = "started" }));
    #ok(#loanRecalled({ loan = id; day; returnDay }))
  };

  /// The legs an event posts. A repo: the cash against the payable (or the receivable for a reverse), the
  /// interest accrued to expense (or income), the margin cash given or received, the close reversing the lot. A
  /// loan: the cash collateral received against its payable, the fee to income against a receivable, the rebate
  /// to expense against the collateral payable, the return settling both; a manufactured payment as a receivable.
  public func legsOf(p : FT.Policy, cash : TT.CashAccount, r : ?RepoRow, l : ?LoanRow, ev : FT.Event) : [JT.Leg] {
    let ls = List.empty<JT.Leg>();
    let cs = switch (cash.sub) { case (?t) ?Posting.subledgerOf(t); case null null };
    func add(account : Text, sub : ?JT.SubledgerKey, side : JT.Side, ccy : Text, amount : Nat) { if (amount > 0) List.add(ls, Posting.leg(account, sub, side, ccy, amount)) };
    func signed(account : Text, sub : ?JT.SubledgerKey, ccy : Text, v : Int, up : JT.Side) { if (v > 0) add(account, sub, up, ccy, Int.abs(v)) else if (v < 0) add(account, sub, if (up == #debit) #credit else #debit, ccy, Int.abs(v)) };
    switch (ev, r) {
      case (#repoStarted(_), ?x) {
        let sub = ?repoSub(x.id);
        if (x.reverse) { add(p.reverseRepoReceivable, sub, #debit, x.currency, x.cash); add(cash.account, cs, #credit, x.currency, x.cash) }
        else { add(cash.account, cs, #debit, x.currency, x.cash); add(p.repoPayable, sub, #credit, x.currency, x.cash) };
      };
      case (#repoAccrued(e), ?x) { let sub = ?repoSub(x.id); if (x.reverse) { signed(p.repoInterestReceivable, sub, x.currency, e.interest, #debit); signed(p.repoInterestIncome, null, x.currency, e.interest, #credit) } else { signed(p.repoInterestExpense, null, x.currency, e.interest, #debit); signed(p.repoInterestPayable, sub, x.currency, e.interest, #credit) } };
      case (#repoRateReset(e), ?x) { let sub = ?repoSub(x.id); if (x.reverse) { signed(p.repoInterestReceivable, sub, x.currency, e.catchUp, #debit); signed(p.repoInterestIncome, null, x.currency, e.catchUp, #credit) } else { signed(p.repoInterestExpense, null, x.currency, e.catchUp, #debit); signed(p.repoInterestPayable, sub, x.currency, e.catchUp, #credit) } };
      case (#marginMet(e), ?x) {
        // margin cash returns what the other side holds before it becomes margin the other way: the desk paying
        // first gives back what it received, the counterparty paying first gives back what the desk gave
        let sub = ?repoSub(x.id);
        if (e.cash > 0) {
          switch (e.payer) {
            case (#desk) {
              let held = if (x.marginCash > 0) Int.abs(x.marginCash) else 0;
              let back = Nat.min(held, e.cash);
              add(p.marginCashReceived, sub, #debit, x.currency, back); add(p.marginCashGiven, sub, #debit, x.currency, e.cash - back); add(cash.account, cs, #credit, x.currency, e.cash);
            };
            case (#counterparty) {
              let given = if (x.marginCash < 0) Int.abs(x.marginCash) else 0;
              let back = Nat.min(given, e.cash);
              add(cash.account, cs, #debit, x.currency, e.cash); add(p.marginCashGiven, sub, #credit, x.currency, back); add(p.marginCashReceived, sub, #credit, x.currency, e.cash - back);
            };
          };
        };
      };
      case (#repoClosed(e), ?x) {
        // the interest posted so far leaves the payable (or the receivable); the rest of it to the day is caught up straight to expense (or income)
        let sub = ?repoSub(x.id);
        let posted = Int.abs(x.accruedPosted);
        let rest = if (e.interest > posted) e.interest - posted else 0;
        if (x.reverse) { add(cash.account, cs, #debit, x.currency, e.principal + e.interest); add(p.reverseRepoReceivable, sub, #credit, x.currency, e.principal); add(p.repoInterestReceivable, sub, #credit, x.currency, Nat.min(posted, e.interest)); add(p.repoInterestIncome, null, #credit, x.currency, rest) }
        else { add(p.repoPayable, sub, #debit, x.currency, e.principal); add(p.repoInterestPayable, sub, #debit, x.currency, Nat.min(posted, e.interest)); add(p.repoInterestExpense, null, #debit, x.currency, rest); add(cash.account, cs, #credit, x.currency, e.principal + e.interest) };
        // the margin cash goes back the way it came
        if (e.marginReturned > 0) { add(p.marginCashReceived, sub, #debit, x.currency, Int.abs(e.marginReturned)); add(cash.account, cs, #credit, x.currency, Int.abs(e.marginReturned)) }
        else if (e.marginReturned < 0) { add(cash.account, cs, #debit, x.currency, Int.abs(e.marginReturned)); add(p.marginCashGiven, sub, #credit, x.currency, Int.abs(e.marginReturned)) };
      };
      case (_) {};
    };
    switch (ev, l) {
      case (#loanStarted(_), ?x) { if (x.cashCollateral > 0) { let sub = ?loanSub(x.id); add(cash.account, cs, #debit, x.currency, x.cashCollateral); add(p.cashCollateralPayable, sub, #credit, x.currency, x.cashCollateral) } };
      case (#loanAccrued(e), ?x) { let sub = ?loanSub(x.id); signed(p.lendingFeeReceivable, sub, x.currency, e.fee, #debit); signed(p.lendingFeeIncome, null, x.currency, e.fee, #credit); signed(p.rebateExpense, null, x.currency, e.rebate, #debit); signed(p.cashCollateralPayable, sub, x.currency, e.rebate, #credit) };
      case (#loanReturned(e), ?x) {
        let sub = ?loanSub(x.id);
        // the fee comes in, what was accrued off the receivable and the rest to income; the cash collateral and the
        // rebate on it go back, the rebate's rest to expense
        let feePosted = Int.abs(x.feePosted); let feeRest = if (e.fee > feePosted) e.fee - feePosted else 0;
        add(cash.account, cs, #debit, x.currency, e.fee); add(p.lendingFeeReceivable, sub, #credit, x.currency, Nat.min(feePosted, e.fee)); add(p.lendingFeeIncome, null, #credit, x.currency, feeRest);
        if (x.cashCollateral > 0) {
          let rebatePosted = Int.abs(x.rebatePosted); let rebateRest = if (e.rebate > rebatePosted) e.rebate - rebatePosted else 0;
          add(p.cashCollateralPayable, sub, #debit, x.currency, x.cashCollateral + Nat.min(rebatePosted, e.rebate)); add(p.rebateExpense, null, #debit, x.currency, rebateRest); add(cash.account, cs, #credit, x.currency, x.cashCollateral + e.rebate);
        };
      };
      case (#manufacturedPayment(e), ?x) { add(p.manufacturedPaymentReceivable, ?loanSub(x.id), #debit, x.currency, e.amount); add(cash.account, cs, #credit, x.currency, e.amount) };
      case (_) {};
    };
    List.toArray(ls)
  };

  /// What falls due for a started repo on the day: the accrual to the day, then the close at maturity. The mark
  /// and the margin call are the end of day's, from the day's price.
  public func planRepoDue(s : State, id : FT.RepoId, day : Nat) : Res<?FT.Event> {
    let ?r = repo(s, id) else return #err(#UnknownRepo({ repo = id }));
    if (r.state != #started) return #ok(null);
    let target = repoInterestTarget(r, day);
    if (target != r.accruedPosted) return #ok(?#repoAccrued({ repo = id; interest = target - r.accruedPosted; day }));
    #ok(null)
  };
  /// The close of a started repo: at maturity, or by an act for an open repo; the repurchase price is the cash
  /// and the interest to the day, and the margin cash goes back.
  public func planClose(s : State, id : FT.RepoId, day : Nat) : Res<FT.Event> {
    let ?r = repo(s, id) else return #err(#UnknownRepo({ repo = id }));
    if (r.state != #started) return #err(#RepoNotIn({ repo = id; state = FT.repoStateText(r.state); wanted = "started" }));
    if (r.maturity != 0 and day < r.maturity) return #err(#NotDue({ id; due = r.maturity; day }));
    if (repoInterestTarget(r, day) < r.accruedPosted) return bad("the close is not dated before the interest already accrued");
    // the interest is the whole of it to the day: what was accrued and what the close catches up; an open call is
    // moot at the close, since every margin cash goes back in full with it
    #ok(#repoClosed({ repo = id; principal = r.cash; interest = Int.abs(repoInterestTarget(r, day)); marginReturned = r.marginCash; day }))
  };
  public func planLoanDue(s : State, id : FT.LoanId, day : Nat) : Res<?FT.Event> {
    let ?r = loan(s, id) else return #err(#UnknownLoan({ loan = id }));
    if (r.state != #started and r.state != #recalled) return #ok(null);
    let fee = loanFeeTarget(r, day); let rebate = loanRebateTarget(r, day);
    if (fee != r.feePosted or rebate != r.rebatePosted) return #ok(?#loanAccrued({ loan = id; fee = fee - r.feePosted; rebate = rebate - r.rebatePosted; day }));
    #ok(null)
  };
  public func planReturn(s : State, id : FT.LoanId, day : Nat) : Res<FT.Event> {
    let ?r = loan(s, id) else return #err(#UnknownLoan({ loan = id }));
    if (r.state != #recalled) return #err(#LoanNotIn({ loan = id; state = FT.loanStateText(r.state); wanted = "recalled" }));
    if (day < r.returnDay) return #err(#NotDue({ id; due = r.returnDay; day }));
    if (loanFeeTarget(r, day) < r.feePosted) return bad("the return is not dated before the fee already accrued");
    #ok(#loanReturned({ loan = id; fee = Int.abs(loanFeeTarget(r, day)); rebate = Int.abs(loanRebateTarget(r, day)); day }))
  };

  // ─── the fold ──────────────────────────────────────────────────────────────

  func indexRepo(s : State, r : RepoRow) { ignore RI.put(s.reposByBook, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.book, 32)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1])) };
  func indexLoan(s : State, r : LoanRow) { ignore RI.put(s.loansByBook, Blob.fromArray(Array.concat<Nat8>(Blob.toArray(R.textKey(r.book, 32)), Blob.toArray(R.key(r.id, 8)))), Blob.fromArray([1])) };
  func withRepo(s : State, id : Nat, block : Nat, f : RepoRow -> RepoRow) { switch (repo(s, id)) { case (?r) putRepo(s, f({ r with lastBlock = block })); case null {} } };
  func withLoan(s : State, id : Nat, block : Nat, f : LoanRow -> LoanRow) { switch (loan(s, id)) { case (?r) putLoan(s, f({ r with lastBlock = block })); case null {} } };

  public func fold(s : State, block : Nat, ev : FT.Event) {
    switch (ev) {
      case (#policySet(p)) s.policy := ?p;
      case (#repoOpened(x)) {
        let t = x.terms;
        let r : RepoRow = { id = block; state = #open; reverse = t.reverse; book = x.book; cpHash = hash8(x.counterparty.name); currency = t.currency; cash = t.cash; rateBps = t.rateBps; dayCount = convCode(t.dayCount); start = t.start;
                            maturity = switch (t.maturity) { case (?m) m; case null 0 }; isin = t.collateral.isin; nominal = t.collateral.nominal; haircutBps = t.haircutBps; thresholdBps = t.thresholdBps; accrualFrom = t.start; accruedBefore = 0; accruedPosted = 0;
                            marginCash = 0; marginCalled = 0; marginPayer = 0; marginDue = 0; lastValue = 0; depot = t.depot; lastBlock = block; refHash = hash8(x.reference) };
        putRepo(s, r); indexRepo(s, r); s.repoCount += 1; s.openRepos += 1;
      };
      case (#repoStarted(x)) { withRepo(s, x.repo, block, func(r) { { r with state = #started } }); pledge(s, x.repo, x.lots, true) };
      case (#repoAccrued(x)) withRepo(s, x.repo, block, func(r) { { r with accruedPosted = r.accruedPosted + x.interest } });
      case (#repoRateReset(x)) withRepo(s, x.repo, block, func(r) { let posted = r.accruedPosted + x.catchUp; { r with rateBps = x.rateBps; accrualFrom = x.day; accruedBefore = posted; accruedPosted = posted } });
      case (#collateralMarked(x)) withRepo(s, x.repo, block, func(r) { { r with lastValue = x.value } });
      case (#marginCallRaised(x)) { withRepo(s, x.repo, block, func(r) { { r with marginCalled = x.amount; marginPayer = payerCode(x.payer); marginDue = x.due } }); s.marginCallsOpen += 1 };
      case (#marginMet(x)) {
        withRepo(s, x.repo, block, func(r) {
          let cashMoved : Int = switch (x.payer) { case (#desk) -(x.cash : Int); case (#counterparty) (x.cash : Int) };
          let more = switch (x.collateral) { case (?c) c.nominal; case null 0 };
          { r with marginCash = r.marginCash + cashMoved; nominal = r.nominal + more; marginCalled = 0; marginPayer = 0; marginDue = 0 }
        });
        pledge(s, x.repo, x.lots, true);
        if (s.marginCallsOpen > 0) s.marginCallsOpen -= 1;
      };
      case (#collateralSubstituted(x)) { withRepo(s, x.repo, block, func(r) { { r with nominal = r.nominal - x.out.nominal + x.in_.nominal } }); pledge(s, x.repo, x.outLots, false); pledge(s, x.repo, x.inLots, true) };
      case (#repoClosed(x)) {
        switch (repo(s, x.repo)) { case (?r) { if (r.marginCalled > 0 and s.marginCallsOpen > 0) s.marginCallsOpen -= 1 }; case null {} };
        withRepo(s, x.repo, block, func(r) { { r with state = #closed; marginCash = 0; marginCalled = 0; marginPayer = 0; marginDue = 0 } });
        for ((lot, _) in lotsOfRepo(s, x.repo).vals()) ignore RI.put(s.reposByLot, R.key2(lot, 8, x.repo, 8), R.key(0, 8));
        if (s.openRepos > 0) s.openRepos -= 1;
      };
      case (#loanOpened(x)) {
        let t = x.terms;
        let (cc, rb, ci, cn) = switch (t.collateral) { case (#cash(c)) (c.amount, c.rebateBps, "", 0); case (#securities(c)) (0, 0, c.isin, c.nominal) };
        let r : LoanRow = { id = block; state = #open; book = x.book; cpHash = hash8(x.counterparty.name); currency = t.currency; isin = t.isin; nominal = t.nominal; valueMicro = t.valueMicro; feeBps = t.feeBps; dayCount = convCode(t.dayCount);
                            cashCollateral = cc; rebateBps = rb; collateralIsin = ci; collateralNominal = cn; start = t.start; noticeDays = t.noticeDays; returnDay = 0; accrualFrom = t.start;
                            feeBefore = 0; feePosted = 0; rebateBefore = 0; rebatePosted = 0; manufactured = 0; depot = t.depot; lastBlock = block; refHash = hash8(x.reference) };
        putLoan(s, r); indexLoan(s, r); s.loanCount += 1; s.openLoans += 1;
      };
      case (#loanStarted(x)) {
        withLoan(s, x.loan, block, func(r) { { r with state = #started } });
        for ((lot, n) in x.lots.vals()) ignore RI.put(s.loansByLot, R.key2(lot, 8, x.loan, 8), R.key(n, 8));
      };
      case (#loanAccrued(x)) withLoan(s, x.loan, block, func(r) { { r with feePosted = r.feePosted + x.fee; rebatePosted = r.rebatePosted + x.rebate } });
      case (#loanRecalled(x)) withLoan(s, x.loan, block, func(r) { { r with state = #recalled; returnDay = x.returnDay } });
      case (#loanReturned(x)) {
        withLoan(s, x.loan, block, func(r) { { r with state = #returned } });
        // the lots come back: every entry of the loan is zeroed
        for ((lot, _) in lotsOfLoan(s, x.loan).vals()) ignore RI.put(s.loansByLot, R.key2(lot, 8, x.loan, 8), R.key(0, 8));
        if (s.openLoans > 0) s.openLoans -= 1;
      };
      case (#manufacturedPayment(x)) withLoan(s, x.loan, block, func(r) { { r with manufactured = r.manufactured + x.amount } });
    }
  };
  /// The lots a loan took, or a repo holds pledged, with what is out on each: the index is keyed by lot, so the
  /// walk is over the lots on loan or pledged, which is bounded by the open financings.
  func lotsUnder(idx : RI.State, id : Nat) : [(TT.DealId, Nat)] {
    let out = List.empty<(Nat, Nat)>();
    var cursor : ?Blob = null;
    let (lo, hi) = R.fullRange(16);
    label walk loop {
      let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
      for ((k, v) in page.entries.vals()) { let a = Blob.toArray(k); if (R.getNat(a, 8, 8) == id) { let n = R.getNat(Blob.toArray(v), 0, 8); if (n > 0) List.add(out, (R.getNat(a, 0, 8), n)) } };
      switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
    };
    List.toArray(out)
  };
  public func lotsOfLoan(s : State, id : FT.LoanId) : [(TT.DealId, Nat)] { lotsUnder(s.loansByLot, id) };
  public func lotsOfRepo(s : State, id : FT.RepoId) : [(TT.DealId, Nat)] { lotsUnder(s.reposByLot, id) };
  func pledge(s : State, id : FT.RepoId, lots : [(TT.DealId, Nat)], more : Bool) {
    for ((lot, n) in lots.vals()) {
      let have = switch (RI.get(s.reposByLot, R.key2(lot, 8, id, 8))) { case (?v) R.getNat(Blob.toArray(v), 0, 8); case null 0 };
      ignore RI.put(s.reposByLot, R.key2(lot, 8, id, 8), R.key(if (more) have + n else (if (n > have) 0 else have - n), 8));
    };
  };

  // ─── reads ────────────────────────────────────────────────────────────────

  public func repoView(r : RepoRow, counterparty : Text, reference : Text) : FT.RepoView {
    { id = r.id; book = r.book; counterparty; reference; reverse = r.reverse; currency = r.currency; cash = r.cash; rateBps = r.rateBps; start = r.start; maturity = if (r.maturity == 0) null else ?r.maturity; isin = r.isin; nominal = r.nominal;
      haircutBps = r.haircutBps; thresholdBps = r.thresholdBps; state = FT.repoStateText(r.state); accruedPosted = r.accruedPosted; marginCash = r.marginCash; marginCalled = r.marginCalled;
      marginPayer = switch (payerOf(r.marginPayer)) { case (?p) ?FT.payerText(p); case null null }; marginDue = if (r.marginDue == 0) null else ?r.marginDue; lastValue = r.lastValue; lastBlock = r.lastBlock }
  };
  public func loanView(r : LoanRow, counterparty : Text, reference : Text) : FT.LoanView {
    { id = r.id; book = r.book; counterparty; reference; isin = r.isin; nominal = r.nominal; currency = r.currency; feeBps = r.feeBps; start = r.start; state = FT.loanStateText(r.state); feePosted = r.feePosted; rebatePosted = r.rebatePosted;
      returnDay = if (r.returnDay == 0) null else ?r.returnDay; manufactured = r.manufactured; lastBlock = r.lastBlock }
  };
  public func status(s : State) : FT.Status { { repos = s.repoCount; loans = s.loanCount; openRepos = s.openRepos; openLoans = s.openLoans; marginCallsOpen = s.marginCallsOpen } };

  public func fingerprintInto(w : C.Writer, s : State) {
    switch (s.policy) { case null w.byte(0); case (?p) { w.byte(1); for (a in accountsOf(p).vals()) w.text(a); w.nat(p.marginGraceDays) } };
    w.nat(s.repoCount); w.nat(s.loanCount); w.nat(s.openRepos); w.nat(s.openLoans); w.nat(s.marginCallsOpen);
    for ((idx, width) in [(s.repos, 8), (s.loans, 8), (s.reposByBook, 40), (s.loansByBook, 40), (s.loansByLot, 16), (s.reposByLot, 16)].vals()) {
      let (lo, hi) = R.fullRange(width);
      var n = 0;
      var cursor : ?Blob = null;
      label walk loop {
        let page = RI.range(idx, lo, hi, cursor, MAX_PAGE);
        for ((k, v) in page.entries.vals()) { w.blob(k); w.blob(v); n += 1 };
        switch (page.cursor) { case null break walk; case (?c) cursor := ?c };
      };
      w.nat(n);
    };
  };
}
