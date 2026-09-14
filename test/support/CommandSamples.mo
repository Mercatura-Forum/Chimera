/// CommandSamples.mo: one sample of every command family, for the freeze and the round trips.
///
/// The values are chosen so every optional takes both branches somewhere and every text is non-empty; the hash of
/// each sample under each encoding version is recorded in `test/CommandVectors.mo`, and a recorded hash that moves
/// is a build failure.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Blob "mo:core/Blob";
import Principal "mo:core/Principal";

import Freeze "mo:kernel/domain/Freeze";
import TT "mo:manticore/TreasuryTypes";

import CallT "../../src/CallTypes";
import CuT "../../src/CustodyTypes";
import ST "../../src/SettlementTypes";
import FT "../../src/FinancingTypes";

import T "../../src/DeskTypes";
import Can "../../src/DeskCanonical";

module {

  public func alice() : Principal { Principal.fromText("2vxsx-fae") };
  public func bob() : Principal { Principal.fromText("aaaaa-aa") };
  public func h(n : Nat8) : Blob { Blob.fromArray([n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n]) };

  public let policy : TT.Policy = {
    mmPlacements = "1300"; mmTakings = "2300"; mmInterestReceivable = "1310"; mmInterestPayable = "2310"; mmInterestIncome = "4320"; mmInterestExpense = "5320";
    fxForwardMark = "1400"; irsMark = "1410"; fxOptionValue = "1420"; unrealisedTradingGain = "4400"; unrealisedTradingLoss = "5400"; realisedTradingGain = "4410"; realisedTradingLoss = "5410";
    securitiesAmortisedCost = "1500"; securitiesFvoci = "1510"; securitiesFvtpl = "1520"; fvociReserve = "3500"; couponReceivable = "1530"; couponIncome = "4500"; amortisationIncome = "4510"; amortisationExpense = "5510";
    nostroSuspense = "1990"; lotMethod = #fifo; confirmationDueDays = 2; breakAgeAlertDays = 5; maxCurvePoints = 8;
  };
  public let citi : TT.Counterparty = { party = null; name = "CITI"; bic = "CITIUS33"; lei = "6SHGI4ZSSLCXXQSBB395" };
  public let nostro : TT.CashAccount = { account = "1100"; sub = ?"NOSTRO-USD" };
  public let cash : TT.CashAccount = { account = "1101"; sub = null };
  public let forward : TT.FxForward = { base = "USD"; quote = "EGP"; direction = #buy; baseAmount = 500_000_00; rateMicro = 48_120_000; valueDate = 20700; spotMicro = 48_000_000; forwardPointsMicro = 120_000; baseAccount = nostro; quoteAccount = cash; pointsCurve = "USDEGP-PTS"; discountCurve = "EGP-ZERO" };
  public let mm : TT.MoneyMarket = { placement = true; currency = "USD"; principal = 1_000_000_00; rateBps = 450; dayCount = #a003_Act360; start = 20670; maturity = 20700; cash = nostro };
  public let security : TT.SecurityTrade = { isin = "EG0000012345"; direction = #buy; nominal = 10_000_000_00; priceMicro = 98_500_000; settlement = 20672; classification = #fvoci; cash; priceCurve = "EG0000012345"; venue = ?"EGX" };

  public let callTerms : CallT.Terms = { placement = true; currency = "USD"; principal = 750_000_00; rateBps = 430; dayCount = #a003_Act360; noticeDays = 7; interestEveryDays = 30; capitalise = false; cash = nostro; start = 20670 };

  public let depot : CuT.Depot = { id = "DEPOT-CITI"; custodian = citi; place = "MCSD"; safekeepingAccount = "SAFE-001" };
  public let announcement : CuT.Announcement = { isin = "EG0000012345"; kind = #coupon({ perHundredMicro = 6_000_000 }); recordDate = 20700; exDate = 20699; paymentDate = 20702; source = "\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07" : Blob };

  public func venue() : ST.Venue { { core = bob(); deadlineSecs = 3600; recycleLimit = 3; claimsAccount = "1540" } };
  public func instruction() : ST.Instruction { { family = #treasury; deal = 40; leg = 0; cycle = 20672; role = #taker; counterparty = alice(); assetLedger = bob(); assetAmount = 10_000_000_00; cashLedger = alice(); cashAmount = 9_850_000_00; tradeId = ?7; reference = "security-1"; documentHash = "\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07\07" : Blob } };

  public let financingPolicy : FT.Policy = { repoPayable = "2600"; reverseRepoReceivable = "1600"; repoInterestPayable = "2610"; repoInterestReceivable = "1610"; repoInterestExpense = "5600"; repoInterestIncome = "4600"; marginCashGiven = "1620"; marginCashReceived = "2620"; lendingFeeReceivable = "1630"; lendingFeeIncome = "4630"; cashCollateralPayable = "2630"; rebateExpense = "5630"; manufacturedPaymentReceivable = "1640"; marginGraceDays = 2 };
  public let repoTerms : FT.RepoTerms = { reverse = false; currency = "EGP"; cash = 9_500_000_00; rateBps = 1900; dayCount = #a004_Act365Fixed; start = 20670; maturity = ?20700; collateral = { isin = "EG0000012345"; nominal = 10_000_000_00 }; haircutBps = 500; thresholdBps = 200; cashAccount = cash; depot = "DEPOT-CITI" };
  public let loanTerms : FT.LoanTerms = { isin = "EG0000012345"; nominal = 5_000_000_00; currency = "EGP"; valueMicro = 98_000_000; feeBps = 50; dayCount = #a004_Act365Fixed; collateral = #cash({ amount = 5_100_000_00; rebateBps = 1800 }); start = 20670; noticeDays = 3; cashAccount = cash; depot = "DEPOT-CITI" };

  public func samples() : [Freeze.Sample<T.Command>] {
    [
      { family = "setFinancingPolicy"; command = #setFinancingPolicy(financingPolicy) },
      { family = "openRepo"; command = #openRepo({ book = "BR01"; counterparty = citi; terms = repoTerms; reference = "repo-1" }) },
      { family = "settleRepoLeg"; command = #settleRepoLeg({ repo = 80; leg = 0; postingDate = 20670; valueDate = 20670; period = "2026-08"; narration = "repo start" }) },
      { family = "resetRepoRate"; command = #resetRepoRate({ repo = 80; rateBps = 1950; postingDate = 20675; valueDate = 20675; period = "2026-08"; narration = "reprice" }) },
      { family = "meetMarginCall"; command = #meetMarginCall({ repo = 80; cash = 50_000_00; collateral = ?{ isin = "EG0000012345"; nominal = 200_000_00 }; postingDate = 20676; valueDate = 20676; period = "2026-08"; narration = "margin" }) },
      { family = "substituteCollateral"; command = #substituteCollateral({ repo = 80; out = { isin = "EG0000012345"; nominal = 1_000_000_00 }; in_ = { isin = "EG0000012345"; nominal = 1_050_000_00 } }) },
      { family = "openLoan"; command = #openLoan({ book = "BR01"; counterparty = citi; terms = loanTerms; reference = "loan-1" }) },
      { family = "settleLoanLeg"; command = #settleLoanLeg({ loan = 81; leg = 1; postingDate = 20690; valueDate = 20690; period = "2026-08"; narration = "loan return" }) },
      { family = "recallLoan"; command = #recallLoan({ loan = 81 }) },
      { family = "instructFinancing"; command = #instructFinancing({ family = #repo; id = 80; leg = 1; counterparty = alice(); tradeId = ?9; reference = "repo-1/close" }) },
      { family = "setSettlementVenue"; command = #setSettlementVenue({ venue = venue() }) },
      { family = "setSettlementLedger"; command = #setSettlementLedger({ declaration = { role = #security({ isin = "EG0000012345" }); ledger = bob(); partial = true } }) },
      { family = "openSettlementCycle"; command = #openSettlementCycle({ cycle = { businessDate = 20672; market = "EGX"; priceSource = "EGX closing" } }) },
      { family = "instructSettlement"; command = #instructSettlement({ deal = 40; counterparty = alice(); tradeId = ?7; reference = "security-1" }) },
      { family = "setInstructionTrade"; command = #setInstructionTrade({ instruction = 90; tradeId = 8 }) },
      { family = "recycleSettlement"; command = #recycleSettlement({ instruction = 90; cycle = 20673 }) },
      { family = "recordSettlementStatus"; command = #recordSettlementStatus({ instruction = 90; document = "<Document/>" : Blob }) },
      { family = "buyIn"; command = #buyIn({ instruction = 90; counterparty = citi; priceMicro = 99_100_000; settlement = 20675; reference = "buy-in-1"; postingDate = 20673; valueDate = 20673; period = "2026-08"; narration = "buy-in" }) },
      { family = "cancelSettlement"; command = #cancelSettlement({ instruction = 90; ourConsent = h(1); theirConsent = h(2); reason = "both parties agree" }) },
      { family = "splitDeal"; command = #splitDeal({ deal = 40; parts = [6_000_000_00, 4_000_000_00] }) },
      { family = "setCustodyPolicy"; command = #setCustodyPolicy({ entitlementBasis = #contractual }) },
      { family = "extendInstrument"; command = #extendInstrument({ extension = { isin = "EG0000012345"; lei = "5493001KJTIIGC8Y1R12"; classification = #sovereign; market = "EGX"; settlementCycleDays = 2; quotation = #pricePer100; minDenomination = 100_00 } }) },
      { family = "openDepot"; command = #openDepot({ depot }) },
      { family = "setBookDepot"; command = #setBookDepot({ book = "BR01"; depot = "DEPOT-CITI" }) },
      { family = "assignDealDepot"; command = #assignDealDepot({ deal = 40; depot = "DEPOT-CITI" }) },
      { family = "transferDepot"; command = #transferDepot({ lot = 40; from = "DEPOT-CITI"; to = "DEPOT-HSBC"; nominal = 1_000_000_00; reference = "fop-1" }) },
      { family = "announceCorporateAction"; command = #announceCorporateAction({ announcement }) },
      { family = "cancelCorporateAction"; command = #cancelCorporateAction({ action = 70; reason = "withdrawn by the issuer" }) },
      { family = "processCorporateAction"; command = #processCorporateAction({ action = 70; postingDate = 20702; valueDate = 20702; period = "2026-08"; narration = "coupon" }) },
      { family = "revaluePositions"; command = #revaluePositions({ period = "2026-08"; postingDate = 20700; valueDate = 20700; narration = "month end" }) },
      { family = "openCall"; command = #openCall({ book = "BR01"; counterparty = citi; terms = callTerms; reference = "call-1"; approver = null }) },
      { family = "resetCallRate"; command = #resetCallRate({ call = 50; rateBps = 450; postingDate = 20680; valueDate = 20680; period = "2026-08"; narration = "reset" }) },
      { family = "adjustCallBalance"; command = #adjustCallBalance({ call = 50; delta = -250_000_00; approver = null; postingDate = 20685; valueDate = 20685; period = "2026-08"; narration = "draw" }) },
      { family = "serveCallNotice"; command = #serveCallNotice({ call = 50 }) },
      { family = "settleCall"; command = #settleCall({ call = 50; postingDate = 20700; valueDate = 20700; period = "2026-08"; narration = "repay" }) },
      { family = "openBook"; command = #openBook({ id = "BR01"; name = "Desk 1"; parent = ?"HQ"; sharia = false }) },
      { family = "closeBook"; command = #closeBook({ id = "BR01" }) },
      { family = "defineRole"; command = #defineRole({ id = "trader"; name = "Trader"; permissions = ["command.perform", "treasury.deal.capture"] }) },
      { family = "grantRole"; command = #grantRole({ subject = alice(); role = "trader"; scope = { partitions = ?["BR01"]; currencies = null; ceiling = ?[{ currency = "USD"; amount = 5_000_000_00 }]; dailyLimit = null } }) },
      { family = "revokeRole"; command = #revokeRole({ subject = alice(); role = "trader" }) },
      { family = "setDualPolicy"; command = #setDualPolicy({ permission = "treasury.deal.settle"; required = 1; eligibleRole = "checker"; ttlSeconds = 86_400 }) },
      { family = "clearDualPolicy"; command = #clearDualPolicy({ permission = "treasury.deal.capture" }) },
      { family = "setFeatureActivation"; command = #setFeatureActivation({ feature = "treasury"; height = 12 }) },
      { family = "setDeskIdentity"; command = #setDeskIdentity({ name = "Desk 1"; bic = "MENSEGCX"; lei = "5493001KJTIIGC8Y1R12" }) },
      { family = "journalRegisterCurrency"; command = #journalRegisterCurrency({ code = "EGP"; minorUnits = 2 }) },
      { family = "journalOpenAccount"; command = #journalOpenAccount({ code = "1100"; name = "Nostro accounts"; normalSide = #debit; category = #asset; constraint = #none }) },
      { family = "journalCloseAccount"; command = #journalCloseAccount({ code = "1100" }) },
      { family = "journalOpenPeriod"; command = #journalOpenPeriod({ id = "2026-09"; start = 20697; end = 20726 }) },
      { family = "journalClosePeriod"; command = #journalClosePeriod({ id = "2026-09" }) },
      { family = "journalSetCalendar"; command = #journalSetCalendar({ calendar = ?{ restDays = [5, 6]; holidays = [20700]; policy = #reject } }) },
      { family = "journalSetCalendarAuthority"; command = #journalSetCalendarAuthority({ authority = #businessDate; maxRollDays = 7; businessDate = ?20670 }) },
      { family = "journalRollBusinessDate"; command = #journalRollBusinessDate({ day = 20671 }) },
      { family = "journalSetActivationHeight"; command = #journalSetActivationHeight({ height = 0 }) },
      { family = "setFunctionalCurrency"; command = #setFunctionalCurrency({ currency = "EGP" }) },
      { family = "setFxPair"; command = #setFxPair({ pair = { currency = "USD"; position = "1800"; equivalent = "1801"; unrealised = "4800"; realised = "4801"; monetary = true } }) },
      { family = "setFxRate"; command = #setFxRate({ rate = { currency = "USD"; functional = "EGP"; numerator = 48_000_000; denominator = 1_000_000; asOf = 20670; source = "CBE reference" } }) },
      { family = "recordRateFixing"; command = #recordRateFixing({ index = "CBE-ON"; day = 20670; rateBps = 2000 }) },
      { family = "openEndOfDay"; command = #openEndOfDay({ book = "BR01"; businessDate = 20670; shardSize = 8 }) },
      { family = "setRetryPolicy"; command = #setRetryPolicy({ book = "BR01"; limit = 2 }) },
      { family = "resolveEndOfDayFailure"; command = #resolveEndOfDayFailure({ book = "BR01"; businessDate = 20670; item = 0; entity = 77; reason = "the rate was published late" }) },
      { family = "clearAlert"; command = #clearAlert({ alert = 90; reason = "the confirmation arrived" }) },
      { family = "setTreasuryPolicy"; command = #setTreasuryPolicy(policy) },
      { family = "registerSecurity"; command = #registerSecurity({ terms = { isin = "EG0000012345"; issuer = "ARE"; currency = "EGP"; couponBps = 1200; couponsPerYear = 2; dayCount = #a001_ActActIcma({ couponsPerYear = 2 }); issue = 20500; maturity = 21600 } }) },
      { family = "publishCurve"; command = #publishCurve({ curve = { id = "EGP-ZERO"; kind = #zeroRates; currency = "EGP"; day = 20670; points = [(1, 2000), (30, 2050), (365, 2200)]; source = h(1) } }) },
      { family = "setTreasuryLimit"; command = #setTreasuryLimit({ limit = { book = "BR01"; kind = #counterpartyExposure; currency = "USD"; subject = "HSBC"; value = 2_000_000_00 } }) },
      { family = "registerNostro"; command = #registerNostro({ nostro = { id = "NOSTRO-USD-CITI"; account = "1100"; sub = ?"NOSTRO-USD"; currency = "USD"; correspondent = citi; iban = ""; valueDateToleranceDays = 2 } }) },
      { family = "captureDeal"; command = #captureDeal({ book = "BR01"; counterparty = citi; kind = #fxForward(forward); reference = "fxForward-1"; approver = ?bob() }) },
      { family = "confirmDeal"; command = #confirmDeal({ deal = 40; confirmation = h(2); fields = ?{ kind = "moneyMarket"; amount1 = 1_000_000_00; currency1 = "USD"; amount2 = 0; currency2 = ""; valueDate = 20700; rateMicro = 450; counterparty = "CITIUS33" }; document = null }) },
      { family = "amendDeal"; command = #amendDeal({ deal = 40; kind = #moneyMarket(mm); reason = "the rate was agreed at 4.50" }) },
      { family = "cancelDeal"; command = #cancelDeal({ deal = 40; reason = "the counterparty withdrew" }) },
      { family = "settleDealLeg"; command = #settleDealLeg({ deal = 40; leg = 0; postingDate = 20670; valueDate = 20670; period = "2026-08"; narration = "start" }) },
      { family = "markDeal"; command = #markDeal({ deal = 41; postingDate = 20671; valueDate = 20671; period = "2026-08"; narration = "mark" }) },
      { family = "recordNostroStatement"; command = #recordNostroStatement({ nostro = "NOSTRO-USD-CITI"; statement = h(3); from = 20660; to = 20670; entries = [{ reference = "fxForward-1"; amount = 500_000_00; credit = true; valueDay = 20665; bookingDay = 20665; counterparty = "CITI" }]; document = null }) },
      { family = "resolveNostroBreak"; command = #resolveNostroBreak({ breakId = 55; resolution = "the correspondent's fee, booked"; correction = ?{ account = "5900"; sub = null; debit = true; amount = 45_00; currency = "USD" }; postingDate = 20671; valueDate = 20671; period = "2026-08"; narration = "fee" }) },
    ]
  };

  /// One sample of every event family the log records, for the codec's round trips.
  public func events() : [T.Event] {
    let proposed = { permission = "treasury.deal.settle"; partition = ?"BR01"; maker = alice(); required = 1; eligibleRole = "checker"; expiresAt = 1_700_000_000_000_000_000 : Nat64; justification = "settle"; commandHash = h(4); commandEncoding = Can.COMMAND_ENCODING };
    [
      #deskInstalled({ installer = alice() }),
      #bookOpened({ id = "BR01"; name = "Desk 1"; parent = ?"HQ"; sharia = false }),
      #bookClosed({ id = "BR01" }),
      #roleDefined({ id = "trader"; name = "Trader"; permissions = ["command.perform"] }),
      #roleGranted({ subject = alice(); role = "trader"; scope = { partitions = null; currencies = ?["USD", "EGP"]; ceiling = null; dailyLimit = ?[{ currency = "USD"; amount = 1 }] } }),
      #roleRevoked({ subject = alice(); role = "trader" }),
      #dualPolicySet({ permission = "book.create"; required = 2; eligibleRole = "checker"; ttlSeconds = 3600 }),
      #dualPolicyCleared({ permission = "treasury.deal.capture" }),
      #featureActivationSet({ feature = "treasury"; height = 0xFFFF_FFFF_FFFF_FFFF }),
      #identitySet({ name = "Desk 1"; bic = "MENSEGCX"; lei = "" }),
      #commandProposed(proposed),
      #commandApproved({ proposal = 10; checker = bob(); commandHash = h(4) }),
      #commandExecuted({ proposal = 10; commandHash = h(4); effects = [3, 4] }),
      #commandRejected({ proposal = 11; checker = bob(); reason = "no" }),
      #commandExpired({ proposal = 12 }),
      #operationRefused({ subject = alice(); permission = "treasury.deal.capture"; reason = #overCeiling; detail = "OverCeiling" }),
      #emergencyOverride({ commandHash = h(5); commandEncoding = 1; actor_ = alice(); witness = bob(); justification = "the market closed" }),
      #overrideReviewed({ override_ = 20; reviewer = bob(); disposition = "accepted" }),
      #dailyConsumed({ subject = alice(); currency = "USD"; day = 20670; amount = 500_000_00 }),
      #close(#fxRateSet({ rate = { currency = "USD"; functional = "EGP"; numerator = 48_000_000; denominator = 1_000_000; asOf = 20670; source = "CBE reference" } })),
      #fixingRecorded({ index = "CBE-ON"; day = 20670; rateBps = 2000 }),
      #journalMark({ height = 31 }),
      #eod(#opened({ book = "BR01"; businessDate = 20670; shardSize = 8; openedAtHeight = 40; maxDeal = 39; planHash = h(6); items = 1; entities = 1 })),
      #eod(#chunk({ book = "BR01"; businessDate = 20670; cursorFrom = 0; cursorTo = 1; posted = 12; examined = 26; zeroMovement = 0; failures = [{ seq = 0; job = "treasury"; scope = "BR01"; entity = 44; reason = "NoRate"; attempts = 1 }] })),
      #eod(#retry({ book = "BR01"; businessDate = 20671; resolved = [{ item = 0; entity = 44 }]; failures = []; posted = 1 })),
      #eod(#completed({ book = "BR01"; businessDate = 20670; posted = 12; examined = 26; zeroMovement = 0; failures = 1 })),
      #eod(#retryPolicySet({ book = "BR01"; limit = 2 })),
      #eod(#failureResolved({ book = "BR01"; businessDate = 20670; item = 0; entity = 44; reason = "the rate was published" })),
      #alert(#alertOpened({ finding = { rule = "nostro.break.aged"; version = 1; account = 55; day = 20680; postings = []; detail = "nostro break 55 open for 5 days" }; source = #endOfDay })),
      #alert(#alertCleared({ alert = 60; reason = "resolved" })),
      #treasury(#dealCaptured({ book = "BR01"; counterparty = citi; kind = #security(security); reference = "security-1"; trader = alice(); day = 20670; withinLimits = true; approver = null; secondAmount = 985_000_000 })),
      #treasury(#legSettled({ deal = 40; leg = 0; amount = 1; currency = "USD"; realised = -5; day = 20670; accrual = 0; amortisation = 0; fv = 0; nominal = 0; cost = 0 })),
      #treasury(#nostroBreak({ nostro = "NOSTRO-USD-CITI"; statement = h(3); side = #onStatementOnly; amount = 45_00; credit = false; valueDay = 20665; reference = "FEE"; posting = null; day = 20670 })),
      #close(#fxRevalued({ currency = "USD"; position = 500_000_00; equivalent = 24_000_000_00; revalued = 24_050_000_00; movement = 50_000_00; direction = #gain; rateNumerator = 48_100_000; rateDenominator = 1_000_000; rateAsOf = 20700; day = 20700 })),
      #call(#opened({ book = "BR01"; counterparty = citi; terms = callTerms; reference = "call-1"; trader = alice(); day = 20670; withinLimits = true; approver = null })),
      #call(#funded({ call = 50; amount = 750_000_00; day = 20670 })),
      #call(#rateReset({ call = 50; rateBps = 450; day = 20680; catchUp = 89_583 })),
      #call(#balanceAdjusted({ call = 50; delta = -250_000_00; day = 20685; catchUp = 46_875 })),
      #call(#noticeServed({ call = 50; day = 20690; repayDay = 20697 })),
      #call(#accrued({ call = 50; interest = 6_250; day = 20691 })),
      #call(#interestSettled({ call = 50; amount = 150_000; capitalised = false; day = 20700 })),
      #call(#repaid({ call = 50; principal = 500_000_00; interest = 41_667; day = 20697 })),
      #custody(#policySet({ entitlementBasis = #actual })),
      #custody(#instrumentExtended({ extension = { isin = "EG0000012345"; lei = ""; classification = #corporate; market = "OTC"; settlementCycleDays = 1; quotation = #yield; minDenomination = 1_00 }; day = 20670 })),
      #custody(#depotOpened({ depot; day = 20670 })),
      #custody(#bookDepotSet({ book = "BR01"; depot = "DEPOT-CITI"; day = 20670 })),
      #custody(#dealDepotAssigned({ deal = 40; depot = "DEPOT-CITI"; day = 20670 })),
      #custody(#transferred({ lot = 40; from = "DEPOT-CITI"; to = "DEPOT-HSBC"; nominal = 1_000_000_00; reference = "fop-1"; day = 20680 })),
      #custody(#announced({ announcement; day = 20690 })),
      #custody(#cancelled({ action = 70; reason = "withdrawn"; day = 20691 })),
      #custody(#entitlementRecorded({ action = 70; lot = 40; depot = "DEPOT-CITI"; nominal = 1_000_000_00; amount = 60_000_00; basis = #contractual; day = 20700 })),
      #custody(#entitled({ action = 70; lots = 1; total = 60_000_00; day = 20700 })),
      #custody(#entitlementPaid({ action = 70; lot = 40; amount = 60_000_00; nominal = 0; realised = -3; day = 20702 })),
      #custody(#paid({ action = 70; lots = 1; total = 60_000_00; day = 20702 })),
      #custody(#entitlementClaimed({ action = 70; lot = 40; amount = 60_000_00; accrued = 59_800_00; day = 20701 })),
      #settlement(#venueSet(venue())),
      #settlement(#ledgerSet({ role = #cash({ currency = "EGP" }); ledger = alice(); partial = false })),
      #settlement(#cycleOpened({ cycle = { businessDate = 20672; market = "EGX"; priceSource = "EGX closing" }; day = 20670 })),
      #settlement(#cycleClosed({ businessDate = 20672; settled = 3; failed = 1; pending = 0; day = 20672 })),
      #settlement(#instructed({ instruction = instruction(); day = 20670 })),
      #settlement(#tradeOpened({ instruction = 90; tradeId = 7; escrowed = true; note = "asset leg escrowed at ledger block 3"; day = 20670 })),
      #settlement(#tradeVerified({ instruction = 90; tradeId = 7; day = 20670 })),
      #settlement(#fundingRecorded({ instruction = 90; tradeId = 7; escrowed = true; bothEscrowed = false; note = ""; day = 20670 })),
      #settlement(#callRefused({ instruction = 90; step = "fundTaker"; reason = "past funding deadline"; day = 20671 })),
      #settlement(#auditSynced({ from = 2; leaves = [h(3), h(4)]; root = h(5); day = 20672 })),
      #settlement(#receiptVerified({ instruction = 90; tradeId = 7; seq = 3; leaf = h(4); root = h(5); assetPaid = 10_000_000_00; cashPaid = 9_850_000_00; day = 20672 })),
      #settlement(#settled({ instruction = 90; tradeId = 7; day = 20672 })),
      #settlement(#failed({ instruction = 90; cause = "not settled by the close of cycle 20672"; fails = 1; day = 20672 })),
      #settlement(#recycled({ instruction = 90; cycle = 20673; fails = 1; day = 20672 })),
      #settlement(#reclaimed({ instruction = 90; tradeId = 7; note = "legB refund ok"; day = 20673 })),
      #settlement(#tradeReset({ instruction = 90; previous = 7; day = 20673 })),
      #settlement(#tradeAssigned({ instruction = 90; tradeId = 8; day = 20673 })),
      #settlement(#boughtIn({ instruction = 90; replacement = 120; claim = 15_000_00; day = 20675 })),
      #settlement(#cancelled({ instruction = 90; ourConsent = h(1); theirConsent = h(2); reason = "both parties agree"; day = 20675 })),
      #settlement(#statusReceived({ instruction = 90; status = "SttlmSts/Pdg"; quantity = 10_000_000_00; amount = 9_850_000_00; matched = true; documentHash = h(6); day = 20671 })),
      #settlement(#split({ deal = 40; parts = [6_000_000_00, 4_000_000_00]; day = 20670 })),
      #custody(#pledged({ lot = 40; depot = "DEPOT-CITI"; nominal = 1_000_000_00; reference = "repo/80"; day = 20670 })),
      #custody(#released({ lot = 40; depot = "DEPOT-CITI"; nominal = 1_000_000_00; reference = "repo/80"; day = 20700 })),
      #custody(#lent({ lot = 40; depot = "DEPOT-CITI"; nominal = 500_000_00; reference = "loan/81"; day = 20670 })),
      #custody(#lentReturned({ lot = 40; depot = "DEPOT-CITI"; nominal = 500_000_00; reference = "loan/81"; day = 20690 })),
      #custody(#collateralReceived({ isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 2_000_000_00; reference = "repo/82"; day = 20670 })),
      #custody(#collateralReturned({ isin = "EG0000012345"; depot = "DEPOT-CITI"; nominal = 2_000_000_00; reference = "repo/82"; day = 20700 })),
      #financing(#policySet(financingPolicy)),
      #financing(#repoOpened({ book = "BR01"; counterparty = citi; terms = repoTerms; reference = "repo-1"; trader = alice(); day = 20670 })),
      #financing(#repoStarted({ repo = 80; lots = [(40, 6_000_000_00), (41, 4_000_000_00)]; day = 20670 })),
      #financing(#repoAccrued({ repo = 80; interest = 49_452_05; day = 20671 })),
      #financing(#repoRateReset({ repo = 80; rateBps = 1950; day = 20675; catchUp = 123 })),
      #financing(#collateralMarked({ repo = 80; value = 9_300_000_00; exposure = 9_520_000_00; priceMicro = 97_900_000; day = 20676 })),
      #financing(#marginCallRaised({ repo = 80; amount = 220_000_00; payer = #desk; day = 20676; due = 20677 })),
      #financing(#marginMet({ repo = 80; cash = 20_000_00; collateral = ?{ isin = "EG0000012345"; nominal = 200_000_00 }; lots = [(42, 200_000_00)]; payer = #desk; day = 20677 })),
      #financing(#collateralSubstituted({ repo = 80; out = { isin = "EG0000012345"; nominal = 1_000_000_00 }; in_ = { isin = "EG0000012345"; nominal = 1_050_000_00 }; outLots = [(40, 1_000_000_00)]; inLots = [(43, 1_050_000_00)]; day = 20680 })),
      #financing(#repoClosed({ repo = 80; principal = 9_500_000_00; interest = 148_356_16; marginReturned = -20_000_00; day = 20700 })),
      #financing(#loanOpened({ book = "BR01"; counterparty = citi; terms = loanTerms; reference = "loan-1"; trader = alice(); day = 20670 })),
      #financing(#loanStarted({ loan = 81; lots = [(40, 5_000_000_00)]; day = 20670 })),
      #financing(#loanAccrued({ loan = 81; fee = 671; rebate = 25_150; day = 20671 })),
      #financing(#loanRecalled({ loan = 81; day = 20686; returnDay = 20690 })),
      #financing(#loanReturned({ loan = 81; fee = 13_425; rebate = 503_013; day = 20690 })),
      #financing(#manufacturedPayment({ loan = 81; action = 70; lot = 40; amount = 30_000_00; day = 20688 })),
    ]
  };
}
