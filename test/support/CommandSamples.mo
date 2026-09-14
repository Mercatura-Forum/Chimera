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

  public func samples() : [Freeze.Sample<T.Command>] {
    [
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
    ]
  };
}
