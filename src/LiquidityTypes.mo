/// LiquidityTypes.mo: the cash-flow ladder, the liquidity coverage and net stable funding ratios, the large
/// exposures and the treasury return, every factor a recorded policy value and every class a recorded
/// declaration: a position without a class refuses the report and names itself, never a default weight.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

module {

  public type Day = Nat;

  /// The high-quality liquid asset level of an instrument (BCBS 238 paragraphs 49 to 54), or none.
  public type HqlaLevel = { #level1; #level2A; #level2B; #none };
  public func hqlaText(l : HqlaLevel) : Text { switch (l) { case (#level1) "level1"; case (#level2A) "level2A"; case (#level2B) "level2B"; case (#none) "none" } };

  /// The counterparty type that sets the run-off and inflow rates (BCBS 238) and the funding factors (BCBS 295).
  public type CounterpartyType = { #retailStable; #retailLessStable; #smallBusiness; #nonFinancialCorporate; #sovereign; #centralBank; #financial; #operational };
  public func counterpartyTypeText(t : CounterpartyType) : Text {
    switch (t) { case (#retailStable) "retailStable"; case (#retailLessStable) "retailLessStable"; case (#smallBusiness) "smallBusiness"; case (#nonFinancialCorporate) "nonFinancialCorporate"; case (#sovereign) "sovereign"; case (#centralBank) "centralBank"; case (#financial) "financial"; case (#operational) "operational" }
  };

  /// One rate per counterparty type, in basis points.
  public type Rate = { counterpartyType : CounterpartyType; bps : Nat };
  /// One factor per level, in basis points.
  public type LevelFactor = { level : HqlaLevel; bps : Nat };

  /// Every factor the ratios read: the HQLA haircuts and caps, the run-off rate of a liability to each
  /// counterparty type within thirty days, the inflow rate of an asset from each type within thirty days and
  /// the cap on inflows, the available stable funding factor of a liability to each type by residual maturity,
  /// the required stable funding factor of an asset by level and by counterparty type and maturity.
  public type Factors = {
    hqlaHaircuts : [LevelFactor];
    level2CapBps : Nat;
    level2BCapBps : Nat;
    runoff : [Rate];
    inflow : [Rate];
    inflowCapBps : Nat;
    /// Liabilities: under six months, six months to a year, a year and over.
    asfUnderSixMonths : [Rate];
    asfSixToTwelve : [Rate];
    asfOverYearBps : Nat;
    /// Assets: liquid assets by level; other assets by counterparty type under six months, six months to a
    /// year, a year and over; derivative assets.
    rsfHqla : [LevelFactor];
    rsfUnderSixMonths : [Rate];
    rsfSixToTwelve : [Rate];
    rsfOverYear : [Rate];
    rsfDerivativesBps : Nat;
    /// The bound of a single exposure against the eligible capital (BCBS 283).
    largeExposureBps : Nat;
    largeExposureReportBps : Nat;
  };

  public type Event = {
    #factorsSet : { factors : Factors; day : Day };
    #instrumentClassified : { isin : Text; level : HqlaLevel; day : Day };
    #counterpartyClassified : { name : Text; counterpartyType : CounterpartyType; day : Day };
    #capitalDeclared : { currency : Text; amount : Nat; day : Day };
  };

  public type Error = {
    #NoFactors;
    #InvalidFactors : { reason : Text };
    #NoCapital;
    #UnclassifiedInstrument : { isin : Text; deal : Nat };
    #UnclassifiedCounterparty : { name : Text; deal : Nat };
    #MissingFactor : { what : Text };
    #MissingRate : { currency : Text; day : Day };
    #MissingPrice : { isin : Text; day : Day };
  };

  /// The ladder's buckets by days from the day: overnight, 2 to 7, 8 to 30, 31 to 90, 91 to 180, 181 to 365, over.
  public let BUCKETS : [(Text, Nat, Nat)] = [("overnight", 0, 1), ("2-7", 2, 7), ("8-30", 8, 30), ("31-90", 31, 90), ("91-180", 91, 180), ("181-365", 181, 365), ("over-365", 366, 1_000_000)];
  public func bucketOf(days : Nat) : Text { for ((name, lo, hi) in BUCKETS.vals()) { if (days >= lo and days <= hi) return name }; "over-365" };

  /// One contractual flow: the row it comes from, its day, its currency, signed from the desk's side.
  public type Flow = { family : Text; id : Nat; kind : Text; currency : Text; day : Day; amount : Int; counterparty : Text };
  public type LadderRow = { currency : Text; bucket : Text; inflows : Nat; outflows : Nat; net : Int; flows : Nat };
  public type LadderView = { day : Day; height : Nat; rows : [LadderRow]; positions : Nat; flows : Nat };
  public type LcrView = {
    day : Day; height : Nat; currency : Text; level1 : Nat; level2A : Nat; level2B : Nat; capAdjustment : Nat; hqla : Nat; outflows : Nat; inflows : Nat; inflowsCounted : Nat; netOutflows : Nat; ratioBps : Nat; positions : Nat;
  };
  public type NsfrView = { day : Day; height : Nat; currency : Text; capital : Nat; asf : Nat; rsf : Nat; ratioBps : Nat; positions : Nat };
  public type ExposureRow = { counterparty : Text; group : Text; exposure : Nat; shareBps : Nat; breach : Bool };
  public type LargeExposuresView = { day : Day; height : Nat; currency : Text; capital : Nat; boundBps : Nat; rows : [ExposureRow]; breaches : Nat };
  public type Status = { factors : Bool; instruments : Nat; counterparties : Nat; capital : ?(Text, Nat) };
}
