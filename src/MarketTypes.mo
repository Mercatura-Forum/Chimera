/// MarketTypes.mo: the sealed settlement cycle over Tachyon's matching. A market binds an instrument to the
/// engine that matches it and the ledgers it settles on; an order is staged in the desk within the trader's
/// standing and the tree; a cycle takes the staged orders of a day at the feed's reference price, hands them to
/// the engine's batch auction and reads the fills back; every fill of the desk's is captured as a deal and
/// instructed for settlement on the trade the engine settled, through the one settlement path, with its receipt
/// verified against Tachyon's root before anything is posted.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";
import FT "FeedTypes";

module {

  public type Day = Nat;
  public type OrderId = Nat;
  public type CycleId = Nat;

  /// A market: the instrument, the engine, the ledgers the engine settles on, the face per share unit of the
  /// instrument's ledger (in minor units of the instrument's currency), the venue's funding deadline, and the
  /// participants the desk trades with, each a principal on the ledgers and a counterparty in the books.
  public type Market = {
    isin : Text; engine : Principal; sharesLedger : Principal; cashLedger : Principal; currency : Text; unitNominal : Nat; deadlineSecs : Nat;
    participants : [Participant];
  };
  public type Participant = { principal : Principal; counterparty : TT.Counterparty };

  public type Side = { #buy; #sell };
  public func sideText(x : Side) : Text { switch (x) { case (#buy) "buy"; case (#sell) "sell" } };

  /// An order: the book, the instrument, the side, the quantity in share units, the classification and the cash
  /// account of the deal a fill becomes, and the trader's reference.
  public type OrderTerms = { book : Text; isin : Text; side : Side; units : Nat; classification : TT.Classification; cash : TT.CashAccount; reference : Text };

  public type OrderState = { #staged; #submitted; #filled; #partlyFilled; #unfilled; #refused; #cancelled };
  public func orderStateText(x : OrderState) : Text {
    switch (x) { case (#staged) "staged"; case (#submitted) "submitted"; case (#filled) "filled"; case (#partlyFilled) "partlyFilled"; case (#unfilled) "unfilled"; case (#refused) "refused"; case (#cancelled) "cancelled" }
  };
  public type CycleState = { #open; #clearing; #settling; #closed };
  public func cycleStateText(x : CycleState) : Text { switch (x) { case (#open) "open"; case (#clearing) "clearing"; case (#settling) "settling"; case (#closed) "closed" } };
  public type FillState = { #recorded; #unattributed; #captured; #instructed; #settled };
  public func fillStateText(x : FillState) : Text { switch (x) { case (#recorded) "recorded"; case (#unattributed) "unattributed"; case (#captured) "captured"; case (#instructed) "instructed"; case (#settled) "settled" } };

  public type Event = {
    #marketDeclared : { market : Market; day : Day };
    #orderStaged : { terms : OrderTerms; trader : Principal; withinLimits : Bool; approver : ?Principal; day : Day };
    #orderCancelled : { order : OrderId; reason : Text; day : Day };
    /// A cycle opened over the staged orders of the instrument at the feed's reference price.
    #cycleOpened : { isin : Text; referenceMicro : Nat; referenceBlock : Nat; orders : [OrderId]; day : Day };
    /// An order handed to the engine: the engine's order id, the window it rests in, the limit per unit.
    #orderSubmitted : { cycle : CycleId; order : OrderId; engineOrder : Nat; window : Nat; limit : Nat; day : Day };
    #orderRefused : { cycle : CycleId; order : OrderId; reason : Text; day : Day };
    /// The engine's clear advanced by one chunk; the last one carries the schedule's totals.
    #clearAdvanced : { cycle : CycleId; window : Nat; clearingPrice : ?Nat; targetVolume : Nat; filled : Nat; chunks : Nat; complete : Bool; day : Day };
    /// A fill of the desk's read from the engine's obligations.
    #filled : { cycle : CycleId; order : OrderId; seq : Nat; price : Nat; units : Nat; counterparty : Principal; day : Day };
    /// A fill against a principal the market does not name: recorded, captured once the participant is declared.
    #fillUnattributed : { cycle : CycleId; seq : Nat; counterparty : Principal; day : Day };
    #fillCaptured : { cycle : CycleId; seq : Nat; deal : TT.DealId; day : Day };
    #fillInstructed : { cycle : CycleId; seq : Nat; deal : TT.DealId; instruction : Nat; day : Day };
    #fillTradeSet : { cycle : CycleId; seq : Nat; tradeId : Nat; day : Day };
    #fillsRead : { cycle : CycleId; through : Nat; fills : Nat; complete : Bool; day : Day };
    /// An order of the desk's still resting on the engine after the clear, withdrawn before the cycle closes.
    #orderWithdrawn : { cycle : CycleId; order : OrderId; engineOrder : Nat; day : Day };
    #callRefused : { cycle : CycleId; step : Text; reason : Text; day : Day };
    /// The cycle closed: every fill instructed, every unfilled order marked.
    #cycleClosed : { cycle : CycleId; fills : Nat; unfilled : [OrderId]; day : Day };
  };

  public type Error = {
    #NoMarket : { isin : Text };
    #InvalidMarket : { reason : Text };
    #InvalidOrder : { reason : Text };
    #UnknownOrder : { order : OrderId };
    #OrderNotIn : { order : OrderId; state : Text; wanted : Text };
    #NotTheTrader : { order : OrderId };
    #UnknownCycle : { cycle : CycleId };
    #CycleNotIn : { cycle : CycleId; state : Text; wanted : Text };
    #CycleOpen : { isin : Text; cycle : CycleId };
    #NothingStaged : { isin : Text };
    #UnknownFill : { cycle : CycleId; seq : Nat };
    #FeedError : { error : FT.Error };
    #EngineRefused : { cycle : CycleId; step : Text; reason : Text };
    #Waiting : { cycle : CycleId; state : Text; reason : Text };
  };

  public type OrderView = {
    id : OrderId; book : Text; isin : Text; side : Side; units : Nat; filled : Nat; state : Text; cycle : ?CycleId; engineOrder : ?Nat; trader : Principal; reference : Text; day : Day; lastBlock : Nat;
  };
  public type CycleView = {
    id : CycleId; isin : Text; day : Day; state : Text; referenceMicro : Nat; window : ?Nat; orders : Nat; submitted : Nat; refused : Nat; fills : Nat; instructed : Nat; settled : Nat; unattributed : Nat;
    clearingPrice : ?Nat; volume : Nat; chunks : Nat; nextSeq : Nat; lastBlock : Nat;
  };
  public type FillView = { cycle : CycleId; seq : Nat; order : OrderId; price : Nat; units : Nat; counterparty : Principal; deal : ?TT.DealId; instruction : ?Nat; tradeId : ?Nat; state : Text; block : Nat };
  public type Status = { markets : Nat; orders : Nat; staged : Nat; cycles : Nat; openCycles : Nat; fills : Nat; settled : Nat; unattributed : Nat };
}
