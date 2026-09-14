/// SettlementTypes.mo: securities settlement through Tachyon's delivery-versus-payment core.
///
/// A security deal captured in the desk settles as a Tachyon trade: the security leg on the instrument's ledger,
/// the cash leg on the desk's cash ledger for the currency, both ICRC ledgers on the substrate. The desk is a party
/// to the trade: the maker of a sale (it delivers the security and receives the cash) or the taker of a purchase
/// (it pays the cash and receives the security). Inter-contract calls are not atomic with the desk's log, so the
/// desk records what it is about to do before it calls and what the reply said after, as separate blocks, and it
/// records a settlement only from Tachyon's own receipt verified against Tachyon's audit root.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import TT "mo:manticore/TreasuryTypes";

module {

  public type Day = Nat;
  public type InstructionId = Nat;   // the desk block index of #instructed

  /// The venue: Tachyon's core, the funding deadline every trade is opened with, how many times a failed
  /// instruction is recycled into the next cycle before it waits for a decision, and the account a buy-in claim is
  /// recorded in.
  public type Venue = { core : Principal; deadlineSecs : Nat; recycleLimit : Nat; claimsAccount : Text };
  /// A ledger the desk settles on: the cash ledger of a currency, or the ledger of an instrument (with whether a
  /// partial delivery is accepted for it). Amounts on both are the desk's own minor units.
  public type LedgerRole = { #cash : { currency : Text }; #security : { isin : Text } };
  public type LedgerDeclaration = { role : LedgerRole; ledger : Principal; partial : Bool };

  /// What an instruction settles: a treasury deal's delivery leg, a repo's start (0) or close (1) leg, a loan's
  /// start (0) or return (1) leg.
  public type Family = { #treasury; #repo; #loan; #collateral };
  public func familyText(f : Family) : Text { switch (f) { case (#treasury) "treasury"; case (#repo) "repo"; case (#loan) "loan"; case (#collateral) "collateral" } };
  public type Role = { #maker; #taker };
  public func roleText(r : Role) : Text { switch (r) { case (#maker) "maker"; case (#taker) "taker" } };

  public type CycleState = { #open; #closed };
  public type Cycle = { businessDate : Day; market : Text; priceSource : Text };

  /// The states an instruction passes through. The desk's own leg is escrowed in `#funded`; `#settled` is reached
  /// only from a verified receipt; `#failed` waits for the next cycle or a decision; `#boughtIn` and `#cancelled`
  /// are terminal decisions.
  public type InstructionState = { #instructed; #opened; #verified; #funded; #settled; #failed; #boughtIn; #cancelled; #matched };
  public func stateText(s : InstructionState) : Text {
    switch (s) { case (#instructed) "instructed"; case (#opened) "opened"; case (#verified) "verified"; case (#funded) "funded"; case (#settled) "settled"; case (#failed) "failed"; case (#boughtIn) "boughtIn"; case (#cancelled) "cancelled"; case (#matched) "matched" }
  };

  public type Instruction = {
    family : Family; deal : Nat; leg : Nat; cycle : Day; role : Role; counterparty : Principal;
    assetLedger : Principal; assetAmount : Nat; cashLedger : Principal; cashAmount : Nat;
    /// The Tachyon trade the counterparty opened, for a purchase; a sale's trade id is the desk's own opening.
    tradeId : ?Nat; reference : Text; documentHash : Blob;
    /// A fill of a market cycle: the trade is the engine's, settled by its relayer; the desk observes it and
    /// verifies the receipt, and opens or funds nothing unless that trade aborted.
    matched : Bool;
  };

  public type Event = {
    #venueSet : Venue;
    #ledgerSet : LedgerDeclaration;
    #cycleOpened : { cycle : Cycle; day : Day };
    #cycleClosed : { businessDate : Day; settled : Nat; failed : Nat; pending : Nat; day : Day };
    /// What the desk is about to settle, recorded before any call is made.
    #instructed : { instruction : Instruction; day : Day };
    /// A sale's trade opened by the desk as maker: Tachyon's id and whether the security leg went into escrow in
    /// the same call.
    #tradeOpened : { instruction : InstructionId; tradeId : Nat; escrowed : Bool; note : Text; day : Day };
    /// A purchase's trade read back from Tachyon and found to match the instruction leg for leg.
    #tradeVerified : { instruction : InstructionId; tradeId : Nat; day : Day };
    /// The reply to the desk's own funding call.
    #fundingRecorded : { instruction : InstructionId; tradeId : Nat; escrowed : Bool; bothEscrowed : Bool; note : Text; day : Day };
    /// A call that did not do what was asked, with the reply or the reason; the instruction stays where it was.
    #callRefused : { instruction : InstructionId; step : Text; reason : Text; day : Day };
    /// Tachyon's audit log mirrored: the leaves appended from Tachyon's enumeration, from the sequence the mirror
    /// had reached, and the root Tachyon published over the whole log, which the mirror must reach with them.
    #auditSynced : { from : Nat; leaves : [Blob]; root : Blob; day : Day };
    /// The trade's settlement event found in the mirror at its sequence, under the root recorded.
    #receiptVerified : { instruction : InstructionId; tradeId : Nat; seq : Nat; leaf : Blob; root : Blob; assetPaid : Nat; cashPaid : Nat; day : Day };
    /// The instruction settled: the treasury leg settles in the same act, after this event.
    #settled : { instruction : InstructionId; tradeId : Nat; day : Day };
    #failed : { instruction : InstructionId; cause : Text; fails : Nat; day : Day };
    #recycled : { instruction : InstructionId; cycle : Day; fails : Nat; day : Day };
    #reclaimed : { instruction : InstructionId; tradeId : Nat; note : Text; day : Day };
    /// The trade a recycled instruction re-enters the cycle with: the previous one aborted and reclaimed.
    #tradeReset : { instruction : InstructionId; previous : Nat; day : Day };
    /// A purchase's trade named after the instruction: the counterparty's new trade for a recycled instruction.
    #tradeAssigned : { instruction : InstructionId; tradeId : Nat; day : Day };
    #boughtIn : { instruction : InstructionId; replacement : TT.DealId; claim : Nat; day : Day };
    #cancelled : { instruction : InstructionId; ourConsent : Blob; theirConsent : Blob; reason : Text; day : Day };
    /// An incoming sese.024 from the counterparty's custodian: matched against the instruction by reference,
    /// quantity and amount, or recorded as a mismatch.
    #statusReceived : { instruction : InstructionId; status : Text; quantity : Nat; amount : Nat; matched : Bool; documentHash : Blob; day : Day };
    /// A security deal split into parts for partial delivery: the original cancelled, the parts captured after.
    #split : { deal : TT.DealId; parts : [Nat]; day : Day };
  };

  public type Error = {
    #NoVenue;
    #NoLedger : { role : LedgerRole };
    #NoCycle : { businessDate : Day };
    #InvalidTerms : { reason : Text };
    #UnknownInstruction : { instruction : InstructionId };
    #InstructionNotIn : { instruction : InstructionId; state : Text; wanted : Text };
    #DealNotSettleable : { deal : TT.DealId; reason : Text };
    #AlreadyInstructed : { deal : TT.DealId; instruction : InstructionId };
    #CallFailed : { step : Text; reason : Text };
    #TradeMismatch : { instruction : InstructionId; reason : Text };
    #ReceiptNotVerified : { instruction : InstructionId; reason : Text };
    #Waiting : { instruction : InstructionId; state : Text; tradeStatus : Text };
    #PartialNotAllowed : { isin : Text };
  };

  public type InstructionView = {
    id : InstructionId; family : Text; deal : Nat; leg : Nat; cycle : Day; role : Text; counterparty : Principal;
    assetLedger : Principal; assetAmount : Nat; cashLedger : Principal; cashAmount : Nat; tradeId : ?Nat;
    state : Text; fails : Nat; reference : Text; lastBlock : Nat;
  };
  public type CycleView = { businessDate : Day; market : Text; priceSource : Text; state : Text; settled : Nat; failed : Nat; pending : Nat; instructions : Nat };
  /// What a drive did: the step taken and the instruction's state after it.
  public type Step = { step : Text; state : Text; tradeId : ?Nat };
  public type Status = { instructions : Nat; cycles : Nat; ledgers : Nat; mirroredLeaves : Nat; settled : Nat; failed : Nat };
}
