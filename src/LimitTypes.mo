/// LimitTypes.mo: the limit tree over the desk's open rows, and the risk sweep that measures it.
///
/// Manticore's six limit kinds bound one book at capture, each measured over the book's open deals. The tree
/// bounds the desk: a node names a counterparty, a group of counterparties, a country of risk, an issuer, an
/// instrument class, a tenor bucket, a book or a desk (a parent book), carries one limit in one currency, and may
/// hang under a parent whose bound covers it. Utilisation is a fold over every open row of every family, computed
/// in bounded slices by the risk sweep and published as one figure per node; the check at capture reads the last
/// published figure plus what was captured since, so it costs the same whatever the desk's size. A breach without
/// an approver is refused and the refusal recorded; with one, the row is recorded and the breach beside it; both
/// are counted per node per day.
///
/// The group and the country of a counterparty are recorded data, never inferred from a name.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import CuT "CustodyTypes";

module {

  public type Day = Nat;
  public type NodeId = Text;

  /// What a node bounds. The subject of a counterparty, group, country, issuer, book or desk node is its name;
  /// an instrument class node names the classification the custody extension records; a tenor node bounds the
  /// rows whose remaining life on the day is within its bucket.
  public type NodeKind = {
    #counterparty : Text;
    #group : Text;
    #country : Text;
    #issuer : Text;
    #instrumentClass : CuT.Classification;
    #tenor : { fromDays : Nat; toDays : Nat };
    #book : Text;
    #desk : Text;
  };
  public func kindText(k : NodeKind) : Text {
    switch (k) {
      case (#counterparty(_)) "counterparty"; case (#group(_)) "group"; case (#country(_)) "country"; case (#issuer(_)) "issuer";
      case (#instrumentClass(_)) "instrumentClass"; case (#tenor(_)) "tenor"; case (#book(_)) "book"; case (#desk(_)) "desk";
    }
  };

  public type Node = { id : NodeId; kind : NodeKind; parent : ?NodeId; currency : Text; limit : Nat };

  /// The families a row can belong to, as the sweep and the counters name them.
  public type Family = { #treasury; #call; #repo; #loan };
  public func familyText(f : Family) : Text { switch (f) { case (#treasury) "treasury"; case (#call) "call"; case (#repo) "repo"; case (#loan) "loan" } };

  public type Counterparty = { name : Text; group : Text; country : Text };

  public type Event = {
    #nodeSet : { node : Node; day : Day };
    #nodeRemoved : { node : NodeId; day : Day };
    #counterpartyAmended : { counterparty : Counterparty; day : Day };
    /// A row captured within the tree's bounds or with an approver: what it adds to each node it falls under,
    /// counted until the next publication folds it.
    #utilised : { family : Family; id : Nat; currency : Text; amount : Nat; nodes : [NodeId]; day : Day };
    /// A breach accepted by an approver: the row is recorded, the breach beside it.
    #breached : { node : NodeId; family : Family; id : Nat; measured : Nat; limit : Nat; approver : Principal; day : Day };
    /// A breach refused: no row was recorded; the refusal is.
    #breachRefused : { node : NodeId; family : Family; subject : Principal; amount : Nat; measured : Nat; limit : Nat; day : Day };
    /// The risk sweep: opened for a day under a bound (rows at or above it are the next sweep's), advanced a
    /// bounded slice at a time, each slice recording what it added per node and per agreement, published when
    /// the last family's last slice is done.
    #sweepOpened : { day : Day; bound : Nat; sliceSize : Nat };
    #sweepSliced : { day : Day; slice : Nat; family : Family; visited : Nat; nextCursor : ?Blob; familyDone : Bool; nodes : [(NodeId, Nat)]; agreements : [(Text, Int, Nat)] };
    #sweepPublished : { day : Day; slices : Nat; rows : Nat; nodes : [(NodeId, Nat)] };
  };

  public type Error = {
    #InvalidNode : { reason : Text };
    #UnknownNode : { node : NodeId };
    #NodeExists : { node : NodeId };
    #NodeHasChildren : { node : NodeId };
    #InvalidCounterparty : { reason : Text };
    #Breached : { node : NodeId; measured : Nat; limit : Nat };
    #SweepOpen : { day : Day };
    #NoSweep;
    #SweepExists : { day : Day };
    #RunOpen : { book : Text; businessDate : Day };
    #InvalidSweep : { reason : Text };
  };

  public type NodeView = {
    id : NodeId; kind : Text; subject : Text; fromDays : Nat; toDays : Nat; parent : ?NodeId; currency : Text; limit : Nat;
    published : Nat; publishedDay : Day; sinceFold : Nat; utilisation : Nat; removed : Bool; lastBlock : Nat;
  };
  public type CounterView = { node : NodeId; day : Day; recorded : Nat; refused : Nat };
  public type SweepView = { day : Day; bound : Nat; sliceSize : Nat; family : Text; cursor : ?Blob; slices : Nat; rows : Nat; complete : Bool };
  public type Status = { nodes : Nat; removed : Nat; counterparties : Nat; breachesRecorded : Nat; breachesRefused : Nat; sweeps : Nat; lastPublishedDay : Day; sweepOpen : Bool };
}
