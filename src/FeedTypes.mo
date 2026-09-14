/// FeedTypes.mo: the freeze-gated price feed. An instrument's price is accepted only when at least three
/// declared sources, each signing its figure with a registered key, agree within the declared band; a
/// disagreement halts the instrument rather than averaging it, a figure older than the bound halts it too, and a
/// halt is lifted by a recorded decision once the sources agree again.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

module {

  public type Day = Nat;

  /// A declared source: its name and the ML-DSA-44 public key its submissions are signed with.
  public type Source = { id : Text; publicKey : Blob };

  /// A feed for one instrument: its sources (three at least), the band within which they must agree (in basis
  /// points of the lowest figure) and the age past which a figure is stale (in seconds).
  public type Feed = { isin : Text; sources : [Source]; bandBps : Nat; staleSeconds : Nat };

  /// One signed figure from one source: the price per 100 in micro units, the source's own time of the
  /// figure (nanoseconds), and the signature over the canonical message.
  public type Submission = { isin : Text; source : Text; priceMicro : Nat; asOf : Nat64; signature : Blob };

  public type HaltReason = { #disagreement; #stale };
  public func haltReasonText(r : HaltReason) : Text { switch (r) { case (#disagreement) "disagreement"; case (#stale) "stale" } };

  /// A source's figure as the assessment read it.
  public type Figure = { source : Text; priceMicro : Nat; asOf : Nat64 };

  public type Event = {
    #feedDeclared : { feed : Feed; day : Day };
    #priceSubmitted : { isin : Text; source : Text; priceMicro : Nat; asOf : Nat64; signature : Blob; day : Day };
    /// The sources agreed: the accepted price is the median of the fresh figures, every figure beside it.
    #priceAccepted : { isin : Text; priceMicro : Nat; asOf : Nat64; figures : [Figure]; day : Day };
    #instrumentHalted : { isin : Text; reason : HaltReason; figures : [Figure]; day : Day };
    #haltLifted : { isin : Text; day : Day };
  };

  public type Error = {
    #NoFeed : { isin : Text };
    #InvalidFeed : { reason : Text };
    #UnknownSource : { isin : Text; source : Text };
    #BadSignature : { isin : Text; source : Text };
    #StaleSubmission : { isin : Text; source : Text; asOf : Nat64; now : Nat64 };
    #FutureSubmission : { isin : Text; source : Text; asOf : Nat64; now : Nat64 };
    #Replayed : { isin : Text; source : Text; asOf : Nat64; latest : Nat64 };
    #Halted : { isin : Text; reason : HaltReason };
    #NotHalted : { isin : Text };
    #NotAgreeing : { isin : Text };
    #NoPrice : { isin : Text };
    #Stale : { isin : Text; asOf : Nat64; now : Nat64 };
  };

  public type FeedView = {
    isin : Text; sources : Nat; bandBps : Nat; staleSeconds : Nat; halted : ?HaltReason;
    accepted : ?{ priceMicro : Nat; asOf : Nat64; block : Nat }; acceptances : Nat; haltBlock : Nat; lastBlock : Nat;
  };
  public type SourceView = { isin : Text; source : Text; latest : ?{ priceMicro : Nat; asOf : Nat64; block : Nat }; submissions : Nat };
  public type Status = { feeds : Nat; halted : Nat; submissions : Nat; acceptances : Nat };
}
