/// EndOfDay.mo: the end-of-day runs of the desk, which are the fold of their blocks.
///
/// The plan is the kernel's (`batch/Batch`): fixed when the run opens from the open deals of the book, hashed into
/// the opening block, recomputed and checked at every advance, never stored. What the fold keeps per run is the
/// cursor, the counters and the bounded failure list; a chunk moves the cursor in the block that records the work,
/// so the state after a restart is the state to resume from. A failing item advances the cursor past itself with
/// its reason recorded; the retry pass re-attempts failures below the book's declared limit before new work; a
/// parked failure stays until a recorded decision resolves it. The shape is Manticore's batch fold over the kernel's
/// plan, with one job: the treasury's.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import List "mo:core/List";
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Order "mo:core/Order";
import Runtime "mo:core/Runtime";
import Text "mo:core/Text";

import JC "mo:journal/Canonical";
import Batch "mo:kernel/batch/Batch";

import T "DeskTypes";

module {

  /// The one job of the desk's end of day, at rank 1: the treasury's coupon, accrual, mark and legs due.
  public let JOB_TREASURY : Batch.Job = { name = "treasury"; rank = 1 };
  /// The call money's job, after the treasury's: the funding, the accrual, the interest and the repayment due.
  public let JOB_CALLS : Batch.Job = { name = "call"; rank = 2 };

  public type Run = {
    book : Text;
    businessDate : Nat;
    shardSize : Nat;
    openedAtBlock : Nat;
    openedAtHeight : Nat;
    /// The bound the plan was built under: a deal captured after the run opened is not the run's.
    maxDeal : Nat;
    planHash : Blob;
    items : Nat;
    entities : Nat;
    var cursor : Nat;
    var posted : Nat;
    var examined : Nat;
    var zeroMovement : Nat;
    var chunks : Nat;
    failures : List.List<Batch.Failure>;
    var state : Batch.RunState;
  };

  public type State = {
    runs : Map.Map<(Text, Nat), Run>;
    retry : Map.Map<Text, Batch.RetryPolicy>;
  };

  func cmpTN(a : (Text, Nat), b : (Text, Nat)) : Order.Order {
    switch (Text.compare(a.0, b.0)) { case (#equal) Nat.compare(a.1, b.1); case (o) o }
  };

  public func newState() : State { { runs = Map.empty<(Text, Nat), Run>(); retry = Map.empty<Text, Batch.RetryPolicy>() } };

  public func getRun(s : State, book : Text, day : Nat) : ?Run { Map.get(s.runs, cmpTN, (book, day)) };
  public func runCount(s : State) : Nat { Map.size(s.runs) };
  public func listRuns(s : State) : [Run] { Array.map<((Text, Nat), Run), Run>(Map.toArray(s.runs), func((_, r)) { r }) };
  public func isComplete(r : Run) : Bool { switch (r.state) { case (#completed) true; case (_) false } };
  public func isOpen(r : Run) : Bool { switch (r.state) { case (#open or #running) true; case (_) false } };
  public func failureCount(r : Run) : Nat { List.size(r.failures) };
  public func failuresOf(r : Run) : [Batch.Failure] { List.toArray(r.failures) };
  public func hasFailure(r : Run, item : Nat, entity : Nat) : Bool {
    for (f in List.values(r.failures)) { if (f.seq == item and f.entity == entity) return true };
    false
  };
  public func retryLimit(s : State, book : Text) : Nat {
    switch (Map.get(s.retry, Text.compare, book)) { case (?p) p.limit; case null Batch.DEFAULT_RETRY_LIMIT }
  };
  /// The failures a run still carries whose attempts are below the book's limit: what the next advance re-attempts.
  public func retryable(s : State, r : Run) : [Batch.Failure] {
    let limit = retryLimit(s, r.book);
    let out = List.empty<Batch.Failure>();
    for (f in List.values(r.failures)) { if (f.attempts < limit) List.add(out, f) };
    List.toArray(out)
  };
  /// Whether an open run for the book covers a value date: an act dated into a running day is refused.
  public func openRunCovering(s : State, book : Text, valueDate : Nat) : ?Run {
    for (((b, d), r) in Map.entries(s.runs)) { if (Text.equal(b, book) and valueDate <= d and isOpen(r)) return ?r };
    null
  };
  public func unresolvedFailures(s : State, book : Text, from : Nat, to : Nat) : Nat {
    var n = 0;
    for (((b, d), r) in Map.entries(s.runs)) { if (Text.equal(b, book) and d >= from and d <= to) n += List.size(r.failures) };
    n
  };

  public func view(r : Run) : T.RunView {
    {
      book = r.book; businessDate = r.businessDate; shardSize = r.shardSize; openedAtBlock = r.openedAtBlock; openedAtHeight = r.openedAtHeight;
      maxDeal = r.maxDeal; planHash = r.planHash; items = r.items; entities = r.entities; cursor = r.cursor; done = r.cursor;
      posted = r.posted; examined = r.examined; zeroMovement = r.zeroMovement; chunks = r.chunks; failures = List.toArray(r.failures); state = Batch.runStateText(r.state);
    }
  };

  /// The plan of a book's end of day: one per-scope item for the treasury job, which walks the open deals of the
  /// book itself. A pure function of the book and the shard size, so the recorded hash re-derives at every advance.
  public func planInput(book : Text, shardSize : Nat) : Batch.Input {
    { scopes = [{ scope = book; qualifier = ""; entities = 0; jobs = [JOB_TREASURY, JOB_CALLS] }]; shardSize }
  };

  func mustRun(s : State, book : Text, day : Nat) : Run {
    switch (Map.get(s.runs, cmpTN, (book, day))) { case (?r) r; case null Runtime.trap("end of day: unknown run " # book # "/" # Nat.toText(day)) }
  };

  public func fold(s : State, block : Nat, e : T.EodEvent) {
    switch (e) {
      case (#retryPolicySet(x)) Map.add(s.retry, Text.compare, x.book, { scope = x.book; limit = x.limit });
      case (#opened(x)) {
        let r : Run = {
          book = x.book; businessDate = x.businessDate; shardSize = x.shardSize; openedAtBlock = block; openedAtHeight = x.openedAtHeight;
          maxDeal = x.maxDeal; planHash = x.planHash; items = x.items; entities = x.entities;
          var cursor = 0; var posted = 0; var examined = 0; var zeroMovement = 0; var chunks = 0; failures = List.empty<Batch.Failure>(); var state = #open;
        };
        Map.add(s.runs, cmpTN, (x.book, x.businessDate), r);
      };
      case (#chunk(x)) {
        let r = mustRun(s, x.book, x.businessDate);
        r.cursor := x.cursorTo; r.posted += x.posted; r.examined += x.examined; r.zeroMovement += x.zeroMovement; r.chunks += 1; r.state := #running;
        for (f in x.failures.vals()) List.add(r.failures, f);
      };
      case (#retry(x)) {
        let r = mustRun(s, x.book, x.businessDate);
        r.posted += x.posted;
        // every entry the pass resolved is dropped, every entry re-attempted is replaced by the one carrying the
        // raised attempt count, everything untouched is kept as it was
        let kept = List.empty<Batch.Failure>();
        for (f in List.values(r.failures)) {
          var drop = false;
          for (rz in x.resolved.vals()) { if (rz.item == f.seq and rz.entity == f.entity) drop := true };
          for (g in x.failures.vals()) { if (g.seq == f.seq and g.entity == f.entity) drop := true };
          if (not drop) List.add(kept, f);
        };
        for (g in x.failures.vals()) List.add(kept, g);
        List.clear(r.failures);
        List.addAll(r.failures, List.values(kept));
      };
      case (#failureResolved(x)) {
        let r = mustRun(s, x.book, x.businessDate);
        let kept = List.empty<Batch.Failure>();
        for (f in List.values(r.failures)) { if (not (f.seq == x.item and f.entity == x.entity)) List.add(kept, f) };
        List.clear(r.failures);
        List.addAll(r.failures, List.values(kept));
      };
      case (#completed(x)) mustRun(s, x.book, x.businessDate).state := #completed;
    }
  };

  public func fingerprintInto(w : JC.Writer, s : State) {
    w.nat(Map.size(s.runs));
    for ((_, r) in Map.entries(s.runs)) {
      w.text(r.book); w.nat(r.businessDate); w.nat(r.shardSize); w.nat(r.openedAtBlock); w.nat(r.openedAtHeight); w.nat(r.maxDeal);
      w.blobRaw(r.planHash); w.nat(r.items); w.nat(r.entities); w.nat(r.cursor); w.nat(r.posted); w.nat(r.examined); w.nat(r.zeroMovement); w.nat(r.chunks);
      w.text(Batch.runStateText(r.state));
      w.len16(List.size(r.failures));
      for (f in List.values(r.failures)) { w.nat(f.seq); w.text(f.job); w.text(f.scope); w.nat(f.entity); w.text(f.reason); w.nat(f.attempts) };
    };
    w.nat(Map.size(s.retry));
    for ((_, p) in Map.entries(s.retry)) { w.text(p.scope); w.nat(p.limit) };
  };
}
