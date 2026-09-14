/// DeskRegistry.mo: many desks on one service, one contract each, held by the kernel's registry and rolled out
/// by the kernel's fleet rule. The registry holds the tenants (id, contract, name, kind, owner, invitation, the
/// module hash at activation, status, parent), the operator's invitations as the hash of the code and never the
/// code, and a membership index; nothing about a person. A release is pinned from a contract that was installed,
/// with the hash every validator reports; a rollout upgrades one tenant, waits for a person to look at it, then
/// rolls the rest in id order and stops at the first failure with the remainder untouched. Every figure a tenant
/// reports before its upgrade must equal the figure after, or the rollout stops.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Int "mo:core/Int";
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Principal "mo:core/Principal";
import Result "mo:core/Result";
import Text "mo:core/Text";
import Time "mo:core/Time";

import C "mo:kernel/codec/Canonical";
import Registry "mo:kernel/fleet/Registry";
import Fleet "mo:kernel/fleet/Fleet";

shared (initMsg) persistent actor class DeskRegistry() = self {

  /// The operator: the principal that installed the registry. Every act but the redemption of an invitation is the
  /// operator's; a tenant's owner redeems its own invitation.
  let operator : Principal = initMsg.caller;
  let reg : Registry.State = Registry.newState();
  /// The contracts by their registry number and back: the kernel's registry names a contract by a number, and the
  /// substrate names it by a principal.
  let contracts = Map.empty<Nat, Principal>();
  let contractIds = Map.empty<Principal, Nat>();
  var nextContract : Nat = 1;
  var rollout : ?Fleet.Rollout = null;
  var rollouts : Nat = 0;

  public type Error = { #NotTheOperator; #Registry : Text; #Fleet : Text; #NoRollout; #UnknownContract : { contract : Principal } };
  type Res<X> = Result.Result<X, Error>;

  func today() : Nat { Int.abs(Time.now()) / 86_400_000_000_000 };
  func operatorOnly(caller : Principal) : ?Error { if (Principal.equal(caller, operator)) null else ?#NotTheOperator };
  func regErr<X>(e : Registry.Error) : Res<X> { #err(#Registry(Registry.errorText(e))) };
  func fleetErr<X>(e : Fleet.Error) : Res<X> { #err(#Fleet(Fleet.errorText(e))) };
  func contractNumber(p : Principal) : Nat {
    switch (Map.get(contractIds, Principal.compare, p)) {
      case (?n) n;
      case null { let n = nextContract; nextContract += 1; Map.add(contracts, Nat.compare, n, p); Map.add(contractIds, Principal.compare, p, n); n };
    }
  };

  public type TenantView = { id : Nat; contract : ?Principal; name : Text; kind : Text; parent : ?Nat; owner : Principal; invitation : Nat; moduleHash : Text; status : Text; createdAtDay : Nat; activatedAtDay : Nat; statusAtDay : Nat };
  func view(t : Registry.Tenant) : TenantView {
    { id = t.id; contract = switch (t.contract) { case (?n) Map.get(contracts, Nat.compare, n); case null null }; name = t.name; kind = t.kind; parent = t.parent; owner = t.owner; invitation = t.invitation;
      moduleHash = t.moduleHash; status = Registry.statusText(t.status); createdAtDay = t.createdAtDay; activatedAtDay = t.activatedAtDay; statusAtDay = t.statusAtDay }
  };

  // ─── the registry ───
  public shared ({ caller }) func issueInvitation(codeHash : Text, expiresAtDay : Nat, note : Text) : async Res<Nat> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.issueInvitation(reg, today(), codeHash, expiresAtDay, note)) { case (#ok(i)) #ok(i.id); case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func revokeInvitation(id : Nat) : async Res<()> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.revokeInvitation(reg, id)) { case (#ok) #ok(()); case (#err(e)) regErr(e) }
  };
  /// A tenant's owner redeems the invitation by the code itself; the registry keeps the hash it issued against.
  public shared ({ caller }) func redeem(code : Text, name : Text, kind : Text) : async Res<TenantView> {
    if (Principal.isAnonymous(caller)) return #err(#NotTheOperator);
    switch (Registry.redeem(reg, caller, today(), code, name, kind)) { case (#ok(t)) { Registry.noteMember(reg, caller, t.id); #ok(view(t)) }; case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func activate(id : Nat, contract : Principal, moduleHash : Text) : async Res<TenantView> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.activate(reg, today(), id, contractNumber(contract), moduleHash)) { case (#ok(t)) #ok(view(t)); case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func adopt(contract : Principal, name : Text, kind : Text, owner : Principal, moduleHash : Text) : async Res<TenantView> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.adopt(reg, today(), contractNumber(contract), name, kind, owner, moduleHash)) { case (#ok(t)) { Registry.noteMember(reg, owner, t.id); #ok(view(t)) }; case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func suspend(id : Nat) : async Res<TenantView> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.suspend(reg, today(), id)) { case (#ok(t)) #ok(view(t)); case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func resume(id : Nat) : async Res<TenantView> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.resume(reg, today(), id)) { case (#ok(t)) #ok(view(t)); case (#err(e)) regErr(e) }
  };
  public shared ({ caller }) func setParent(id : Nat, parent : ?Nat) : async Res<TenantView> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Registry.setParent(reg, id, parent)) { case (#ok(t)) #ok(view(t)); case (#err(e)) regErr(e) }
  };

  // ─── the fleet ───
  /// A release pinned from a reference contract that was installed: the hash every validator reports must be one
  /// hash and the reference's.
  public shared ({ caller }) func pin(reference : Text, validatorHashes : [Text], buildLabel : Text) : async Res<Text> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (Fleet.pin(reg, reference, validatorHashes, buildLabel)) { case (#ok(h)) #ok(h); case (#err(e)) fleetErr(e) }
  };
  public shared ({ caller }) func openRollout(canary : ?Nat, buildLabel : Text) : async Res<Fleet.Progress> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    switch (rollout) { case (?r) { switch (r.phase) { case (#complete or #stopped(_)) {}; case (_) return #err(#Fleet("a rollout is " # Fleet.phaseText(r.phase))) } }; case null {} };
    switch (Fleet.open(reg, canary, buildLabel)) { case (#ok(r)) { rollout := ?r; rollouts += 1; #ok(Fleet.progress(r)) }; case (#err(e)) fleetErr(e) }
  };
  public shared ({ caller }) func recordBefore(id : Nat, figures : [(Text, Text)]) : async Res<()> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    let ?r = rollout else return #err(#NoRollout);
    switch (Fleet.recordBefore(r, id, figures)) { case (#ok) #ok(()); case (#err(e)) fleetErr(e) }
  };
  /// One tenant's upgrade reported: the module it now runs and the figures it now holds; a failure stops the
  /// rollout and leaves the rest untouched. A tenant upgraded to the pin has its module recorded in the registry.
  public shared ({ caller }) func report(id : Nat, moduleAfter : Text, figuresAfter : [(Text, Text)]) : async Res<{ remaining : Nat }> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    let ?r = rollout else return #err(#NoRollout);
    switch (Fleet.report(r, id, moduleAfter, figuresAfter)) {
      case (#ok(x)) { switch (Registry.recordModule(reg, today(), id, moduleAfter)) { case (#ok(_)) {}; case (#err(e)) return regErr(e) }; #ok(x) };
      case (#err(e)) fleetErr(e);
    }
  };
  public shared ({ caller }) func canaryInspected() : async Res<()> {
    switch (operatorOnly(caller)) { case (?e) return #err(e); case null {} };
    let ?r = rollout else return #err(#NoRollout);
    switch (Fleet.canaryInspected(r)) { case (#ok) #ok(()); case (#err(e)) fleetErr(e) }
  };

  // ─── reads ───
  public query func tenant(id : Nat) : async ?TenantView { switch (Registry.get(reg, id)) { case (?t) ?view(t); case null null } };
  public query func tenantOfContract(contract : Principal) : async ?TenantView {
    let ?n = Map.get(contractIds, Principal.compare, contract) else return null;
    switch (Registry.byContractId(reg, n)) { case (?id) { switch (Registry.get(reg, id)) { case (?t) ?view(t); case null null } }; case null null }
  };
  public query func tenantsOf(who : Principal) : async [Nat] { Registry.tenantsOf(reg, who) };
  public query func activeTenants(from : Nat, limit : Nat) : async { tenants : [Nat]; next : ?Nat } { Registry.selectActive(reg, from, limit) };
  public query func pinnedModule() : async Text { reg.pinnedModule };
  public query func figures() : async Registry.Figures { Registry.figures(reg) };
  /// The next tenant the rollout upgrades, with its contract, or nothing: the canary first, and nothing else until
  /// the canary has been inspected.
  public query func nextToUpgrade() : async ?{ tenant : Nat; contract : ?Principal } {
    let ?r = rollout else return null;
    switch (Fleet.next(r)) {
      case (?id) { let contract = switch (Registry.get(reg, id)) { case (?t) { switch (t.contract) { case (?n) Map.get(contracts, Nat.compare, n); case null null } }; case null null }; ?{ tenant = id; contract } };
      case null null;
    }
  };
  public query func rolloutProgress() : async ?Fleet.Progress { switch (rollout) { case (?r) ?Fleet.progress(r); case null null } };
  public query func rolloutOutcome(id : Nat) : async ?Text { switch (rollout) { case (?r) { switch (Fleet.outcomeOf(r, id)) { case (?o) ?Fleet.outcomeText(o); case null null } }; case null null } };
  public query func rolloutCount() : async Nat { rollouts };
  /// The registry's fingerprint: the closed set of fields the kernel declares, nothing else.
  public query func fingerprint() : async Blob { let w = C.Writer(); Registry.fingerprintInto(w, reg); C.hashWithDomainBlob("THEBES-DESK-REGISTRY-v1", w.toBlob()) };
  public query func tenantFields() : async [Text] { Registry.tenantFields() };
  /// The hash an invitation is issued against, of a code the operator hands out and the registry never stores.
  public query func codeHashOf(code : Text) : async Text { Registry.codeHashOf(code) };
  public query func operatorPrincipal() : async Principal { operator };
}
