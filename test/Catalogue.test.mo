/// Catalogue.test.mo: the permission table validates in both directions against the command names and the guarded
/// methods, every treasury row is Manticore's, and the summary's counts cannot be zero.
///
/// engine: both.

import Debug "mo:core/Debug";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

import P "mo:kernel/auth/Permissions";
import Cat "../src/Catalogue";

var failures = 0;
func check(cond : Bool, what : Text) { if (not cond) { failures += 1; Debug.print("FAIL: " # what) } };

let report = Cat.validate();
check(P.clean(report), "the catalogue is clean: " # debug_show (report.faults));
Debug.print("count: catalogue checks = " # Nat.toText(report.checked));

let summary = P.summarise(Cat.catalogue());
check(summary.commands == Cat.commandNames().size(), "one command row per family");
check(summary.methods == Cat.guardedMethods().size(), "one method row per guarded method");
check(summary.moneyMoving == 5, "five money-moving rows: amend, cancel, settle, mark, resolve");
Debug.print("count: catalogue rows = " # Nat.toText(summary.rows));
Debug.print("count: money-moving rows = " # Nat.toText(summary.moneyMoving));
Debug.print("count: dual-by-default rows = " # Nat.toText(summary.dual));

// the thirteen treasury rows carry Manticore's identifiers, targets and decisions
let treasury : [(Text, Text, Bool, Bool)] = [
  ("treasury.policy", "setTreasuryPolicy", false, true), ("treasury.security.register", "registerSecurity", false, true), ("treasury.curve.publish", "publishCurve", false, true),
  ("treasury.limit.update", "setTreasuryLimit", false, true), ("nostro.register", "registerNostro", false, true), ("treasury.deal.capture", "captureDeal", false, false),
  ("treasury.deal.confirm", "confirmDeal", false, false), ("treasury.deal.amend", "amendDeal", true, true), ("treasury.deal.cancel", "cancelDeal", true, true),
  ("treasury.deal.settle", "settleDealLeg", true, true), ("treasury.deal.mark", "markDeal", true, true), ("nostro.statement.record", "recordNostroStatement", false, false),
  ("nostro.break.resolve", "resolveNostroBreak", true, true),
];
var rows = 0;
for ((id, cmd, money, dual) in treasury.vals()) {
  switch (Cat.byId(id)) {
    case (?p) { check(p.guards == #command(cmd) and p.moneyMoving == money and p.dualByDefault == dual, id # " is Manticore's row"); rows += 1 };
    case null check(false, id # " is missing");
  };
};
Debug.print("count: treasury rows identical to Manticore's = " # Nat.toText(rows));

// every open method has a reason, and none is also guarded
for ((m, reason) in Cat.openMethods().vals()) {
  check(Text.size(reason) > 0, m # " states its reason");
  check(Cat.byMethod(m) == null, m # " is not also guarded");
};
Debug.print("count: open methods with a stated reason = " # Nat.toText(Cat.openMethods().size()));

if (failures > 0) { Debug.print("FAILURES: " # Nat.toText(failures)); assert false };
Debug.print("CATALOGUE GREEN");
