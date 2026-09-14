/// IsolationJudge.mo: the kernel's verdict rule for "a stranger called this tenant's every method", exposed as a
/// contract so the tenancy battery's probes are judged by the kernel's own code and not by the tool's reading
/// of it. The probes are the tool's; the rule is the kernel's (`fleet/Isolation`); the desk's own authority
/// phrases and self-only reads come from its catalogue.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";

import Isolation "mo:kernel/fleet/Isolation";
import Cat "Catalogue";

persistent actor class IsolationJudge() {
  public query func judge(probes : [Isolation.Probe]) : async Isolation.Report {
    Isolation.judgeAll(probes, Cat.selfOnlyReads(), Cat.authorityPhrases())
  };
  public query func verdicts(probes : [Isolation.Probe]) : async [(Text, Text, Text)] {
    Array.map<Isolation.Row, (Text, Text, Text)>(Isolation.judgeAll(probes, Cat.selfOnlyReads(), Cat.authorityPhrases()).rows, func(x) { (x.method, x.person, Isolation.verdictText(x.verdict)) })
  };
  public query func authorityPhrases() : async [Text] { Isolation.authorityPhrases() };
  public query func selfOnlyReads() : async [Text] { Cat.selfOnlyReads() };
  public query func scopedReads() : async [Text] { Cat.scopedReads() };
}
