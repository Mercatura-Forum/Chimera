/// ReconciliationMessages.mo: the correspondent's and the custodian's documents parsed by the contract: the
/// camt.054 debit and credit notification by the statement's own entry shape, the semt.002 holdings report and
/// the semt.017 transaction posting report. Every parser is Manticore's `Xml` over the element paths the schema
/// fixes, and a document outside them is refused with the path named.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";

import CivilDate "mo:journal/CivilDate";
import TT "mo:manticore/TreasuryTypes";
import TreasuryMessages "mo:manticore/TreasuryMessages";
import Xml "mo:manticore/Xml";

import RT "ReconciliationTypes";

module {

  func dayOf(t : ?Text) : ?Nat {
    switch (t) {
      case (?x) { let s = Xml.trim(x); CivilDate.fromText(if (s.size() >= 10) Text.fromArray(Array.sliceToArray<Char>(Text.toArray(s), 0, 10)) else s) };
      case null null;
    }
  };
  func parseQuantity(el : Xml.Element, minorUnits : Nat8) : ?Nat {
    // FaceAmt for a nominal in currency units, Unit for a count; both read to the currency's minor units
    switch (Xml.textAt(el, ["FaceAmt"])) {
      case (?f) TreasuryMessages.parseDecimal(Xml.trim(f), minorUnits);
      case null { switch (Xml.textAt(el, ["Unit"])) { case (?u) TreasuryMessages.parseDecimal(Xml.trim(u), minorUnits); case null null } };
    }
  };

  /// A camt.054: the notification's entries in the statement entry's shape, the account, the notification id.
  public func parseCamt054(doc : Blob, currency : Text, minorUnits : Nat8) : Result.Result<{ entries : [TT.StatementEntry]; account : Text; id : Text }, Text> {
    let root = switch (Xml.parse(doc)) { case (#ok(e)) e; case (#err(i)) return #err("not well-formed XML: " # i.rule) };
    if (root.name != "Document") return #err("the root is Document");
    let ?body = Xml.child(root, "BkToCstmrDbtCdtNtfctn") else return #err("BkToCstmrDbtCdtNtfctn missing");
    let ?ntf = Xml.child(body, "Ntfctn") else return #err("Ntfctn missing");
    let id = switch (Xml.textAt(ntf, ["Id"])) { case (?i) Xml.trim(i); case null "" };
    let acctCcy = switch (Xml.textAt(ntf, ["Acct", "Ccy"])) { case (?c) Xml.trim(c); case null "" };
    if (acctCcy.size() > 0 and not Text.equal(acctCcy, currency)) return #err("the notification's account is in " # acctCcy # ", the nostro in " # currency);
    let account = switch (Xml.textAt(ntf, ["Acct", "Id", "IBAN"])) { case (?i) Xml.trim(i); case null { switch (Xml.textAt(ntf, ["Acct", "Id", "Othr", "Id"])) { case (?o) Xml.trim(o); case null "" } } };
    var out : [TT.StatementEntry] = [];
    for (n in Xml.children(ntf, "Ntry").vals()) {
      let ?amtEl = Xml.child(n, "Amt") else return #err("an entry has no Amt");
      switch (Xml.attribute(amtEl, "Ccy")) { case (?c) { if (not Text.equal(c, currency)) return #err("an entry is in " # c # ", the nostro in " # currency) }; case null {} };
      let ?amount = TreasuryMessages.parseDecimal(amtEl.text, minorUnits) else return #err("an entry's Amt is not a decimal");
      let ?cd = Xml.textAt(n, ["CdtDbtInd"]) else return #err("an entry has no CdtDbtInd");
      let credit = Text.equal(Xml.trim(cd), "CRDT");
      if (not credit and not Text.equal(Xml.trim(cd), "DBIT")) return #err("CdtDbtInd is CRDT or DBIT");
      let ?valueDay = dayOf(Xml.textAt(n, ["ValDt", "Dt"])) else return #err("an entry has no value date");
      let bookingDay = switch (dayOf(Xml.textAt(n, ["BookgDt", "Dt"]))) { case (?d) d; case null valueDay };
      let reference = switch (Xml.textAt(n, ["NtryRef"])) {
        case (?r) Xml.trim(r);
        case null { switch (Xml.textAt(n, ["NtryDtls", "TxDtls", "Refs", "EndToEndId"])) { case (?e) Xml.trim(e); case null { switch (Xml.textAt(n, ["AcctSvcrRef"])) { case (?a) Xml.trim(a); case null "" } } } };
      };
      let counterparty = switch (Xml.textAt(n, ["NtryDtls", "TxDtls", "RltdPties", "Dbtr", "Pty", "Nm"])) {
        case (?d) Xml.trim(d);
        case null { switch (Xml.textAt(n, ["NtryDtls", "TxDtls", "RltdPties", "Cdtr", "Pty", "Nm"])) { case (?c) Xml.trim(c); case null "" } };
      };
      out := Array.concat(out, [{ reference; amount; credit; valueDay; bookingDay; counterparty }]);
    };
    #ok({ entries = out; account; id })
  };

  /// A semt.002 holdings report: the safekeeping account, the statement date, the basis, and one aggregate
  /// balance per instrument; a report on any basis but the settled one is refused, since the depot fold is the
  /// settled position.
  public func parseSemt002(doc : Blob, minorUnits : Nat8) : Result.Result<{ account : Text; statementDate : Nat; holdings : [RT.ReportedHolding]; id : Text }, Text> {
    let root = switch (Xml.parse(doc)) { case (#ok(e)) e; case (#err(i)) return #err("not well-formed XML: " # i.rule) };
    if (root.name != "Document") return #err("the root is Document");
    let ?rpt = Xml.child(root, "SctiesBalCtdyRpt") else return #err("SctiesBalCtdyRpt missing");
    let ?gen = Xml.child(rpt, "StmtGnlDtls") else return #err("StmtGnlDtls missing");
    let id = switch (Xml.textAt(gen, ["StmtId"])) { case (?i) Xml.trim(i); case null "" };
    let ?statementDate = dayOf(Xml.textAt(gen, ["StmtDtTm", "Dt"])) else return #err("StmtGnlDtls/StmtDtTm/Dt missing");
    switch (Xml.textAt(gen, ["StmtBsis", "Cd"])) { case (?b) { if (not Text.equal(Xml.trim(b), "SETT")) return #err("the report's basis is " # Xml.trim(b) # "; the depot fold is the settled position (SETT)") }; case null return #err("StmtGnlDtls/StmtBsis/Cd missing") };
    let ?account = Xml.textAt(rpt, ["SfkpgAcct", "Id"]) else return #err("SfkpgAcct/Id missing");
    var out : [RT.ReportedHolding] = [];
    for (b in Xml.children(rpt, "BalForAcct").vals()) {
      let ?isin = Xml.textAt(b, ["FinInstrmId", "ISIN"]) else return #err("a balance has no ISIN");
      let ?agg = Xml.path(b, ["AggtBal", "Qty", "Qty", "Qty"]) else return #err("a balance has no AggtBal/Qty/Qty/Qty");
      let ?nominal = parseQuantity(agg, minorUnits) else return #err("a balance's quantity is not a FaceAmt or a Unit");
      switch (Xml.textAt(b, ["AggtBal", "ShrtLngInd"])) { case (?s) { if (not Text.equal(Xml.trim(s), "LONG")) return #err("a short balance is not a holding") }; case null {} };
      out := Array.concat(out, [{ isin = Xml.trim(isin); nominal }]);
    };
    #ok({ account = Xml.trim(account); statementDate; holdings = out; id })
  };

  /// A semt.017 transaction posting report: the safekeeping account, the period, and every posting with the
  /// account owner's reference, the instrument, the quantity, the direction and the effective settlement day.
  public func parseSemt017(doc : Blob, minorUnits : Nat8) : Result.Result<{ account : Text; from : Nat; to : Nat; transactions : [RT.ReportedTransaction]; id : Text }, Text> {
    let root = switch (Xml.parse(doc)) { case (#ok(e)) e; case (#err(i)) return #err("not well-formed XML: " # i.rule) };
    if (root.name != "Document") return #err("the root is Document");
    let ?rpt = Xml.child(root, "SctiesTxPstngRpt") else return #err("SctiesTxPstngRpt missing");
    let ?gen = Xml.child(rpt, "StmtGnlDtls") else return #err("StmtGnlDtls missing");
    let id = switch (Xml.textAt(gen, ["StmtId"])) { case (?i) Xml.trim(i); case null "" };
    let ?from = dayOf(Xml.textAt(gen, ["StmtPrd", "FrDtToDt", "FrDt"])) else return #err("StmtPrd/FrDtToDt/FrDt missing");
    let ?to = dayOf(Xml.textAt(gen, ["StmtPrd", "FrDtToDt", "ToDt"])) else return #err("StmtPrd/FrDtToDt/ToDt missing");
    if (from > to) return #err("the period runs from its start to its end");
    let ?account = Xml.textAt(rpt, ["SfkpgAcct", "Id"]) else return #err("SfkpgAcct/Id missing");
    var out : [RT.ReportedTransaction] = [];
    for (fi in Xml.children(rpt, "FinInstrmDtls").vals()) {
      let ?isin = Xml.textAt(fi, ["FinInstrmId", "ISIN"]) else return #err("an instrument has no ISIN");
      for (tx in Xml.children(fi, "Tx").vals()) {
        let ?reference = Xml.textAt(tx, ["AcctOwnrTxId"]) else return #err("a transaction has no AcctOwnrTxId");
        let ?d = Xml.child(tx, "TxDtls") else return #err("a transaction has no TxDtls");
        let ?mv = Xml.textAt(d, ["SctiesMvmntTp"]) else return #err("a transaction has no SctiesMvmntTp");
        let delivered = Text.equal(Xml.trim(mv), "DELI");
        if (not delivered and not Text.equal(Xml.trim(mv), "RECE")) return #err("SctiesMvmntTp is DELI or RECE");
        let ?qty = Xml.path(d, ["PstngQty", "Qty"]) else return #err("a transaction has no PstngQty/Qty");
        let ?nominal = parseQuantity(qty, minorUnits) else return #err("a transaction's quantity is not a FaceAmt or a Unit");
        let ?effectiveDay = dayOf(Xml.textAt(d, ["FctvSttlmDt", "Dt"])) else return #err("a transaction has no FctvSttlmDt/Dt");
        if (effectiveDay < from or effectiveDay > to) return #err("a transaction's effective day lies in the period");
        out := Array.concat(out, [{ reference = Xml.trim(reference); isin = Xml.trim(isin); nominal; delivered; effectiveDay }]);
      };
    };
    #ok({ account = Xml.trim(account); from; to; transactions = out; id })
  };

  /// Which of the two custodian reports a document is, by its root element.
  public func depotDocumentKind(doc : Blob) : ?RT.StatementKind {
    let root = switch (Xml.parse(doc)) { case (#ok(e)) e; case (#err(_)) return null };
    if (Xml.child(root, "SctiesBalCtdyRpt") != null) return ?#holdings;
    if (Xml.child(root, "SctiesTxPstngRpt") != null) return ?#transactions;
    null
  };
}
