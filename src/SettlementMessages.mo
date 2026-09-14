/// SettlementMessages.mo: the settlement status advice (sese.024.001.09) and the settlement confirmation
/// (sese.025.001.09) rendered from an instruction's recorded state, and an incoming status advice parsed to the
/// fields the desk matches against its instruction. The instruction itself is Manticore's sese.023.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Result "mo:core/Result";
import Text "mo:core/Text";

import CivilDate "mo:journal/CivilDate";
import TT "mo:manticore/TreasuryTypes";
import TreasuryMessages "mo:manticore/TreasuryMessages";
import Xml "mo:manticore/Xml";

import ST "SettlementTypes";

module {

  func el(indent : Nat, name : Text, body : Text) : Text { sp(indent) # "<" # name # ">" # Xml.escape(body) # "</" # name # ">\n" };
  func sp(n : Nat) : Text { var s = ""; var i = 0; while (i < n) { s #= " "; i += 1 }; s };
  func clip(t : Text, n : Nat) : Text { if (t.size() <= n) t else Text.fromArray(Array.sliceToArray<Char>(Text.toArray(t), 0, n)) };

  /// The status branch an instruction's state renders as: processing acknowledged while the desk is putting the
  /// trade up, settlement pending while it waits for the counterparty's leg, failing once the cycle has passed,
  /// cancelled by a decision. A settled instruction has a confirmation, not a status.
  public type Status = { branch : Text; code : Text; info : Text };
  public func statusOf(state : ST.InstructionState, role : ST.Role, fails : Nat) : Status {
    let awaiting = switch (role) { case (#maker) "AWMO"; case (#taker) "AWSH" };
    func st(branch : Text, code : Text, info : Text) : Status { { branch; code; info } };
    switch (state) {
      case (#instructed) st("PrcgSts/AckdAccptd", "NORE", "instructed");
      case (#opened) st("PrcgSts/AckdAccptd", "NORE", "trade opened");
      case (#verified) st("PrcgSts/AckdAccptd", "NORE", "trade verified");
      case (#funded) st("SttlmSts/Pdg", awaiting, "own leg escrowed; awaiting the counterparty's leg");
      case (#failed) st("SttlmSts/Flng", awaiting, "not settled by the cycle's close; fails " # Nat.toText(fails));
      case (#boughtIn) st("SttlmSts/Flng", "BYIY", "bought in");
      case (#cancelled) st("PrcgSts/Canc", "CANI", "cancelled by consent");
      case (#settled) st("PrcgSts/AckdAccptd", "NORE", "settled");
    }
  };

  func statusXml(s : Status) : Text {
    switch (s.branch) {
      case "PrcgSts/AckdAccptd" "    <PrcgSts><AckdAccptd><NoSpcfdRsn>NORE</NoSpcfdRsn></AckdAccptd></PrcgSts>\n";
      case "PrcgSts/Canc" "    <PrcgSts><Canc><Rsn><Cd><Cd>" # s.code # "</Cd></Cd><AddtlRsnInf>" # Xml.escape(clip(s.info, 210)) # "</AddtlRsnInf></Rsn></Canc></PrcgSts>\n";
      case "SttlmSts/Pdg" "    <SttlmSts><Pdg><Rsn><Cd><Cd>" # s.code # "</Cd></Cd><AddtlRsnInf>" # Xml.escape(clip(s.info, 210)) # "</AddtlRsnInf></Rsn></Pdg></SttlmSts>\n";
      case _ "    <SttlmSts><Flng><Rsn><Cd><Cd>" # s.code # "</Cd></Cd><AddtlRsnInf>" # Xml.escape(clip(s.info, 210)) # "</AddtlRsnInf></Rsn></Flng></SttlmSts>\n";
    }
  };

  func txDetails(indent : Nat, safekeepingAccount : Text, isin : Text, nominal : Nat, currency : Text, amount : Nat, minorUnits : Nat8, settlementDay : Nat, receive : Bool) : Text {
    sp(indent) # "<SfkpgAcct><Id>" # Xml.escape(clip(safekeepingAccount, 35)) # "</Id></SfkpgAcct>\n"
    # sp(indent) # "<FinInstrmId><ISIN>" # Xml.escape(isin) # "</ISIN></FinInstrmId>\n"
    # sp(indent) # "<SttlmQty><Qty><FaceAmt>" # TreasuryMessages.decimalText(nominal, minorUnits) # "</FaceAmt></Qty></SttlmQty>\n"
    # sp(indent) # "<SttlmAmt><Amt Ccy=\"" # Xml.escape(currency) # "\">" # TreasuryMessages.decimalText(amount, minorUnits) # "</Amt><CdtDbtInd>" # (if (receive) "DBIT" else "CRDT") # "</CdtDbtInd></SttlmAmt>\n"
    # sp(indent) # "<SttlmDt><Dt><Dt>" # CivilDate.toText(settlementDay) # "</Dt></Dt></SttlmDt>\n"
    # el(indent, "SctiesMvmntTp", if (receive) "RECE" else "DELI") # el(indent, "Pmt", "APMT")
    # sp(indent) # "<SttlmParams><SctiesTxTp><Cd>TRAD</Cd></SctiesTxTp></SttlmParams>\n"
  };

  /// The desk's status advice for an instruction: the account owner's reference, Tachyon's trade id as the market
  /// infrastructure's, the status branch, then the transaction as instructed.
  public func sese024Xml(reference : Text, tradeId : ?Nat, status : Status, safekeepingAccount : Text, isin : Text, nominal : Nat, currency : Text, amount : Nat, minorUnits : Nat8, settlementDay : Nat, receive : Bool) : Text {
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    # "<Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:sese.024.001.09\">\n"
    # "  <SctiesSttlmTxStsAdvc>\n"
    # "    <TxId>\n" # el(6, "AcctOwnrTxId", clip(reference, 35)) # (switch (tradeId) { case (?t) el(6, "MktInfrstrctrTxId", Nat.toText(t)); case null "" }) # "    </TxId>\n"
    # statusXml(status)
    # "    <TxDtls>\n" # txDetails(6, safekeepingAccount, isin, nominal, currency, amount, minorUnits, settlementDay, receive) # "    </TxDtls>\n"
    # "  </SctiesSttlmTxStsAdvc>\n"
    # "</Document>\n"
  };

  /// The desk's confirmation of a settled instruction: the effective settlement day is the day the receipt was
  /// verified and the treasury leg settled.
  public func sese025Xml(reference : Text, tradeId : Nat, safekeepingAccount : Text, isin : Text, nominal : Nat, currency : Text, amount : Nat, minorUnits : Nat8, settlementDay : Nat, effectiveDay : Nat, receive : Bool) : Text {
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    # "<Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:sese.025.001.09\">\n"
    # "  <SctiesSttlmTxConf>\n"
    # "    <TxIdDtls>\n" # el(6, "AcctOwnrTxId", clip(reference, 35)) # el(6, "MktInfrstrctrTxId", Nat.toText(tradeId)) # el(6, "SctiesMvmntTp", if (receive) "RECE" else "DELI") # el(6, "Pmt", "APMT") # "    </TxIdDtls>\n"
    # "    <TradDtls>\n" # sp(6) # "<SttlmDt><Dt><Dt>" # CivilDate.toText(settlementDay) # "</Dt></Dt></SttlmDt>\n" # sp(6) # "<FctvSttlmDt><Dt><Dt>" # CivilDate.toText(effectiveDay) # "</Dt></Dt></FctvSttlmDt>\n" # "    </TradDtls>\n"
    # "    <FinInstrmId>" # "<ISIN>" # Xml.escape(isin) # "</ISIN></FinInstrmId>\n"
    # "    <QtyAndAcctDtls>\n" # sp(6) # "<SttldQty><Qty><FaceAmt>" # TreasuryMessages.decimalText(nominal, minorUnits) # "</FaceAmt></Qty></SttldQty>\n" # sp(6) # "<SfkpgAcct><Id>" # Xml.escape(clip(safekeepingAccount, 35)) # "</Id></SfkpgAcct>\n" # "    </QtyAndAcctDtls>\n"
    # "    <SttlmParams><SctiesTxTp><Cd>TRAD</Cd></SctiesTxTp></SttlmParams>\n"
    # "    <SttldAmt><Amt Ccy=\"" # Xml.escape(currency) # "\">" # TreasuryMessages.decimalText(amount, minorUnits) # "</Amt><CdtDbtInd>" # (if (receive) "DBIT" else "CRDT") # "</CdtDbtInd></SttldAmt>\n"
    # "  </SctiesSttlmTxConf>\n"
    # "</Document>\n"
  };

  /// An incoming status advice: the account owner's reference, the status branch it carries, and the quantity and
  /// amount it names, which the desk holds against the instruction.
  public type Incoming = { reference : Text; tradeId : ?Nat; status : Text; isin : Text; quantity : Nat; amount : Nat; currency : Text };
  public func parseSese024(doc : Blob, minorUnits : Nat8) : Result.Result<Incoming, Text> {
    let root = switch (Xml.parse(doc)) { case (#ok(e)) e; case (#err(i)) return #err("not well-formed XML: " # i.rule) };
    if (root.name != "Document") return #err("the root is Document");
    let ?adv = Xml.child(root, "SctiesSttlmTxStsAdvc") else return #err("SctiesSttlmTxStsAdvc missing");
    let ?reference = Xml.textAt(adv, ["TxId", "AcctOwnrTxId"]) else return #err("AcctOwnrTxId missing");
    let tradeId = switch (Xml.textAt(adv, ["TxId", "MktInfrstrctrTxId"])) { case (?t) Nat.fromText(Xml.trim(t)); case null null };
    var status = "";
    for (branch in ["PrcgSts", "IfrrdMtchgSts", "MtchgSts", "SttlmSts"].vals()) {
      switch (Xml.child(adv, branch)) {
        case (?b) { for (c in b.children.vals()) status := branch # "/" # c.name };
        case null {};
      };
    };
    if (status.size() == 0) return #err("no status branch");
    let ?tx = Xml.child(adv, "TxDtls") else return #err("TxDtls missing");
    let isin = switch (Xml.textAt(tx, ["FinInstrmId", "ISIN"])) { case (?i) Xml.trim(i); case null "" };
    let ?qtyText = Xml.textAt(tx, ["SttlmQty", "Qty", "FaceAmt"]) else return #err("SttlmQty FaceAmt missing");
    let ?quantity = TreasuryMessages.parseDecimal(Xml.trim(qtyText), minorUnits) else return #err("the quantity is not a decimal");
    let (amount, currency) = switch (Xml.path(tx, ["SttlmAmt", "Amt"])) {
      case (?a) { switch (TreasuryMessages.parseDecimal(Xml.trim(a.text), minorUnits)) { case (?v) (v, switch (Xml.attribute(a, "Ccy")) { case (?c) c; case null "" }); case null return #err("the amount is not a decimal") } };
      case null (0, "");
    };
    #ok({ reference = Xml.trim(reference); tradeId; status; isin; quantity; amount; currency })
  };
}
