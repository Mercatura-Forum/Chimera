/// MarketMessages.mo: the securities trade confirmation (setr.027.001.05) rendered from a fill of a market
/// cycle: the fill's identification, the side, the trade and settlement dates, the face confirmed, the gross
/// amount, the clean price as a rate, the accrued interest, the instrument and the two parties.
///
/// Attribution: Thebes Core Team. Licence: Apache 2.0.

import Array "mo:core/Array";
import Nat "mo:core/Nat";
import Nat8 "mo:core/Nat8";
import Text "mo:core/Text";

import CivilDate "mo:journal/CivilDate";
import TT "mo:manticore/TreasuryTypes";
import TreasuryMessages "mo:manticore/TreasuryMessages";
import Xml "mo:manticore/Xml";

import MT "MarketTypes";

module {

  func el(indent : Nat, name : Text, body : Text) : Text { sp(indent) # "<" # name # ">" # Xml.escape(body) # "</" # name # ">\n" };
  func sp(n : Nat) : Text { var s = ""; var i = 0; while (i < n) { s #= " "; i += 1 }; s };
  func clip(t : Text, n : Nat) : Text { if (t.size() <= n) t else Text.fromArray(Array.sliceToArray<Char>(Text.toArray(t), 0, n)) };
  func amount(indent : Nat, name : Text, currency : Text, minor : Nat, minorUnits : Nat8) : Text {
    sp(indent) # "<" # name # " Ccy=\"" # currency # "\">" # TreasuryMessages.decimalText(minor, minorUnits) # "</" # name # ">\n"
  };
  /// A price per 100 in micro as a percentage rate with six decimals.
  func rate(priceMicro : Nat) : Text { TreasuryMessages.decimalText(priceMicro, 6) };
  func isBic(t : Text) : Bool {
    let n = t.size();
    if (n != 8 and n != 11) return false;
    for (c in t.chars()) { if (not ((c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9'))) return false };
    true
  };
  func party(indent : Nat, role : Text, cp : TT.Counterparty) : Text {
    let id = if (isBic(cp.bic)) el(indent + 4, "AnyBIC", cp.bic) else sp(indent + 4) # "<NmAndAdr>\n" # el(indent + 6, "Nm", clip(cp.name, 350)) # sp(indent + 4) # "</NmAndAdr>\n";
    sp(indent) # "<" # role # ">\n" # sp(indent + 2) # "<Id>\n" # id # sp(indent + 2) # "</Id>\n" # sp(indent) # "</" # role # ">\n"
  };

  /// One fill as a securities trade confirmation: the desk on its side, the participant on the other.
  public func setr027Xml(cycle : Nat, seq : Nat, orderId : Nat, side : MT.Side, isin : Text, currency : Text, minorUnits : Nat8, nominal : Nat, cash : Nat, priceMicro : Nat, accrued : Nat, tradeDay : Nat, settlementDay : Nat, desk : TT.Counterparty, participant : TT.Counterparty) : Text {
    let (buyer, seller) = switch (side) { case (#buy) (desk, participant); case (#sell) (participant, desk) };
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    # "<Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:setr.027.001.05\">\n"
    # "  <SctiesTradConf>\n"
    # "    <Id>\n" # el(6, "TxId", clip(Nat.toText(cycle) # "-" # Nat.toText(seq), 35)) # "    </Id>\n"
    # "    <TradDtls>\n"
    # el(6, "OrdrId", clip(Nat.toText(orderId), 35))
    # el(6, "Sd", switch (side) { case (#buy) "BUYI"; case (#sell) "SELL" })
    # "      <TradDt><Dt><Dt>" # CivilDate.toText(tradeDay) # "</Dt></Dt></TradDt>\n"
    # "      <SttlmDt><Dt><Dt>" # CivilDate.toText(settlementDay) # "</Dt></Dt></SttlmDt>\n"
    # "      <ConfQty><Qty><FaceAmt>" # TreasuryMessages.decimalText(nominal, minorUnits) # "</FaceAmt></Qty></ConfQty>\n"
    # "      <GrssTradAmt>\n" # amount(8, "Amt", currency, cash, minorUnits) # "      </GrssTradAmt>\n"
    # "      <DealPric><Val><Rate>" # rate(priceMicro) # "</Rate></Val></DealPric>\n"
    # "      <AcrdIntrstAmt>\n" # amount(8, "Amt", currency, accrued, minorUnits) # "      </AcrdIntrstAmt>\n"
    # "    </TradDtls>\n"
    # "    <FinInstrmId>\n" # el(6, "ISIN", isin) # "    </FinInstrmId>\n"
    # "    <ConfPties>\n" # party(6, "Buyr", buyer) # party(6, "Sellr", seller) # "    </ConfPties>\n"
    # "  </SctiesTradConf>\n"
    # "</Document>\n"
  };
}
