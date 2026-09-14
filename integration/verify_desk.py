#!/usr/bin/env python3
"""verify_desk.py: verify a desk-log entry without trusting the contract.

The same five-step check verify_entry.py performs on a journal entry, applied to the desk log and its own subtree
of the combined certified tree:

  1. the certificate's BLS signature against the root key, through the subnet delegation (reused from verify_entry);
  2. the contract's certified_data equals the hash of the returned hash tree;
  3. the tree contains the desk's MMR root under thebes_desk/mmr_root and the journal's under thebes_journal/mmr_root;
  4. the block's own stored bytes hash to the hash it carries, decoded by this file's own reader;
  5. the inclusion proof bags its peaks to the certified root.

Nothing here imports the contract's code. The desk's frame is the kernel's: version(1) index timestamp caller
parentHash then the event as a length-prefixed body whose first byte selects the family; a proposal's command body
travels in the trailer behind the hash. The treasury vocabulary (command bytes 0x20 to 0x2C, event tag 0x55) is
Manticore's, and its decoders below are Manticore's verify_bank.py at 9c0c30e, copied without change; so are the
close event, the alert event, the finding and the reader primitives they use. Everything else is the desk's own.
"""
import hashlib

import verify_entry as V

BLOCK_DOMAIN = b"THEBES-DESK-BLOCK"
SUPPORTED_BLOCK_VERSIONS = (0x01,)
SUPPORTED_COMMAND_ENCODINGS = (1,)
CURRENT_COMMAND_ENCODING = 1

LABEL_DESK = b"thebes_desk"
LABEL_JOURNAL = b"thebes_journal"

REFUSALS = ["noGrant", "outsidePartition", "outsideCurrency", "overCeiling", "overDailyLimit", "notEligibleChecker",
            "selfApproval", "commandHashMismatch", "proposalExpired", "noPolicy", "noWitness", "unknown"]
CATEGORIES = ["asset", "liability", "equity", "income", "expense"]
CONSTRAINTS = ["none", "debitsNotExceedCredits", "creditsNotExceedDebits"]
SHIFT_POLICIES = ["reject", "previous", "next", "nearest"]


def command_domain(version):
    return b"THEBES-DESK-COMMAND-v%d" % version


def _domain_hash(domain, payload):
    h = hashlib.sha256()
    h.update(len(domain).to_bytes(2, "big"))
    h.update(domain)
    h.update(payload)
    return h.digest()


def block_hash(preimage):
    return _domain_hash(BLOCK_DOMAIN, preimage)


class Reader(V.Reader):
    """The journal verifier's reader, the desk's own shapes, and Manticore's treasury, close and alert decoders."""

    # ── the desk's own shapes ──

    def scope(self):
        return {"partitions": self.opt_texts(), "currencies": self.opt_texts(),
                "ceiling": self.opt_money(), "dailyLimit": self.opt_money()}

    def policy(self):
        return {"permission": self.text(), "required": self.nat(), "eligibleRole": self.text(), "ttlSeconds": self.nat()}

    def identity(self):
        return {"name": self.text(), "bic": self.text(), "lei": self.text()}

    def failures(self):
        return [{"seq": self.nat(), "job": self.text(), "scope": self.text(), "entity": self.nat(), "reason": self.text(), "attempts": self.nat()}
                for _ in range(self.len16())]

    def opt_day(self):
        return self.opt(self.nat)

    def command(self, encoding=CURRENT_COMMAND_ENCODING):
        """A command under a recorded encoding version. Version 1 is every family."""
        assert encoding in SUPPORTED_COMMAND_ENCODINGS, f"command encoding {encoding} is not one this verifier implements"
        tag = self.byte()
        if 0x20 <= tag <= 0x2C:
            return self.treasury_command(tag)
        if tag == 0x01:
            return {"openBook": {"id": self.text(), "name": self.text(), "parent": self.opt_text(), "sharia": self.bool()}}
        if tag == 0x02:
            return {"closeBook": {"id": self.text()}}
        if tag == 0x03:
            return {"defineRole": {"id": self.text(), "name": self.text(), "permissions": self.texts()}}
        if tag == 0x04:
            return {"grantRole": {"subject": self.principal(), "role": self.text(), "scope": self.scope()}}
        if tag == 0x05:
            return {"revokeRole": {"subject": self.principal(), "role": self.text()}}
        if tag == 0x06:
            return {"setDualPolicy": self.policy()}
        if tag == 0x07:
            return {"clearDualPolicy": {"permission": self.text()}}
        if tag == 0x08:
            return {"setFeatureActivation": {"feature": self.text(), "height": self.nat64()}}
        if tag == 0x09:
            return {"setDeskIdentity": self.identity()}
        if tag == 0x10:
            return {"journalRegisterCurrency": {"code": self.text(), "minorUnits": self.byte()}}
        if tag == 0x11:
            return {"journalOpenAccount": {"code": self.text(), "name": self.text(), "normalSide": self.side(), "category": self.category(), "constraint": self.constraint()}}
        if tag == 0x12:
            return {"journalCloseAccount": {"code": self.text()}}
        if tag == 0x13:
            return {"journalOpenPeriod": {"id": self.text(), "start": self.nat(), "end": self.nat()}}
        if tag == 0x14:
            return {"journalClosePeriod": {"id": self.text()}}
        if tag == 0x15:
            return {"journalSetCalendar": {"calendar": self.calendar()}}
        if tag == 0x16:
            return {"journalSetCalendarAuthority": {"authority": self.calendar_authority(), "maxRollDays": self.nat(), "businessDate": self.opt_day()}}
        if tag == 0x17:
            return {"journalRollBusinessDate": {"day": self.nat()}}
        if tag == 0x18:
            return {"journalSetActivationHeight": {"height": self.nat64()}}
        if tag == 0x30:
            return {"setFunctionalCurrency": {"currency": self.text()}}
        if tag == 0x31:
            return {"setFxPair": {"pair": self.c_pair()}}
        if tag == 0x32:
            return {"setFxRate": {"rate": self.c_rate()}}
        if tag == 0x33:
            return {"recordRateFixing": {"index": self.text(), "day": self.nat(), "rateBps": self.nat()}}
        if tag == 0x34:
            return {"openEndOfDay": {"book": self.text(), "businessDate": self.nat(), "shardSize": self.nat()}}
        if tag == 0x35:
            return {"setRetryPolicy": {"book": self.text(), "limit": self.nat()}}
        if tag == 0x36:
            return {"resolveEndOfDayFailure": {"book": self.text(), "businessDate": self.nat(), "item": self.nat(), "entity": self.nat(), "reason": self.text()}}
        if tag == 0x37:
            return {"clearAlert": {"alert": self.nat(), "reason": self.text()}}
        raise ValueError(f"unknown command tag {tag:#x}")

    def proposed(self):
        return {"permission": self.text(), "partition": self.opt_text(), "maker": self.principal(), "required": self.nat(), "eligibleRole": self.text(),
                "expiresAt": self.nat64(), "justification": self.text(), "commandHash": self.blob(), "commandEncoding": self.byte()}

    def eod_event(self):
        t = self.byte()
        if t == 0x01:
            return {"opened": {"book": self.text(), "businessDate": self.nat(), "shardSize": self.nat(), "openedAtHeight": self.nat(), "maxDeal": self.nat(),
                               "planHash": self.blob(), "items": self.nat(), "entities": self.nat()}}
        if t == 0x02:
            return {"chunk": {"book": self.text(), "businessDate": self.nat(), "cursorFrom": self.nat(), "cursorTo": self.nat(), "posted": self.nat(),
                              "examined": self.nat(), "zeroMovement": self.nat(), "failures": self.failures()}}
        if t == 0x03:
            book = self.text(); day = self.nat()
            resolved = [{"item": self.nat(), "entity": self.nat()} for _ in range(self.len16())]
            return {"retry": {"book": book, "businessDate": day, "resolved": resolved, "failures": self.failures(), "posted": self.nat()}}
        if t == 0x04:
            return {"completed": {"book": self.text(), "businessDate": self.nat(), "posted": self.nat(), "examined": self.nat(), "zeroMovement": self.nat(), "failures": self.nat()}}
        if t == 0x05:
            return {"retryPolicySet": {"book": self.text(), "limit": self.nat()}}
        if t == 0x06:
            return {"failureResolved": {"book": self.text(), "businessDate": self.nat(), "item": self.nat(), "entity": self.nat(), "reason": self.text()}}
        raise ValueError(f"unknown end-of-day event tag {t:#x}")

    def desk_event(self):
        """The event body: a tag byte, then the family's fields. Called on a reader over the body alone."""
        t = self.byte()
        if t == 0x01:
            return {"deskInstalled": {"installer": self.principal()}}
        if t == 0x02:
            return {"bookOpened": {"id": self.text(), "name": self.text(), "parent": self.opt_text(), "sharia": self.bool()}}
        if t == 0x03:
            return {"bookClosed": {"id": self.text()}}
        if t == 0x04:
            return {"roleDefined": {"id": self.text(), "name": self.text(), "permissions": self.texts()}}
        if t == 0x05:
            return {"roleGranted": {"subject": self.principal(), "role": self.text(), "scope": self.scope()}}
        if t == 0x06:
            return {"roleRevoked": {"subject": self.principal(), "role": self.text()}}
        if t == 0x07:
            return {"dualPolicySet": self.policy()}
        if t == 0x08:
            return {"dualPolicyCleared": {"permission": self.text()}}
        if t == 0x09:
            return {"featureActivationSet": {"feature": self.text(), "height": self.nat64()}}
        if t == 0x0A:
            return {"identitySet": self.identity()}
        if t == 0x10:
            return {"commandProposed": self.proposed()}
        if t == 0x11:
            return {"commandApproved": {"proposal": self.nat(), "checker": self.principal(), "commandHash": self.blob()}}
        if t == 0x12:
            return {"commandExecuted": {"proposal": self.nat(), "commandHash": self.blob(), "effects": self.nats()}}
        if t == 0x13:
            return {"commandRejected": {"proposal": self.nat(), "checker": self.principal(), "reason": self.text()}}
        if t == 0x14:
            return {"commandExpired": {"proposal": self.nat()}}
        if t == 0x15:
            return {"operationRefused": {"subject": self.principal(), "permission": self.text(), "reason": REFUSALS[self.byte()], "detail": self.text()}}
        if t == 0x16:
            return {"emergencyOverride": {"commandHash": self.blob(), "commandEncoding": self.byte(), "actor": self.principal(), "witness": self.principal(), "justification": self.text()}}
        if t == 0x17:
            return {"overrideReviewed": {"override": self.nat(), "reviewer": self.principal(), "disposition": self.text()}}
        if t == 0x18:
            return {"dailyConsumed": {"subject": self.principal(), "currency": self.text(), "day": self.nat(), "amount": self.nat()}}
        if t == 0x20:
            return {"close": self.close_event()}
        if t == 0x21:
            return {"fixingRecorded": {"index": self.text(), "day": self.nat(), "rateBps": self.nat()}}
        if t == 0x22:
            return {"nostroIndexFrom": {"nostro": self.text(), "journalHeight": self.nat()}}
        if t == 0x30:
            return {"eod": self.eod_event()}
        if t == 0x48:
            return {"alert": self.alert_event()}
        if t == 0x55:
            return {"treasury": self.treasury_event()}
        raise ValueError(f"unknown desk event tag {t:#x}")

    # ── lifted without change from Manticore's verify_bank.py at 9c0c30e ──

    def opt_texts(self):
        t = self.byte()
        if t == 0:
            return None
        assert t == 1, "bad option tag for a text list"
        return [self.text() for _ in range(self.len16())]
    def opt_money(self):
        t = self.byte()
        if t == 0:
            return None
        assert t == 1, "bad option tag for a money list"
        return [{"currency": self.text(), "amount": self.nat()} for _ in range(self.len16())]
    def texts(self):
        return [self.text() for _ in range(self.len16())]
    def nats(self):
        return [self.nat() for _ in range(self.len16())]
    def category(self):
        return CATEGORIES[self.byte()]
    def constraint(self):
        return CONSTRAINTS[self.byte()]
    def int_(self):
        sign = self.byte()
        assert sign in (0, 1), "bad sign byte"
        mag = self.nat()
        if sign == 1:
            assert mag != 0, "negative zero is not an encoding"
            return -mag
        return mag
    def bool(self):
        b = self.byte()
        assert b in (0, 1), f"bad boolean byte {b}"
        return b == 1
    def opt_nat(self):
        return self.opt(self.nat)
    def opt_text(self):
        present = self.byte()
        if present == 0:
            return None
        assert present == 1, "bad option byte"
        return self.text()
    DAY_COUNTS = {0x01: "A001", 0x03: "A003", 0x04: "A004", 0x05: "A005",
                  0x06: "A006", 0x07: "A007", 0x0B: "A011"}
    def p_convention(self):
        t = self.byte()
        code = self.DAY_COUNTS[t]
        if t == 0x01:
            return {"code": code, "couponsPerYear": self.nat()}
        return {"code": code}
    VALUE_DATE_CONVENTIONS = ["sameDay", "following", "modifiedFollowing", "preceding",
                              "modifiedPreceding", "endOfMonth"]
    FX_DIRECTION = ["gain", "loss", "unchanged"]
    ADJ_DIRECTION = ["increase", "decrease", "unchanged"]
    DEFERRAL_KIND = ["unearnedIncome", "prepaidExpense"]
    def c_convention(self):
        return self.VALUE_DATE_CONVENTIONS[self.byte()]
    def c_rate(self):
        return {"currency": self.text(), "functional": self.text(), "numerator": self.nat(),
                "denominator": self.nat(), "asOf": self.nat(), "source": self.text()}
    def c_pair(self):
        return {"currency": self.text(), "position": self.text(), "equivalent": self.text(),
                "unrealised": self.text(), "realised": self.text(), "monetary": self.byte() == 1}
    def c_window(self):
        return {"book": self.text(), "freeDays": self.nat(), "approvedDays": self.nat()}
    def c_direction(self):
        return self.FX_DIRECTION[self.byte()]
    def c_adj_direction(self):
        return self.ADJ_DIRECTION[self.byte()]
    def c_schedule(self):
        return {"id": self.text(), "kind": self.DEFERRAL_KIND[self.byte()], "currency": self.text(),
                "amount": self.nat(), "periods": self.nat(), "deferralAccount": self.text(),
                "recognitionAccount": self.text(), "book": self.text(), "openedOn": self.nat()}
    def close_event(self):
        t = self.byte()
        if t == 0x01:
            return {"functionalCurrencySet": {"currency": self.text()}}
        if t == 0x02:
            return {"fxPairSet": {"pair": self.c_pair()}}
        if t == 0x03:
            return {"fxRateSet": {"rate": self.c_rate()}}
        if t == 0x04:
            return {"backValueWindowSet": {"window": self.c_window()}}
        if t == 0x05:
            return {"backValueApproved": {"book": self.text(), "valueDate": self.nat(),
                                          "approver": self.principal(), "reason": self.text()}}
        if t == 0x06:
            return {"fxDealBooked": {"sell": self.text(), "sellAmount": self.nat(),
                                     "buy": self.text(), "buyAmount": self.nat(),
                                     "rateNumerator": self.nat(), "rateDenominator": self.nat(),
                                     "asOf": self.nat(), "day": self.nat()}}
        if t == 0x07:
            return {"fxRevalued": {"currency": self.text(), "position": self.nat(),
                                   "equivalent": self.nat(), "revalued": self.nat(),
                                   "movement": self.nat(), "direction": self.c_direction(),
                                   "rateNumerator": self.nat(), "rateDenominator": self.nat(),
                                   "rateAsOf": self.nat(), "day": self.nat()}}
        if t == 0x08:
            return {"fxRealised": {"currency": self.text(), "closedPosition": self.nat(),
                                   "bookedEquivalent": self.nat(), "proceeds": self.nat(),
                                   "movement": self.nat(), "direction": self.c_direction(),
                                   "day": self.nat()}}
        if t == 0x09:
            return {"accrualAdjusted": {"product": self.text(), "currency": self.text(),
                                        "from": self.nat(), "to": self.nat(),
                                        "recomputed": self.nat(), "booked": self.nat(),
                                        "movement": self.nat(), "direction": self.c_adj_direction(),
                                        "causedBy": self.nat(), "examined": self.nat()}}
        if t == 0x0A:
            return {"deferralScheduleOpened": {"schedule": self.c_schedule()}}
        if t == 0x0B:
            return {"deferralAmortised": {"schedule": self.text(), "period": self.text(),
                                          "sequence": self.nat(), "amount": self.nat(),
                                          "remaining": self.nat()}}
        if t == 0x10:
            return {"periodEndOpened": {"book": self.text(), "period": self.text(),
                                        "closingDate": self.nat()}}
        if t == 0x11:
            return {"periodEndRatesRecorded": {"book": self.text(), "period": self.text(),
                                               "currencies": self.nat()}}
        if t == 0x12:
            return {"periodEndAccrualComplete": {"book": self.text(), "period": self.text(),
                                                 "lastBusinessDay": self.nat()}}
        if t == 0x13:
            return {"periodEndRevalued": {"book": self.text(), "period": self.text(),
                                          "currencies": self.nat(), "posted": self.nat(),
                                          "total": self.nat()}}
        if t == 0x14:
            book = self.text()
            period = self.text()
            total = self.nat()
            rows = [{"schedule": self.text(), "sequence": self.nat(), "amount": self.nat(),
                     "remaining": self.nat()} for _ in range(self.len16())]
            return {"periodEndDeferralsAmortised": {"book": book, "period": period,
                                                    "total": total, "rows": rows}}
        if t == 0x15:
            return {"periodEndReconciled": {"book": self.text(), "period": self.text(),
                                            "controls": self.nat()}}
        if t == 0x16:
            return {"periodEndClosed": {"book": self.text(), "period": self.text()}}
        if t == 0x17:
            return {"bookClosedForPeriod": {"book": self.text(), "period": self.text()}}
        if t == 0x18:
            book = self.text()
            period = self.text()
            retained = self.text()
            closed = self.nat()
            results = [{"currency": self.text(), "profitCredits": self.nat(),
                        "lossDebits": self.nat()} for _ in range(self.len16())]
            return {"yearEndRolled": {"book": book, "period": period,
                                      "retainedEarnings": retained,
                                      "accountsClosed": closed, "results": results}}
        raise ValueError("unknown close event tag %#x" % t)
    def finding(self):
        return {"rule": self.text(), "version": self.nat(), "account": self.nat(), "day": self.nat(),
                "postings": self.nats(), "detail": self.text()}
    def alert_event(self):
        sub = self.byte()
        if sub == 0x01:
            finding = self.finding()
            return {"alertOpened": {"finding": finding, "source": {0: "posting", 1: "endOfDay"}[self.byte()]}}
        if sub == 0x02:
            return {"alertCleared": {"alert": self.nat(), "reason": self.text()}}
        if sub == 0x03:
            return {"alertEscalated": {"alert": self.nat(), "reportRef": self.text()}}
        raise ValueError(f"unknown alert event sub-tag {sub:#x}")
    def dates(self):
        return {"postingDate": self.nat(), "valueDate": self.nat(), "period": self.text(), "narration": self.text()}
    # ── treasury (S3.2) ──
    TREASURY_POLICY = ["mmPlacements", "mmTakings", "mmInterestReceivable", "mmInterestPayable", "mmInterestIncome", "mmInterestExpense", "fxForwardMark", "irsMark", "fxOptionValue",
                       "unrealisedTradingGain", "unrealisedTradingLoss", "realisedTradingGain", "realisedTradingLoss", "securitiesAmortisedCost", "securitiesFvoci", "securitiesFvtpl", "fvociReserve",
                       "couponReceivable", "couponIncome", "amortisationIncome", "amortisationExpense", "nostroSuspense"]
    CURVE_KINDS = ["zeroRates", "forwardPoints", "volatility", "securityPrice"]
    LIMIT_KINDS = ["counterpartyExposure", "openFxPosition", "tenorBucket", "dv01", "stopLoss", "issuerConcentration"]

    def t_policy(self):
        out = {k: self.text() for k in self.TREASURY_POLICY}
        out["lotMethod"] = ["fifo", "averageCost"][self.byte()]
        out["confirmationDueDays"] = self.nat(); out["breakAgeAlertDays"] = self.nat(); out["maxCurvePoints"] = self.nat()
        return out

    def t_points(self):
        n = self.len16()
        return [(self.nat(), self.int_()) for _ in range(n)]

    def t_nats(self):
        n = self.len16()
        return [self.nat() for _ in range(n)]

    def t_opt_text(self):
        b = self.byte()
        assert b in (0, 1)
        return self.text() if b == 1 else None

    def t_opt_principal(self):
        b = self.byte()
        assert b in (0, 1)
        return self.principal() if b == 1 else None

    def t_counterparty(self):
        return {"party": self.opt_nat(), "name": self.text(), "bic": self.text(), "lei": self.text()}

    def t_cash(self):
        return {"account": self.text(), "sub": self.t_opt_text()}

    def t_security_terms(self):
        return {"isin": self.text(), "issuer": self.text(), "currency": self.text(), "couponBps": self.nat(), "couponsPerYear": self.nat(), "dayCount": self.p_convention(), "issue": self.nat(), "maturity": self.nat()}

    def t_curve(self):
        return {"id": self.text(), "kind": self.CURVE_KINDS[self.byte()], "currency": self.text(), "day": self.nat(), "points": self.t_points(), "source": self.blob()}

    def t_limit(self):
        return {"book": self.text(), "kind": self.LIMIT_KINDS[self.byte()], "currency": self.text(), "subject": self.text(), "value": self.nat()}

    def t_nostro(self):
        return {"id": self.text(), "account": self.text(), "sub": self.t_opt_text(), "currency": self.text(), "correspondent": self.t_counterparty(), "iban": self.text(), "valueDateToleranceDays": self.nat()}

    def t_forward(self):
        return {"base": self.text(), "quote": self.text(), "direction": ["buy", "sell"][self.byte()], "baseAmount": self.nat(), "rateMicro": self.nat(), "valueDate": self.nat(), "spotMicro": self.nat(),
                "forwardPointsMicro": self.int_(), "baseAccount": self.t_cash(), "quoteAccount": self.t_cash(), "pointsCurve": self.text(), "discountCurve": self.text()}

    def t_kind(self):
        k = self.byte()
        if k == 1:
            return {"moneyMarket": {"placement": self.bool(), "currency": self.text(), "principal": self.nat(), "rateBps": self.nat(), "dayCount": self.p_convention(), "start": self.nat(), "maturity": self.nat(), "cash": self.t_cash()}}
        if k == 2:
            return {"fxForward": self.t_forward()}
        if k == 3:
            return {"fxSwap": {"near": self.t_forward(), "far": self.t_forward()}}
        if k == 4:
            return {"security": {"isin": self.text(), "direction": ["buy", "sell"][self.byte()], "nominal": self.nat(), "priceMicro": self.nat(), "settlement": self.nat(),
                                 "classification": ["amortisedCost", "fvoci", "fvtpl"][self.byte()], "cash": self.t_cash(), "priceCurve": self.text(), "venue": self.t_opt_text()}}
        if k == 5:
            return {"irs": {"currency": self.text(), "notional": self.nat(), "payFixed": self.bool(), "fixedBps": self.nat(), "floatingIndex": self.text(), "spreadBps": self.int_(), "start": self.nat(),
                            "maturity": self.nat(), "paymentMonths": self.nat(), "dayCount": self.p_convention(), "cash": self.t_cash(), "discountCurve": self.text()}}
        if k == 6:
            return {"fxOption": {"base": self.text(), "quote": self.text(), "call": self.bool(), "bought": self.bool(), "baseAmount": self.nat(), "strikeMicro": self.nat(), "expiry": self.nat(), "premium": self.nat(),
                                 "start": self.nat(), "cash": self.t_cash(), "domesticCurve": self.text(), "foreignCurve": self.text(), "volCurve": self.text()}}
        raise ValueError(f"unknown deal kind {k}")

    def t_entries(self):
        n = self.len16()
        return [{"reference": self.text(), "amount": self.nat(), "credit": self.bool(), "valueDay": self.nat(), "bookingDay": self.nat(), "counterparty": self.text()} for _ in range(n)]

    def t_fields(self):
        return {"kind": self.text(), "amount1": self.nat(), "currency1": self.text(), "amount2": self.nat(), "currency2": self.text(), "valueDate": self.nat(), "rateMicro": self.nat(), "counterparty": self.text()}

    def t_opt_fields(self):
        b = self.byte()
        assert b in (0, 1)
        return self.t_fields() if b == 1 else None

    def t_opt_correction(self):
        b = self.byte()
        assert b in (0, 1)
        return {"account": self.text(), "sub": self.t_opt_text(), "debit": self.bool(), "amount": self.nat(), "currency": self.text()} if b == 1 else None

    def opt_blob_(self):
        b = self.byte()
        assert b in (0, 1)
        return self.blob() if b == 1 else None

    def treasury_command(self, sub):
        D = self.dates
        if sub == 0x20:
            return {"setTreasuryPolicy": self.t_policy()}
        if sub == 0x21:
            return {"registerSecurity": {"terms": self.t_security_terms()}}
        if sub == 0x22:
            return {"publishCurve": {"curve": self.t_curve()}}
        if sub == 0x23:
            return {"setTreasuryLimit": {"limit": self.t_limit()}}
        if sub == 0x24:
            return {"registerNostro": {"nostro": self.t_nostro()}}
        if sub == 0x25:
            return {"captureDeal": {"book": self.text(), "counterparty": self.t_counterparty(), "kind": self.t_kind(), "reference": self.text(), "approver": self.t_opt_principal()}}
        if sub == 0x26:
            return {"confirmDeal": {"deal": self.nat(), "confirmation": self.blob(), "fields": self.t_opt_fields(), "document": self.opt_blob_()}}
        if sub == 0x27:
            return {"amendDeal": {"deal": self.nat(), "kind": self.t_kind(), "reason": self.text()}}
        if sub == 0x28:
            return {"cancelDeal": {"deal": self.nat(), "reason": self.text()}}
        if sub == 0x29:
            return {"settleDealLeg": {"deal": self.nat(), "leg": self.nat(), **D()}}
        if sub == 0x2A:
            return {"markDeal": {"deal": self.nat(), **D()}}
        if sub == 0x2B:
            return {"recordNostroStatement": {"nostro": self.text(), "statement": self.blob(), "from": self.nat(), "to": self.nat(), "entries": self.t_entries(), "document": self.opt_blob_()}}
        if sub == 0x2C:
            return {"resolveNostroBreak": {"breakId": self.nat(), "resolution": self.text(), "correction": self.t_opt_correction(), **D()}}
        raise ValueError(f"unknown treasury command byte {sub:#x}")

    def treasury_event(self):
        t = self.byte()
        if t == 0x01:
            return {"policySet": self.t_policy()}
        if t == 0x02:
            return {"securityRegistered": {"terms": self.t_security_terms(), "day": self.nat()}}
        if t == 0x03:
            return {"curvePublished": {"curve": self.t_curve()}}
        if t == 0x04:
            return {"limitSet": {"limit": self.t_limit(), "day": self.nat()}}
        if t == 0x05:
            return {"nostroRegistered": {"nostro": self.t_nostro(), "day": self.nat()}}
        if t == 0x06:
            return {"dealCaptured": {"book": self.text(), "counterparty": self.t_counterparty(), "kind": self.t_kind(), "reference": self.text(), "trader": self.principal(), "day": self.nat(),
                                     "withinLimits": self.bool(), "approver": self.t_opt_principal(), "secondAmount": self.nat()}}
        if t == 0x07:
            return {"limitBreached": {"limit": self.t_limit(), "measured": self.nat(), "deal": self.nat(), "approver": self.principal(), "day": self.nat()}}
        if t == 0x08:
            return {"dealConfirmed": {"deal": self.nat(), "confirmation": self.blob(), "day": self.nat()}}
        if t == 0x09:
            return {"confirmationMismatch": {"deal": self.nat(), "confirmation": self.blob(), "field": self.text(), "ours": self.text(), "theirs": self.text(), "day": self.nat()}}
        if t == 0x0A:
            return {"dealAmended": {"deal": self.nat(), "kind": self.t_kind(), "reason": self.text(), "day": self.nat(), "secondAmount": self.nat()}}
        if t == 0x0B:
            return {"dealCancelled": {"deal": self.nat(), "reason": self.text(), "day": self.nat()}}
        if t == 0x0C:
            return {"legSettled": {"deal": self.nat(), "leg": self.nat(), "amount": self.nat(), "currency": self.text(), "realised": self.int_(), "day": self.nat(), "accrual": self.int_(),
                                   "amortisation": self.int_(), "fv": self.int_(), "nominal": self.nat(), "cost": self.nat()}}
        if t == 0x0D:
            return {"lotConsumed": {"lot": self.nat(), "by": self.nat(), "nominal": self.nat(), "cost": self.nat(), "amortisation": self.int_(), "fv": self.int_(), "accrual": self.int_(), "day": self.nat()}}
        if t == 0x0E:
            return {"accrued": {"deal": self.nat(), "interest": self.int_(), "amortisation": self.int_(), "day": self.nat()}}
        if t == 0x0F:
            return {"marked": {"deal": self.nat(), "value": self.int_(), "previous": self.int_(), "day": self.nat()}}
        if t == 0x10:
            return {"couponPaid": {"deal": self.nat(), "amount": self.nat(), "day": self.nat()}}
        if t == 0x11:
            return {"statementRecorded": {"nostro": self.text(), "statement": self.blob(), "from": self.nat(), "to": self.nat(), "entries": self.nat(), "matches": self.t_nats(), "breaks": self.nat(), "day": self.nat()}}
        if t == 0x12:
            return {"nostroBreak": {"nostro": self.text(), "statement": self.blob(), "side": ["onStatementOnly", "inOurBooksOnly"][self.byte()], "amount": self.nat(), "credit": self.bool(), "valueDay": self.nat(),
                                    "reference": self.text(), "posting": self.opt_nat(), "day": self.nat()}}
        if t == 0x13:
            return {"breakResolved": {"breakId": self.nat(), "resolution": self.text(), "corrected": self.bool(), "day": self.nat()}}
        if t == 0x14:
            return {"breakAged": {"breakId": self.nat(), "ageDays": self.nat(), "day": self.nat()}}
        if t == 0x15:
            return {"confirmationOverdue": {"deal": self.nat(), "ageDays": self.nat(), "day": self.nat()}}
        raise ValueError(f"unknown treasury event tag {t:#x}")


def command_hash(command_bytes, version=CURRENT_COMMAND_ENCODING):
    """The hash a checker approves, over the canonical command bytes under the recorded encoding version; the
    domain names the version, so a body hashed under another version never matches."""
    assert version in SUPPORTED_COMMAND_ENCODINGS, f"command encoding {version} is not one this verifier implements"
    return _domain_hash(command_domain(version), command_bytes)


def _event_body(r):
    """The length-prefixed body: decoded by its own reader, which must consume it exactly."""
    n = r.nat()
    body = r.take(n)
    br = Reader(body)
    event = br.desk_event()
    assert br.p == len(body), "the event body has trailing bytes"
    return event


def decode_block(raw):
    """Decode raw desk block bytes; verify the embedded hash; return (fields, hash). Any malformed input is a
    verification failure, never a crash."""
    try:
        return _decode_block(raw)
    except (IndexError, ValueError, KeyError, UnicodeDecodeError, AssertionError) as e:
        raise AssertionError(f"malformed desk block bytes: {e}") from None


def _decode_block(raw):
    r = Reader(raw)
    version = r.byte()
    assert version in SUPPORTED_BLOCK_VERSIONS, f"desk block version {version}"
    index = r.nat()
    timestamp = r.nat64()
    caller = r.principal()
    parent = r.opt(r.blob)
    event = _event_body(r)
    preimage_len = r.p
    stored = r.take(32)
    # the trailer: a proposal's or an override's body, bound to the preimage by its commandHash
    carrier = event.get("commandProposed") or event.get("emergencyOverride")
    if carrier is not None:
        enc = carrier["commandEncoding"]
        assert enc in SUPPORTED_COMMAND_ENCODINGS, f"the block records command encoding {enc}, which this verifier does not implement"
        flag = r.byte()
        if flag == 1:
            start = r.p
            body = r.command(enc)
            assert command_hash(raw[start:r.p], enc) == carrier["commandHash"], "the trailer's body does not hash to the preimage's commandHash under its recorded encoding"
            carrier["command"] = body
        elif flag != 0:
            raise ValueError("bad trailer flag %d" % flag)
    assert r.p == len(raw), "trailing bytes"
    computed = block_hash(raw[:preimage_len])
    assert computed == stored, "embedded hash does not match recomputed hash"
    return {"index": index, "timestamp": timestamp, "caller": caller, "parentHash": parent, "event": event}, stored


def command_bytes_of(raw):
    """The canonical command bytes and their encoding version from a proposal or override block, so the command
    hash can be recomputed from the block rather than taken from it. None once a pack dropped the body."""
    fields, _ = _decode_block(raw)
    carrier = fields["event"].get("commandProposed") or fields["event"].get("emergencyOverride")
    assert carrier is not None, "the block carries no command"
    r = Reader(raw)
    r.byte(); r.nat(); r.nat64(); r.principal(); r.opt(r.blob)
    n = r.nat(); r.take(n); r.take(32)
    if r.byte() != 1:
        return None
    start = r.p
    r.command(carrier["commandEncoding"])
    return raw[start:r.p], carrier["commandEncoding"]


def certified_roots(tip, root_key_der, canister_id_bytes):
    """Steps 1 to 3: verify the certificate, tie the returned tree to certified_data, and return both MMR roots out
    of the one tree. A tree that carries only one of them fails here, which is the property that makes a posting and
    the authority behind it provable against the same certificate."""
    tree = V.verify_certificate(tip["certificate"], root_key_der, canister_id_bytes)
    certified = V.lookup(tree, [b"canister", canister_id_bytes, b"certified_data"])
    assert certified is not None, "certificate has no certified_data for this canister"
    ht = V.cbor2.loads(bytes(tip["hash_tree"]))
    assert V.hash_tree(ht) == certified, "hash tree root != certified_data"
    desk = V.lookup(ht, [LABEL_DESK, b"mmr_root"])
    journal = V.lookup(ht, [LABEL_JOURNAL, b"mmr_root"])
    assert desk is not None, "certified tree has no thebes_desk/mmr_root"
    assert journal is not None, "certified tree has no thebes_journal/mmr_root"
    return {
        "desk": desk, "journal": journal,
        "desk_index": V.lookup(ht, [LABEL_DESK, b"last_block_index"]), "desk_hash": V.lookup(ht, [LABEL_DESK, b"last_block_hash"]),
        "journal_index": V.lookup(ht, [LABEL_JOURNAL, b"last_block_index"]), "journal_hash": V.lookup(ht, [LABEL_JOURNAL, b"last_block_hash"]),
    }


def verify_desk_entry(raw_block, proof, tip, root_key_der, canister_id_bytes, expect_index=None):
    """The five-step check for a desk block. Returns the decoded fields."""
    roots = certified_roots(tip, root_key_der, canister_id_bytes)
    fields, h = decode_block(bytes(raw_block))
    if expect_index is not None:
        assert fields["index"] == expect_index, "desk block index mismatch"
    assert V.mmr_verify(h, fields["index"], [bytes(x) for x in proof["siblings"]], [bytes(x) for x in proof["peaks"]], proof["peakIndex"], roots["desk"]), \
        "desk inclusion proof does not bag to the certified root"
    return fields


def verify_journal_entry(raw_block, proof, tip, root_key_der, canister_id_bytes, expect_index=None):
    """The same check for a journal block, against the journal root inside the same certificate."""
    roots = certified_roots(tip, root_key_der, canister_id_bytes)
    fields, h = V.decode_block(bytes(raw_block))
    if expect_index is not None:
        assert fields["index"] == expect_index, "journal block index mismatch"
    assert V.mmr_verify(h, fields["index"], [bytes(x) for x in proof["siblings"]], [bytes(x) for x in proof["peaks"]], proof["peakIndex"], roots["journal"]), \
        "journal inclusion proof does not bag to the certified root"
    return fields
