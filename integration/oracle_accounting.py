#!/usr/bin/env python3
"""oracle_accounting.py — external correctness oracle (hledger and Beancount).

Two independent checks. First, the S0X harness (integration/oracle/s0x_oracle.py,
vendored unmodified) is run on its own fixture — the posting set the battery
also pushed through the canister — and its three-way-agreed balances are
compared with the journal's trial balance. Second, the whole population the
battery booked is rendered for hledger and Beancount by this module (the S0X
fixture format is single-currency and has no value dates) and compared row by
row per period, cumulatively, and by value date.

Takes the evidence file written by battery.py — every posting the journal
booked, decoded from the raw committed bytes, plus the journal's own trial
balances — renders the same postings as an hledger journal and as a Beancount
ledger, asks each tool for per-period and cumulative balances, and compares
them with the journal's trial balance row by row. Value dates are rendered as
hledger secondary dates and compared with the journal's value-dated balances.
Disagreement is our bug until proven otherwise; every comparison is counted.

Usage: oracle_accounting.py --evidence evidence.json [--hledger PATH] [--bean-query PATH]
"""
import argparse
import csv
import io
import json
import os
import subprocess
import sys
from datetime import date, timedelta
from decimal import Decimal

EPOCH = date(1970, 1, 1)
ROOT = {"asset": "Assets", "liability": "Liabilities", "equity": "Equity", "income": "Income", "expense": "Expenses"}
MINOR = {"EGP": 2, "USD": 2, "KWD": 3}


def dtxt(day):
    return (EPOCH + timedelta(days=day)).isoformat()


def amt(minor, ccy):
    e = MINOR[ccy]
    q = Decimal(minor).scaleb(-e)
    return f"{q:.{e}f}"


def acct_name(code, accounts):
    return f"{ROOT[accounts[code]['category']]}:A{code.replace('.', '-')}"


def render_hledger(ev):
    out = []
    for p in ev["postings"]:
        out.append(f"{dtxt(p['postingDate'])}={dtxt(p['valueDate'])} {p['narration'].replace(chr(10), ' ')}")
        for l in p["legs"]:
            sign = "" if l["side"] == "debit" else "-"
            out.append(f"    {acct_name(l['account'], ev['accounts'])}    {sign}{amt(l['amount'], l['currency'])} {l['currency']}")
        out.append("")
    return "\n".join(out)


def render_beancount(ev):
    out = ["option \"operating_currency\" \"EGP\"", ""]
    for code in sorted(ev["accounts"]):
        out.append(f"1970-01-01 open {acct_name(code, ev['accounts'])}")
    out.append("")
    for p in ev["postings"]:
        narr = p["narration"].replace('"', "'")
        out.append(f"{dtxt(p['postingDate'])} * \"{narr}\"")
        for l in p["legs"]:
            sign = "" if l["side"] == "debit" else "-"
            out.append(f"  {acct_name(l['account'], ev['accounts'])}  {sign}{amt(l['amount'], l['currency'])} {l['currency']}")
        out.append("")
    return "\n".join(out)


def hledger_balances(hledger, path, begin=None, end=None, date2=False):
    cmd = [hledger, "-f", path, "balance", "--flat", "--no-total", "-O", "csv", "--layout", "bare"]
    if begin:
        cmd += ["-b", begin]
    if end:
        cmd += ["-e", end]
    if date2:
        cmd += ["--date2"]
    r = subprocess.run(cmd, capture_output=True, text=True, check=True)
    out = {}
    for row in csv.DictReader(io.StringIO(r.stdout)):
        out[(row["account"], row["commodity"])] = Decimal(row["balance"].replace(",", ""))
    return out


def beancount_balances(_unused, path, begin=None, end=None):
    """Balances from Beancount's own loader: the ledger is parsed, booked and
    validated by Beancount (errors fail the run), then its booked postings are
    summed per (account, currency) within [begin, end). Beancount 3 ships the
    query shell as a separate package, so the loader is used directly."""
    from beancount import loader
    from beancount.core import data
    from datetime import date as _date
    entries, errors, _ = loader.load_file(path)
    assert not errors, errors
    lo = _date.fromisoformat(begin) if begin else None
    hi = _date.fromisoformat(end) if end else None
    out = {}
    for e in entries:
        if not isinstance(e, data.Transaction):
            continue
        if (lo and e.date < lo) or (hi and e.date >= hi):
            continue
        for p in e.postings:
            k = (p.account, p.units.currency)
            out[k] = out.get(k, Decimal(0)) + p.units.number
    return out


def journal_nets(ev, period):
    """(account name, currency) -> (period net, closing net) from the journal's trial balance."""
    tb = ev["trial_balances"][period]
    out = {}
    for r in tb["rows"]:
        k = (acct_name(r["account"], ev["accounts"]), r["currency"])
        out[k] = (Decimal(r["periodDebits"] - r["periodCredits"]).scaleb(-MINOR[r["currency"]]),
                  Decimal(r["closingDebits"] - r["closingCredits"]).scaleb(-MINOR[r["currency"]]))
    return out


def s0x_oracle(ev, hledger, work):
    """Run the S0X harness (integration/oracle/s0x_oracle.py, three-way hledger/
    Beancount/fold agreement) on its own fixture and compare its balances with
    the journal's trial balance for the same accounts, both for the full set
    and for the 2026-01 period cut."""
    here = os.path.dirname(os.path.abspath(__file__))
    spec_path = os.path.join(here, "oracle", "postings.json")
    spec = json.load(open(spec_path))
    code_of = ev["fixture_accounts"]                 # oracle account name -> journal code (remapped by the battery)
    assert set(code_of) == set(spec["accounts"]), "fixture accounts differ between battery and oracle"
    scale = 10 ** spec["minor_units"]
    compared = 0
    for cut, period in ((None, "2026-02"), ("2026-01", "2026-01")):
        # the harness writes oracle.journal/oracle.beancount next to itself; run it from a copy in work
        import shutil
        odir = os.path.join(work, "s0x-oracle")
        os.makedirs(odir, exist_ok=True)
        shutil.copy(os.path.join(here, "oracle", "s0x_oracle.py"), os.path.join(odir, "oracle.py"))
        shutil.copy(spec_path, os.path.join(odir, "postings.json"))
        cmd = [sys.executable, os.path.join(odir, "oracle.py"), os.path.join(odir, "postings.json")] + ([cut] if cut else [])
        r = subprocess.run(cmd, capture_output=True, text=True, env=dict(os.environ, HLEDGER=hledger))
        assert r.returncode == 0, ("s0x oracle disagreement", r.stdout, r.stderr)
        rows = {}
        for line in r.stdout.splitlines():
            parts = line.split()
            if len(parts) >= 4 and parts[0] in code_of:
                fold, hl, bc = int(parts[1]), int(parts[2]), int(parts[3])
                assert fold == hl == bc
                rows[code_of[parts[0]]] = fold
        assert len(rows) == len(code_of), ("oracle printed fewer rows than accounts", rows)
        tb = ev["trial_balances"][period]
        ours = {r_["account"]: r_["closingDebits"] - r_["closingCredits"] for r_ in tb["rows"] if r_["currency"] == spec["currency"]}
        for code, net in rows.items():
            assert ours.get(code, 0) == net, ("journal vs s0x oracle", period, code, ours.get(code), net)
            compared += 1
        print(f"s0x oracle {'full set' if cut is None else 'cut at ' + cut}: {len(rows)} accounts agree with the journal closing balances of {period}")
    print(f"count: journal balances agreeing with the S0X three-way oracle = {compared}")
    assert compared > 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--evidence", required=True)
    ap.add_argument("--hledger", default=os.environ.get("HLEDGER", "hledger"))
    ap.add_argument("--bean-query", default=os.environ.get("BEAN_QUERY", "beancount-loader"))
    ap.add_argument("--workdir", default=None)
    a = ap.parse_args()
    ev = json.load(open(a.evidence))
    work = a.workdir or os.path.dirname(os.path.abspath(a.evidence))
    if "fixture_accounts" in ev:
        s0x_oracle(ev, a.hledger, work)
    else:
        print("s0x oracle: evidence carries no fixture accounts (not a journal-battery run); skipped, the full-population check follows")
    hpath = os.path.join(work, "oracle.journal")
    bpath = os.path.join(work, "oracle.beancount")
    open(hpath, "w").write(render_hledger(ev))
    open(bpath, "w").write(render_beancount(ev))
    print(f"count: postings rendered to hledger and beancount = {len(ev['postings'])}")
    assert len(ev["postings"]) > 0
    subprocess.run([a.hledger, "-f", hpath, "check"], check=True)
    subprocess.run(["bean-check", bpath], check=True)
    print("hledger check and bean-check: clean")

    rows_h = rows_b = 0
    for period, meta in sorted(ev["periods"].items(), key=lambda kv: kv[1]["start"]):
        begin, end = dtxt(meta["start"]), dtxt(meta["end"] + 1)
        ours = journal_nets(ev, period)
        h_period = hledger_balances(a.hledger, hpath, begin, end)
        h_close = hledger_balances(a.hledger, hpath, None, end)
        b_period = beancount_balances(a.bean_query, bpath, begin, end)
        b_close = beancount_balances(a.bean_query, bpath, None, end)
        for k, (pnet, cnet) in ours.items():
            hp, hc = h_period.get(k, Decimal(0)), h_close.get(k, Decimal(0))
            bp, bc = b_period.get(k, Decimal(0)), b_close.get(k, Decimal(0))
            assert hp == pnet and hc == cnet, ("hledger", period, k, hp, hc, pnet, cnet)
            assert bp == pnet and bc == cnet, ("beancount", period, k, bp, bc, pnet, cnet)
            rows_h += 1; rows_b += 1
        # every account the oracles report must appear in ours (no missing rows)
        for k, v in list(h_close.items()) + list(b_close.items()):
            if v != 0:
                assert k in ours, ("oracle has a row the journal lacks", period, k, v)
        print(f"period {period}: {len(ours)} rows agree with hledger and beancount (period and closing)")
    print(f"count: trial balance rows agreeing with hledger = {rows_h}")
    print(f"count: trial balance rows agreeing with beancount = {rows_b}")
    assert rows_h > 0 and rows_b > 0

    # value-dated balances vs hledger secondary dates
    vd = 0
    for v in ev["value_dated"]:
        k = (acct_name(v["account"], ev["accounts"]), v["currency"])
        h = hledger_balances(a.hledger, hpath, None, dtxt(v["asOf"] + 1), date2=True)
        ours = Decimal(v["debits"] - v["credits"]).scaleb(-MINOR[v["currency"]])
        assert h.get(k, Decimal(0)) == ours, ("value-dated", v, h.get(k), ours)
        vd += 1
    print(f"count: value-dated balances agreeing with hledger --date2 = {vd}")
    assert vd > 0
    print("ORACLE GREEN")


if __name__ == "__main__":
    main()
