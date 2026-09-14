#!/usr/bin/env python3
"""Accounting correctness oracle: render one posting set to hledger and beancount, run both,
and compare the resulting trial balances to each other and to an independent fold.
Exit code 0 only when all three agree on every account. Prints counts."""
import json, subprocess, sys, os, re, decimal
from decimal import Decimal
D=Decimal
here=os.path.dirname(os.path.abspath(__file__))
spec=json.load(open(sys.argv[1] if len(sys.argv)>1 else os.path.join(here,'postings.json')))
cur=spec['currency']; mu=spec['minor_units']; scale=D(10)**mu
period=sys.argv[2] if len(sys.argv)>2 else None   # e.g. 2026-01 : trial balance up to end of that month by posting date
hledger=os.environ.get('HLEDGER','hledger')
def amt(minor): return (D(minor)/scale).quantize(D(1)/scale)

# ---- 1. independent fold (posting-date basis) ----
fold={}
n_entries=0
for p in spec['postings']:
    if period and p['date'][:len(period)]>period: continue
    dr=sum(e.get('debit',0) for e in p['entries']); cr=sum(e.get('credit',0) for e in p['entries'])
    assert dr==cr, f"fixture {p['id']} unbalanced"
    for e in p['entries']:
        n_entries+=1
        fold[e['account']]=fold.get(e['account'],0)+e.get('debit',0)-e.get('credit',0)
fold={k:v for k,v in fold.items()}

# ---- 2. hledger journal ----
lines=[]
for a in spec['accounts']: lines.append(f"account {a}")
for p in spec['postings']:
    lines.append(f"{p['date']} {p['id']} {p['narration']}")
    for e in p['entries']:
        v=e.get('debit',0)-e.get('credit',0)
        lines.append(f"    {e['account']}    {amt(v)} {cur}")
open(os.path.join(here,'oracle.journal'),'w').write('\n'.join(lines)+'\n')
args=[hledger,'-f',os.path.join(here,'oracle.journal'),'balance','--flat','--no-total','-N','-O','csv','--commodity-style',f'1000.00 {cur}']
if period: args+=['-e',{'2026-01':'2026-02-01','2026-02':'2026-03-01'}.get(period,period)]
out=subprocess.run(args,capture_output=True,text=True)
if out.returncode!=0: print('hledger failed',out.stderr); sys.exit(2)
hl={}
import csv,io
for row in csv.reader(io.StringIO(out.stdout)):
    if row and row[0]!='account':
        m=re.match(r'\s*(-?[\d,]+\.\d+)',row[1]); hl[row[0]]=int((D(m.group(1).replace(',',''))*scale).to_integral_value()) if m else 0
# hledger 'balance' omits zero-balance accounts unless -E; fill zeros
for a in spec['accounts']: hl.setdefault(a,0)

# ---- 3. beancount ledger ----
bl=[f"option \"operating_currency\" \"{cur}\""]
for a in spec['accounts']: bl.append(f"2025-12-31 open {a} {cur}")
for p in spec['postings']:
    bl.append(f"{p['date']} * \"{p['id']}\" \"{p['narration']}\"")
    for e in p['entries']:
        v=e.get('debit',0)-e.get('credit',0)
        bl.append(f"  {e['account']}  {amt(v)} {cur}")
open(os.path.join(here,'oracle.beancount'),'w').write('\n'.join(bl)+'\n')
from beancount import loader
from beancount.core import data
entries,errors,options=loader.load_file(os.path.join(here,'oracle.beancount'))
if errors: print('beancount errors',errors); sys.exit(3)
import datetime
if period:
    cutoff={'2026-01':datetime.date(2026,2,1),'2026-02':datetime.date(2026,3,1)}[period]
    entries=[e for e in entries if e.date<cutoff]
# beancount: the loader has parsed, booked and validated the ledger (errors==[] above);
# balances are folded from its booked postings so the number is beancount's, not ours.
from collections import defaultdict
bcf=defaultdict(lambda: D(0))
for e in entries:
    if isinstance(e,data.Transaction):
        for p in e.postings: bcf[p.account]+=p.units.number
bc={a:int((bcf[a]*scale).to_integral_value()) for a in spec['accounts']}

# ---- 4. compare ----
mismatch=0
print(f"{'account':34s} {'fold':>14s} {'hledger':>14s} {'beancount':>14s}")
for a in sorted(spec['accounts']):
    f=fold.get(a,0); h=hl.get(a,0); b=bc.get(a,0)
    flag='' if f==h==b else '  <-- MISMATCH'
    if flag: mismatch+=1
    print(f"{a:34s} {f:14d} {h:14d} {b:14d}{flag}")
print(f"postings={sum(1 for p in spec['postings'] if not period or p['date'][:len(period)]<=period)} entries={n_entries} accounts={len(spec['accounts'])} hledger_rows={len(hl)} beancount_rows={len(bc)} mismatches={mismatch} sum_of_balances={sum(fold.values())}")
sys.exit(1 if mismatch else 0)
