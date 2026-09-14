#!/usr/bin/env python3
"""widen_scope_check.py: the tenancy battery's negative control. A copy of Desk.mo with one scope check taken out,
so a stranger's read of another desk's deals is answered; the isolation suite must go red against the contract
built from it, or the suite is not connected to what it claims to test. Used by run_integration.sh; never on the
source in place."""
import sys

path = sys.argv[1]
s = open(path).read()
a = """    if (not Auth.mayReadBook(readScope(caller), book)) return #err(#OutsideBookScope({ book }));
    #ok(Array.map<TreasuryCore.DealRow, TT.DealView>(TreasuryCore.dealsOfBook(desk.treasury, book)"""
b = """    ignore caller;
    #ok(Array.map<TreasuryCore.DealRow, TT.DealView>(TreasuryCore.dealsOfBook(desk.treasury, book)"""
assert s.count(a) == 1, "the scope check the negative control widens is not where it expects it"
open(path, "w").write(s.replace(a, b))
print(f"widened treasuryDealsOfBook in {path}")
