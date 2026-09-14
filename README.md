# Chimera: Thebes Treasury and Capital Markets

**Chimera is a treasury and capital-markets desk system that runs as a smart
contract on the Thebes substrate.** Every deal, position, valuation, settlement
and reconciliation is a command on a certified log, folded into positions,
settled delivery-versus-payment through Tachyon, and posted to Manticore's
journal. Money market, foreign exchange, fixed income and equities, repo and
securities lending, vanilla derivatives, custody, collateral and liquidity
management: one product made of several natures. Written in Motoko. Apache 2.0.

- **Deals as commands, positions as folds.** A deal is canonical bytes under
  maker-checker; positions per book, instrument and counterparty are folds over
  the log and re-verify from their bytes outside the contract.
- **Valuation by declared curves.** Curves and prices are recorded data with
  provenance, never fetched at valuation time; marks, accruals and P&L are folds
  with the model named where it is used, twinned bit for bit by an independent
  Python computation.
- **Settlement both or neither.** Every securities trade settles as a Tachyon
  DvP trade: the cash leg and the security leg move together, or neither moves,
  with a certified receipt for each event.
- **Reconciled, not assumed.** Nostro, depot and cash statements are matched
  by reference then amount and date; breaks are recorded facts with ageing.
- **The desk holds no funds.** Cash sits on the bank's ledgers, securities on
  instrument ledgers, a leg in Tachyon's escrow only between funding and
  settlement of a live trade.

| | |
|---|---|
| Foundation | the Thebes kernel (rows, commands, permissions, packing, registry, evidence, exact arithmetic) |
| Journal | Manticore's provable double-entry journal, GL, P&L and reserves |
| Settlement | Tachyon, BIS DvP Model 1, ICRC-1/2/7 legs, Merkle mountain range receipts |
| Messages | ISO 20022 (setr, sese, fxtr, camt) through Manticore's hub |
| Status | verified and in the repository: Manticore's treasury domain runs inside the desk contract on the kernel's spine with the treasury battery reproducing Manticore's counts; call money, spot, the revaluation and the limit across both families; depots, transfers and corporate actions over Manticore's lots; settlement through Tachyon as delivery versus payment with every receipt verified against Tachyon's root; repo, reverse repo and securities lending as financings with margin and manufactured payments; valuation with QuantLib as a second oracle, the result attributed by period as a fold, hedge relationships; collateral agreements with their pools and calls against an independent computation, and a limit tree over every family with its utilisation swept in bounded slices; intraday nostro notifications, the custodian's holdings and postings and the settlement cash against the ledger reconciled with every break a recorded fact; the cash-flow ladder, the liquidity coverage and net stable funding ratios, the large exposures and the treasury liquidity return against an independent computation, every factor and class a recorded declaration; the freeze-gated price feed with every figure verified under its ML-DSA-44 signature, the settlement cycle over Tachyon's matching engine with the fill schedule equal to an independent computation and every fill settled through the one path, and the tenancy with the kernel's registry, fleet rule and isolation suite; securities movements free of payment (the four collateral movements, a transfer to and from a custodian's depot) settled as Tachyon's delivery on the taker's acceptance, the receipt under the venue's root, sese.023 free of payment |

## Domains, in dependency order

| Domain | |
|---|---|
| Deal capture and positions | |
| Money market and FX | |
| Securities and custody | |
| Settlement through Tachyon | |
| Repo, reverse repo and securities lending | |
| Valuation and P&L | |
| Collateral and limits | |
| Nostro, depot and cash reconciliation | |
| Liquidity and regulatory | |
| Market interfaces | |


## Layout

```
src/            the desk contract and its modules, the registry and the isolation judge beside it
test/           the interpreter batteries
integration/    the Python twins and their oracles
vendor/         the pinned dependencies (kernel, Manticore, Tachyon), by commit
tools/          packages.sh, the build and the gate
```

## Building and testing

Requirements: `moc` 1.4.1 and `mops`; the pinned dependencies are git
submodules (`git submodule update --init --recursive`). The feed battery signs
with the ML-DSA-44 reference tool, built by `tools/pq/mldsa44-ref/build.sh`
from a pq-crystals checkout named by `DILITHIUM_REF`.

```
mops install
S=$(./tools/packages.sh)
moc --legacy-persistence $S -o build/Desk.wasm src/Desk.mo
./test/run.sh                          # the Motoko battery and the permission audit
./tools/permission_audit_negative.sh   # the audit's negative control
```

Contracts are built with legacy (classical) persistence so that an in-place
upgrade keeps its state.

## Licence

Apache License 2.0 (see `LICENSE` and `NOTICE`).

Attribution: Thebes Core Team.
