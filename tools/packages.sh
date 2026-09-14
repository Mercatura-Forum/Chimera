#!/usr/bin/env bash
# Emits the moc --package flags for this repository: the mops dependencies plus the
# pinned products under vendor/ (git submodules at the commits git records):
#   kernel     the Thebes kernel
#   manticore  Manticore's domain modules (src/bank), with the journal and ledger it vendors
#   tachyon    Tachyon's settlement core
set -eu
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
V="$ROOT/vendor"
echo "$(mops sources 2>/dev/null) --package kernel $V/thebes-kernel/src --package manticore $V/manticore/src/bank --package journal $V/manticore/vendor/thebes-ledger-core/src/journal --package ledger $V/manticore/vendor/thebes-ledger-core/src/ledger --package tachyon $V/tachyon/core/src"
