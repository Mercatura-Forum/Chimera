#!/usr/bin/env bash
# Build the ML-DSA-44 signing tool against the pq-crystals reference sources (DILITHIUM_MODE=2): DILITHIUM_REF names
# a checkout of https://github.com/pq-crystals/dilithium (its ref/ directory). Provenance: Manticore tools/pq at 9c0c30e.
set -eu
REF="${DILITHIUM_REF:?set DILITHIUM_REF to the ref/ directory of a pq-crystals dilithium checkout}"
HERE="$(cd "$(dirname "$0")" && pwd)"
cc -O2 -DDILITHIUM_MODE=2 -I"$REF" -o "$HERE/mldsa44_tool" "$HERE/mldsa44_tool.c" \
  "$REF/sign.c" "$REF/packing.c" "$REF/polyvec.c" "$REF/poly.c" "$REF/ntt.c" "$REF/reduce.c" "$REF/rounding.c" "$REF/fips202.c" "$REF/symmetric-shake.c"
echo "built $HERE/mldsa44_tool"
