#!/usr/bin/env bash
# Type-checks every module under src/ and test/ against the pinned packages.
set -eu
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MOC="${MOC:-$(ls "$HOME/.cache/mops/moc/1.4.1/moc" 2>/dev/null | head -1)}"
S="$("$ROOT/tools/packages.sh")"
rc=0
for f in $(find "$ROOT/src" "$ROOT/test" -name '*.mo' 2>/dev/null | sort); do
  if "$MOC" --check $S "$f" 2>&1 | grep -v 'M0155\|^  Nat$\|^  Int$\|^help' | grep -q 'error'; then echo "FAIL $f"; rc=1; else echo "ok   $f"; fi
done
exit $rc
