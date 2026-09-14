#!/usr/bin/env bash
# Regenerates test/CommandVectors.mo from the samples. A hash that changes for a family under a version that already
# shipped is not something to regenerate over: it is a new encoding version with the old encoder kept.
set -eu
cd "$(dirname "$0")/.."
MOC="${MOC:-$HOME/.cache/mops/moc/1.4.1/moc}"
"$MOC" -r $(./tools/packages.sh) tools/record_command_vectors.mo 2>/dev/null > test/CommandVectors.mo
echo "wrote test/CommandVectors.mo ($(grep -c 'family =' test/CommandVectors.mo) vectors)"
