#!/usr/bin/env bash
# Fetch the official ISO 20022 schemas the batteries validate the desk's messages against, and check them against
# the recorded checksums. The schemas are not redistributed with this repository (tools/iso20022/schemas/ is
# ignored); schemas.json is the handoff: file, bytes, SHA-256, source URL. A copy already present locally is
# accepted when its checksum matches.
#
# Provenance: Manticore tools/iso20022/fetch_schemas.sh at 9c0c30e, over the five schemas the desk validates.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$HERE/schemas"
python3 - "$HERE" <<'PY'
import json, hashlib, os, sys, urllib.request
here = sys.argv[1]
man = json.load(open(os.path.join(here, "schemas.json")))
local = os.environ.get("ISO_SCHEMAS_LOCAL", "")
bad = 0
for name, m in man.items():
    path = os.path.join(here, "schemas", name)
    if not os.path.exists(path) and local and os.path.exists(os.path.join(local, name)):
        open(path, "wb").write(open(os.path.join(local, name), "rb").read())
    if not os.path.exists(path):
        print(f"fetching {name} from {m['url']}")
        open(path, "wb").write(urllib.request.urlopen(urllib.request.Request(m["url"], headers={"User-Agent": "Mozilla/5.0 (X11; Linux x86_64)"}), timeout=120).read())
    got = hashlib.sha256(open(path, "rb").read()).hexdigest()
    ok = got == m["sha256"]
    print(f"{'ok  ' if ok else 'BAD '} {name} {got[:16]}")
    bad += 0 if ok else 1
sys.exit(1 if bad else 0)
PY
