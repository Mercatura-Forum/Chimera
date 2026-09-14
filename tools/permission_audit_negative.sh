#!/usr/bin/env bash
# Negative control for tools/permission_audit.py: the audit must FAIL on each injected fault. A gate that has never
# been seen to fail is not a gate. Four faults perturb only the catalogue, so they are checked against the baseline
# Candid interface; one adds an unguarded public update method, so it needs its own Candid build.
#
# Provenance: Manticore tools/permission_audit_negative.sh at 9c0c30e, with the paths and the injected lines changed.
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MOC="${MOC:-$HOME/.cache/mops/moc/1.4.1/moc}"
WORK="${WORK:-$(mktemp -d)}"
PKGS="$("$ROOT/tools/packages.sh")"
cases=0
bad=0

mkdir -p "$WORK"
BASE_DID="$WORK/baseline.did"
"$MOC" $PKGS --idl -o "$BASE_DID" "$ROOT/src/Desk.mo" 2> "$WORK/baseline.build.log" || {
  echo "baseline Desk.mo did not build"; grep -v M0155 "$WORK/baseline.build.log" | sed -n '1,10p'; exit 2
}
python3 "$ROOT/tools/permission_audit.py" "$BASE_DID" "$ROOT/src/Catalogue.mo" > "$WORK/baseline.audit.log" 2>&1 || {
  echo "baseline audit does not pass; fix that before trusting the negative control"
  tail -10 "$WORK/baseline.audit.log"; exit 2
}
echo "baseline: audit passes"

catalogue_case() {
  local name="$1"; local py="$2"
  local f="$WORK/$name.Catalogue.mo"
  cp "$ROOT/src/Catalogue.mo" "$f"
  python3 -c "
import pathlib, sys
p = pathlib.Path(sys.argv[1]); s = p.read_text()
$py
p.write_text(s)
" "$f"
  cases=$((cases + 1))
  if python3 "$ROOT/tools/permission_audit.py" "$BASE_DID" "$f" > "$WORK/$name.log" 2>&1; then
    echo "CASE $name: AUDIT PASSED BUT SHOULD HAVE FAILED"
    bad=$((bad + 1))
  else
    echo "CASE $name: audit failed as required"
    grep -E '^  - ' "$WORK/$name.log" | head -2
  fi
}

catalogue_case command_without_permission \
  "before = s
s = s.replace('      p(\"book.close\", \"book\", #close, #command(\"closeBook\"), false, true),\n', '')
assert s != before, 'the catalogue line to remove was not found'"

catalogue_case money_not_dual \
  "before = s
s = s.replace('#command(\"settleDealLeg\"), true, true', '#command(\"settleDealLeg\"), true, false')
assert s != before"

catalogue_case open_without_reason \
  "before = s
s = s.replace('\"expiry is a fact of the clock; the block is attributed to the contract and the caller chooses nothing\"', '\"\"')
assert s != before"

catalogue_case phantom_method \
  "before = s
s = s.replace('#method(\"reviewOverride\")', '#method(\"reviewOverrideXXX\")')
assert s != before"

catalogue_case duplicate_identifier \
  "before = s
s = s.replace('      p(\"book.create\", \"book\", #create, #command(\"openBook\"), false, true),',
              '      p(\"book.create\", \"book\", #create, #command(\"openBook\"), false, true),\n      p(\"book.create\", \"book\", #close, #command(\"closeBook\"), false, true),')
assert s != before"

UG="$WORK/unguarded"
mkdir -p "$UG"
cp -a "$ROOT/src" "$UG/"
python3 - "$UG/src/Desk.mo" <<'PY'
import pathlib, sys
p = pathlib.Path(sys.argv[1]); s = p.read_text()
marker = "  public query func deskHeight() : async Nat { desk.height };"
assert marker in s
s = s.replace(marker, "  public shared func sneakyWrite(x : Nat) : async Nat { x };\n" + marker, 1)
p.write_text(s)
PY
cases=$((cases + 1))
if "$MOC" $PKGS --idl -o "$UG/Desk.did" "$UG/src/Desk.mo" 2> "$UG/build.log"; then
  if python3 "$ROOT/tools/permission_audit.py" "$UG/Desk.did" "$ROOT/src/Catalogue.mo" > "$UG/audit.log" 2>&1; then
    echo "CASE unguarded_method: AUDIT PASSED BUT SHOULD HAVE FAILED"
    bad=$((bad + 1))
  else
    echo "CASE unguarded_method: audit failed as required"
    grep -E '^  - ' "$UG/audit.log" | head -2
  fi
else
  echo "CASE unguarded_method: the patched actor did not build"
  grep -v M0155 "$UG/build.log" | sed -n '1,5p'
  bad=$((bad + 1))
fi

echo "count: permission audit negative cases = $cases"
echo "count: negative cases that wrongly passed = $bad"
if [ "$bad" -eq 0 ]; then echo "PERMISSION AUDIT NEGATIVE CONTROL GREEN ($WORK)"; fi
[ "$bad" -eq 0 ]
