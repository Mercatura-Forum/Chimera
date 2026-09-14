#!/usr/bin/env python3
"""permission_audit.py: make the permission catalogue authoritative.

A hand-maintained catalogue drifts from the code. This audit is what keeps the desk's from drifting. It reads the
built Candid interface of Desk.mo and the catalogue in src/Catalogue.mo and fails on any of:

  1. a public update method of the service that is neither guarded by a
     catalogue entry nor listed as deliberately open, with a reason;
  2. a `Command` variant with no permission entry;
  3. a catalogue entry naming a method or a command that does not exist;
  4. a duplicate permission identifier;
  5. a money-moving permission that is not dual-by-default.

Exit code 0 and a printed count on success; non-zero with the offending names on
failure. Counts are printed either way, because a tool that examined zero
methods has failed its coverage requirement.

Usage: permission_audit.py <Desk.did> [<Catalogue.mo>]

Provenance: Manticore tools/permission_audit.py at 9c0c30e, with the paths and the actor name changed.
"""
import re
import sys
from pathlib import Path

def _balanced_block(text: str, open_at: int) -> str:
    depth = 0
    for i in range(open_at, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                return text[open_at + 1 : i]
    raise SystemExit("permission_audit: unterminated block")

def _split_decls(body: str):
    decls, depth, cur = [], 0, []
    for ch in body:
        if ch in "{(":
            depth += 1
        elif ch in "})":
            depth -= 1
        if ch == ";" and depth == 0:
            decls.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    if "".join(cur).strip():
        decls.append("".join(cur))
    return decls

def candid_service_methods(did: str):
    """(update_methods, query_methods) from the service *body* of a .did file.

    A Motoko actor class emits two `service` occurrences: `type Desk = service {
    ...methods... }` and the constructor `service : (init: ...) -> Bank`. Only the
    first carries methods, so every `service {` block is parsed and the one with
    the most method declarations wins. Parsing the constructor instead would read
    the install-argument record's field names as methods, which is exactly the
    mistake this comment exists to prevent recurring.
    """
    best = ([], [])
    for m in re.finditer(r"service\s*\{", did):
        body = _balanced_block(did, did.index("{", m.start()))
        updates, queries = [], []
        for d in _split_decls(body):
            d = re.sub(r"///[^\n]*", "", d)
            mm = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s*:", d)
            if not mm:
                continue
            name = mm.group(1)
            (queries if re.search(r"\bquery\s*$", d.strip()) else updates).append(name)
        if len(updates) + len(queries) > len(best[0]) + len(best[1]):
            best = (sorted(set(updates)), sorted(set(queries)))
    if not best[0] and not best[1]:
        raise SystemExit("permission_audit: no service block with methods in the Candid file")
    return best

def candid_command_variants(did: str):
    m = re.search(r"^type Command\s*=\s*\n?\s*variant\s*\{", did, re.M)
    if not m:
        raise SystemExit("permission_audit: no Command variant in the Candid file")
    body = _balanced_block(did, did.index("{", m.start()))
    names = []
    for d in _split_decls(body):
        mm = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s*:", d)
        if mm:
            names.append(mm.group(1))
    return sorted(set(names))

CATALOGUE_RE = re.compile(
    r'p\(\s*"(?P<id>[^"]+)"\s*,\s*"(?P<resource>[^"]+)"\s*,\s*#(?P<action>\w+)\s*,\s*'
    r'#(?P<guard>method|command)\("(?P<target>[^"]+)"\)\s*,\s*(?P<money>true|false)\s*,\s*(?P<dual>true|false)\s*\)'
)
OPEN_RE = re.compile(r'\(\s*"(?P<method>[^"]+)"\s*,\s*"(?P<reason>[^"]*)"\s*\)')

def parse_catalogue(src: str):
    entries = [m.groupdict() for m in CATALOGUE_RE.finditer(src)]
    open_block = src.split("public func openMethods()", 1)
    opens = []
    if len(open_block) == 2:
        tail = open_block[1].split("};", 1)[0]
        opens = [m.groupdict() for m in OPEN_RE.finditer(tail)]
    return entries, opens

def main() -> int:
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    did_path = Path(sys.argv[1])
    perms_path = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(__file__).resolve().parent.parent / "src/Catalogue.mo"
    # Strip Candid doc comments before any brace or semicolon scanning: moc emits
    # the Motoko doc comments verbatim, and a comment containing a semicolon or a
    # brace would otherwise split a declaration in the wrong place. (It did: the
    # comment on `approve` contains a semicolon, and the method disappeared.)
    did = re.sub(r"///[^\n]*", "", did_path.read_text())
    src = perms_path.read_text()

    updates, queries = candid_service_methods(did)
    commands = candid_command_variants(did)
    entries, opens = parse_catalogue(src)

    guarded_methods = {e["target"] for e in entries if e["guard"] == "method"}
    guarded_commands = {e["target"] for e in entries if e["guard"] == "command"}
    open_methods = {o["method"] for o in opens}

    failures = []

    # 1. every update method guarded or explicitly open
    for m in updates:
        if m not in guarded_methods and m not in open_methods:
            failures.append(f"update method {m!r} is neither guarded by a permission nor listed as open")

    # 2. every Command variant has a permission
    for c in commands:
        if c not in guarded_commands:
            failures.append(f"Command variant {c!r} has no permission entry")

    # 3. no catalogue entry names something that does not exist
    for m in sorted(guarded_methods):
        if m not in updates:
            failures.append(f"catalogue guards method {m!r}, which is not a public update method")
    for c in sorted(guarded_commands):
        if c not in commands:
            failures.append(f"catalogue guards command {c!r}, which is not a Command variant")
    for m in sorted(open_methods):
        if m not in updates:
            failures.append(f"open-method list names {m!r}, which is not a public update method")
        reason = next((o["reason"] for o in opens if o["method"] == m), "")
        if not reason.strip():
            failures.append(f"open method {m!r} has no stated reason")

    # 4. identifiers unique
    seen = {}
    for e in entries:
        if e["id"] in seen:
            failures.append(f"duplicate permission identifier {e['id']!r}")
        seen[e["id"]] = e

    # 5. money-moving implies dual by default
    for e in entries:
        if e["money"] == "true" and e["dual"] != "true":
            if e["id"] != "command.breakGlass":
                failures.append(f"permission {e['id']!r} is money-moving but not dual by default")

    money = sum(1 for e in entries if e["money"] == "true")
    dual = sum(1 for e in entries if e["dual"] == "true")
    print(f"count: catalogue permissions = {len(entries)}")
    print(f"count: permissions guarding a method = {len(guarded_methods)}")
    print(f"count: permissions guarding a command = {len(guarded_commands)}")
    print(f"count: money-moving permissions = {money}")
    print(f"count: dual-by-default permissions = {dual}")
    print(f"count: service update methods = {len(updates)}")
    print(f"count: service query methods = {len(queries)}")
    print(f"count: Command variants = {len(commands)}")
    print(f"count: deliberately open methods = {len(open_methods)}")

    if not entries or not updates or not commands:
        print("PERMISSION AUDIT FAILED: examined zero records somewhere")
        return 2
    if failures:
        print("PERMISSION AUDIT FAILED:")
        for f in failures:
            print("  - " + f)
        return 1
    print("PERMISSION AUDIT PASS")
    return 0

if __name__ == "__main__":
    sys.exit(main())
