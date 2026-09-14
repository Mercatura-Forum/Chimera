"""Independent verifier for a journal entry.

This module trusts nothing the canister says. Given the raw bytes of a block,
its inclusion proof, and the tip certificate, it establishes — with its own
implementations, sharing no code with the canister — that:

  1. the certificate is signed by the network root key (via the delegation),
     checked with the BLS verifier (integration/bls-verify);
  2. the certificate commits to the canister's certified_data;
  3. that certified_data is the root hash of the returned hash tree;
  4. the hash tree contains the journal's MMR root under thebes_journal/mmr_root;
  5. the raw block bytes hash (SHA-256, domain "THEBES-JOURNAL-BLOCK-v1") to
     the block hash embedded at their end, and decode to the claimed fields;
  6. the Merkle Mountain Range proof links that block hash to the MMR root.

References: the certification format (hash tree, domain
separators, certificate CBOR, delegation); Merkle Mountain Ranges (Peter Todd,
opentimestamps), with the peak-bagging order defined in
src/journal/JournalProof.mo.
"""
import hashlib
import os
import subprocess

import cbor2


# ─── hash tree ────────────────────────────────────────────────────────────

def _domain(label, *parts):
    h = hashlib.sha256()
    h.update(bytes([len(label)]))
    h.update(label.encode())
    for p in parts:
        h.update(p)
    return h.digest()


def hash_tree(t):
    tag = t[0]
    if tag == 0:
        return _domain("ic-hashtree-empty")
    if tag == 1:
        return _domain("ic-hashtree-fork", hash_tree(t[1]), hash_tree(t[2]))
    if tag == 2:
        return _domain("ic-hashtree-labeled", bytes(t[1]), hash_tree(t[2]))
    if tag == 3:
        return _domain("ic-hashtree-leaf", bytes(t[1]))
    if tag == 4:
        return bytes(t[1])
    raise ValueError(f"bad hash tree tag {tag}")


def _flatten(t):
    if t[0] == 0:
        return []
    if t[0] == 1:
        return _flatten(t[1]) + _flatten(t[2])
    return [t]


def lookup(t, path):
    """Return the leaf value at `path` (list of bytes), or None if absent/unknown."""
    if not path:
        return bytes(t[1]) if t[0] == 3 else None
    for node in _flatten(t):
        if node[0] == 2 and bytes(node[1]) == path[0]:
            return lookup(node[2], path[1:])
    return None


# ─── certificate ─────────────────────────────────────────────────────────────

BLS_BIN = os.environ.get("THEBES_BLS_VERIFY", os.path.join(os.path.dirname(__file__), "bls-verify", "target", "debug", "thebes-bls-verify"))
DER_PREFIX = bytes.fromhex("308182301d060d2b0601040182dc7c0503010201060c2b0601040182dc7c05030201036100")


def _bls_ok(sig, msg, pk):
    r = subprocess.run([BLS_BIN, sig.hex(), msg.hex(), pk.hex()], capture_output=True, text=True)
    return r.returncode == 0 and r.stdout.strip() == "BLS_OK"


def _extract_der_key(der):
    assert der[: len(DER_PREFIX)] == DER_PREFIX, "unexpected DER prefix"
    return der[len(DER_PREFIX):]


def _signed_message(tree):
    """The bytes the subnet signs: domain_sep("ic-state-root") || root hash of the
    tree, where domain_sep(s) = byte(len(s)) || s (the certificate format)."""
    return bytes([len(b"ic-state-root")]) + b"ic-state-root" + hash_tree(tree)


def _canister_ranges(dtree, subnet_id):
    """Ranges the delegation grants the subnet, from either layout the spec
    allows: subnet/<id>/canister_ranges (single CBOR leaf) or the newer
    canister_ranges/<id>/<first canister of range> (one CBOR leaf per range)."""
    leaf = lookup(dtree, [b"subnet", subnet_id, b"canister_ranges"])
    if leaf is not None:
        return [(bytes(a), bytes(b)) for a, b in cbor2.loads(leaf)]
    sub = None
    for node in _flatten(dtree):
        if node[0] == 2 and bytes(node[1]) == b"canister_ranges":
            for n2 in _flatten(node[2]):
                if n2[0] == 2 and bytes(n2[1]) == subnet_id:
                    sub = n2[2]
    assert sub is not None, "delegation has no canister ranges"
    out = []
    for n3 in _flatten(sub):
        if n3[0] == 2 and n3[2][0] == 3:
            out.extend((bytes(a), bytes(b)) for a, b in cbor2.loads(bytes(n3[2][1])))
    return out


def verify_certificate(cert_bytes, root_key_der, canister_id_bytes):
    """Verify signature and delegation; return the certificate tree."""
    cert = cbor2.loads(cert_bytes)
    tree = cert["tree"]
    sig = bytes(cert["signature"])
    root_pk = _extract_der_key(bytes(root_key_der))
    signer_pk = root_pk
    if cert.get("delegation") is not None:
        d = cert["delegation"]
        dcert = cbor2.loads(bytes(d["certificate"]))
        assert dcert.get("delegation") is None, "nested delegations are not allowed"
        dsig = bytes(dcert["signature"])
        assert _bls_ok(dsig, _signed_message(dcert["tree"]), root_pk), "delegation certificate signature invalid"
        subnet_id = bytes(d["subnet_id"])
        pk_der = lookup(dcert["tree"], [b"subnet", subnet_id, b"public_key"])
        assert pk_der is not None, "delegation has no subnet public key"
        ranges = _canister_ranges(dcert["tree"], subnet_id)
        assert any(lo <= canister_id_bytes <= hi for lo, hi in ranges), "canister not in delegated subnet ranges"
        signer_pk = _extract_der_key(pk_der)
    assert _bls_ok(sig, _signed_message(tree), signer_pk), "certificate signature invalid"
    return tree


# ─── canonical block decoding (independent of Canonical.mo) ──────────────────

class Reader:
    def __init__(self, data):
        self.d = data
        self.p = 0

    def byte(self):
        b = self.d[self.p]; self.p += 1; return b

    def take(self, n):
        v = self.d[self.p:self.p + n]
        assert len(v) == n, "truncated"
        self.p += n
        return v

    def nat(self):
        n = self.byte()
        if n == 0:
            return 0
        bs = self.take(n)
        assert bs[0] != 0, "non-minimal nat"
        return int.from_bytes(bs, "big")

    def nat64(self):
        return int.from_bytes(self.take(8), "big")

    def len16(self):
        return int.from_bytes(self.take(2), "big")

    def text(self):
        return self.take(self.len16()).decode("utf-8")

    def blob(self):
        return self.take(self.len16())

    def principal(self):
        return self.take(self.byte())

    def opt(self, f):
        t = self.byte()
        if t == 0:
            return None
        assert t == 1
        return f()

    def side(self):
        return ["debit", "credit"][self.byte()]

    def leg(self):
        return {"account": self.text(), "subledger": self.opt(self.blob), "side": self.side(), "currency": self.text(), "amount": self.nat()}

    def posting(self):
        r = {"idempotencyKey": self.blob(), "postingDate": self.nat(), "valueDate": self.nat(), "period": self.text()}
        n = self.len16()
        r["legs"] = [self.leg() for _ in range(n)]
        r["sourceRef"] = {"kind": self.text(), "id": self.text()}
        r["narration"] = self.text()
        rel = self.byte()
        if rel == 0:
            r["relation"] = None
        else:
            assert rel == 1
            r["relation"] = {"original": self.nat(), "kind": ["reversal", "correction"][self.byte()]}
        r["valueDateRequested"] = self.opt(self.nat)
        return r

    def resolution(self):
        return {"postingDate": self.nat(), "valueDate": self.nat(), "period": self.text(), "valueDateRequested": self.opt(self.nat)}

    def calendar_authority(self):
        """Where the journal's today comes from: the substrate's clock, or the rolled business date alone."""
        return ["substrateClock", "businessDate"][self.byte()]

    def calendar(self):
        t = self.byte()
        if t == 0:
            return None
        assert t == 1
        rest = [self.nat() for _ in range(self.len16())]
        hol = [self.nat() for _ in range(self.len16())]
        return {"restDays": rest, "holidays": hol, "policy": ["reject", "previous", "next", "nearest"][self.byte()]}

    def attributes(self):
        usage = ["header", "detail"][self.byte()]
        manual = self.byte()
        present = self.byte()
        parent = None if present == 0 else self.text()
        return {"usage": usage, "manualEntriesAllowed": manual == 1, "parent": parent}

    def limit(self):
        kind = self.byte()
        if kind == 0:
            return "none"
        if kind == 1:
            return {"debitsNotExceedCreditsPlus": self.nat()}
        if kind == 2:
            return {"creditsNotExceedDebitsPlus": self.nat()}
        raise ValueError("bad limit tag")

    def checkpoint_part(self):
        """A checkpoint part, the journal's derived state in the shape `Canonical.checkpointPart` writes."""
        tag = self.byte()
        if tag == 0x01:
            cfg = {"admin": self.principal(), "activationHeight": self.nat64(), "businessDate": self.opt(self.nat), "calendar": self.calendar()}
            cfg["leadsheet"] = [{"lo": self.nat(), "hi": self.nat(), "leadsheet": self.text(), "name": self.text(), "category": self.text(), "cycle": self.text()} for _ in range(self.nat())]
            cfg["currencies"] = [(self.text(), self.byte()) for _ in range(self.nat())]
            cfg["posters"] = [self.principal() for _ in range(self.nat())]
            cfg["posterScopes"] = [(self.principal(), [self.text() for _ in range(self.nat())]) for _ in range(self.nat())]
            cfg["balanceLimits"] = [{"account": self.text(), "subledger": self.blob(), "currency": self.text(), "limit": self.limit()} for _ in range(self.nat())]
            cfg["accountAttributes"] = [(self.text(), self.attributes()) for _ in range(self.nat())]
            cfg["accountOrdinals"] = [(self.text(), self.nat()) for _ in range(self.nat())]
            cfg["currencyOrdinals"] = [(self.text(), self.nat()) for _ in range(self.nat())]
            cfg["periodOrdinals"] = [(self.text(), self.nat()) for _ in range(self.nat())]
            cfg["postedCount"], cfg["voidedCount"], cfg["datedRolledUpThrough"] = self.nat(), self.nat(), self.nat()
            cfg["calendarAuthority"], cfg["maxRollDays"] = self.calendar_authority(), self.nat()
            return {"config": cfg}
        if tag == 0x02:
            return {"accounts": [{"code": self.text(), "name": self.text(), "normalSide": self.side(),
                                  "category": ["asset", "liability", "equity", "income", "expense"][self.byte()],
                                  "constraint": ["none", "debitsNotExceedCredits", "creditsNotExceedDebits"][self.byte()],
                                  "active": self.byte() == 1, "openedAtBlock": self.nat(), "closedAtBlock": self.opt(self.nat)} for _ in range(self.nat())]}
        if tag == 0x03:
            return {"periods": [{"id": self.text(), "start": self.nat(), "end": self.nat(), "open": self.byte() == 1,
                                 "openedAtBlock": self.nat(), "closedAtBlock": self.opt(self.nat), "postings": self.nat(), "pendings": self.nat()} for _ in range(self.nat())]}
        if tag == 0x04:
            return {"balances": [{"account": self.text(), "subledger": self.blob(), "currency": self.text(), "drPosted": self.nat(),
                                  "crPosted": self.nat(), "drPending": self.nat(), "crPending": self.nat()} for _ in range(self.nat())]}
        if tag == 0x05:
            return {"periodBalances": [{"period": self.text(), "account": self.text(), "currency": self.text(), "debits": self.nat(), "credits": self.nat()} for _ in range(self.nat())]}
        if tag == 0x06:
            value_dated = self.byte() == 1
            return {"dated": {"valueDated": value_dated, "rows": [{"account": self.text(), "currency": self.text(), "subledger": self.blob(), "day": self.nat(), "debits": self.nat(), "credits": self.nat()} for _ in range(self.nat())]}}
        if tag == 0x07:
            return {"pendings": {"open": [self.nat() for _ in range(self.nat())], "byAccount": [(self.text(), self.nat()) for _ in range(self.nat())]}}
        raise ValueError(f"unknown checkpoint part tag {tag:#x}")

    def event(self):
        tag = self.byte()
        if tag == 0x10:
            return {"posted": self.posting()}
        if tag == 0x11:
            return {"pending": {"record": self.posting(), "expiresAt": self.opt(self.nat64)}}
        if tag == 0x12:
            return {"post": {"pendingIndex": self.nat(), "resolution": self.resolution()}}
        if tag == 0x13:
            return {"void": {"pendingIndex": self.nat(), "reason": ["requested", "expired"][self.byte()]}}
        if tag == 0x20:
            return {"currencyRegistered": {"code": self.text(), "minorUnits": self.byte()}}
        if tag == 0x21:
            return {"accountOpened": {"code": self.text(), "name": self.text(), "normalSide": self.side(),
                                      "category": ["asset", "liability", "equity", "income", "expense"][self.byte()],
                                      "constraint": ["none", "debitsNotExceedCredits", "creditsNotExceedDebits"][self.byte()]}}
        if tag == 0x22:
            return {"accountClosed": {"code": self.text()}}
        if tag == 0x23:
            return {"periodOpened": {"id": self.text(), "start": self.nat(), "end": self.nat()}}
        if tag == 0x24:
            return {"periodClosed": {"id": self.text()}}
        if tag == 0x25:
            return {"activationHeight": {"height": self.nat64()}}
        if tag == 0x26:
            n = self.len16()
            return {"leadsheetSchema": {"ranges": [{"lo": self.nat(), "hi": self.nat(), "leadsheet": self.text(),
                                                     "name": self.text(), "category": self.text(), "cycle": self.text()} for _ in range(n)]}}
        if tag == 0x27:
            return {"posterAdded": {"poster": self.principal()}}
        if tag == 0x28:
            return {"posterRemoved": {"poster": self.principal()}}
        if tag == 0x29:
            return {"adminTransferred": {"admin": self.principal()}}
        if tag == 0x2A:
            return {"businessDateRolled": {"day": self.nat()}}
        if tag == 0x2B:
            return {"calendarSet": {"calendar": self.calendar()}}
        if tag == 0x2F:
            return {"calendarAuthoritySet": {"authority": self.calendar_authority(), "maxRollDays": self.nat(), "businessDate": self.opt(self.nat)}}
        if tag == 0x2C:
            poster = self.principal()
            present = self.byte()
            if present == 0:
                accounts = None
            elif present == 1:
                accounts = [self.text() for _ in range(self.len16())]
            else:
                raise ValueError("posterScopeSet: bad option tag")
            return {"posterScopeSet": {"poster": poster, "accounts": accounts}}
        if tag == 0x2D:
            account = self.text()
            subledger = self.opt(self.blob)
            currency = self.text()
            kind = self.byte()
            if kind == 0:
                limit = "none"
            elif kind == 1:
                limit = {"debitsNotExceedCreditsPlus": self.nat()}
            elif kind == 2:
                limit = {"creditsNotExceedDebitsPlus": self.nat()}
            else:
                raise ValueError("balanceLimitSet: bad limit tag")
            return {"balanceLimitSet": {"account": account, "subledger": subledger,
                                        "currency": currency, "limit": limit}}
        if tag == 0x2E:
            code = self.text()
            u = self.byte()
            if u == 0:
                usage = "header"
            elif u == 1:
                usage = "detail"
            else:
                raise ValueError("accountAttributesSet: bad usage tag")
            manual = self.byte()
            if manual not in (0, 1):
                raise ValueError("accountAttributesSet: bad manual-entry flag")
            present = self.byte()
            if present == 0:
                parent = None
            elif present == 1:
                parent = self.text()
            else:
                raise ValueError("accountAttributesSet: bad parent option tag")
            return {"accountAttributesSet": {"code": code, "attributes": {
                "usage": usage, "manualEntriesAllowed": manual == 1, "parent": parent}}}
        if tag == 0x30:
            through, seq, last = self.nat(), self.nat(), self.byte()
            assert last in (0, 1), "checkpoint: bad last flag"
            return {"checkpoint": {"through": through, "seq": seq, "last": last == 1, "part": self.checkpoint_part()}}
        raise ValueError(f"unknown event tag {tag:#x}")


BLOCK_DOMAIN = b"THEBES-JOURNAL-BLOCK-v1"
SUPPORTED_BLOCK_VERSIONS = (0x03, 0x04)


def block_hash(preimage):
    h = hashlib.sha256()
    h.update(len(BLOCK_DOMAIN).to_bytes(2, "big"))
    h.update(BLOCK_DOMAIN)
    h.update(preimage)
    return h.digest()


def decode_block(raw):
    """Decode raw block bytes; verify the embedded hash; return (fields, hash).
    Any malformed input — truncated, bad tag, bad UTF-8, non-minimal number —
    is a verification failure (AssertionError), never a crash."""
    try:
        return _decode_block(raw)
    except (IndexError, ValueError, KeyError, UnicodeDecodeError) as e:
        raise AssertionError(f"malformed block bytes: {e}") from None


def _decode_block(raw):
    r = Reader(raw)
    # Versions this verifier reads. Version 3 is the S1.3 vocabulary; version 4
    # adds the S2 tags from 0x2C. The version byte is inside the hashed preimage,
    # so a block written by an earlier build keeps its hash and must stay
    # verifiable — which is why this is a set and not an equality.
    version = r.byte()
    assert version in SUPPORTED_BLOCK_VERSIONS, f"block version {version}"
    index = r.nat()
    timestamp = r.nat64()
    caller = r.principal()
    parent = r.opt(r.blob)
    event = r.event()
    preimage_len = r.p
    stored = r.take(32)
    assert r.p == len(raw), "trailing bytes"
    computed = block_hash(raw[:preimage_len])
    assert computed == stored, "embedded hash does not match recomputed hash"
    return {"index": index, "timestamp": timestamp, "caller": caller, "parentHash": parent, "event": event}, stored


# ─── Merkle Mountain Range ───────────────────────────────────────────────────

def mmr_leaf(block_hash_bytes):
    return hashlib.sha256(b"\x00" + block_hash_bytes).digest()


def mmr_node(left, right):
    return hashlib.sha256(b"\x01" + left + right).digest()


def mmr_verify(block_hash_bytes, leaf_index, siblings, peaks, peak_index, root):
    if peak_index >= len(peaks):
        return False
    cur = mmr_leaf(block_hash_bytes)
    idx = leaf_index
    for s in siblings:
        cur = mmr_node(cur, s) if idx % 2 == 0 else mmr_node(s, cur)
        idx //= 2
    if cur != peaks[peak_index]:
        return False
    acc = None
    for p in peaks:               # highest peak first, as generated
        acc = p if acc is None else mmr_node(p, acc)
    return acc == root


def assemble_archived_proof(above, lower_from_archives):
    """An archived block's proof from the parent's upper part (`journalProofAbove`) and the lower
    siblings the archives regenerated, in the shape `mmr_verify` takes."""
    siblings = list(lower_from_archives) + [bytes(x) for x in above["siblings"]]
    return {"siblings": siblings, "peaks": [bytes(p) for p in above["peaks"]], "peakIndex": above["peakIndex"]}


def resolve_subtree(subtree_root_at, leaf_start, height):
    """The root of the aligned subtree (leaf_start, height) across archives: the archive holding
    `leaf_start` answers it whole when it holds every block of it; otherwise the two halves are
    resolved and hashed together. `subtree_root_at(leaf_start, height)` asks the archive that holds
    `leaf_start` and returns bytes or None."""
    got = subtree_root_at(leaf_start, height)
    if got is not None:
        return got
    assert height > 0, f"leaf {leaf_start} is held by no archive"
    half = 2 ** (height - 1)
    return mmr_node(resolve_subtree(subtree_root_at, leaf_start, height - 1),
                    resolve_subtree(subtree_root_at, leaf_start + half, height - 1))


def lower_siblings(above, index, subtree_root_at):
    """The siblings below the parent's kept height: the parent's own where it still has them, the
    archives' otherwise."""
    out = []
    idx = index
    for h, mine in enumerate(above["lower"]):
        sib = idx + 1 if idx % 2 == 0 else idx - 1
        if mine:
            out.append(bytes(mine[0]))
        else:
            out.append(resolve_subtree(subtree_root_at, sib * (2 ** h), h))
        idx //= 2
    return out


# ─── end to end ──────────────────────────────────────────────────────────────

LABEL_ROOT, LABEL_MMR, LABEL_HASH, LABEL_INDEX = b"thebes_journal", b"mmr_root", b"last_block_hash", b"last_block_index"


def verify_entry(raw_block, proof, tip, root_key_der, canister_id_bytes, expect_index=None):
    """Full verification. Returns the decoded block fields. Raises AssertionError on any failure."""
    tree = verify_certificate(tip["certificate"], root_key_der, canister_id_bytes)
    certified = lookup(tree, [b"canister", canister_id_bytes, b"certified_data"])
    assert certified is not None, "certificate has no certified_data for this canister"
    ht = cbor2.loads(tip["hash_tree"])
    assert hash_tree(ht) == certified, "hash tree root != certified_data"
    mmr_root = lookup(ht, [LABEL_ROOT, LABEL_MMR])
    assert mmr_root is not None, "hash tree has no mmr_root"
    fields, h = decode_block(raw_block)
    if expect_index is not None:
        assert fields["index"] == expect_index, "block index mismatch"
    assert mmr_verify(h, fields["index"], proof["siblings"], proof["peaks"], proof["peakIndex"], mmr_root), "MMR proof does not verify"
    return fields
