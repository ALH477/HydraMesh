"""Python bindings for StreamDB — the DeMoD embedded reverse-trie key/value store.

Thin ctypes wrapper over the C edition (``lisp/streamdb/streamdb.c``): an
embedded, thread-safe, binary-safe KV store with on-disk persistence (atomic
temp-file + rename on flush; reloads on open). Binary keys and values, durable
on ``flush()`` (a background auto-flush thread + flush-on-close back it up).

The shared library is located via, in order:
  1. $STREAMDB_LIB (explicit path to libstreamdb.so)
  2. a prebuilt libstreamdb.so next to lisp/streamdb/streamdb.c
  3. compiled on demand from lisp/streamdb/streamdb.c with the system C compiler

Example:
    from streamdb import StreamDB
    with StreamDB("mesh.db", flush_ms=2000) as db:   # file-backed; None = memory-only
        db[b"000123@0x00a1@duet"] = b"hello"           # bytes or str keys/values
        db.flush()                                    # durable on disk now
        print(db[b"000123@0x00a1@duet"])              # b"hello"
        for k, v in db.search(b"@duet"):              # SUFFIX search (reverse trie)
            ...

Note the data structure is a *reverse trie*: ``search(x)`` matches every key that
**ends with** ``x`` (a suffix match), not a prefix. Put the attribute you want to
scan by at the END of the key — e.g. ``<seq>@<src>@<channel>`` so ``search("@duet")``
returns a whole channel and ``search("@0x00a1@duet")`` a single peer's traffic.

Note: get() copies the value out of the C heap and frees the C-side copy, so
returned bytes are owned by Python. See DCF_CODE_REVIEW.md (C5/C8/C9) for known
bugs in the underlying C lib — notably deleting the *last* remaining key.
"""
import ctypes
import os
import subprocess
import sys
import threading

_HERE = os.path.dirname(os.path.abspath(__file__))
_SRC = os.path.normpath(os.path.join(_HERE, "..", "..", "lisp", "streamdb", "streamdb.c"))
_DEFAULT_SO = os.path.join(os.path.dirname(_SRC), "libstreamdb.so")

_libc = ctypes.CDLL(None)
_libc.free.argtypes = [ctypes.c_void_p]
_libc.free.restype = None


class _Result(ctypes.Structure):
    pass


_Result._fields_ = [
    ("key", ctypes.POINTER(ctypes.c_ubyte)),
    ("key_len", ctypes.c_size_t),
    ("value", ctypes.c_void_p),
    ("value_size", ctypes.c_size_t),
    ("next", ctypes.POINTER(_Result)),
]


def _build(dest=_DEFAULT_SO):
    """Compile the C edition into a shared library at `dest`."""
    cc = os.environ.get("CC") or ("cc" if _which("cc") else "gcc")
    cmd = [cc, "-std=c11", "-O2", "-fPIC", "-pthread", "-D_GNU_SOURCE",
           "-shared", _SRC, "-o", dest]
    subprocess.run(cmd, check=True)
    return dest


def _which(name):
    return any(os.access(os.path.join(p, name), os.X_OK)
               for p in os.environ.get("PATH", "").split(os.pathsep) if p)


def _load():
    path = os.environ.get("STREAMDB_LIB")
    if not path:
        path = _DEFAULT_SO if os.path.exists(_DEFAULT_SO) else _build()
    lib = ctypes.CDLL(path)
    lib.streamdb_init.argtypes = [ctypes.c_char_p, ctypes.c_int]
    lib.streamdb_init.restype = ctypes.c_void_p
    lib.streamdb_insert.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t,
                                    ctypes.c_char_p, ctypes.c_size_t]
    lib.streamdb_insert.restype = ctypes.c_int
    lib.streamdb_get.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t,
                                 ctypes.POINTER(ctypes.c_size_t)]
    lib.streamdb_get.restype = ctypes.c_void_p
    lib.streamdb_delete.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.streamdb_delete.restype = ctypes.c_int
    lib.streamdb_prefix_search.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.streamdb_prefix_search.restype = ctypes.POINTER(_Result)
    lib.streamdb_free_results.argtypes = [ctypes.POINTER(_Result)]
    lib.streamdb_free_results.restype = None
    lib.streamdb_flush.argtypes = [ctypes.c_void_p]
    lib.streamdb_flush.restype = ctypes.c_int
    lib.streamdb_free.argtypes = [ctypes.c_void_p]
    lib.streamdb_free.restype = None
    return lib


_LIB = None
_LIB_LOCK = threading.Lock()


def _lib():
    global _LIB
    if _LIB is None:
        with _LIB_LOCK:
            if _LIB is None:
                _LIB = _load()
    return _LIB


def _b(x):
    return x.encode("utf-8") if isinstance(x, str) else bytes(x)


class StreamDB:
    """Embedded reverse-trie key/value store. Keys and values are bytes (str is
    UTF-8 encoded). Thread-safe at the C level; durable on flush()."""

    def __init__(self, file_path=None, flush_ms=5000):
        lib = _lib()
        cpath = _b(file_path) if file_path is not None else None
        self._db = lib.streamdb_init(cpath, int(flush_ms))
        if not self._db:
            raise RuntimeError("streamdb_init failed")
        self._lib = lib

    # ── core ops ──────────────────────────────────────────────────────────────
    def insert(self, key, value):
        k, v = _b(key), _b(value)
        if self._lib.streamdb_insert(self._db, k, len(k), v, len(v)) == 0:
            raise RuntimeError("streamdb_insert failed")

    def get(self, key, default=None):
        k = _b(key)
        size = ctypes.c_size_t(0)
        ptr = self._lib.streamdb_get(self._db, k, len(k), ctypes.byref(size))
        if not ptr:
            return default
        try:
            return ctypes.string_at(ptr, size.value)   # copy into Python-owned bytes
        finally:
            _libc.free(ptr)                            # free the C-side copy

    def delete(self, key):
        """Delete a key. Returns True if it removed something. (Known C-lib bug:
        deleting the *last* remaining key returns 0 — see DCF_CODE_REVIEW.md C5.)"""
        k = _b(key)
        return self._lib.streamdb_delete(self._db, k, len(k)) != 0

    def search(self, fragment):
        """Suffix search (reverse trie): return [(key, value), ...] for every key
        that ENDS WITH `fragment`. Not a prefix match — see the module docstring."""
        f = _b(fragment)
        head = self._lib.streamdb_prefix_search(self._db, f, len(f))
        out = []
        node = head
        while node:
            r = node.contents
            key = ctypes.string_at(r.key, r.key_len) if r.key else b""
            val = ctypes.string_at(r.value, r.value_size) if r.value else b""
            out.append((key, val))
            node = r.next
        if head:
            self._lib.streamdb_free_results(head)
        return out

    def flush(self):
        """Persist to disk now. Returns True on success; False for memory-only DBs
        (the C lib's flush returns 0 when there's no file backend)."""
        return self._lib.streamdb_flush(self._db) != 0

    def close(self):
        if getattr(self, "_db", None):
            self._lib.streamdb_free(self._db)   # auto-flushes if dirty
            self._db = None

    # ── sugar ─────────────────────────────────────────────────────────────────
    def __setitem__(self, key, value):
        self.insert(key, value)

    def __getitem__(self, key):
        v = self.get(key, _MISSING)
        if v is _MISSING:
            raise KeyError(key)
        return v

    def __delitem__(self, key):
        if not self.delete(key):
            raise KeyError(key)

    def __contains__(self, key):
        return self.get(key, _MISSING) is not _MISSING

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


_MISSING = object()


def _selftest():
    import tempfile
    # memory-only
    with StreamDB(None) as db:
        db[b"a"] = b"1"
        db["k"] = "hello \U0001f680"
        assert db[b"a"] == b"1"
        assert db["k"] == "hello \U0001f680".encode()
        assert b"a" in db and b"zzz" not in db
        assert db.get(b"missing") is None
    # file-backed: write, flush, reopen, read back
    path = os.path.join(tempfile.mkdtemp(), "sdb.dat")
    with StreamDB(path, flush_ms=0) as db:
        for i in range(5):
            db[f"{i:06d}@0x00a1@duet".encode()] = f"msg {i}".encode()
        db[b"000000@0x00b2@other"] = b"off-channel"   # different suffix
        assert db.flush()
    with StreamDB(path, flush_ms=0) as db:
        assert db[b"000003@0x00a1@duet"] == b"msg 3"
        assert len(db.search(b"@duet")) == 5, "suffix scan of channel duet"
        assert len(db.search(b"@0x00a1@duet")) == 5, "suffix scan of one peer"
        assert len(db.search(b"@other")) == 1, "off-channel isolated by suffix"
    print("streamdb python binding: CERTIFIED "
          "(insert/get/delete/search + flush->reopen, mem + file)")


if __name__ == "__main__":
    _selftest()
