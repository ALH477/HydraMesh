#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# ci-local.sh — run the Wire Certification workflow (.github/workflows/wire-certify.yml)
# job-for-job on this machine instead of GitHub Actions. Host toolchains are used
# where present; the rest (node, lua, sbcl, jdk, kotlin, ghc, swift) are supplied
# hermetically with `nix shell nixpkgs#…`. Prints a PASS/SKIP/FAIL summary and exits
# non-zero iff a job actually FAILED (SKIP — a missing toolchain — does not fail).
#
#   make ci-local            # or: bash .github/ci-local.sh
#   CI_LOCAL_REPORT=1 make ci-local   # also refresh .github/LOCAL_CI_RESULTS.md
set -uo pipefail
cd "$(git rev-parse --show-toplevel)"

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
G="\033[32m"; Y="\033[33m"; Rd="\033[31m"; Z="\033[0m"
declare -a SUMMARY
pass() { SUMMARY+=("PASS  $1"); printf "  ${G}PASS${Z}  %s\n" "$1"; }
skip() { SUMMARY+=("SKIP  $1 — $2"); printf "  ${Y}SKIP${Z}  %s (%s)\n" "$1" "$2"; }
failj(){ SUMMARY+=("FAIL  $1"); printf "  ${Rd}FAIL${Z}  %s\n" "$1"; tail -8 "$WORK/$1.log" | sed 's/^/      | /'; }

# run <name> <command…> : execute in an isolated subshell, capture, classify.
run() { local n="$1"; shift; echo "== certify-$n =="; if ( set -e; eval "$*" ) >"$WORK/$n.log" 2>&1; then pass "certify-$n"; else failj "certify-$n"; fi; }

have() { command -v "$1" >/dev/null 2>&1; }
HAVE_NIX=0; have nix && HAVE_NIX=1
# nrun <tool> <"nixpkgs#pkg …"> <cmd…> : run cmd with `tool` from host, else via nix.
nrun() {
  local tool="$1" pkgs="$2"; shift 2
  if have "$tool"; then bash -c "$*"
  elif [ "$HAVE_NIX" = 1 ]; then nix shell $pkgs -c bash -c "$*"
  else return 200; fi
}

echo "### Local wire-certify run — host toolchains + nix for the rest ###"

# ── host-toolchain jobs ───────────────────────────────────────────────────────
run python '
  python3 python/MCP/verify_laws.py '"$WORK"'/gv.json
  python3 python/MCP/certify_sdk.py --selftest
  if python3 -c "import mcp" 2>/dev/null; then python3 python/MCP/wirelab_mcp.py --selftest; else echo "(skip wirelab_mcp: mcp not installed)"; fi
  python3 python/MCP/superpack.py
  python3 python/MCP/gen_superpack_vectors.py '"$WORK"'/superpack_vectors.json
  diff '"$WORK"'/superpack_vectors.json Documentation/superpack_vectors.json
  diff '"$WORK"'/superpack_vectors.json python/MCP/superpack_vectors.json
  diff '"$WORK"'/superpack_vectors.gen.h codec/superpack_vectors.gen.h
  python3 python/MCP/modulationlab_core.py
  python3 python/MCP/gen_modulation_vectors.py '"$WORK"'/modulation_vectors.json
  diff '"$WORK"'/modulation_vectors.json Documentation/modulation_vectors.json
  diff '"$WORK"'/modulation_vectors.json python/MCP/modulation_vectors.json
  diff '"$WORK"'/modulation_vectors.gen.h codec/modulation_vectors.gen.h
  python3 python/MCP/meshlab_core.py
  python3 python/MCP/gen_mesh_vectors.py '"$WORK"'/mesh_vectors.json
  diff '"$WORK"'/mesh_vectors.json Documentation/mesh_vectors.json
  diff '"$WORK"'/mesh_vectors.json python/MCP/mesh_vectors.json
  diff '"$WORK"'/mesh_vectors.gen.h codec/mesh_vectors.gen.h'

run rust '( cd codec && cargo test --tests )'

run go '( cd go && go vet ./... && go test ./... && go test -race ./node/ )'

run c '
  gcc -std=c11 -Wall -Wextra -I codec -o '"$WORK"'/wc C_SDK/tests/test_wire_certify.c -lm && '"$WORK"'/wc
  gcc -std=c11 -Wall -Wextra -I codec -o '"$WORK"'/spc C_SDK/tests/test_superpack_certify.c -lm && '"$WORK"'/spc
  gcc -std=c11 -Wall -Wextra -I codec -o '"$WORK"'/mc C_SDK/tests/test_modulation_certify.c && '"$WORK"'/mc
  gcc -std=c11 -Wall -Wextra -I codec -o '"$WORK"'/meshc C_SDK/tests/test_mesh_certify.c && '"$WORK"'/meshc
  gcc -std=c11 -I codec -o '"$WORK"'/ac C_SDK/tests/test_audio_certify.c -lm && '"$WORK"'/ac
  gcc -std=c11 -I codec -o '"$WORK"'/gc C_SDK/tests/test_game_certify.c -lm && '"$WORK"'/gc
  gcc -std=c11 -I codec -o '"$WORK"'/tc C_SDK/tests/test_text_certify.c -lm && '"$WORK"'/tc
  cmake -S C_SDK -B '"$WORK"'/cbuild -DDCF_BUILD_NODE=ON -DDCF_BUILD_TESTS=ON -DDCF_BUILD_EXAMPLES=OFF
  cmake --build '"$WORK"'/cbuild --target dcfnode test_proto_certify test_modulation_certify test_mesh_certify test_modem_loopback
  ctest --test-dir '"$WORK"'/cbuild -R "proto_certify|modulation_certify|mesh_certify|modem_loopback" --output-on-failure'

run cpp '
  g++ -std=c++17 -Wall -Wextra -I cpp/include -o '"$WORK"'/cppc cpp/tests/certify.cpp && '"$WORK"'/cppc Documentation/golden_vectors.json
  g++ -std=c++17 -Wall -Wextra -I cpp/include -o '"$WORK"'/cpps cpp/tests/certify_superpack.cpp && '"$WORK"'/cpps'

run perl '( cd perl && perl Makefile.PL >/dev/null && make >/dev/null 2>&1 && make test )'

for adapter in audio game text; do
  run "$adapter" '
    python3 python/MCP/gen_'"$adapter"'_vectors.py '"$WORK"'/'"$adapter"'_vectors.json
    diff '"$WORK"'/'"$adapter"'_vectors.json Documentation/'"$adapter"'_vectors.json
    diff '"$WORK"'/'"$adapter"'_vectors.gen.h codec/'"$adapter"'_vectors.gen.h
    gcc -std=c11 -I codec -o '"$WORK"'/'"$adapter"'c C_SDK/tests/test_'"$adapter"'_certify.c -lm && '"$WORK"'/'"$adapter"'c'
done

# ── nix-supplied toolchain jobs ───────────────────────────────────────────────
nixjob() { # nixjob <name> <tool> <"nixpkgs#pkg …"> <cmd…>
  local n="$1" tool="$2" pkgs="$3"; shift 3
  echo "== certify-$n =="
  if ! have "$tool" && [ "$HAVE_NIX" != 1 ]; then skip "certify-$n" "no $tool and no nix"; return; fi
  if ( set -e; nrun "$tool" "$pkgs" "$*" ) >"$WORK/$n.log" 2>&1; then pass "certify-$n"; else failj "certify-$n"; fi
}

nixjob node node "nixpkgs#nodejs" 'node JS/nodejs/test/certify.js && node JS/nodejs/test/certify_superpack.js'
nixjob lua lua "nixpkgs#lua5_4" 'lua lua/selftest.lua && lua lua/dcf_jam.lua --passphrase ci-jam --codec pcm --blocks 20 && lua lua/dcf_superpack.lua'
nixjob lisp sbcl "nixpkgs#sbcl" 'sbcl --non-interactive --load lisp/src/wire.lisp'
nixjob java javac "nixpkgs#jdk" 'javac -d '"$WORK"'/jout java/com/demod/dcf/Frame.java java/com/demod/dcf/Certify.java java/com/demod/dcf/SuperPack.java java/com/demod/dcf/SuperPackCertify.java && java -cp '"$WORK"'/jout com.demod.dcf.Certify Documentation/golden_vectors.json && java -cp '"$WORK"'/jout com.demod.dcf.SuperPackCertify Documentation/superpack_vectors.json'
nixjob kotlin kotlinc "nixpkgs#kotlin nixpkgs#jdk" 'kotlinc kotlin/src/main/kotlin/dcf/Frame.kt kotlin/src/main/kotlin/dcf/SuperPack.kt kotlin/src/main/kotlin/dcf/Certify.kt -include-runtime -d '"$WORK"'/kcert.jar && java -cp '"$WORK"'/kcert.jar dcf.CertifyKt Documentation/golden_vectors.json'

# Haskell — compile with a pinned package set, env-files disabled.
echo "== certify-haskell =="
if [ "$HAVE_NIX" = 1 ]; then
  if ( set -e; nix shell --impure --expr 'with import <nixpkgs> {}; [ (haskellPackages.ghcWithPackages (p: [p.aeson p.bytestring p.text p.scientific p.directory])) ]' \
        -c bash -c 'cd haskell && GHC_ENVIRONMENT=- ghc -isrc -outputdir '"$WORK"'/hsout -o '"$WORK"'/hscert test/Certify.hs && '"$WORK"'/hscert' ) >"$WORK/haskell.log" 2>&1; then
    pass "certify-haskell"; else failj "certify-haskell"; fi
else skip "certify-haskell" "no nix (needs ghc + aeson)"; fi

# Swift — the nix Swift-on-Linux wrapper omits `swift-test`; detect and skip.
echo "== certify-swift =="
if have swift || [ "$HAVE_NIX" = 1 ]; then
  if ( set -e; nrun swift "nixpkgs#swift" 'cd swift && swift test' ) >"$WORK/swift.log" 2>&1; then
    pass "certify-swift"
  elif grep -q "swift-test: not found" "$WORK/swift.log"; then
    skip "certify-swift" "nix swift wrapper lacks swift-test; CI uses swift-actions"
  else failj "certify-swift"; fi
else skip "certify-swift" "no swift toolchain"; fi

# ── summary ───────────────────────────────────────────────────────────────────
echo; echo "================= SUMMARY ================="
printf '%s\n' "${SUMMARY[@]}" | sort
P=$(printf '%s\n' "${SUMMARY[@]}" | grep -c '^PASS')
S=$(printf '%s\n' "${SUMMARY[@]}" | grep -c '^SKIP')
F=$(printf '%s\n' "${SUMMARY[@]}" | grep -c '^FAIL')
echo "==========================================="
echo "PASS: $P   SKIP: $S   FAIL: $F"

if [ "${CI_LOCAL_REPORT:-0}" = 1 ]; then
  {
    echo "# Local CI results — \`wire-certify.yml\` run off-platform"
    echo
    echo "- **Date:** $(date -u +%Y-%m-%d)   **Commit:** \`$(git rev-parse --short HEAD)\`"
    echo "- Generated by \`make ci-local\` (.github/ci-local.sh): host toolchains + nix for node/lua/sbcl/jdk/kotlin/ghc."
    echo "- **Result: $P PASS · $S SKIP · $F FAIL**"
    echo
    printf '%s\n' "${SUMMARY[@]}" | sort | sed -E 's/^(PASS|SKIP|FAIL)  (.*)$/- \1 — \2/'
  } > .github/LOCAL_CI_RESULTS.md
  echo "wrote .github/LOCAL_CI_RESULTS.md"
fi

[ "$F" -eq 0 ]
