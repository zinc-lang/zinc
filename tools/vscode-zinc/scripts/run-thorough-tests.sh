#!/usr/bin/env bash
# Full automated verification for vscode-zinc (unit + compiler integration + package).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

if [[ -x "${HOME}/.local/node20/bin/node" ]]; then
  export PATH="${HOME}/.local/node20/bin:${PATH}"
fi

export ZINC_COMPILER="${ZINC_COMPILER:-$ROOT/../../out/stage1/bin/zinc}"

echo "==> npm test"
npm test

echo "==> npm run test:ui"
npm run test:ui

echo "==> lint"
npm run lint

if [[ -x "$ZINC_COMPILER" ]]; then
  echo "==> zinc --check-only examples/hello.zn"
  "$ZINC_COMPILER" --check-only --speak=en "$ROOT/examples/hello.zn"
else
  echo "WARN: zinc not found at $ZINC_COMPILER (integration tests may skip)"
fi

if command -v npx >/dev/null && [[ "$(node -p "process.versions.node.split('.')[0]")" -ge 20 ]]; then
  echo "==> vsce package (validation)"
  cp -f "$ROOT/../../LICENSE" "$ROOT/LICENSE" 2>/dev/null || true
  npx --yes @vscode/vsce@3.2.1 package --no-update-package-json >/tmp/zinc-vsce-pack.log
  tail -5 /tmp/zinc-vsce-pack.log
else
  echo "SKIP: vsce package (need Node >= 20)"
fi

echo "ALL THOROUGH CHECKS PASSED"
