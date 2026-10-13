#!/usr/bin/env bash
# Publish to VS Code Marketplace and/or Open VSX.
#
# Prerequisites:
#   1. Create a publisher at https://marketplace.visualstudio.com/manage
#      whose ID matches package.json "publisher" (currently F001).
#   2. Create an Azure DevOps PAT with Marketplace: Manage scope.
#   3. export VSCE_PAT=...
#   Optional Open VSX: export OVSX_PAT=... (https://open-vsx.org)
#
# Usage:
#   ./scripts/publish.sh           # marketplace (vsce publish)
#   ./scripts/publish.sh package   # build .vsix only
#   ./scripts/publish.sh openvsx   # Open VSX only (needs OVSX_PAT + existing .vsix or packages first)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# Prefer Node 20+ for @vscode/vsce
if [[ -x "${HOME}/.local/node20/bin/node" ]]; then
  export PATH="${HOME}/.local/node20/bin:${PATH}"
fi

NODE_MAJOR="$(node -p "process.versions.node.split('.')[0]")"
if [[ "$NODE_MAJOR" -lt 20 ]]; then
  echo "Node >= 20 required for vsce (found $(node -v))." >&2
  echo "Install to ~/.local/node20 or upgrade Node." >&2
  exit 1
fi

MODE="${1:-marketplace}"
VERSION="$(node -p "require('./package.json').version")"
PUBLISHER="$(node -p "require('./package.json').publisher")"
NAME="$(node -p "require('./package.json').name")"
VSIX="${ROOT}/${NAME}-${VERSION}.vsix"

REPO_ROOT="$(cd "$ROOT/../.." && pwd)"
if [[ -f "$REPO_ROOT/LICENSE" ]]; then
  cp -f "$REPO_ROOT/LICENSE" "$ROOT/LICENSE"
fi

echo "Extension: ${PUBLISHER}.${NAME}@${VERSION}"

package_vsix() {
  npm run compile
  npx --yes @vscode/vsce@3.2.1 package --no-update-package-json
  echo "Built $VSIX"
}

case "$MODE" in
  package)
    package_vsix
    ;;
  marketplace)
    if [[ -z "${VSCE_PAT:-}" ]]; then
      echo "Set VSCE_PAT to an Azure DevOps PAT with Marketplace (Manage) scope." >&2
      echo "Create publisher + token: https://code.visualstudio.com/api/working-with-extensions/publishing-extension" >&2
      exit 1
    fi
    package_vsix
    npx --yes @vscode/vsce@3.2.1 publish --packagePath "$VSIX" --pat "$VSCE_PAT"
    echo "Published https://marketplace.visualstudio.com/items?itemName=${PUBLISHER}.${NAME}"
    ;;
  openvsx)
    if [[ -z "${OVSX_PAT:-}" ]]; then
      echo "Set OVSX_PAT from https://open-vsx.org/user-settings/tokens" >&2
      exit 1
    fi
    if [[ ! -f "$VSIX" ]]; then
      package_vsix
    fi
    npx --yes ovsx publish "$VSIX" -p "$OVSX_PAT"
    echo "Published to Open VSX: ${PUBLISHER}.${NAME}"
    ;;
  *)
    echo "Usage: $0 [marketplace|package|openvsx]" >&2
    exit 1
    ;;
esac
