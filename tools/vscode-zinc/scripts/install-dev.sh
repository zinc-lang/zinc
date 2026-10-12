#!/usr/bin/env bash
# Smoke-install the extension into Cursor (WSL remote). Prefer F5 for daily work.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

VERSION="$(node -p "require('./package.json').version")"
VSIX="$ROOT/zinc-${VERSION}.vsix"
STAGE="${TMPDIR:-/tmp}/zinc-ext-stage-$$"
VSIX_ROOT="${TMPDIR:-/tmp}/zinc-vsix-$$"

cleanup() {
  rm -rf "$STAGE" "$VSIX_ROOT"
}
trap cleanup EXIT

echo "==> compile"
npm run compile

echo "==> stage production tree"
rm -rf "$STAGE"
mkdir -p "$STAGE"
cp package.json package-lock.json README.md CHANGELOG.md language-configuration.json zinc-logo.png "$STAGE/"
cp -r out syntaxes snippets "$STAGE/"
rm -rf "$STAGE/out/test"
(
  cd "$STAGE"
  npm ci --omit=dev
)

echo "==> package $VSIX"
rm -rf "$VSIX_ROOT"
mkdir -p "$VSIX_ROOT/extension"
cp -a "$STAGE"/. "$VSIX_ROOT/extension/"
cat > "$VSIX_ROOT/extension.vsixmanifest" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<PackageManifest Version="2.0.0" xmlns="http://schemas.microsoft.com/developer/vsx-schema/2011">
  <Metadata>
    <Identity Language="en-US" Id="zinc" Version="${VERSION}" Publisher="F001" />
    <DisplayName>Zinc</DisplayName>
    <Description xml:space="preserve">Zinc language support with LSP</Description>
    <Categories>Programming Languages</Categories>
    <Properties>
      <Property Id="Microsoft.VisualStudio.Code.Engine" Value="^1.75.0" />
      <Property Id="Microsoft.VisualStudio.Code.ExtensionKind" Value="workspace" />
    </Properties>
  </Metadata>
  <Installation><InstallationTarget Id="Microsoft.VisualStudio.Code"/></Installation>
  <Dependencies/>
  <Assets>
    <Asset Type="Microsoft.VisualStudio.Code.Manifest" Path="extension/package.json" Addressable="true" />
  </Assets>
</PackageManifest>
EOF
cat > "$VSIX_ROOT/[Content_Types].xml" <<'EOF'
<?xml version="1.0" encoding="utf-8"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension=".json" ContentType="application/json"/>
  <Default Extension=".vsixmanifest" ContentType="text/xml"/>
  <Default Extension=".js" ContentType="application/javascript"/>
  <Default Extension=".md" ContentType="text/markdown"/>
  <Default Extension=".png" ContentType="image/png"/>
</Types>
EOF
(
  cd "$VSIX_ROOT"
  rm -f "$VSIX"
  zip -r "$VSIX" . >/dev/null
)

if ! command -v cursor >/dev/null 2>&1; then
  echo "cursor CLI not found; VSIX built at $VSIX"
  exit 0
fi

echo "==> install into Cursor"
cursor --uninstall-extension F001.zinc >/dev/null 2>&1 || true
cursor --install-extension "$VSIX" --force
cursor --list-extensions --show-versions | grep -i zinc || true
echo "Done. Reload Window in Cursor, then open a .zn file."
echo "For F5 development, uninstall first: cursor --uninstall-extension F001.zinc"
