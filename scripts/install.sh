#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hconf/main/scripts/install.sh | bash -s -- <version>
# Example:
#   curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hconf/main/scripts/install.sh | bash -s -- 0.3.6

# Version is REQUIRED as first argument
VERSION="${1:-}"
if [[ -z "$VERSION" ]]; then
  echo "ERROR: version is required." >&2
  echo "Usage: curl -fsSL <install.sh-url> | bash -s -- <version>" >&2
  echo "Example: curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hconf/main/scripts/install.sh | bash -s -- 0.3.6" >&2
  exit 2
fi

# Repo/app (override via env if needed)
REPO="${REPO:-nalchevanidze/hconf}"
APP_NAME="${APP_NAME:-hconf}"

# Colors (disable if not a TTY)
if [[ -t 1 ]]; then
  ALERT='\033[0;31m'
  STD='\033[0m'
  INFO='\033[1;36m'
  WARN='\033[1;33m'
  SUCCESS='\033[1;32m'
else
  ALERT='' STD='' INFO='' WARN='' SUCCESS=''
fi

# Portable printf-based output (avoids macOS echo -e quirks)
say() { printf "%b\n" "$*"; }

# OS tag
case "${RUNNER_OS:-}" in
  Windows) OS_TAG="windows" ;;
  macOS)   OS_TAG="macos" ;;
  Linux)   OS_TAG="linux" ;;
  "")
    case "$(uname -s)" in
      Darwin) OS_TAG="macos" ;;
      Linux)  OS_TAG="linux" ;;
      *)      OS_TAG="linux" ;;
    esac
    ;;
  *) OS_TAG="$(echo "${RUNNER_OS}" | tr '[:upper:]' '[:lower:]')" ;;
esac

# Arch tag (prefer Actions envs; otherwise detect locally)
ARCH_RAW="${RUNNER_ARCH:-${PROCESSOR_ARCHITECTURE:-}}"
if [[ -z "${ARCH_RAW}" || "${ARCH_RAW}" == "unknown" ]]; then
  ARCH_RAW="$(uname -m 2>/dev/null || echo unknown)"
fi

ARCH_TAG="$(echo "$ARCH_RAW" | tr '[:upper:]' '[:lower:]')"
case "$ARCH_TAG" in
  x86_64|x64|amd64) ARCH_TAG="x64" ;;
  arm64|aarch64)    ARCH_TAG="arm64" ;;
  *)                ARCH_TAG="unknown" ;;
esac

SUFFIX="${OS_TAG}-${ARCH_TAG}"

# Binary name inside zip
BIN_FILE="$APP_NAME"
if [[ "$OS_TAG" == "windows" ]]; then
  BIN_FILE="${APP_NAME}.exe"
fi

# Choose install dir
if [[ -d "$HOME/bin" ]]; then
  BIN_DIR="$HOME/bin"
elif [[ -d "$HOME/.local/bin" ]]; then
  BIN_DIR="$HOME/.local/bin"
else
  BIN_DIR="$HOME/.local/bin"
  mkdir -p "$BIN_DIR"
fi

# Temp workdir + cleanup
WORKDIR="$(mktemp -d 2>/dev/null || (rm -rf .pkg-local && mkdir -p .pkg-local && echo ".pkg-local"))"
cleanup() { rm -rf "$WORKDIR" 2>/dev/null || true; }
trap cleanup EXIT

ASSET="${APP_NAME}-${SUFFIX}.zip"
url_for_tag() {
  local tag="$1"
  echo "https://github.com/$REPO/releases/download/$tag/$ASSET"
}

download() {
  local url="$1"
  curl -fSLsS "$url" -o "$WORKDIR/$APP_NAME.zip"
}

URL="$(url_for_tag "$VERSION")"

say "\n${INFO}Installing $APP_NAME (tag: $VERSION)${STD}"
say " - detected: OS=$OS_TAG ARCH=$ARCH_TAG (raw: $ARCH_RAW)"
say " - asset: $ASSET"
say " - source: $URL"

# Download (fallback to v-prefixed tag if needed)
if ! download "$URL"; then
  if [[ "$VERSION" != v* ]]; then
    URL="$(url_for_tag "v$VERSION")"
    say "${WARN}First download failed; retrying with tag v$VERSION${STD}"
    say " - source: $URL"
    download "$URL"
  else
    say "${ALERT}Download failed for tag $VERSION${STD}" >&2
    exit 1
  fi
fi

say " - extracting"
cd "$WORKDIR"

if command -v unzip >/dev/null 2>&1; then
  unzip -q "$APP_NAME.zip"
elif command -v 7z >/dev/null 2>&1; then
  7z x "$APP_NAME.zip" -y >/dev/null
else
  say "${ALERT}Neither 'unzip' nor '7z' found. Please install one of them.${STD}" >&2
  exit 1
fi

if [[ ! -f "./$BIN_FILE" ]]; then
  say "${ALERT}Expected binary ./$BIN_FILE not found after extraction.${STD}" >&2
  say "Contents:" >&2
  ls -la >&2 || true
  exit 1
fi

# Ensure executable bit on non-Windows
if [[ "$OS_TAG" != "windows" ]]; then
  chmod 755 "./$BIN_FILE"
fi

say " - copying binary to $BIN_DIR"
if command -v install >/dev/null 2>&1; then
  # install(1) isn't available on all platforms, but is on macOS/Linux
  install -m 755 "./$BIN_FILE" "$BIN_DIR/$BIN_FILE"
else
  cp "./$BIN_FILE" "$BIN_DIR/$BIN_FILE"
  [[ "$OS_TAG" != "windows" ]] && chmod 755 "$BIN_DIR/$BIN_FILE" || true
fi

say ""
if command -v "$APP_NAME" >/dev/null 2>&1; then
  say "${SUCCESS}Installation succeeded: $("$APP_NAME" about)${STD}"
else
  say "${WARN}Installed to ${BIN_DIR}${STD}"
  say "Add it to PATH to run '$APP_NAME'."
fi
say ""
