#!/usr/bin/env bash
set -euo pipefail

PACKAGE=""
EXECUTABLE=""        # base name, e.g. "hconf"
ZIP_NAME=""          # optional override; if empty, defaults to EXECUTABLE (base)
STACK_BUILD_ARGS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --package) PACKAGE="$2"; shift 2;;
    --executable) EXECUTABLE="$2"; shift 2;;
    --zip-name) ZIP_NAME="$2"; shift 2;;
    --stack-build-args) STACK_BUILD_ARGS="$2"; shift 2;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$PACKAGE" || -z "$EXECUTABLE" ]]; then
  echo "Missing required inputs: --package and --executable" >&2
  exit 1
fi

case "$(uname)" in
  Darwin) OS=mac-os;;
  MINGW64_NT-*|MSYS_NT-*) OS=windows;;
  *) OS=linux;;
esac

# OS-specific binary filename (only this part gets .exe on Windows)
BIN_NAME="$EXECUTABLE"
if [[ "$OS" == "windows" ]]; then
  BIN_NAME="${EXECUTABLE}.exe"
fi

OUT_DIR="out"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

# Build
# shellcheck disable=SC2086
stack build "$PACKAGE" $STACK_BUILD_ARGS

# Copy built executable
cp "$(stack exec -- which "$BIN_NAME")" "./$OUT_DIR/$BIN_NAME"

if [[ "$OS" != "windows" ]]; then
  chmod +x "./$OUT_DIR/$BIN_NAME"
fi

# Zip name: ALWAYS base executable name by default (=> hconf.zip on all OSes)
ZIP_BASE="${ZIP_NAME:-$EXECUTABLE}"
ZIP_FILE="${ZIP_BASE}.zip"

pushd "$OUT_DIR" >/dev/null
7z a "../$ZIP_FILE" .
popd >/dev/null

rm -rf "$OUT_DIR"

echo "Produced: $ZIP_FILE"

# Produces: <os>-<arch>
# Examples: linux-x64, macos-arm64, windows-x64

# OS tag (prefer GitHub RUNNER_OS when available)
case "${RUNNER_OS:-}" in
  Windows) OS_TAG="windows" ;;
  macOS)   OS_TAG="macos" ;;
  Linux)   OS_TAG="linux" ;;
  "")
    # Fallback for local runs
    case "$(uname)" in
      Darwin) OS_TAG="macos" ;;
      *)      OS_TAG="linux" ;;
    esac
    ;;
  *)
    OS_TAG="$(echo "${RUNNER_OS}" | tr '[:upper:]' '[:lower:]')" ;;
esac

# Arch tag (prefer GitHub RUNNER_ARCH when available; fallback to common envs)
ARCH_RAW="${RUNNER_ARCH:-${PROCESSOR_ARCHITECTURE:-unknown}}"
ARCH_TAG="$(echo "$ARCH_RAW" | tr '[:upper:]' '[:lower:]')"

# Normalize common values
case "$ARCH_TAG" in
  x86_64|x64|amd64) ARCH_TAG="x64" ;;
  aarch64|arm64)    ARCH_TAG="arm64" ;;
esac

ARTFACT="${ZIP_NAME}${OS_TAG}-${ARCH_TAG}.zip"

# Always print (nice for local / debugging)
echo "$ARTFACT"

# Outputs for composite action
{
  echo "artifact=$ARTFACT"
  echo "zip_path=$ZIP_FILE"
  echo "executable_name=$BIN_NAME"
} >> "$GITHUB_OUTPUT"
