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

# Outputs for composite action
{
  echo "zip_path=$ZIP_FILE"
  echo "executable_name=$BIN_NAME"
} >> "$GITHUB_OUTPUT"
