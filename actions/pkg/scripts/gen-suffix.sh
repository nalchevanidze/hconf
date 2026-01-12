#!/usr/bin/env bash
set -euo pipefail

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

SUFFIX="${OS_TAG}-${ARCH_TAG}"

# Always print (nice for local / debugging)
echo "$SUFFIX"

# If running in GitHub Actions, also set step output
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "suffix=$SUFFIX" >> "$GITHUB_OUTPUT"
fi
