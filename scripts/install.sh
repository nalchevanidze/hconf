#!/usr/bin/env bash
set -euo pipefail

REPO="nalchevanidze/hmm"
APP="hmm"

DEFAULT_VERSION="v1.1.0" # update this on release
VERSION="${1:-$DEFAULT_VERSION}"

curl -fsSL "https://raw.githubusercontent.com/${REPO}/main/actions/install/scripts/install.sh" \
  | bash -s -- --repo "$REPO" --app "$APP" --version "$VERSION"
