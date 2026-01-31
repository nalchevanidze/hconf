# HMM (Haskell Monorepo Manager)

CLI for Haskell monorepos that need **multi-GHC Stack builds**, **shared dependency bounds**, and **coordinated versioning** — all from a single `hmm.yaml`.

> **Design:** `hmm.yaml` is the source of truth.  
> HMM keeps your repo in sync by generating/updating config files and validating package metadata against the version/bounds rules you describe.

---

## What HMM does

HMM helps you manage a monorepo with multiple internal packages and a compiler matrix:

- **Select an active build** (GHC + resolver) and write the active Stack config.
- **Sync package metadata** (e.g. `package.yaml`) across all packages from one place.
- **Validate cabal files** against the bounds/version policy declared in `hmm.yaml`.
- **Update dependency bounds** by checking Hackage for newer releases and applying updates safely.

---

## Features

- **Monorepo coordination:** define groups of packages and where they live in the repo
- **Multi-build management:** configure a build matrix (GHC + resolver, allow-newer, extra deps)
- **Dependency bounds policy:** keep dependency constraints consistent across packages
- **Coordinated versioning:** bump version + update bounds (semantic major/minor/patch)
- **Generation + validation:**
  - regenerates `**/**/package.yaml`
  - validates `**/**/<name>.cabal` against bounds/version rules
  - validates `builds[].extra` versions exist on Hackage (no typos / non-existent releases)
- **HLS support:** generate/update `hie.yaml`
- **Formatting:** Ormolu format/check across the repo

---


## Commands

- `hmm use <ghc>`  
  Select a build from `hmm.yaml` and generate the active build config:
  - rewrites `stack.yaml` for that GHC/resolver

- `hmm sync`  
  Sync package metadata to match `hmm.yaml`:
  - generates/updates `hie.yaml`
  - regenerates `**/**/package.yaml`
  - validates `**/**/<name>.cabal` against version/bounds policy
  - validates `builds[].extra` dependency versions exist on Hackage

- `hmm version`  
  Show the current **monorepo/project version** and config (from `hmm.yaml`).
  
  > **Note:** This shows the version of your monorepo/project, **not** the version of the HMM CLI tool itself. To see the CLI version, use `hmm --version`.

- `hmm version <bump>` (`major|minor|patch`)  
  Bump project version and update bounds in `hmm.yaml` (then you typically run `hmm sync`).

- `hmm update-deps`  
  Check dependencies in `hmm.yaml` against Hackage, update bounds if newer versions exist, and **sync packages after updating**.

- `hmm format [--check]`  
  Format/check with Ormolu.

- `hmm publish [NAME]`  
  Publish packages to Hackage. Optionally specify a package group name to publish only that group.

- `hmm --version`  
  Show the HMM CLI tool version.

- `hmm --quiet`  
  Run quietly with minimal output (can be combined with any command).

---

## Installation

### Quick Install (Recommended)

```bash
curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hmm/main/scripts/install.sh | bash
```

Install a specific version:

```bash
curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hmm/main/scripts/install.sh | bash -s -- v0.5.0
```

The installer detects your platform (Linux, macOS, Windows) and architecture (x64, arm64).

### GitHub Actions Integration

#### Option 1: HMM + GHC Setup (All-in-One)

```yaml
- name: Setup GHC and HMM
  uses: nalchevanidze/hmm/actions/ghc@main
  with:
    ghc: 9.6.3 # Optional, defaults to 9.6.3
```

#### Option 2: HMM Only

```yaml
- name: Install HMM
  uses: nalchevanidze/hmm/actions/cli@main
  with:
    version: 0.4.1 # Optional
```

### Alternative Methods

```bash
# Install from source using Stack (slower)
git clone https://github.com/nalchevanidze/hmm.git
cd hmm
stack install

# Or install directly from Hackage
stack install hmm
```

---

## Quick Start

### 1) Create `hmm.yaml`

```yaml
version: 0.4.1 # Current project version
bounds: ">= 0.4.0 && < 0.5.0" # Version bounds for this project

groups:
  - name: myproject # Group name
    packages: # Package dirs relative to 'dir'
      - mylib
      - myapp
    dir: ./ # Base directory for this group

builds: # Build matrix
  - ghc: 9.0.2 # GHC version
    resolver: lts-19.28 # Stack resolver
  - ghc: 9.2.7
    resolver: lts-20.26
  - ghc: 9.4.5
    resolver: lts-21.25

dependencies: # Global dependency bounds applied across packages
  - base   >= 4.7  && < 5.0
  - text   >= 1.2  && < 3.0
  - aeson  >= 1.4  && < 3.0
```

### 2) Select a build and sync packages

```bash
hmm use 9.4.5   # writes stack.yaml for the selected build
hmm sync        # generates hie.yaml, regenerates **/**/package.yaml, validates cabal files, validates build extras
```

### 3) Build/test with Stack normally

Because `stack.yaml` is the active config:

```bash
stack test
```

---

## How `use` and `sync` fit together

- `hmm use <ghc>` is **build selection** (writes `stack.yaml`).
- `hmm sync` is **repo/package synchronization** (regenerates `package.yaml`, validates `.cabal`, generates `hie.yaml`).

“Classic setup” is simply:

```bash
hmm use 9.4.5
hmm sync
```

To switch compilers locally:

```bash
hmm use 9.0.2 && stack test
hmm use 9.2.7 && stack test
```

> CI note: matrix jobs run in separate workspaces, so rewriting `stack.yaml` is safe per job.

---

## Dependency Management (`update-deps`)

`hmm update-deps` reads the `dependencies:` section in `hmm.yaml`, queries Hackage, and updates bounds when newer releases exist.

What it does (high level):

1. **Parse and validate bounds** in `dependencies:` (your `>= … && < …` / `<= …` style constraints).
2. **Check Hackage** for newer released versions of each dependency.
3. **Update `hmm.yaml`** dependency bounds where appropriate (HMM adjusts version numbers while keeping the intent/shape of your constraints).
4. **Sync packages automatically** after updating (equivalent to running `hmm sync`) so `**/**/package.yaml` picks up the new bounds and cabal files are re-validated.

Example `dependencies:` format (dense bounds style):

```yaml
dependencies:
  - Glob                  >=  0.7.0    &&  <   1.0.0
  - aeson                 >=  1.4.4    &&  <   3.0.0
  - base                  >=  4.7.0    &&  <   5.0.0
  - bytestring            >=  0.10.4   &&  <   0.15.0
  - containers            >=  0.4.2.1  &&  <=  0.7.0
  - directory             >=  1.0      &&  <   2.0
  - filepath              >=  1.1.0    &&  <=  1.5.3.0
  - modern-uri            >=  0.1.0.0  &&  <   1.0.0
  - optparse-applicative  >=  0.12.0   &&  <   0.20.0
  - ormolu                >=  0.5.0    &&  <   1.0.0
  - process               >=  1.0.0    &&  <   2.0.0
  - relude                >=  0.3.0    &&  <   2.0.0
  - req                   >=  3.0.0    &&  <   4.0.0
  - text                  >=  1.2.3    &&  <   3.0.0
  - unordered-containers  >=  0.2.8    &&  <   0.3.0
  - yaml                  >=  0.8.32   &&  <   1.0.0
```

Run it:

```bash
hmm update-deps
```

---

## Build extras validation (`builds[].extra`)

If you specify per-build `extra` dependencies, HMM validates that the pinned versions exist on Hackage (avoids typos / non-existent versions):

```yaml
builds:
  - ghc: 8.4.4
    resolver: lts-12.26
    extra:
      aeson: 1.4.4.0
      base-orphans: 0.8.1
      fastsum: 0.1.0.0
      megaparsec: 7.0.5
      modern-uri: 0.3.0.0
      relude: 0.3.0
      retry: 0.8.1.0
      time-compat: 1.9.2.2
      websockets: 0.12.6.0
```

---

## Version Management

Show current project version and info:

```bash
hmm version
```

Bump version:

```bash
hmm version patch   # 1.0.0 -> 1.0.1
hmm version minor   # 1.0.1 -> 1.1.0
hmm version major   # 1.1.0 -> 2.0.0
```

Typical workflow:

```bash
hmm version minor
hmm sync
hmm use 9.4.5
stack test
```

--- 
## Package Groups and Publishing

HMM supports **package groups** to help you organize and manage related packages within your monorepo. A package group is a named collection of packages (usually libraries or executables) that live in a common directory or share a common purpose.

### Why use package groups?

- **Logical organization:** Group related packages (e.g., core libraries, backend, frontend, plugins) for clarity and maintainability.
- **Scoped operations:** Some commands (like `hmm publish [NAME]`) can target a specific group, making it easy to publish or operate on just a subset of your monorepo.
- **Scalability:** As your monorepo grows, groups help keep configuration and workflows manageable.

### Example

```yaml
groups:
  - name: backend
    packages:
      - api-server
      - database-layer
    dir: ./backend/
  - name: frontend
    prefix: web
    packages:
      - client # -> web-client
    dir: ./frontend/
```

In this example, you have two groups: `backend` and `frontend`, each with their own packages and base directory. You can then run commands like:

```bash
hmm publish backend   # Only publish the backend group
```

Groups are defined in your `hmm.yaml` and are referenced by name in commands that support them.

### Publishing Packages

The `hmm publish` command allows you to publish all packages, or just a specific group, to Hackage:

- `hmm publish` — Publish all packages in the monorepo.
- `hmm publish <group>` — Publish only the packages in the specified group.

This makes it easy to coordinate releases for related packages, or to release only a subset of your monorepo as needed.

---

## Code Formatting

```bash
hmm format
hmm format --check
```

---

## Configuration Reference

### Basic Structure

```yaml
version: 0.4.1 # Current project version
bounds: ">= 0.4.0 && < 0.5.0" # Version bounds for this project

groups: # Package groups
  - name: core # Group name
    packages: # Packages in this group
      - mylib-core
      - mylib-utils
    dir: ./libs/ # Base directory for packages

builds: # Build matrix
  - ghc: 9.4.5 # GHC version
    resolver: lts-21.25 # Stack resolver
    allow-newer: true # Allow newer versions (optional)
    extra: # Extra deps for this build (optional)
      some-package: 1.2.3.0

dependencies: # Global dependency bounds
  - base >= 4.7 && < 5.0
  - text >= 1.2 && < 3.0
```

### Advanced Configuration Example

```yaml
version: 1.0.0
bounds: ">= 1.0.0 && < 2.0.0"

groups:
  - name: backend
    packages:
      - api-server
      - database-layer
    dir: ./backend/

  - name: frontend
    packages:
      - web-client
    dir: ./frontend/

builds:
  - ghc: 8.10.7
    resolver: lts-18.28
    extra:
      servant: 0.18.3
      persistent: 2.13.3.5

  - ghc: 9.0.2
    resolver: lts-19.28
    allow-newer: true
    extra:
      servant: 0.19
      persistent: 2.14.0.0

  - ghc: 9.4.5
    resolver: lts-21.25
    extra:
      servant: 0.20
      persistent: 2.14.5.0

dependencies:
  - base                  >= 4.14  && < 5.0
  - text                  >= 1.2   && < 3.0
  - aeson                 >= 1.5   && < 3.0
  - bytestring            >= 0.10  && < 0.13
  - containers            >= 0.6   && < 0.7
  - mtl                   >= 2.2   && < 2.4
  - transformers          >= 0.5   && < 0.7
```

---

## Generated / Updated Files

- `stack.yaml` — written by `hmm use <ghc>` (**overwritten each run**)
- `hie.yaml` — written by `hmm sync` (**overwritten as needed**)
- `**/**/package.yaml` — written by `hmm sync` (**overwritten as needed**)

Validated (not necessarily rewritten):

- `**/**/<name>.cabal` — checked by `hmm sync` to match bounds/version rules from `hmm.yaml`
- `builds[].extra` — checked by `hmm sync` to ensure pinned versions exist on Hackage

---

## Workflow Example (Release)

```bash
# 1) Update dependency bounds from Hackage (also syncs packages)
hmm update-deps

# 2) Test across supported GHCs
for ghc in 9.0.2 9.2.7 9.4.5; do
  echo "Testing with GHC $ghc"
  hmm use "$ghc"
  stack test
done

# 3) Format code
hmm format

# 4) Bump version + sync packages + test
hmm version minor
hmm sync
hmm use 9.4.5
stack test

# 5) Upload
stack upload .
```

---

## CI/CD Integration

### Complete Workflow (GHC matrix)

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        ghc: [9.0.2, 9.2.7, 9.4.5, 9.6.3]

    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup GHC and HMM
        uses: nalchevanidze/hmm/actions/ghc@main
        with:
          ghc: ${{ matrix.ghc }}

      - name: Select build (stack.yaml)
        run: hmm use ${{ matrix.ghc }}

      - name: Sync packages (hie.yaml + package.yaml + validation)
        run: hmm sync

      - name: Build
        run: stack build

      - name: Test
        run: stack test

  format-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install HMM
        uses: nalchevanidze/hmm/actions/cli@main

      - name: Check formatting
        run: hmm format --check
```
