# HMM (Haskell Monorepo Manager)

A powerful command-line tool for managing Haskell monorepos with multi-GHC support.

## What is HMM?

HMM (Haskell Monorepo Manager) is a specialized CLI tool designed to streamline the development workflow for Haskell monorepos that need to support multiple GHC versions and Stack resolvers. It provides a unified configuration system that allows you to manage multiple packages, dependencies, version bounds, and build configurations across different compiler versions from a single `hmm.yaml` file.

**Perfect for:**
- **Multi-package Haskell projects** with shared dependencies
- **Library authors** supporting multiple GHC versions  
- **Teams** maintaining consistent build environments
- **CI/CD pipelines** testing across GHC matrix builds

## Why does this project exist?

Managing Haskell monorepos across multiple GHC versions and Stack resolvers can be incredibly tedious and error-prone. Traditional approaches require:

- Manually maintaining separate stack.yaml files for each GHC version
- Coordinating dependency versions across multiple packages
- Manually updating dependency bounds when packages change
- Repeating build configurations across different environments  
- Time-consuming manual testing across multiple compiler versions
- Managing inter-package dependencies and version compatibility

HMM solves these problems by providing:

✅ **Monorepo Management**: Coordinate multiple packages in a single repository  
✅ **Centralized Configuration**: Define all your build matrices, dependencies, and version constraints in one place  
✅ **Automated Dependency Management**: Automatically check and update dependency bounds across all packages
✅ **Multi-Version Testing**: Easy setup and management of builds across different GHC versions  
✅ **Coordinated Versioning**: Automated version bumping and release management across packages
✅ **Code Formatting**: Integrated Ormolu formatting with checking capabilities  
✅ **HIE Integration**: Automatic generation of hie.yaml files for IDE support

## Key Features

- **Monorepo Coordination**: Manage multiple related Haskell packages in a single repository
- **Multi-Build Management**: Configure builds for different GHC versions and Stack resolvers
- **Inter-Package Dependencies**: Handle complex dependency relationships between packages
- **Dependency Bounds Checking**: Automatically verify and update dependency version bounds
- **Coordinated Versioning**: Semantic versioning with coordinated major/minor/patch bumps
- **Code Formatting**: Built-in Ormolu integration for consistent code style across packages
- **HIE Generation**: Automatic hie.yaml file generation for Haskell Language Server
- **Package Validation**: Verify package configurations across all builds

## Commands

- `hmm setup [version]` - Generate Stack configurations and HIE files for multi-GHC builds
- `hmm version` - Display current project version and configuration details
- `hmm version <bump>` - Bump project version (major|minor|patch) and update configurations  
- `hmm update-deps` - Check and update dependency version bounds
- `hmm format [--check]` - Format Haskell source files using Ormolu
- `hmm --version` - Display HMM tool version

## Installation

### Quick Install (Recommended)

Use the provided install script to download the latest binary release:

```bash
curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hmm/main/scripts/install.sh | bash
```

Install specific version:

```bash
curl -fsSL https://raw.githubusercontent.com/nalchevanidze/hmm/main/scripts/install.sh | bash -s -- v0.5.0
```

The installer automatically detects your platform (Linux, macOS, Windows) and architecture (x64, arm64).

### GitHub Actions Integration

For CI/CD, use the pre-built GitHub Actions:

#### Option 1: HMM + GHC Setup (All-in-One)

```yaml
- name: Setup GHC and HMM
  uses: nalchevanidze/hmm/actions/ghc@main
  with:
    ghc: 9.6.3  # Optional, defaults to 9.6.3
```

#### Option 2: HMM Only

```yaml
- name: Install HMM
  uses: nalchevanidze/hmm/actions/hmm@main
  with:
    version: 0.4.1  # Optional, defaults to 0.4.0
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

## Quick Start

1. **Initialize your project** with a `hmm.yaml` configuration file:

```yaml
version: 0.4.1
bounds: '>= 0.4.0 && < 0.5.0'

groups:
- name: myproject
  packages:
  - mylib
  - myapp
  dir: ./

builds:
- ghc: 9.0.2
  resolver: lts-19.28
- ghc: 9.2.7
  resolver: lts-20.26
- ghc: 9.4.5
  resolver: lts-21.25

dependencies:
- base                  >= 4.7   && < 5.0
- text                  >= 1.2   && < 3.0
- aeson                 >= 1.4   && < 3.0
```

2. **Set up your build environments**:

```bash
# Generate stack.yaml files for all GHC versions and create HIE configuration
hmm setup
```

This will create:
- `stack-ghc-9.0.2.yaml`
- `stack-ghc-9.2.7.yaml` 
- `stack-ghc-9.4.5.yaml`
- `hie.yaml` (for Haskell Language Server)

3. **Build and test across all versions**:

```bash
# Test with GHC 9.0.2
stack --stack-yaml stack-ghc-9.0.2.yaml test

# Test with GHC 9.2.7
stack --stack-yaml stack-ghc-9.2.7.yaml test

# Test all versions
for f in stack-ghc-*.yaml; do stack --stack-yaml $f test; done
```

## Configuration Reference

### Basic Structure

```yaml
version: 0.4.1                    # Current project version
bounds: '>= 0.4.0 && < 0.5.0'    # Version bounds for this project

groups:                           # Package groups
- name: core                      # Group name
  packages:                       # List of packages in this group
  - mylib-core
  - mylib-utils
  dir: ./libs/                   # Base directory for packages

builds:                          # Build matrix
- ghc: 9.4.5                    # GHC version
  resolver: lts-21.25           # Stack resolver
  allow-newer: true             # Allow newer versions (optional)
  extra:                        # Extra dependencies for this build
    some-package: 1.2.3.0

dependencies:                    # Global dependencies
- base >= 4.7 && < 5.0
- text >= 1.2 && < 3.0
```

### Advanced Configuration Example

```yaml
version: 1.0.0
bounds: '>= 1.0.0 && < 2.0.0'

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
- bytestring           >= 0.10  && < 0.13
- containers           >= 0.6   && < 0.7
- mtl                  >= 2.2   && < 2.4
- transformers         >= 0.5   && < 0.7
```

## Detailed Command Usage

### Setup Command

```bash
# Setup with latest resolver versions
hmm setup

# Setup with specific version tag
hmm setup 1.0.0

# What this does:
# 1. Generates stack-ghc-X.Y.Z.yaml files for each build
# 2. Creates hie.yaml for IDE support  
# 3. Validates package configurations
# 4. Checks dependency consistency
```

### Version Management

```bash
# Show current project version and info
hmm version

# Bump patch version (1.0.0 -> 1.0.1)
hmm version patch

# Bump minor version (1.0.1 -> 1.1.0)  
hmm version minor

# Bump major version (1.1.0 -> 2.0.0)
hmm version major

# Show HMM tool version
hmm --version
```

After running `version <bump>`, HMM will:
- Update the version in `hmm.yaml`
- Update version bounds accordingly
- Regenerate all stack configuration files

### Dependency Management

```bash
# Check and update dependency bounds
hmm update-deps

# Example output:
# Checking bounds for base...        ✓ OK
# Checking bounds for text...        ⚠ Updating 1.2 -> 2.0
# Checking bounds for aeson...       ✓ OK
# Updated 1 dependency bounds
```

### Code Formatting

```bash
# Format all Haskell files
hmm format

# Check formatting without making changes
hmm format --check

# Example output:
# Formatting src/Main.hs...          ✓ OK
# Formatting src/Lib.hs...           ✗ Needs formatting
# Formatted 1 file(s)
```

## Generated Files

### Stack Configuration Example

When you run `hmm setup`, it generates stack.yaml files like this:

```yaml
# stack-ghc-9.4.5.yaml
resolver: lts-21.25

packages:
- mylib-core
- mylib-utils

extra-deps:
- some-package-1.2.3.0

allow-newer: true
```

### HIE Configuration

HMM also generates a `hie.yaml` file for Haskell Language Server:

```yaml
cradle:
  stack:
    - path: "./mylib-core"
      component: "mylib-core:lib"
    - path: "./mylib-utils" 
      component: "mylib-utils:lib"
```

## Workflow Examples

### Release Workflow

```bash
# 1. Update dependencies
hmm update-deps

# 2. Test across all GHC versions
for f in stack-ghc-*.yaml; do 
  echo "Testing with $f"
  stack --stack-yaml $f test
done

# 3. Format code
hmm format

# 4. Bump version and regenerate configs
hmm version minor
hmm setup

# 5. Build and upload to Hackage
stack upload .
```

### CI/CD Integration

#### Complete Workflow with HMM Actions

```yaml
# .github/workflows/ci.yml
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
    
    # Use the all-in-one action that sets up both GHC and HMM
    - name: Setup GHC and HMM  
      uses: nalchevanidze/hmm/actions/ghc@main
      with:
        ghc: ${{ matrix.ghc }}
    
    - name: Setup project
      run: hmm setup
      
    - name: Build and Test
      run: |
        stack_file="stack-ghc-${{ matrix.ghc }}.yaml"
        stack --stack-yaml $stack_file build
        stack --stack-yaml $stack_file test

  format-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    
    # Just install HMM for formatting
    - name: Install HMM
      uses: nalchevanidze/hmm/actions/hmm@main
    
    - name: Check formatting
      run: hmm format --check
```

#### Manual Setup (Alternative)

```yaml
# .github/workflows/ci.yml
name: CI
on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        stack-yaml: 
          - stack-ghc-9.0.2.yaml
          - stack-ghc-9.2.7.yaml
          - stack-ghc-9.4.5.yaml
    
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Haskell
      uses: haskell-actions/setup@v2
      with:
        enable-stack: true
    
    - name: Install HMM
      run: |
        curl -sSL https://raw.githubusercontent.com/nalchevanidze/hmm/main/scripts/install.sh | bash -s -- \
          --repo nalchevanidze/hmm \
          --app hmm \
          --version 0.4.1
    
    - name: Cache dependencies
      uses: actions/cache@v4
      with:
        path: ~/.stack
        key: ${{ runner.os }}-stack-${{ hashFiles('**/stack.yaml.lock') }}
    
    - name: Setup project
      run: hmm setup
    
    - name: Build and Test
      run: |
        stack --stack-yaml ${{ matrix.stack-yaml }} build
        stack --stack-yaml ${{ matrix.stack-yaml }} test
```

## Troubleshooting

### Common Issues

**Q: "Could not find hmm.yaml"**
```bash
# Make sure you're in the project root directory
pwd
ls hmm.yaml
```

**Q: "Resolver not found"**
```bash
# Update to a valid Stack resolver
# Check available LTS versions at: https://www.stackage.org/
```

**Q: "Dependency conflicts"**
```bash
# Run update-deps to fix bounds
hmm update-deps

# Or manually edit hmm.yaml to adjust version constraints
```

## Configuration

HMM uses a `hmm.yaml` file to define your monorepo configuration. The file supports:

- **Package Groups**: Organize related packages together in your monorepo
- **Build Matrices**: Test across multiple GHC versions and resolvers  
- **Dependency Management**: Centralized version bounds and constraints
- **Version Control**: Automated semantic versioning across all packages
- **Custom Configurations**: Per-build extra dependencies and settings

See the complete `hmm.yaml` in this repository for a real-world example.
