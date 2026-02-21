set unstable := true

# List available recipes
default:
    @just --list

# Build PureScript library
build:
    spago build --quiet --strict

# Run tests
test:
    npm ci --silent && spago test --quiet

# Format sources
format:
    purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'
    find . -name '*.nix' -not -path './.spago/*' | xargs nixfmt

# Lint sources
lint:
    purs-tidy check 'src/**/*.purs' 'test/**/*.purs'

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just lint
    just build
    just test

# Clean build artifacts
clean:
    rm -rf output node_modules .spago
