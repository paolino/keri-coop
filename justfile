set unstable := true

# List available recipes
default:
    @just --list

# -- Haskell server --

# Build Haskell server
build:
    cabal build all -O0

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    just format-client
    hs_files=$(find . -name '*.hs' -not -path './dist-newstyle/*' -not -path './.direnv/*')
    for i in {1..3}; do
        fourmolu -i $hs_files
    done
    find . -name '*.cabal' -not -path './dist-newstyle/*' | xargs cabal-fmt -i
    find . -name '*.nix' -not -path './dist-newstyle/*' | xargs nixfmt

# Run Haskell linter
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    just lint-client

# -- PureScript client --

# Build PureScript client (warnings are errors)
build-client:
    cd client && spago build --strict

# Bundle PureScript client for browser
bundle-client:
    cd client && npm ci && spago bundle --strict

# Format PureScript sources
format-client:
    cd client && purs-tidy format-in-place 'src/**/*.purs'

# Lint PureScript sources
lint-client:
    cd client && purs-tidy check 'src/**/*.purs'

# Run PureScript tests
test-client:
    cd client && spago test

# -- Combined --

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just lint-client
    just build-client
    just bundle-client

# Run server serving client bundle
serve:
    #!/usr/bin/env bash
    set -euo pipefail
    just bundle-client
    cabal run keri-coop-server -O0 -- --static-dir client/dist

# Build docker image
docker:
    #!/usr/bin/env bash
    set -euo pipefail
    just bundle-client
    nix build .#docker-image
    docker load < result

# Clean build artifacts
clean:
    #!/usr/bin/env bash
    cabal clean
    rm -rf result client/output client/node_modules client/.spago
