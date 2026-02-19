set unstable := true

nix := "nix develop --quiet -c bash -c"

# List available recipes
default:
    @just --list

# -- Haskell server --

# Build Haskell server
build:
    {{ nix }} "cabal build all -O0"

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    just format-client
    nix develop --quiet -c bash -c '
        hs_files=$(find . -name "*.hs" -not -path "./dist-newstyle/*" -not -path "./.direnv/*")
        for i in {1..3}; do fourmolu -i $hs_files; done
        find . -name "*.cabal" -not -path "./dist-newstyle/*" | xargs cabal-fmt -i
        find . -name "*.nix" -not -path "./dist-newstyle/*" | xargs nixfmt
    '

# Run Haskell integration tests
test:
    {{ nix }} "cabal test integration-tests -O0 --test-show-details=direct"

# -- PureScript client --

# Build PureScript client (warnings are errors)
build-client:
    {{ nix }} "cd client && spago build --quiet --strict"

# Bundle PureScript client for browser
bundle-client:
    {{ nix }} "cd client && npm ci --silent && spago bundle --quiet --strict"

# Format PureScript sources
format-client:
    {{ nix }} "cd client && purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'"

# Lint PureScript sources
lint-client:
    {{ nix }} "cd client && purs-tidy check 'src/**/*.purs' 'test/**/*.purs'"

# Run PureScript tests
test-client:
    {{ nix }} "cd client && spago test"

# -- Combined --

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just test
    just lint-client
    just build-client
    {{ nix }} "cd client && npm ci --silent"
    just test-client
    just bundle-client

# Run server serving client bundle
serve:
    #!/usr/bin/env bash
    set -euo pipefail
    just bundle-client
    {{ nix }} "cabal run keri-coop-server -O0 -- --static-dir client/dist"

# Build docker image
docker:
    #!/usr/bin/env bash
    set -euo pipefail
    just bundle-client
    nix build .#docker-image
    docker load < result

# -- Docs --

# Serve docs locally
docs-serve:
    nix develop github:paolino/dev-assets?dir=mkdocs -c mkdocs serve -f docs/mkdocs.yml

# Build docs
docs-build:
    nix develop github:paolino/dev-assets?dir=mkdocs -c mkdocs build -f docs/mkdocs.yml

# -- Deployment --

# Deploy main to server (build locally, push to cachix, pull on server)
deploy:
    #!/usr/bin/env bash
    set -euo pipefail
    nix build github:paolino/keri-coop#docker-image --refresh
    nix-store -qR ./result | cachix push paolino
    ssh plutimus.com 'cd ~/services/keri-coop && \
        TAG=$(nix eval github:paolino/keri-coop#imageTag --raw --refresh) && \
        nix build github:paolino/keri-coop#docker-image --refresh && \
        docker load < result && \
        grep -q "^VERSION=" .env 2>/dev/null && sed -i "s/^VERSION=.*/VERSION=$TAG/" .env || echo "VERSION=$TAG" >> .env && \
        docker compose up -d && \
        echo "Deployed $TAG"'

# Deploy branch for testing (build locally, push to cachix, pull on server)
deploy-dev ref:
    #!/usr/bin/env bash
    set -euo pipefail
    nix build github:paolino/keri-coop/{{ ref }}#docker-image --refresh
    nix-store -qR ./result | cachix push paolino
    ssh plutimus.com 'cd ~/services/keri-coop && \
        TAG=$(nix eval github:paolino/keri-coop/{{ ref }}#imageTag --raw --refresh) && \
        nix build github:paolino/keri-coop/{{ ref }}#docker-image --refresh && \
        docker load < result && \
        grep -q "^VERSION=" .env 2>/dev/null && sed -i "s/^VERSION=.*/VERSION=$TAG/" .env || echo "VERSION=$TAG" >> .env && \
        docker compose up -d && \
        echo "Deployed $TAG from {{ ref }}"'

# Clean build artifacts
clean:
    #!/usr/bin/env bash
    cabal clean
    rm -rf result client/output client/node_modules client/.spago
