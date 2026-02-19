# keri-coop

KERI-based collective purchasing (GAS) application.

Browser-based solidarity purchasing group where members manage identities,
credit balances, and collective purchases — all signed with KERI Ed25519 keys.
The server is a dumb append-only relay; clients verify everything.

## Features

- **KERI identity** — Ed25519 keypair generation, CESR encoding, key state machine
- **Event sourcing** — all state derived by replaying a signed event log
- **Governance** — admin voting (referente + cassiere roles) with majority quorum
- **Purchases** — open, commit, approve/reject, vote close/fail lifecycle
- **Wallet** — credit balances, deposits, withdrawals
- **Zero trust** — server stores and forwards; clients sign and verify

## Development

```bash
nix develop
just --list   # see all recipes
just ci       # lint + build + bundle
just serve    # server + client on :3001
```

## Architecture

```
Browser ──┐                                    ┌── Browser
          ├── HTTP + WebSocket ── Haskell Server ──┤
Browser ──┘    (Warp + SQLite)                 └── Browser
```

- **Server (Haskell):** ~200 lines. Warp + wai-websockets + sqlite-simple.
  Append-only log per group, WebSocket push, static file serving.
- **Client (PureScript/Halogen):** KERI crypto, domain model, protocol layer, UI.
  Keys in localStorage.

## API

```
POST   /api/groups                     create group
GET    /api/groups/:id/events?after=N  fetch events after seq N
POST   /api/groups/:id/events          append event (409 on seq conflict)
WS     /ws/groups/:id                  real-time push
GET    /*                              static files (client bundle)
```

## Docker

```bash
just docker                    # build image via nix
docker run -p 3001:3001 -v ./data:/data ghcr.io/paolino/keri-coop:dev
```

## Documentation

Full docs at [paolino.github.io/keri-coop](https://paolino.github.io/keri-coop/).
