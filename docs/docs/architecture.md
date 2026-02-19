# Architecture

```
Browser A ──┐                                    ┌── Browser B
(buyer)     ├── HTTP + WebSocket ── Haskell Server ──┤  (buyer)
Browser C ──┘    (Warp + SQLite)                 └── Browser D
```

## Trust model

The server stores and relays signed events. Clients verify everything.
A compromised server cannot forge events — it can only withhold them.

## Server (Haskell)

Dumb append-only log relay (~200 lines):

- **SQLite** — one table, `(group_id, seq, payload)`, unique on `(group_id, seq)`
- **HTTP API** — create group, append event (409 on seq conflict), fetch events
- **WebSocket** — real-time push to group subscribers
- **Static files** — serves the PureScript client bundle

### API

```
POST   /api/groups                     create group
GET    /api/groups/:id/events?after=N  fetch events after seq N
POST   /api/groups/:id/events          append event (409 on conflict)
WS     /ws/groups/:id                  real-time push
GET    /*                              static files
```

## Client (PureScript)

All interesting logic lives here:

- **KERI** — CESR encoding, Ed25519 signing, Blake3 hashing, key state machine
- **Domain** — events, state replay, authorization rules
- **Protocol** — sign/verify messages, HTTP + WebSocket sync
- **UI** — Halogen components for identity, dashboard, purchases, wallet

Keys stored in localStorage (prototype-grade).
