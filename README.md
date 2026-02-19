# keri-coop

KERI-based collective purchasing (GAS) application.

## Development

```bash
nix develop
just build    # Haskell server
just serve    # server + client on :3001
```

## Architecture

- **Server (Haskell):** Dumb append-only event log relay (Warp + SQLite)
- **Client (PureScript):** All crypto client-side, KERI identity, domain logic
