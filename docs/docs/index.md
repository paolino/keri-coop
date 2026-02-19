# keri-coop

KERI-based collective purchasing for solidarity groups (GAS).

## Quick start

```bash
nix develop
just serve   # server + client on :3001
```

## What it does

A browser application where members of a purchasing group can:

- Register and manage identities (KERI Ed25519 keys)
- Deposit/withdraw credit
- Open, commit to, and close collective purchases
- Vote on governance decisions

All cryptographic operations happen client-side. The server is a dumb
append-only relay that stores and forwards signed events.
