# keri-purs

PureScript KERI library mirroring [keri-hs](https://github.com/paolino/keri-hs).

Implements the [KERI](https://weboftrust.github.io/ietf-keri/draft-ssmith-keri.html) protocol:
CESR encoding, Ed25519 crypto, Blake3 digests, key event log management,
and key state machine.

## Modules

- `Keri.Cesr` — CESR encoding/decoding (derivation codes, primitives)
- `Keri.Crypto` — Ed25519 signatures, Blake3 digests, SAID computation
- `Keri.Event` — Event types (inception, rotation, interaction, receipt)
- `Keri.Kel` — Key Event Log (append, replay)
- `Keri.KeyState` — Key state machine (pre-rotation, signature verification)

## Development

```bash
nix develop
just --list   # see all recipes
just ci       # lint + build + test
```
