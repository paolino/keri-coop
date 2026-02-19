# Architettura

```
Browser A ──┐                                    ┌── Browser B
(compratore)├── HTTP + WebSocket ── Server Haskell ──┤  (compratore)
Browser C ──┘    (Warp + SQLite)                 └── Browser D
```

## Modello di fiducia

Il server archivia e inoltra eventi firmati. I client verificano tutto.
Un server compromesso non può falsificare eventi — può solo trattenerli.

## Server (Haskell)

Relay append-only minimale (~200 righe):

- **SQLite** — una tabella, `(group_id, seq, payload)`, unica su `(group_id, seq)`
- **HTTP API** — crea gruppo, appendi evento (409 su conflitto seq), recupera eventi
- **WebSocket** — push in tempo reale ai sottoscrittori del gruppo
- **File statici** — serve il bundle del client PureScript

### API

```
POST   /api/groups                     crea gruppo
GET    /api/groups/:id/events?after=N  recupera eventi dopo seq N
POST   /api/groups/:id/events          appendi evento (409 su conflitto)
WS     /ws/groups/:id                  push in tempo reale
GET    /*                              file statici
```

## Client (PureScript)

Tutta la logica interessante vive qui:

- **KERI** — codifica CESR, firma Ed25519, hashing Blake3, macchina a stati delle chiavi
- **Dominio** — eventi, replay dello stato, regole di autorizzazione
- **Protocollo** — firma/verifica messaggi, sincronizzazione HTTP + WebSocket
- **UI** — componenti Halogen per identità, dashboard, acquisti, portafoglio

Chiavi conservate in localStorage (livello prototipo).
