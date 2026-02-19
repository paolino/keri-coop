# keri-coop

Acquisti collettivi basati su KERI per gruppi di solidarietà (GAS).

## Avvio rapido

```bash
nix develop
just serve   # server + client su :3001
```

## Cosa fa

Un'applicazione browser dove i membri di un gruppo d'acquisto possono:

- Registrare e gestire identità (chiavi KERI Ed25519)
- Depositare/prelevare credito
- Aprire, impegnarsi e chiudere acquisti collettivi
- Votare su decisioni di governance

Tutte le operazioni crittografiche avvengono lato client. Il server è un
semplice relay append-only che archivia e inoltra eventi firmati.
