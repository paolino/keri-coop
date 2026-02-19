# Modello di dominio

Semplificato da [reactivegas](https://github.com/paolino/reactivegas).

## Ruoli

- **Membro** — ha un saldo credito, si impegna negli acquisti
- **Referente** — apre/chiude acquisti, approva/rifiuta impegni
- **Cassiere** — gestisce il denaro; unica autorità per firmare depositi e prelievi

Referente e Cassiere sono entrambi ruoli di **amministratore** — votano
insieme sulla governance con uguale peso. Un membro può ricoprire entrambi
i ruoli.

## Bootstrap

Il membro che crea un gruppo diventa automaticamente il primo referente e
cassiere. Tutti i cambiamenti di ruolo successivi passano attraverso il
voto degli amministratori.

## Governance (voto amministratori)

Qualsiasi amministratore (referente o cassiere) può esprimere un voto. Non
esiste un passaggio separato di proposta — il primo voto per un'azione *è*
la proposta. La macchina a stati conta i voti corrispondenti; quando il
conteggio raggiunge la maggioranza (`>= (adminCount + 1) / 2`), l'azione
ha effetto e il conteggio viene azzerato. Voti duplicati dallo stesso
amministratore vengono ignorati.

`adminCount` è calcolato — fa parte del fold sul log degli eventi.

### Eventi di voto governance

| Evento | Effetto al quorum |
|--------|-------------------|
| `VoteRegisterMember nome` | Membro registrato |
| `VoteRemoveMember memberId` | Membro rimosso |
| `VoteElectReferente memberId` | Membro diventa referente |
| `VoteRevokeReferente memberId` | Referente rimosso |
| `VoteElectCassiere memberId` | Membro diventa cassiere |
| `VoteRevokeCassiere memberId` | Cassiere rimosso |

## Ciclo di vita degli acquisti

1. Il cassiere deposita denaro per un membro (il membro ora ha credito)
2. Il referente apre un acquisto
3. I membri impegnano importi (addebitati dal credito immediatamente)
4. Il referente approva/rifiuta i singoli impegni
5. Gli amministratori votano per autorizzare la chiusura (maggioranza)
6. Il referente chiude (totale addebitato dalla cassa del referente) o fallisce (tutto rimborsato)

## Eventi

Ogni evento è firmato dalla chiave KERI Ed25519 dell'autore e aggiunto al
log condiviso del gruppo.

### Economia (firmati dal Cassiere)

| Evento | Descrizione |
|--------|-------------|
| `Deposit memberId centesimi` | Accredita il saldo di un membro |
| `Withdraw memberId centesimi motivo` | Addebita il saldo di un membro |

### Acquisti

| Evento | Descrizione |
|--------|-------------|
| `OpenPurchase nome` | Il referente apre un acquisto |
| `Commit memberId centesimi purchaseId` | Il membro si impegna nell'acquisto |
| `ApproveCommitment memberId purchaseId` | Il referente approva |
| `RejectCommitment memberId purchaseId` | Il referente rifiuta |
| `AdjustCommitment memberId centesimi purchaseId` | Modifica importo |
| `VoteClosePurchase purchaseId` | Voto amministratore per chiudere |
| `VoteFailPurchase purchaseId` | Voto amministratore per fallire |
| `ClosePurchase purchaseId` | Il referente chiude |
| `FailPurchase purchaseId` | Il referente fa fallire |

## Stato (calcolato per fold)

Tutto lo stato è derivato dal replay del log degli eventi:

- **Membri** — insieme dei membri registrati con saldi credito
- **Amministratori** — insieme di referenti + cassieri (unione)
- **Conteggio amministratori** — `length admins`, usato per la soglia di maggioranza
- **Conteggi voti** — voti pendenti per azione, azzerati al quorum
- **Acquisti** — acquisti aperti con impegni e conteggi voti

## Importi

Tutti gli importi in **centesimi** (interi). Nessun virgola mobile.
