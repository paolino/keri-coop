# Integrazione KERI

Come keri-coop mappa il dominio degli acquisti cooperativi sulle primitive KERI.

---

## Identità = KERI AID

Ogni membro è identificato dal proprio **Autonomic Identifier** (AID),
derivato dalla chiave pubblica Ed25519 tramite hashing Blake3 e codifica
CESR. Non c'è username/password — l'AID *è* l'identità.

```mermaid
flowchart LR
    KP["Coppia chiavi Ed25519<br>(generata nel browser)"]
    H["Blake3(pub_key)"]
    AID["Prefisso AID<br>codificato CESR"]
    MID["MemberId<br>= AID"]

    KP --> H --> AID --> MID
```

Il tipo `MemberId` è un newtype su `AID`:

```purescript
newtype AID = AID String        -- prefisso codificato CESR
newtype MemberId = MemberId AID -- alias di dominio
```

---

## Eventi di dominio come ancore di interazione KERI

Ogni azione di dominio (voto, deposito, acquisto, ecc.) è incapsulata in
un **evento di interazione** KERI (`ixn`). L'evento di dominio viene
serializzato canonicamente in JSON e inserito nel campo `anchors` dell'`ixn`.
L'`ixn` viene poi firmato con la chiave privata corrente del membro e
aggiunto al log del gruppo.

```mermaid
flowchart TD
    DE["Evento di Dominio<br>es. VoteRegisterMember 'Alice'"]
    CJ["JSON Canonico<br>{'name':'Alice','t':'VoteRegisterMember'}"]
    IXN["Evento ixn KERI<br>prefix: AID firmatario<br>sn: prossimo seq number<br>p: digest evento precedente<br>a: [JSON canonico]"]
    SIG["Firma Ed25519<br>sull'ixn serializzato"]
    GM["GroupMessage<br>{ keriEvent, domainEvent }"]

    DE --> CJ --> IXN --> SIG --> GM
```

Implementato in `Protocol.Message.mkGroupMessage`:

1. Serializza il `DomainEvent` in JSON canonico (ordine campi deterministico)
2. Crea un `ixn` KERI con il JSON come ancora
3. Calcola il SAID (self-addressing identifier) dell'`ixn`
4. Firma l'`ixn` serializzato con la chiave segreta Ed25519 del membro
5. Impacchetta come `GroupMessage` per il trasporto

---

## Due log di eventi, due scopi

keri-coop mantiene due log concettuali con scopi diversi:

```mermaid
flowchart LR
    subgraph KERI["Livello KERI (per membro)"]
        KEL["Key Event Log<br>icp → ixn → rot → ixn → ..."]
        KS["Stato Chiavi<br>chiavi correnti, soglie,<br>impegni pre-rotazione"]
        KEL --> KS
    end

    subgraph Domain["Livello Dominio (per gruppo)"]
        GL["Log del Gruppo<br>Sequenza di eventi di dominio firmati"]
        GS["Stato del Gruppo<br>membri, saldi,<br>acquisti, voti"]
        GL --> GS
    end

    KEL -.->|"ixn ancorano<br>eventi di dominio"| GL
    KS -.->|"verifica<br>firme"| GL
```

| Log | Ambito | Scopo | Eventi |
|-----|--------|-------|--------|
| **KEL** | Per membro | Gestione chiavi, identità | `icp`, `rot`, `ixn` |
| **Log del Gruppo** | Per gruppo | Stato del dominio | Eventi di dominio firmati ancorati in `ixn` |

Il KEL traccia *chi controlla quali chiavi*. Il log del gruppo traccia
*cosa è successo nella cooperativa*. Sono collegati: ogni voce del log
del gruppo è un `ixn` KERI la cui firma è verificata contro lo stato
chiavi corrente del firmatario.

---

## Flusso di verifica

Quando un `GroupMessage` arriva (via HTTP o WebSocket), il client lo
verifica su due livelli prima di applicarlo allo stato del dominio:

```mermaid
sequenceDiagram
    participant S as Server
    participant C as Client
    participant KV as Verificatore KERI
    participant DV as Validatore Dominio

    S->>C: GroupMessage { keriEvent, domainEvent }

    C->>KV: Verifica firma ixn
    Note over KV: 1. Cerca KeyState del firmatario<br>2. Verifica firma Ed25519<br>3. Controlla soglia di firma<br>4. Valida catena hash (sn, digest precedente)
    KV-->>C: firma valida

    C->>DV: Valida evento di dominio
    Note over DV: 1. Estrai AID firmatario → MemberId<br>2. Controlla autorizzazione<br>(admin? referente? cassiere?)<br>3. Controlla precondizioni<br>(saldo, acquisto aperto, ecc.)
    DV-->>C: evento autorizzato

    C->>C: Applica al GroupState
```

---

## Matrice di autorizzazione per ruolo

Gli eventi di dominio richiedono ruoli specifici autenticati via KERI.
L'AID del firmatario determina il suo `MemberId`, che viene verificato
contro il `GroupState` corrente:

```mermaid
flowchart TD
    subgraph Roles["Ruoli (derivati dal GroupState)"]
        Admin["Amministratore<br>= Referente ∪ Cassiere"]
        Ref["Referente"]
        Cas["Cassiere"]
        Mem["Membro"]
    end

    subgraph Gov["Eventi di Governance"]
        VR["VoteRegisterMember"]
        VRM["VoteRemoveMember"]
        VER["VoteElect/RevokeReferente"]
        VEC["VoteElect/RevokeCassiere"]
    end

    subgraph Econ["Eventi Economici"]
        Dep["Deposit"]
        Wit["Withdraw"]
    end

    subgraph Purch["Eventi Acquisti"]
        OP["OpenPurchase"]
        AC["Approve/RejectCommitment"]
        CP["ClosePurchase / FailPurchase"]
        Com["Commit"]
        VCP["VoteClose/FailPurchase"]
    end

    Admin --> Gov
    Cas --> Econ
    Ref --> OP
    Ref --> AC
    Ref --> CP
    Mem --> Com
    Admin --> VCP
```

| Ruolo richiesto | Eventi di dominio |
|-----------------|-------------------|
| **Amministratore** | `VoteRegisterMember`, `VoteRemoveMember`, `VoteElect*`, `VoteRevoke*`, `VoteClosePurchase`, `VoteFailPurchase` |
| **Cassiere** | `Deposit`, `Withdraw` |
| **Referente** | `OpenPurchase`, `Approve/RejectCommitment`, `AdjustCommitment`, `ClosePurchase`, `FailPurchase` |
| **Membro** | `Commit` (solo per sé, richiede saldo sufficiente) |

---

## Bootstrap: dall'inception KERI al primo amministratore

Un nuovo gruppo inizia con un singolo membro che crea un'identità KERI
(inception) e poi diventa sia referente che cassiere:

```mermaid
sequenceDiagram
    participant B as Browser
    participant K as Modulo KERI
    participant S as Server

    Note over B: L'utente clicca "Crea Gruppo"

    B->>K: Genera coppia chiavi Ed25519
    K-->>B: { publicKey, secretKey }

    B->>K: Crea evento inception (icp)
    Note over K: prefix = Blake3(pubKey)<br>nextKeys = [H(next_pub)]<br>threshold = 1

    B->>K: Crea ixn: VoteRegisterMember "fondatore"
    Note over K: Ancora l'evento di dominio<br>Firmato con chiave di inception

    B->>S: POST /api/groups (crea gruppo)
    B->>S: POST /api/groups/:id/events (icp + ixn)

    Note over S: Gruppo creato con il fondatore<br>come unico membro, referente<br>e cassiere
```

Poiché il fondatore è l'unico amministratore al bootstrap, il suo singolo
voto raggiunge immediatamente il quorum (`>= (1+1)/2 = 1`) per tutte le
azioni di governance.

---

## Rotazione chiavi e continuità del dominio

Quando un membro ruota le proprie chiavi KERI, il suo AID (e quindi
`MemberId`) resta lo stesso — cambiano solo le chiavi di controllo.
Lo stato del dominio non è influenzato:

```mermaid
sequenceDiagram
    participant M as Membro (AID: EKpN...)
    participant K as KEL
    participant G as Log del Gruppo

    Note over K: Stato corrente:<br>keys = [pub_key_0]<br>next = [H(pub_key_1)]

    M->>K: evento rot (rivela pub_key_1, impegna H(pub_key_2))
    Note over K: Nuovo stato:<br>keys = [pub_key_1]<br>next = [H(pub_key_2)]

    M->>G: ixn: Deposit membro_X 5000
    Note over G: Firmato con pub_key_1<br>Verificato contro KeyState aggiornato<br>AID invariato → MemberId invariato
```

Questo significa che un membro può ruotare le chiavi (es. dopo un sospetto
di compromissione) senza perdere il proprio ruolo, saldo o storico acquisti.

---

## Flusso eventi: ciclo di vita completo di un acquisto

Un ciclo di acquisto completo che mostra sia il livello KERI che quello
di dominio:

```mermaid
sequenceDiagram
    participant Cas as Cassiere
    participant Ref as Referente
    participant Mem as Membro
    participant Log as Log del Gruppo

    Note over Cas: Ogni freccia è un ixn KERI<br>che ancora un evento di dominio

    Cas->>Log: ixn: Deposit(membro, 10000)
    Note over Log: membro.saldo += 10000

    Ref->>Log: ixn: OpenPurchase("Olio d'Oliva")
    Note over Log: acquisto creato<br>pid = SAID di questo ixn

    Mem->>Log: ixn: Commit(membro, 3000, pid)
    Note over Log: membro.saldo -= 3000<br>impegno = In Attesa

    Ref->>Log: ixn: ApproveCommitment(membro, pid)
    Note over Log: impegno = Approvato

    Cas->>Log: ixn: VoteClosePurchase(pid)
    Ref->>Log: ixn: VoteClosePurchase(pid)
    Note over Log: 2/2 admin hanno votato → quorum raggiunto

    Ref->>Log: ixn: ClosePurchase(pid)
    Note over Log: acquisto.fase = Chiuso
```

Ogni passaggio è un evento `ixn` KERI: firmato crittograficamente,
concatenato tramite hash, e verificabile indipendentemente da qualsiasi
client.

---

## Trasporto: il server come semplice relay

Il server Haskell non interpreta mai gli eventi di dominio. Archivia e
inoltra eventi KERI firmati:

```mermaid
flowchart LR
    subgraph Client["Browser (PureScript)"]
        KERI["Modulo KERI<br>Firma / Verifica"]
        DOM["Modulo Dominio<br>Valida / Applica"]
        UI["UI Halogen"]
        KERI <--> DOM <--> UI
    end

    subgraph Server["Server Haskell"]
        API["HTTP API<br>appendi / recupera"]
        DB["SQLite<br>(group_id, seq, payload)"]
        WS["WebSocket<br>push in tempo reale"]
        API <--> DB
        DB --> WS
    end

    Client <-->|"eventi ixn firmati<br>(opachi per il server)"| Server
```

Il server impone solo:

- **Ordine di sequenza** — 409 Conflict su `seq` duplicato
- **Append-only** — nessun aggiornamento o cancellazione

Tutta la verifica crittografica e la validazione del dominio avvengono
lato client. Un server compromesso non può falsificare eventi — può solo
trattenerli.
