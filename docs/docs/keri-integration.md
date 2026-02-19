# KERI Integration

How keri-coop maps its cooperative purchasing domain onto KERI primitives.

---

## Identity = KERI AID

Every member is identified by their **Autonomic Identifier** (AID), derived
from their Ed25519 public key via Blake3 hashing and CESR encoding. There is
no username/password — the AID *is* the identity.

```mermaid
flowchart LR
    KP["Ed25519 Key Pair\n(generated in browser)"]
    H["Blake3(pub_key)"]
    AID["AID Prefix\nCESR-encoded"]
    MID["MemberId\n= AID"]

    KP --> H --> AID --> MID
```

The `MemberId` type is a newtype over `AID`:

```purescript
newtype AID = AID String        -- CESR-encoded prefix
newtype MemberId = MemberId AID -- domain alias
```

---

## Domain events as KERI interaction anchors

Every domain action (vote, deposit, purchase, etc.) is wrapped in a KERI
**interaction event** (`ixn`). The domain event is canonically serialized to
JSON and placed in the `anchors` field of the `ixn`. The `ixn` is then
signed with the member's current private key and appended to the group log.

```mermaid
flowchart TD
    DE["Domain Event\ne.g. VoteRegisterMember 'Alice'"]
    CJ["Canonical JSON\n{'name':'Alice','t':'VoteRegisterMember'}"]
    IXN["KERI ixn Event\nprefix: signer AID\nsn: next seq number\np: prior event digest\na: [canonical JSON]"]
    SIG["Ed25519 Signature\nover serialized ixn"]
    GM["GroupMessage\n{ keriEvent, domainEvent }"]

    DE --> CJ --> IXN --> SIG --> GM
```

This is implemented in `Protocol.Message.mkGroupMessage`:

1. Serialize the `DomainEvent` to canonical JSON (deterministic field order)
2. Create a KERI `ixn` with the JSON as an anchor
3. Compute the SAID (self-addressing identifier) of the `ixn`
4. Sign the serialized `ixn` with the member's Ed25519 secret key
5. Package as a `GroupMessage` for transport

---

## Two event logs, two purposes

keri-coop maintains two conceptual logs that serve different purposes:

```mermaid
flowchart LR
    subgraph KERI["KERI Layer (per member)"]
        KEL["Key Event Log\nicp → ixn → rot → ixn → ..."]
        KS["Key State\ncurrent keys, thresholds,\npre-rotation commitments"]
        KEL --> KS
    end

    subgraph Domain["Domain Layer (per group)"]
        GL["Group Log\nSequence of signed domain events"]
        GS["Group State\nmembers, balances,\npurchases, votes"]
        GL --> GS
    end

    KEL -.->|"ixn anchors\ndomain events"| GL
    KS -.->|"signature\nverification"| GL
```

| Log | Scope | Purpose | Events |
|-----|-------|---------|--------|
| **KEL** | Per member | Key management, identity | `icp`, `rot`, `ixn` |
| **Group Log** | Per group | Domain state | Signed domain events anchored in `ixn`s |

The KEL tracks *who controls which keys*. The group log tracks *what happened
in the cooperative*. They are linked: every group log entry is a KERI `ixn`
whose signature is verified against the signer's current key state.

---

## Verification flow

When a `GroupMessage` arrives (via HTTP or WebSocket), the client verifies it
in two layers before applying it to the domain state:

```mermaid
sequenceDiagram
    participant S as Server
    participant C as Client
    participant KV as KERI Verifier
    participant DV as Domain Validator

    S->>C: GroupMessage { keriEvent, domainEvent }

    C->>KV: Verify ixn signature
    Note over KV: 1. Look up signer's KeyState\n2. Verify Ed25519 signature\n3. Check signing threshold\n4. Validate hash chain (sn, prior digest)
    KV-->>C: signature valid

    C->>DV: Validate domain event
    Note over DV: 1. Extract signer AID → MemberId\n2. Check authorization\n(admin? referente? cassiere?)\n3. Check preconditions\n(balance, purchase open, etc.)
    DV-->>C: event authorized

    C->>C: Apply to GroupState
```

---

## Role authorization matrix

Domain events require specific KERI-authenticated roles. The signer's AID
determines their `MemberId`, which is checked against the current `GroupState`:

```mermaid
flowchart TD
    subgraph Roles["Roles (derived from GroupState)"]
        Admin["Admin\n= Referente ∪ Cassiere"]
        Ref["Referente"]
        Cas["Cassiere"]
        Mem["Member"]
    end

    subgraph Gov["Governance Events"]
        VR["VoteRegisterMember"]
        VRM["VoteRemoveMember"]
        VER["VoteElect/RevokeReferente"]
        VEC["VoteElect/RevokeCassiere"]
    end

    subgraph Econ["Economic Events"]
        Dep["Deposit"]
        Wit["Withdraw"]
    end

    subgraph Purch["Purchase Events"]
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

| Required role | Domain events |
|---------------|---------------|
| **Admin** | `VoteRegisterMember`, `VoteRemoveMember`, `VoteElect*`, `VoteRevoke*`, `VoteClosePurchase`, `VoteFailPurchase` |
| **Cassiere** | `Deposit`, `Withdraw` |
| **Referente** | `OpenPurchase`, `Approve/RejectCommitment`, `AdjustCommitment`, `ClosePurchase`, `FailPurchase` |
| **Member** | `Commit` (self only, requires sufficient balance) |

---

## Bootstrap: from KERI inception to first admin

A new group starts with a single member who creates a KERI identity
(inception) and then becomes both referente and cassiere:

```mermaid
sequenceDiagram
    participant B as Browser
    participant K as KERI Module
    participant S as Server

    Note over B: User clicks "Create Group"

    B->>K: Generate Ed25519 key pair
    K-->>B: { publicKey, secretKey }

    B->>K: Create inception event (icp)
    Note over K: prefix = Blake3(pubKey)\nnextKeys = [H(next_pub)]\nthreshold = 1

    B->>K: Create ixn: VoteRegisterMember "founder"
    Note over K: Anchors domain event\nSigned by inception key

    B->>S: POST /api/groups (create group)
    B->>S: POST /api/groups/:id/events (icp + ixn)

    Note over S: Group created with founder\nas sole member, referente,\nand cassiere
```

Since the founder is the only admin at bootstrap, their single vote
immediately reaches quorum (`>= (1+1)/2 = 1`) for all governance actions.

---

## Key rotation and domain continuity

When a member rotates their KERI keys, their AID (and thus `MemberId`) stays
the same — only the controlling keys change. The domain state is unaffected:

```mermaid
sequenceDiagram
    participant M as Member (AID: EKpN...)
    participant K as KEL
    participant G as Group Log

    Note over K: Current state:\nkeys = [pub_key_0]\nnext = [H(pub_key_1)]

    M->>K: rot event (reveal pub_key_1, commit H(pub_key_2))
    Note over K: New state:\nkeys = [pub_key_1]\nnext = [H(pub_key_2)]

    M->>G: ixn: Deposit member_X 5000
    Note over G: Signed with pub_key_1\nVerified against updated KeyState\nAID unchanged → MemberId unchanged
```

This means a member can rotate keys (e.g. after a suspected compromise)
without losing their role, balance, or purchase history.

---

## Event flow: complete purchase lifecycle

A full purchase cycle showing both KERI and domain layers:

```mermaid
sequenceDiagram
    participant Cas as Cassiere
    participant Ref as Referente
    participant Mem as Member
    participant Log as Group Log

    Note over Cas: Each arrow is a KERI ixn\nanchoring a domain event

    Cas->>Log: ixn: Deposit(member, 10000)
    Note over Log: member.balance += 10000

    Ref->>Log: ixn: OpenPurchase("Olive Oil")
    Note over Log: purchase created\npid = SAID of this ixn

    Mem->>Log: ixn: Commit(member, 3000, pid)
    Note over Log: member.balance -= 3000\ncommitment = Pending

    Ref->>Log: ixn: ApproveCommitment(member, pid)
    Note over Log: commitment = Approved

    Cas->>Log: ixn: VoteClosePurchase(pid)
    Ref->>Log: ixn: VoteClosePurchase(pid)
    Note over Log: 2/2 admins voted → quorum reached

    Ref->>Log: ixn: ClosePurchase(pid)
    Note over Log: purchase.phase = Closed
```

Every step is a KERI `ixn` event: cryptographically signed, hash-chained,
and independently verifiable by any client.

---

## Transport: server as dumb relay

The Haskell server never interprets domain events. It stores and forwards
signed KERI events:

```mermaid
flowchart LR
    subgraph Client["Browser (PureScript)"]
        KERI["KERI Module\nSign / Verify"]
        DOM["Domain Module\nValidate / Apply"]
        UI["Halogen UI"]
        KERI <--> DOM <--> UI
    end

    subgraph Server["Haskell Server"]
        API["HTTP API\nappend / fetch"]
        DB["SQLite\n(group_id, seq, payload)"]
        WS["WebSocket\nreal-time push"]
        API <--> DB
        DB --> WS
    end

    Client <-->|"signed ixn events\n(opaque to server)"| Server
```

The server enforces only:

- **Sequence ordering** — 409 Conflict on duplicate `seq`
- **Append-only** — no updates or deletes

All cryptographic verification and domain validation happen client-side.
A compromised server cannot forge events — it can only withhold them.
