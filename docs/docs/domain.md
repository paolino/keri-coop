# Domain model

Simplified from [reactivegas](https://github.com/paolino/reactivegas).

## Roles

- **Member** — has credit balance, commits to purchases
- **Referente** — opens/closes purchases, approves/rejects commitments
- **Cassiere** — handles cash; sole authority to sign deposits and withdrawals

Referente and Cassiere are both **admin** roles — they vote together on
governance with equal weight. A member can hold both roles.

## Bootstrap

The member who creates a group is automatically the first referente and
cassiere. All subsequent role changes go through admin voting.

## Governance (admin voting)

Any admin (referente or cassiere) can cast a vote. There is no separate
proposal step — the first vote for an action *is* the proposal. The state
machine counts matching votes; when the count reaches majority
(`>= (adminCount + 1) / 2`), the action takes effect and the tally is
cleared. Duplicate votes from the same admin are ignored.

`adminCount` is computed — it's part of the fold over the event log.

### Governance vote events

| Event | Effect on quorum |
|-------|-----------------|
| `VoteRegisterMember name` | Member registered |
| `VoteRemoveMember memberId` | Member removed |
| `VoteElectReferente memberId` | Member becomes referente |
| `VoteRevokeReferente memberId` | Referente demoted |
| `VoteElectCassiere memberId` | Member becomes cassiere |
| `VoteRevokeCassiere memberId` | Cassiere demoted |

## Purchase lifecycle

1. Cassiere deposits cash for a member (member now has credit)
2. Referente opens a purchase
3. Members commit amounts (debited from credit immediately)
4. Referente approves/rejects individual commitments
5. Admins vote to authorize closure (majority)
6. Referente closes (total debited from referente's cash) or fails (all refunded)

## Events

Each event is signed by the author's KERI Ed25519 key and appended to
the group's shared log.

### Economics (signed by Cassiere)

| Event | Description |
|-------|-------------|
| `Deposit memberId cents` | Credit a member's balance |
| `Withdraw memberId cents reason` | Debit a member's balance |

### Purchases

| Event | Description |
|-------|-------------|
| `OpenPurchase name` | Referente opens a purchase |
| `Commit memberId cents purchaseId` | Member commits to purchase |
| `ApproveCommitment memberId purchaseId` | Referente approves |
| `RejectCommitment memberId purchaseId` | Referente rejects |
| `AdjustCommitment memberId cents purchaseId` | Adjust amount |
| `VoteClosePurchase purchaseId` | Admin votes to close |
| `VoteFailPurchase purchaseId` | Admin votes to fail |
| `ClosePurchase purchaseId` | Referente closes |
| `FailPurchase purchaseId` | Referente fails |

## State (computed by fold)

All state is derived by replaying the event log:

- **Members** — set of registered members with credit balances
- **Admins** — set of referenti + cassieri (union)
- **Admin count** — `length admins`, used for majority threshold
- **Vote tallies** — pending votes per action, cleared on quorum
- **Purchases** — open purchases with commitments and vote counts

## Amounts

All amounts in **cents** (integer). No floating point.
