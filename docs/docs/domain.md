# Domain model

Simplified from [reactivegas](https://github.com/paolino/reactivegas).

## Roles

- **Member** — has credit balance, commits to purchases
- **Referente** — opens/closes purchases, approves/rejects commitments, votes on governance
- **Cassiere** — handles cash; sole authority to sign deposits and withdrawals

Referente and Cassiere are elected from members. A member can hold both roles.

## Purchase lifecycle

1. Cassiere deposits cash for a member (member now has credit)
2. Referente opens a purchase
3. Members commit amounts (debited from credit immediately)
4. Referente approves/rejects individual commitments
5. Referenti vote to authorize closure (majority = `(count + 1) / 2`)
6. Referente closes (total debited from referente's cash) or fails (all refunded)

## Events

Each event is signed by the author's KERI Ed25519 key and appended to
the group's shared log.

### Registry

| Event | Description |
|-------|-------------|
| `RegisterMember name` | Add a new member |
| `ElectReferente memberId` | Promote member to referente |
| `RevokeReferente memberId` | Demote referente to member |
| `ElectCassiere memberId` | Promote member to cassiere |
| `RevokeCassiere memberId` | Demote cassiere to member |

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
| `VoteClosePurchase purchaseId` | Referente votes to close |
| `VoteFailPurchase purchaseId` | Referente votes to fail |
| `ClosePurchase purchaseId` | Referente closes |
| `FailPurchase purchaseId` | Referente fails |

## Amounts

All amounts in **cents** (integer). No floating point.
