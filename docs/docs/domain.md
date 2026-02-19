# Domain model

Simplified from [reactivegas](https://github.com/paolino/reactivegas).

## Roles

- **Member** — has credit balance, commits to purchases
- **Admin** — has cash register, manages purchases, votes on governance

## Purchase lifecycle

1. Admin opens a purchase
2. Members commit amounts (debited from credit immediately)
3. Referente (opening admin) approves/rejects individual commitments
4. Admins vote to authorize closure (majority = `(count + 1) / 2`)
5. Referente closes (total debited from referente's cash) or fails (all refunded)

## Events

Each event is signed by the author's KERI Ed25519 key and appended to
the group's shared log.

### Registry

| Event | Description |
|-------|-------------|
| `RegisterMember name` | Add a new member |
| `ElectAdmin memberId` | Promote member to admin |
| `RevokeAdmin memberId` | Demote admin to member |

### Economics

| Event | Description |
|-------|-------------|
| `Deposit memberId cents` | Add credit |
| `Withdraw memberId cents reason` | Remove credit |

### Purchases

| Event | Description |
|-------|-------------|
| `OpenPurchase name` | Admin opens a purchase |
| `Commit memberId cents purchaseId` | Member commits to purchase |
| `ApproveCommitment memberId purchaseId` | Referente approves |
| `RejectCommitment memberId purchaseId` | Referente rejects |
| `AdjustCommitment memberId cents purchaseId` | Adjust amount |
| `VoteClosePurchase purchaseId` | Admin votes to close |
| `VoteFailPurchase purchaseId` | Admin votes to fail |
| `ClosePurchase purchaseId` | Referente closes |
| `FailPurchase purchaseId` | Referente fails |

## Amounts

All amounts in **cents** (integer). No floating point.
