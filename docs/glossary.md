# Glossary

A quick reference for terms that recur across the codebase and docs.
Most come either from the banking/payments domain, from Vality's
platform vocabulary, or from the machinery/progressor runtime.

## Domain terms

**Party** — an owner of wallets, sources, and destinations. Lives in
`party-management`; fistful only reads. See
[`ff_party`](../apps/fistful/src/ff_party.erl).

**Wallet** — a balance a party holds within Vality. One currency per
wallet, one primary settlement account per wallet. Lives in
`party-management` and DMT.

**Realm** — `test | live`. Attached to each payment institution and
propagated to accounts. Withdrawal and deposit creation fail with
`{realms_mismatch, _}` if the wallet and destination/source realms
don't match.
[`ff_payment_institution:realm/0`](../apps/fistful/src/ff_payment_institution.erl).

**Source** — external funding, e.g. a card or generic rail that can
credit a wallet via a deposit. Fistful namespace `ff/source_v1`.

**Destination** — external payout target. Fistful namespace
`ff/destination_v2`. Carries a `resource` and optional `auth_data`.

**Resource** — typed payload inside a source/destination:
`{bank_card, …}`, `{crypto_wallet, …}`, `{digital_wallet, …}`,
`{generic, …}`. See [`ff_resource`](../apps/fistful/src/ff_resource.erl).

**Deposit** — external source → wallet credit. Single posting
transfer, no routing. Namespace `ff/deposit_v1`.

**Withdrawal** — wallet → external destination debit. Involves
routing, posting transfer, sessions, and possibly adjustments.
Namespace `ff/withdrawal_v2`.

**Session** — a single attempt to push a withdrawal through a
provider. Namespace `ff/withdrawal/session_v2`. Can sleep waiting for
a provider callback.

**Adjustment** — a corrective operation on a finished deposit or
withdrawal. Can change status or replay cash flow at a new domain
revision. See [`ff_adjustment`](../apps/ff_transfer/src/ff_adjustment.erl).

**Quote** — a provider‑supplied price for a withdrawal, valid for a
limited time and redeemable by passing `quote_data` back into `Create`.

**Route** — `(provider_id, terminal_id)` pair chosen for a withdrawal
attempt. See [routing.md](routing.md).

**Payment institution (PI)** — a DMT object that groups providers,
routing rulesets, system accounts, and payment methods. Every wallet
is tied to a PI via its terms.

**Provider / terminal** — the concrete payout channel (acquirer /
terminal). Each terminal has its own `WithdrawalProvisionTerms` (cash
range, supported methods, fees).

**Routing ruleset** — a DMT object encoding selection logic as
selectors reduced against a varset. A PI has two: policies (positive)
and prohibitions (negative).

**Varset** — the set of values against which selectors are reduced
during routing (currency, cash, method, party ID, wallet ID, etc.).
Built via [`ff_varset`](../apps/fistful/src/ff_varset.erl).

## Accounting terms

**Cash** — `{Amount :: integer(), CurrencyID}`, amount in minor units.

**Cash range** — inclusive/exclusive bounded range of cash values. Used
for wallet balance limits and per‑route cash caps.

**Plan cash flow** — a template of postings with symbolic accounts
(`{wallet, sender_source}` etc.) and symbolic volumes (`{fixed, _}`,
`{share, _}`, `{product, _}`).

**Final cash flow** — plan cash flow with concrete account IDs and
concrete amounts. What's sent to shumway.

**Posting** — a single credit/debit pair: `{Sender, Receiver, Volume}`.

**Posting transfer** — a named set of postings with a state machine
(`created → prepared → committed | cancelled`). Backed by shumway's
`Hold` / `CommitPlan` / `RollbackPlan`.

**Plan ID / transaction ID** — the deterministic identifier fistful
supplies to shumway. Derived from withdrawal/deposit ID + route +
iteration. Used for idempotency.

**Turnover limit** — a per‑provider/terminal/wallet rolling limit
(daily, monthly, etc.) tracked in the external `limiter` service.

**Operation ID (limiter)** — the segmented ID fistful gives the
limiter to dedupe holds: `[provider, terminal, withdrawal, iteration?]`.
See [`ff_limiter:make_operation_segments/3`](../apps/ff_transfer/src/ff_limiter.erl#L52).

## Machinery / platform terms

**machinery** — Vality's state‑machine library. Provides the
`machinery:machine/2` / `machine:result/2` shapes, `dispatch_signal`,
`dispatch_call`, etc.

**progressor** — Vality's machine runtime. Persists histories in
PostgreSQL, owns worker pools, schedules timeouts, serializes
concurrent operations on the same `(namespace, id)`.

**machinery_prg_backend** — the progressor‑backed implementation of the
`machinery_backend` behaviour.

**Namespace** — a unique string identifying a machine family, e.g.
`ff/withdrawal_v2`. Each namespace has its own progressor pool, handler
module, and schema module.

**Aux state** — per‑machine blob persisted alongside the event log.
In fistful, this is always `#{ctx => ff_entity_context:context()}`.

**Entity context** — per‑machine user‑defined metadata. Namespaced
map; see [`ff_entity_context`](../apps/fistful/src/ff_entity_context.erl).

**Timestamped event** — `{ev, Timestamp, Change}`. Every fistful event
is wrapped this way by
[`ff_machine:emit_event/1`](../apps/fistful/src/ff_machine.erl#L63).

**Collapse** — the fold‑over‑history that reconstructs the in‑memory
model from events. See
[`ff_machine:collapse/2`](../apps/fistful/src/ff_machine.erl#L60).

**Repair** — a special machine operation that appends arbitrary
events to unstick a machine in a faulted state.

## Networking / RPC terms

**Woody** — Vality's Thrift‑over‑HTTP RPC framework. Uses normal
Thrift binary protocol but over HTTP with a specific header vocabulary
(deadline, parent ID, trace ID).

**Thrift service** — a Thrift IDL interface, e.g.
`fistful_wthd_thrift:'Management'`. Fistful's IDL lives in the
[fistful‑proto](https://github.com/valitydev/fistful-proto) repo.

**DMT** — Vality's *Domain Management Thing*, a versioned store of
domain configuration (payment institutions, providers, routing rules,
etc.). Exposed over Thrift; client lib is `dmt_client`.

**Damsel** — the Thrift IDL repository where DMT's types are defined
(`dmsl_*` namespaces). Erlang dep at
[rebar.config:38](../rebar.config#L38).

**Adapter** — an external process that speaks the
`dmsl_wthd_provider_thrift:Adapter` service. Each withdrawal provider
is an adapter.

**Adapter host** — the opposite direction: fistful serves
`dmsl_wthd_provider_thrift:AdapterHost` so adapters can call back with
asynchronous results.

## Error‑handling idioms

**`do/1` block** — [`ff_pipeline:do/1`](../apps/ff_core/src/ff_pipeline.erl#L28)
wraps a function, catches thrown exceptions, returns `{ok, Value}` or
`{error, Reason}`. The universal error‑propagation idiom.

**`unwrap/1,2`** — extract the `ok` value from a result, or throw its
error. Used inside `do/1`.

**Business error** — a `woody_error:raise(business, #*{...})` invocation
that surfaces a typed Thrift exception to the client. Distinct from
**system errors**, which become Woody system errors (5xx‑ish).

## Config / vocabulary

**sys.config** — OTP application environment baked into the release.

**vm.args** — Erlang VM flags.

**Scoper** — Vality's structured‑logging scope tracker. Every machine
operation and every handler opens a scope; loggers see the merged
scope of the call chain.

**Opentelemetry** — W3C tracing. Optional, enabled by
[compose.tracing.yaml](../compose.tracing.yaml).
