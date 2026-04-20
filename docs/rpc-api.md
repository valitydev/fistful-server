# RPC API

Fistful exposes a **Woody/Thrift** API over HTTP on port `8022` (see
[sys.config:238](../config/sys.config#L238)). Every endpoint is defined
in the [fistful‑proto](https://github.com/valitydev/fistful-proto)
IDL (pulled in as a rebar dep, version `v2.0.2` — see
[rebar.config:40](../rebar.config#L40)). The adapter‑host endpoint uses
[damsel](https://github.com/valitydev/damsel)'s `dmsl_wthd_provider_thrift`.

## Endpoint catalogue

The service table lives in
[`ff_services`](../apps/ff_server/src/ff_services.erl). Paths are mounted
by [`ff_server`](../apps/ff_server/src/ff_server.erl#L77).

| HTTP path | Thrift service | Handler |
|-----------|----------------|---------|
| `/v1/source` | `fistful_source_thrift:'Management'` | [`ff_source_handler`](../apps/ff_server/src/ff_source_handler.erl) |
| `/v1/destination` | `fistful_destination_thrift:'Management'` | [`ff_destination_handler`](../apps/ff_server/src/ff_destination_handler.erl) |
| `/v1/deposit` | `fistful_deposit_thrift:'Management'` | [`ff_deposit_handler`](../apps/ff_server/src/ff_deposit_handler.erl) |
| `/v1/withdrawal` | `fistful_wthd_thrift:'Management'` | [`ff_withdrawal_handler`](../apps/ff_server/src/ff_withdrawal_handler.erl) |
| `/v1/withdrawal_session` | `fistful_wthd_session_thrift:'Management'` | [`ff_withdrawal_session_handler`](../apps/ff_server/src/ff_withdrawal_session_handler.erl) |
| `/v1/repair/deposit` | `fistful_deposit_thrift:'Repairer'` | [`ff_deposit_repair`](../apps/ff_server/src/ff_deposit_repair.erl) |
| `/v1/repair/withdrawal` | `fistful_wthd_thrift:'Repairer'` | [`ff_withdrawal_repair`](../apps/ff_server/src/ff_withdrawal_repair.erl) |
| `/v1/repair/withdrawal/session` | `fistful_wthd_session_thrift:'Repairer'` | [`ff_withdrawal_session_repair`](../apps/ff_server/src/ff_withdrawal_session_repair.erl) |
| `/v1/ff_withdrawal_adapter_host` | `dmsl_wthd_provider_thrift:'AdapterHost'` | [`ff_withdrawal_adapter_host`](../apps/ff_server/src/ff_withdrawal_adapter_host.erl) |

## Handler pattern

Every handler implements the [`ff_woody_wrapper`](../apps/ff_server/src/ff_woody_wrapper.erl)
behaviour:

```erlang
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
```

The uniform recipe inside each clause:

```erlang
handle_function_('Create', {MarshaledParams, MarshaledContext}, _Opts) ->
    Params  = ff_*_codec:unmarshal(params, MarshaledParams),
    Context = ff_*_codec:unmarshal(ctx,    MarshaledContext),
    ok = scoper:add_meta(#{ ... }),
    case ff_*_machine:create(Params, Context) of
        ok              -> handle_function_('Get', {Id, Range}, _);
        {error, exists} -> handle_function_('Get', {Id, Range}, _);
        {error, Reason} -> woody_error:raise(business, #fistful_*{...})
    end.
```

Create is idempotent — a duplicate call to the same ID re‑reads the
current state rather than failing.

## Source management — `/v1/source`

Handler: [`ff_source_handler`](../apps/ff_server/src/ff_source_handler.erl).

| RPC | Signature | Notes |
|-----|-----------|-------|
| `Create` | `(SourceParams, Ctx) → SourceState` | Idempotent on ID |
| `Get` | `(ID, EventRange) → SourceState` | Throws `#fistful_SourceNotFound{}` |
| `GetContext` | `(ID) → Context` | |
| `GetEvents` | `(ID, EventRange) → [Event]` | |

## Destination management — `/v1/destination`

Handler: [`ff_destination_handler`](../apps/ff_server/src/ff_destination_handler.erl).

Same shape as sources: `Create`, `Get`, `GetContext`, `GetEvents`.

## Deposit management — `/v1/deposit`

Handler: [`ff_deposit_handler`](../apps/ff_server/src/ff_deposit_handler.erl).

Four RPCs as above (`Create`, `Get`, `GetContext`, `GetEvents`). Business
errors from Create include `#fistful_SourceNotFound{}`,
`#fistful_WalletNotFound{}`, `#fistful_ForbiddenOperationCurrency{}`,
`#fistful_ForbiddenOperationAmount{}`, `#fistful_RealmsMismatch{}`,
`#wthd_InconsistentDepositCurrency{}` (see
[ff_deposit_handler.erl:30‑](../apps/ff_server/src/ff_deposit_handler.erl#L30)).

## Withdrawal management — `/v1/withdrawal`

Handler: [`ff_withdrawal_handler`](../apps/ff_server/src/ff_withdrawal_handler.erl).

| RPC | Signature | Notes |
|-----|-----------|-------|
| `GetQuote` | `(QuoteParams) → Quote` | Doesn't start a machine; one‑shot |
| `Create` | `(WithdrawalParams, Ctx) → WithdrawalState` | Idempotent on ID |
| `Get` | `(ID, EventRange) → WithdrawalState` | |
| `GetContext` | `(ID) → Context` | |
| `GetEvents` | `(ID, EventRange) → [Event]` | |
| `CreateAdjustment` | `(ID, AdjParams) → AdjustmentState` | See [adjustments.md](adjustments.md) |

Full error mapping is in
[ff_withdrawal_handler.erl:36‑](../apps/ff_server/src/ff_withdrawal_handler.erl#L36).

## Withdrawal session management — `/v1/withdrawal_session`

Handler: [`ff_withdrawal_session_handler`](../apps/ff_server/src/ff_withdrawal_session_handler.erl).

| RPC | Signature |
|-----|-----------|
| `Get` | `(ID, EventRange) → SessionState` |
| `GetEvents` | `(ID, EventRange) → [Event]` |
| `GetContext` | `(ID) → Context` |

Sessions are not directly created over the API — they're spawned by
the withdrawal machine. The management service is read‑only.

## Repairers — `/v1/repair/*`

Three services, one RPC each:

| Service | Handler | RPC |
|---------|---------|-----|
| `/v1/repair/withdrawal` | [`ff_withdrawal_repair`](../apps/ff_server/src/ff_withdrawal_repair.erl) | `Repair(ID, Scenario) → ok` |
| `/v1/repair/withdrawal/session` | [`ff_withdrawal_session_repair`](../apps/ff_server/src/ff_withdrawal_session_repair.erl) | `Repair(ID, Scenario) → ok` |
| `/v1/repair/deposit` | [`ff_deposit_repair`](../apps/ff_server/src/ff_deposit_repair.erl) | `Repair(ID, Scenario) → ok` |

Errors: `#fistful_*NotFound{}` when the machine is missing,
`#fistful_MachineAlreadyWorking{}` when a repair is attempted against a
healthy machine. See [operations.md](operations.md#repair-scenarios) for
the scenario vocabulary.

## Adapter host — `/v1/ff_withdrawal_adapter_host`

Handler: [`ff_withdrawal_adapter_host`](../apps/ff_server/src/ff_withdrawal_adapter_host.erl).
Single RPC: `ProcessCallback(Callback) → ProcessCallbackResult`.

This is the only endpoint *called by external parties into* fistful —
provider adapters push asynchronous results back here. See
[adapter-integration.md](adapter-integration.md).

## Codecs

Codec modules marshal/unmarshal between the Thrift wire types and the
fistful domain types. One codec per entity plus several shared ones:

| Codec | File |
|-------|------|
| [`ff_codec`](../apps/ff_server/src/ff_codec.erl) | Common: ID, cash, currency, cash range, context, event range |
| [`ff_dmsl_codec`](../apps/fistful/src/ff_dmsl_codec.erl) | Damsel types used by domain code |
| [`ff_source_codec`](../apps/ff_server/src/ff_source_codec.erl) | Source params / state / events |
| [`ff_destination_codec`](../apps/ff_server/src/ff_destination_codec.erl) | Destination |
| [`ff_deposit_codec`](../apps/ff_server/src/ff_deposit_codec.erl) | Deposit params / state / events |
| [`ff_withdrawal_codec`](../apps/ff_server/src/ff_withdrawal_codec.erl) | Withdrawal params / state / events / quote / repair scenario |
| [`ff_withdrawal_session_codec`](../apps/ff_server/src/ff_withdrawal_session_codec.erl) | Session state / events |
| [`ff_adapter_withdrawal_codec`](../apps/ff_transfer/src/ff_adapter_withdrawal_codec.erl) | Provider adapter types |
| [`ff_cash_flow_codec`](../apps/ff_server/src/ff_cash_flow_codec.erl) | Final cash flow |
| [`ff_p_transfer_codec`](../apps/ff_server/src/ff_p_transfer_codec.erl) | Posting transfer events |
| [`ff_limit_check_codec`](../apps/ff_server/src/ff_limit_check_codec.erl) | Limit‑check details |
| [`ff_withdrawal_adjustment_codec`](../apps/ff_server/src/ff_withdrawal_adjustment_codec.erl) | Adjustment change + state |
| [`ff_withdrawal_status_codec`](../apps/ff_server/src/ff_withdrawal_status_codec.erl) / [`ff_deposit_status_codec`](../apps/ff_server/src/ff_deposit_status_codec.erl) | Status enums |
| [`ff_entity_context_codec`](../apps/ff_server/src/ff_entity_context_codec.erl) | `ff_entity_context:context()` |
| [`ff_msgpack_codec`](../apps/ff_server/src/ff_msgpack_codec.erl) | Msgpack ↔ Erlang for entity context |

All codecs follow a symmetrical `marshal/2 + unmarshal/2` shape.
`undefined` fields are stripped via `genlib_map:compact/1` on the way
out and defaulted on the way in.

## Internal HTTP endpoints

Exposed on the same `:8022` listener but outside the Thrift dispatch:

| Path | Purpose | Source |
|------|---------|--------|
| `GET /metrics[/:registry]` | Prometheus scrape | [ff_server.erl:119](../apps/ff_server/src/ff_server.erl#L119) |
| `GET /health/liveness` | Liveness probe | [ff_server.erl:103](../apps/ff_server/src/ff_server.erl#L103) |
| `GET /health/readiness` | Readiness probe | [ff_server.erl:103](../apps/ff_server/src/ff_server.erl#L103) |
| `GET /traces/internal/{source_v1\|destination_v2\|deposit_v1\|withdrawal_v2\|withdrawal_session_v2}/:process_id` | Machine trace JSON dump | [ff_machine_handler.erl](../apps/ff_server/src/ff_machine_handler.erl) |

See [observability.md](observability.md) for what these surface.
