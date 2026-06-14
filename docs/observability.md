# Observability

Four pillars:

- **Logs** — structured JSON via `logger_logstash_formatter`, enriched by
  `scoper`.
- **Metrics** — Prometheus, exported on `GET /metrics`.
- **Traces** — OpenTelemetry, optional, enabled by the
  [compose.tracing.yaml](../compose.tracing.yaml) overlay.
- **Machine introspection** — JSON event dumps over
  `/traces/internal/…`.

## Logging

Configured in [sys.config:2‑14](../config/sys.config#L2):

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        {handler, default, logger_std_h, #{
            level => debug,
            config => #{
                type => {file, "/var/log/fistful-server/console.json"},
                sync_mode_qlen => 20
            },
            formatter => {logger_logstash_formatter, #{}}
        }}
    ]}
]}
```

Every log line is one JSON object ready for Logstash / Loki / any
JSON‑ingesting log backend. The file directory is created by the
Dockerfile
([Dockerfile:43‑46](../Dockerfile#L43)). `sync_mode_qlen => 20` forces
synchronous logging if the handler queue exceeds 20 messages — backpressure
on log floods.

### Scoper

[`scoper`](https://github.com/valitydev/scoper) accumulates structured
key/value context across nested scopes. Fistful opens scopes everywhere:

- At the machine boundary in
  [`fistful:scope/3`](../apps/fistful/src/fistful.erl#L168) with
  `{machine, namespace, id, activity}`.
- At the handler boundary — e.g.
  [`ff_withdrawal_handler:handle_function/3`](../apps/ff_server/src/ff_withdrawal_handler.erl#L17)
  opens `withdrawal` and adds `{id, wallet_id, destination_id, external_id}`
  via `scoper:add_meta/1`.
- At the adapter host
  ([`ff_withdrawal_adapter_host:handle_function/3`](../apps/ff_server/src/ff_withdrawal_adapter_host.erl#L19)).

The `scoper_storage_logger` storage
([sys.config:137](../config/sys.config#L137)) attaches the current scope
to every log message automatically, so you see `namespace=ff/withdrawal_v2`
`id=...` on every line without having to pass it around.

### Woody event handler

[`ff_woody_event_handler`](../apps/ff_server/src/ff_woody_event_handler.erl)
logs inbound and outbound Woody calls. The `scoper_woody_event_handler`
variant is used for `dmt_client` and `party_client`
([sys.config:149, 172](../config/sys.config#L149)) with
`formatter_opts.max_length => 1000` to cap payload dumps.

## Metrics

### Exposure

`GET /metrics[/:registry]` on port `:8022`
([`ff_server:get_prometheus_routes/0`](../apps/ff_server/src/ff_server.erl#L119))
serves the default Prometheus registry.

Configured collectors:
[sys.config:277‑279](../config/sys.config#L277): `{collectors, [default]}`.

### Instrumentation

Two metric families are explicitly set up at boot
([`ff_server:setup_metrics/0`](../apps/ff_server/src/ff_server.erl#L164)):

- `woody_ranch_prometheus_collector:setup/0` — Ranch (Cowboy listener)
  metrics for inbound Woody/HTTP.
- `woody_hackney_prometheus_collector:setup/0` — Hackney metrics for
  outbound HTTP.

Hackney is also wired to report request metrics via `hackney.mod_metrics
=> woody_hackney_prometheus` ([sys.config:282](../config/sys.config#L282)).

Out of the box you get:

- Inbound: request count, duration histogram, in‑flight counts, per‑status‑code.
- Outbound: request count, duration, connection‑pool states.
- VM: the default Prometheus Erlang collector publishes memory, process
  count, reductions, GC, etc.

## OpenTelemetry

Disabled by default. Enable by adding
[compose.tracing.yaml](../compose.tracing.yaml) to your `docker compose`
invocation (the Makefile does this via `DOCKERCOMPOSE_W_ENV`):

```
docker compose -f compose.yaml -f compose.tracing.yaml up -d
```

With tracing enabled:

- `OTEL_TRACES_EXPORTER=otlp`
- `OTEL_EXPORTER_OTLP_PROTOCOL=http_protobuf`
- `OTEL_EXPORTER_OTLP_ENDPOINT=http://jaeger:4318`
- Sampler: `parentbased_always_on` for `testrunner`, `parentbased_always_off`
  for the DMT/bender/limiter/party‑management containers (so trace
  propagation works if the upstream samples, but they don't start traces
  on their own).

A Jaeger all‑in‑one container is added at
[compose.tracing.yaml:28](../compose.tracing.yaml#L28):

- UI: `:16686`
- OTLP gRPC receiver: `:4317`
- OTLP HTTP receiver: `:4318`

Tracing libraries are pulled in as rebar deps and loaded as `temporary`
applications ([rebar.config:95](../rebar.config#L95)) — the tracer only
runs if the opentelemetry application is actually started (which it is,
via `application:ensure_all_started`).

## Health checks

From [sys.config:252‑270](../config/sys.config#L252):

**Liveness** (`GET /health/liveness`):

| Check | Module | Runner |
|-------|--------|--------|
| `disk` | `erl_health` | `disk("/", 99)` — raise if > 99% full |
| `memory` | `erl_health` | `cg_memory(99)` — raise if cgroup > 99% |
| `service` | `erl_health` | `service(<<"fistful-server">>)` — sanity check |

**Readiness** (`GET /health/readiness`):

| Check | Runner |
|-------|--------|
| `dmt_client` | `dmt_client:health_check/0` — DMT reachable and caches initialized |
| `progressor` | `progressor:health_check/1` across the five active namespaces |

All checks are wrapped by
[`ff_server:enable_health_logging/1`](../apps/ff_server/src/ff_server.erl#L114)
which attaches the `erl_health_event_handler` so state transitions
(healthy ↔ failed) land in the log.

> [!WARNING]
> `ff/identity` and `ff/wallet_v2` are deliberately **not** part of the
> progressor readiness check. If your deployment still relies on those
> legacy namespaces, add them to the readiness list in `sys.config`.

## Machine trace endpoint

[`ff_machine_handler`](../apps/ff_server/src/ff_machine_handler.erl)
registers five Cowboy routes for JSON trace dumps:

```
GET /traces/internal/source_v1/:process_id
GET /traces/internal/destination_v2/:process_id
GET /traces/internal/deposit_v1/:process_id
GET /traces/internal/withdrawal_v2/:process_id
GET /traces/internal/withdrawal_session_v2/:process_id
```

Response: `200 OK` with a JSON serialization of the machine's full event
log (from `machinery:trace/3`). Status codes:

- `404 Unknown process` if the ID is missing.
- `400 Invalid ProcessID` if the URL segment is malformed.
- `405 Method Not Allowed` for anything other than `GET`.

Use this for post‑mortem debugging — you can rebuild the timeline of any
withdrawal, deposit, or session without opening a shell into the
database.

> [!NOTE]
> The trace endpoint is **not** authenticated. Keep it off the public
> internet.
