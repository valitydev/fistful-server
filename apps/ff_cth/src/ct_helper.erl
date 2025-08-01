-module(ct_helper).

-include_lib("common_test/include/ct.hrl").

-export([cfg/2]).
-export([cfg/3]).

-export([start_apps/1]).
-export([start_app/1]).
-export([stop_apps/1]).
-export([stop_app/1]).

-export([makeup_cfg/2]).

-export([set_context/1]).
-export([unset_context/0]).

-export([woody_ctx/0]).
-export([get_woody_ctx/1]).

-export([test_case_name/1]).
-export([get_test_case_name/1]).

-export([await/2]).
-export([await/3]).

-export([create_account/1]).

-export([trace_testcase/3]).
-export([end_trace/1]).

-type test_case_name() :: atom().
-type group_name() :: atom().
-type config() :: [{atom(), term()}].

-export_type([test_case_name/0]).
-export_type([group_name/0]).
-export_type([config/0]).

%%

-spec cfg(atom(), config()) -> term().
cfg(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        _ -> error({'ct config entry missing', Key})
    end.

-spec cfg(atom(), _, config()) -> config().
cfg(Key, Value, Config) ->
    lists:keystore(Key, 1, Config, {Key, Value}).

%%

-type app_name() :: atom().
-type app_env() :: [{atom(), term()}].
-type app_with_env() :: {app_name(), app_env()}.
-type startup_ctx() :: #{atom() => _}.

-spec start_apps([app_name() | app_with_env()]) -> {[Started :: app_name()], startup_ctx()}.
start_apps(AppNames) ->
    lists:foldl(
        fun(AppName, {SAcc, CtxAcc}) ->
            {Started, Ctx} = start_app(AppName),
            {SAcc ++ Started, maps:merge(CtxAcc, Ctx)}
        end,
        {[], #{}},
        AppNames
    ).

-spec start_app(app_name() | app_with_env()) -> {[Started :: app_name()], startup_ctx()}.
start_app(scoper = AppName) ->
    {
        start_app_with(AppName, [
            {storage, scoper_storage_logger}
        ]),
        #{}
    };
start_app(woody = AppName) ->
    {
        start_app_with(AppName, [
            {acceptors_pool_size, 4}
        ]),
        #{}
    };
start_app(dmt_client = AppName) ->
    {
        start_app_with(AppName, [
            % milliseconds
            {cache_update_interval, 500},
            {max_cache_size, #{
                elements => 10,
                % 50Mb
                memory => 52428800
            }},
            {woody_event_handlers, [
                {ff_woody_event_handler, #{}}
            ]},
            {service_urls, #{
                'AuthorManagement' => <<"http://dmt:8022/v1/domain/author">>,
                'Repository' => <<"http://dmt:8022/v1/domain/repository">>,
                'RepositoryClient' => <<"http://dmt:8022/v1/domain/repository_client">>
            }}
        ]),
        #{}
    };
start_app(party_client = AppName) ->
    {
        start_app_with(AppName, [
            {services, #{
                party_management => "http://party-management:8022/v1/processing/partymgmt"
            }},
            {woody, #{
                cache_mode => safe,
                options => #{
                    woody_client => #{
                        event_handler => {ff_woody_event_handler, #{}}
                    }
                }
            }}
        ]),
        #{}
    };
start_app(ff_server = AppName) ->
    {
        start_app_with(AppName, [
            {ip, "::"},
            {port, 8022}
        ]),
        #{}
    };
start_app(bender_client = AppName) ->
    {
        start_app_with(AppName, [
            {services, #{
                'Bender' => <<"http://bender:8022/v1/bender">>,
                'Generator' => <<"http://bender:8022/v1/generator">>
            }},
            {deadline, 10000},
            {retries, #{
                'GenerateID' => finish,
                'GetInternalID' => finish,
                '_' => finish
            }}
        ]),
        #{}
    };
start_app({AppName, AppEnv}) ->
    {start_app_with(AppName, AppEnv), #{}};
start_app(AppName) ->
    {start_app_with(AppName, []), #{}}.

-spec start_app_with(app_name(), app_env()) -> [app_name()].
start_app_with(AppName, Env) ->
    _ = application:load(AppName),
    _ = set_app_env(AppName, Env),
    case application:ensure_all_started(AppName) of
        {ok, Apps} ->
            Apps;
        {error, Reason} ->
            exit({start_app_failed, AppName, Reason})
    end.

set_app_env(AppName, Env) ->
    lists:foreach(
        fun({K, V}) ->
            ok = application:set_env(AppName, K, V)
        end,
        Env
    ).

-spec stop_apps([app_name()]) -> ok.
stop_apps(AppNames) ->
    lists:foreach(fun stop_app/1, lists:reverse(AppNames)).

-spec stop_app(app_name()) -> ok.
stop_app(AppName) ->
    case application:stop(AppName) of
        ok ->
            case application:unload(AppName) of
                ok ->
                    ok;
                {error, Reason} ->
                    exit({unload_app_failed, AppName, Reason})
            end;
        {error, Reason} ->
            exit({unload_app_failed, AppName, Reason})
    end.

-spec set_context(config()) -> ok.
set_context(C) ->
    ok = ff_context:save(
        ff_context:create(#{
            party_client => party_client:create_client(),
            woody_context => cfg('$woody_ctx', C)
        })
    ).

-spec unset_context() -> ok.
unset_context() ->
    ok = ff_context:cleanup().

%%

-type config_mut_fun() :: fun((config()) -> config()).

-spec makeup_cfg([config_mut_fun()], config()) -> config().
makeup_cfg(CMFs, C0) ->
    lists:foldl(fun(CMF, C) -> CMF(C) end, C0, CMFs).

-spec woody_ctx() -> config_mut_fun().
woody_ctx() ->
    fun(C) -> cfg('$woody_ctx', construct_woody_ctx(C), C) end.

construct_woody_ctx(C) ->
    woody_context:new(construct_rpc_id(get_test_case_name(C))).

construct_rpc_id(TestCaseName) ->
    woody_context:new_rpc_id(
        <<"undefined">>,
        list_to_binary(lists:sublist(atom_to_list(TestCaseName), 32)),
        woody_context:new_req_id()
    ).

-spec get_woody_ctx(config()) -> woody_context:ctx().
get_woody_ctx(C) ->
    cfg('$woody_ctx', C).

%%

-spec test_case_name(test_case_name()) -> config_mut_fun().
test_case_name(TestCaseName) ->
    fun(C) -> cfg('$test_case_name', TestCaseName, C) end.

-spec get_test_case_name(config()) -> test_case_name().
get_test_case_name(C) ->
    cfg('$test_case_name', C).

%%

-spec await(Expect, fun(() -> Expect | _)) -> Expect.
await(Expect, Compute) ->
    await(Expect, Compute, genlib_retry:linear(3, 1000)).

-spec await(Expect, fun(() -> Expect | _), genlib_retry:strategy()) -> Expect.
await(Expect, Compute, Retry0) ->
    case Compute() of
        Expect ->
            Expect;
        NotYet ->
            case genlib_retry:next_step(Retry0) of
                {wait, To, Retry1} ->
                    ok = timer:sleep(To),
                    await(Expect, Compute, Retry1);
                finish ->
                    error({'await failed', NotYet})
            end
    end.

-spec create_account(dmsl_accounter_thrift:'PlanID'()) ->
    {ok, ff_account:account_id()}
    | {error, {exception, any()}}.
create_account(CurrencyCode) ->
    Description = <<"ff_test">>,
    ff_accounting:create_account(CurrencyCode, Description).

-spec trace_testcase(module(), atom(), config()) -> config().
trace_testcase(Mod, Name, C) ->
    SpanName = iolist_to_binary([atom_to_binary(Mod), ":", atom_to_binary(Name), "/1"]),
    SpanCtx = otel_tracer:start_span(opentelemetry:get_application_tracer(Mod), SpanName, #{kind => internal}),
    %% NOTE This also puts otel context to process dictionary
    _ = otel_tracer:set_current_span(SpanCtx),
    [{span_ctx, SpanCtx} | C].

-spec end_trace(config()) -> ok.
end_trace(C) ->
    case lists:keyfind(span_ctx, 1, C) of
        {span_ctx, SpanCtx} ->
            _ = otel_span:end_span(SpanCtx),
            ok;
        _ ->
            ok
    end.
