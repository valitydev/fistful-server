%%%
%%% Server startup
%%%
%%% TODOs
%%%
%%%  - We should probably most of what is hardcoded here to the application
%%%    environment.
%%%  - Provide healthcheck.
%%%

-module(ff_server).

-export([start/0]).

%% Application

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

-define(PROGRESSOR_BACKEND, true).

% 30 seconds
-define(DEFAULT_HANDLING_TIMEOUT, 30000).

%%

-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

%% Application

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% Supervisor

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    IpEnv = genlib_app:env(?MODULE, ip, "::1"),
    Port = genlib_app:env(?MODULE, port, 8022),
    HealthCheck = genlib_app:env(?MODULE, health_check, #{}),
    WoodyOptsEnv = genlib_app:env(?MODULE, woody_opts, #{}),
    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),

    PartyClient = party_client:create_client(),
    DefaultTimeout = genlib_app:env(?MODULE, default_woody_handling_timeout, ?DEFAULT_HANDLING_TIMEOUT),
    WrapperOpts = #{
        party_client => PartyClient,
        default_handling_timeout => DefaultTimeout
    },

    {ok, Ip} = inet:parse_address(IpEnv),
    WoodyOpts = maps:with([net_opts, handler_limits], WoodyOptsEnv),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => {ff_woody_event_handler, EventHandlerOpts}},

    %% NOTE See 'sys.config'
    %% TODO Refactor after namespaces params moved from progressor'
    %% application env.
    {Backends, MachineHandlers, ModernizerHandlers} =
        lists:unzip3([contruct_backend_childspec(N, H, S, PartyClient) || {N, H, S} <- get_namespaces_params()]),
    ok = application:set_env(fistful, backends, maps:from_list(Backends)),

    Services =
        [
            {fistful_admin, ff_server_admin_handler},
            {fistful_provider, ff_provider_handler},
            {ff_withdrawal_adapter_host, ff_withdrawal_adapter_host},
            {wallet_management, ff_wallet_handler},
            {identity_management, ff_identity_handler},
            {destination_management, ff_destination_handler},
            {source_management, ff_source_handler},
            {withdrawal_management, ff_withdrawal_handler},
            {withdrawal_session_management, ff_withdrawal_session_handler},
            {deposit_management, ff_deposit_handler},
            {withdrawal_session_repairer, ff_withdrawal_session_repair},
            {withdrawal_repairer, ff_withdrawal_repair},
            {deposit_repairer, ff_deposit_repair},
            {w2w_transfer_management, ff_w2w_transfer_handler},
            {w2w_transfer_repairer, ff_w2w_transfer_repair},
            {ff_claim_committer, ff_claim_committer_handler}
        ],
    WoodyHandlers = [get_handler(Service, Handler, WrapperOpts) || {Service, Handler} <- Services],

    ServicesChildSpec = woody_server:child_spec(
        ?MODULE,
        maps:merge(
            WoodyOpts,
            #{
                ip => Ip,
                port => Port,
                handlers => WoodyHandlers,
                event_handler => ff_woody_event_handler,
                additional_routes =>
                    get_prometheus_routes() ++
                    machinery_mg_backend:get_routes(MachineHandlers, RouteOpts) ++
                    %% TODO Modernizer
                    machinery_modernizer_mg_backend:get_routes(ModernizerHandlers, RouteOpts) ++
                    [erl_health_handle:get_route(enable_health_logging(HealthCheck))]
            }
        )
    ),
    PartyClientSpec = party_client:child_spec(party_client, PartyClient),
    % TODO
    %  - Zero thoughts given while defining this strategy.
    {ok, {#{strategy => one_for_one}, [PartyClientSpec, ServicesChildSpec]}}.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_prometheus_routes() -> [{iodata(), module(), _Opts :: any()}].
get_prometheus_routes() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}].

-spec get_handler(ff_services:service_name(), woody:handler(_), map()) -> woody:http_handler(woody:th_handler()).
get_handler(Service, Handler, WrapperOpts) ->
    {Path, ServiceSpec} = ff_services:get_service_spec(Service),
    {Path, {ServiceSpec, wrap_handler(Handler, WrapperOpts)}}.

-define(PROCESSOR_OPT_PATTERN(NS, Handler, Schema), #{
    processor := #{
        client := machinery_prg_backend,
        options := #{
            namespace := NS,
            handler := {fistful, #{handler := Handler, party_client := _}},
            schema := Schema
        }
    }
}).

get_namespaces_params() ->
    {ok, Namespaces} = application:get_env(progressor, namespaces),
    lists:map(
        fun({_, ?PROCESSOR_OPT_PATTERN(NS, Handler, Schema)}) ->
            {NS, Handler, Schema}
        end,
        maps:to_list(Namespaces)
    ).

contruct_backend_childspec(NS, Handler, Schema, PartyClient) ->
    {
        construct_machinery_backend_spec(NS, Handler, Schema, PartyClient),
        construct_machinery_handler_spec(NS, Handler, Schema, PartyClient),
        construct_machinery_modernizer_spec(NS, Schema)
    }.

-ifdef(PROGRESSOR_BACKEND).
construct_machinery_backend_spec(NS, Handler, Schema, PartyClient) ->
    %% TODO Maybe support composite/chained backend for BC
    %% NOTE This repeats progressor' sys.config entries and cth'
    %% application start.
    {NS,
        {machinery_prg_backend, #{
            namespace => NS,
            handler => {fistful, #{handler => Handler, party_client => PartyClient}},
            schema => Schema
        }}}.
-else.
construct_machinery_backend_spec(NS, _Handler, Schema, _PartyClient) ->
    %% NOTE Legacy MG backend
    {NS,
        {machinery_mg_backend, #{
            schema => Schema,
            client => get_service_client(automaton)
        }}}.

get_service_client(ServiceID) ->
    case genlib_app:env(fistful, services, #{}) of
        #{ServiceID := V} ->
            ff_woody_client:new(V);
        #{} ->
            error({unknown_service, ServiceID})
    end.
-endif.

construct_machinery_handler_spec(NS, Handler, Schema, PartyClient) ->
    {{fistful, #{handler => Handler, party_client => PartyClient}}, #{
        path => ff_string:join(["/v1/stateproc/", NS]),
        backend_config => #{schema => Schema}
    }}.

construct_machinery_modernizer_spec(NS, Schema) ->
    #{
        path => ff_string:join(["/v1/modernizer/", NS]),
        backend_config => #{schema => Schema}
    }.

wrap_handler(Handler, WrapperOpts) ->
    FullOpts = maps:merge(#{handler => Handler}, WrapperOpts),
    {ff_woody_wrapper, FullOpts}.
