%%%
%%% Keep woody well typed

-module(ff_woody_client).

%%

-type url() :: woody:url().
-type event_handler() :: woody:ev_handler().
-type transport_opts() :: woody_client_thrift_http_transport:transport_options().
-type context() :: woody_context:ctx().

-type service_id() :: atom().

-type client() :: #{
    url := url(),
    event_handler := event_handler(),
    transport_opts => transport_opts()
}.

-type caller() :: #{
    client := client(),
    context => context()
}.

-type request() :: woody:request().

-export_type([client/0]).
-export_type([caller/0]).

-export([new/1]).
-export([call/2]).
-export([call/3]).

%%

-type opts() :: #{
    url := url(),
    event_handler => event_handler(),
    transport_opts => transport_opts()
}.

-spec new(woody:url() | opts()) -> client().
new(#{url := _} = Opts) ->
    EventHandlerOpts = genlib_app:env(ff_server, scoper_event_handler_options, #{}),
    maps:merge(
        #{
            event_handler => {ff_woody_event_handler, EventHandlerOpts}
        },
        maps:with([url, event_handler, transport_opts], Opts)
    );
new(Url) when is_binary(Url); is_list(Url) ->
    new(#{
        url => genlib:to_binary(Url)
    }).

-spec call(service_id() | client(), request()) ->
    {ok, woody:result()}
    | {exception, woody_error:business_error()}.
call(ServiceIdOrClient, Request) ->
    call(ServiceIdOrClient, Request, ff_context:get_woody_context(ff_context:load())).

-spec call(service_id() | client(), request(), woody_context:ctx()) ->
    {ok, woody:result()}
    | {exception, woody_error:business_error()}.
call(ServiceID, Request, Context) when is_atom(ServiceID) ->
    call(get_service_client(ServiceID), Request, Context);
call(Client, Request, Context) when is_map(Client) ->
    woody_client:call(Request, Client, Context).

%%

get_service_client(ServiceID) ->
    case maps:find(ServiceID, genlib_app:env(fistful, services, #{})) of
        {ok, V} ->
            new(V);
        error ->
            error({'woody service undefined', ServiceID})
    end.
