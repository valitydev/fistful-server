%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.

-module(ff_destination).

-type id() :: binary().
-type name() :: binary().
-type account() :: ff_account:account().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type status() :: unauthorized | authorized.
-type metadata() :: ff_entity_context:md().
-type timestamp() :: ff_time:timestamp_ms().

-type resource_params() :: ff_resource:resource_params().
-type resource() :: ff_resource:resource().

-type exp_date() :: {integer(), integer()}.

-define(ACTUAL_FORMAT_VERSION, 5).

-type destination() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    resource := resource(),
    name := name(),
    created_at => timestamp(),
    external_id => id(),
    metadata => metadata()
}.

-type destination_state() :: #{
    account := account() | undefined,
    resource := resource(),
    name := name(),
    status => status(),
    created_at => timestamp(),
    external_id => id(),
    metadata => metadata()
}.

-type params() :: #{
    id := id(),
    identity := ff_identity:id(),
    name := name(),
    currency := ff_currency:id(),
    resource := resource_params(),
    external_id => id(),
    metadata => metadata()
}.

-type event() ::
    {created, destination()}
    | {account, ff_account:event()}
    | {status_changed, status()}.

-type legacy_event() :: any().

-type create_error() ::
    {identity, notfound}
    | {currency, notfound}
    | ff_account:create_error()
    | {terms, ff_party:withdrawal_method_validation_error()}
    | {identity, ff_party:inaccessibility()}.

-export_type([id/0]).
-export_type([destination/0]).
-export_type([destination_state/0]).
-export_type([status/0]).
-export_type([resource_params/0]).
-export_type([resource/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([create_error/0]).
-export_type([exp_date/0]).

%% Accessors

-export([id/1]).
-export([name/1]).
-export([account/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([metadata/1]).

%% API

-export([create/1]).
-export([is_accessible/1]).
-export([authorize/1]).
-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(destination_state()) -> id() | undefined.
-spec name(destination_state()) -> name().
-spec account(destination_state()) -> account() | undefined.
-spec identity(destination_state()) -> identity().
-spec currency(destination_state()) -> currency().
-spec resource(destination_state()) -> resource().
-spec status(destination_state()) -> status() | undefined.

id(Destination) ->
    case account(Destination) of
        undefined ->
            undefined;
        Account ->
            ff_account:id(Account)
    end.

name(#{name := V}) ->
    V.

account(#{account := V}) ->
    V;
account(_) ->
    undefined.

identity(Destination) ->
    ff_account:identity(account(Destination)).

currency(Destination) ->
    ff_account:currency(account(Destination)).

resource(#{resource := V}) ->
    V.

status(#{status := V}) ->
    V;
status(_) ->
    undefined.

-spec external_id(destination_state()) -> id() | undefined.
external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Destination) ->
    undefined.

-spec created_at(destination_state()) -> ff_time:timestamp_ms() | undefined.
created_at(#{created_at := CreatedAt}) ->
    CreatedAt;
created_at(_Destination) ->
    undefined.

-spec metadata(destination_state()) -> ff_entity_context:context() | undefined.
metadata(#{metadata := Metadata}) ->
    Metadata;
metadata(_Destination) ->
    undefined.

%% API

-spec create(params()) ->
    {ok, [event()]}
    | {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{
            id := ID,
            identity := IdentityID,
            name := Name,
            currency := CurrencyID,
            resource := Resource
        } = Params,
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        accessible = unwrap(identity, ff_identity:is_accessible(Identity)),
        valid = ff_resource:check_resource(Resource),
        CreatedAt = ff_time:now(),
        Method = ff_resource:method(Resource),
        Terms = ff_identity:get_terms(Identity, #{timestamp => CreatedAt}),
        valid = unwrap(terms, ff_party:validate_destination_creation(Terms, Method)),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Events = unwrap(ff_account:create(ID, Identity, Currency)),
        [
            {created,
                genlib_map:compact(#{
                    version => ?ACTUAL_FORMAT_VERSION,
                    name => Name,
                    resource => Resource,
                    external_id => maps:get(external_id, Params, undefined),
                    metadata => maps:get(metadata, Params, undefined),
                    created_at => CreatedAt
                })}
        ] ++
            [{account, Ev} || Ev <- Events] ++
            [{status_changed, unauthorized}]
    end).

-spec is_accessible(destination_state()) ->
    {ok, accessible}
    | {error, ff_party:inaccessibility()}.
is_accessible(Destination) ->
    ff_account:is_accessible(account(Destination)).

-spec authorize(destination_state()) -> {ok, [event()]}.
authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec apply_event(event(), ff_maybe:maybe(destination_state())) -> destination_state().
apply_event({created, Destination}, undefined) ->
    Destination;
apply_event({status_changed, S}, Destination) ->
    Destination#{status => S};
apply_event({account, Ev}, Destination = #{account := Account}) ->
    Destination#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Destination) ->
    apply_event({account, Ev}, Destination#{account => undefined}).

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) -> event().
maybe_migrate(Event = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Event;
maybe_migrate(Event, _MigrateParams) ->
    Event.
