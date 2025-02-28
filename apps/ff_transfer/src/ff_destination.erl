%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.

-module(ff_destination).

-type id() :: binary().
-type token() :: binary().
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
    metadata => metadata(),
    auth_data => auth_data()
}.

-type destination_state() :: #{
    account := account() | undefined,
    resource := resource(),
    name := name(),
    status => status(),
    created_at => timestamp(),
    external_id => id(),
    metadata => metadata(),
    auth_data => auth_data()
}.

-type params() :: #{
    id := id(),
    identity := ff_identity:id(),
    name := name(),
    currency := ff_currency:id(),
    resource := resource_params(),
    external_id => id(),
    metadata => metadata(),
    auth_data => auth_data()
}.

-type auth_data() :: #{
    sender := token(),
    receiver := token()
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
-export_type([auth_data/0]).

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
-export([auth_data/1]).

%% API

-export([create/1]).
-export([is_accessible/1]).
-export([authorize/1]).
-export([apply_event/2]).
-export([maybe_migrate/2]).
-export([maybe_migrate_resource/1]).

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

-spec auth_data(destination_state()) -> auth_data() | undefined.
auth_data(#{auth_data := AuthData}) ->
    AuthData;
auth_data(_Destination) ->
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
                    auth_data => maps:get(auth_data, Params, undefined),
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

-spec apply_event(event(), ff_maybe:'maybe'(destination_state())) -> destination_state().
apply_event({created, Destination}, undefined) ->
    Destination;
apply_event({status_changed, S}, Destination) ->
    Destination#{status => S};
apply_event({account, Ev}, #{account := Account} = Destination) ->
    Destination#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Destination) ->
    apply_event({account, Ev}, Destination#{account => undefined}).

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) -> event().
maybe_migrate({created, #{version := ?ACTUAL_FORMAT_VERSION}} = Event, _MigrateParams) ->
    Event;
maybe_migrate({created, Destination = #{version := 4}}, MigrateParams) ->
    maybe_migrate(
        {created, Destination#{
            version => 5
        }},
        MigrateParams
    );
maybe_migrate({created, Destination = #{version := 3, name := Name}}, MigrateParams) ->
    maybe_migrate(
        {created, Destination#{
            version => 4,
            name => maybe_migrate_name(Name)
        }},
        MigrateParams
    );
maybe_migrate({created, Destination = #{version := 2}}, MigrateParams) ->
    Context = maps:get(ctx, MigrateParams, undefined),
    %% TODO add metada migration for eventsink after decouple instruments
    Metadata = ff_entity_context:try_get_legacy_metadata(Context),
    maybe_migrate(
        {created,
            genlib_map:compact(Destination#{
                version => 3,
                metadata => Metadata
            })},
        MigrateParams
    );
maybe_migrate({created, Destination = #{version := 1}}, MigrateParams) ->
    Timestamp = maps:get(timestamp, MigrateParams),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate(
        {created, Destination#{
            version => 2,
            created_at => CreatedAt
        }},
        MigrateParams
    );
maybe_migrate(
    {created,
        Destination = #{
            resource := Resource,
            name := Name
        }},
    MigrateParams
) ->
    NewDestination = genlib_map:compact(#{
        version => 1,
        resource => maybe_migrate_resource(Resource),
        name => Name,
        external_id => maps:get(external_id, Destination, undefined)
    }),
    maybe_migrate({created, NewDestination}, MigrateParams);
%% Other events
maybe_migrate(Event, _MigrateParams) ->
    Event.

-spec maybe_migrate_resource(any()) -> any().
maybe_migrate_resource({crypto_wallet, #{id := ID, currency := ripple, tag := Tag}}) ->
    maybe_migrate_resource({crypto_wallet, #{id => ID, currency => {ripple, #{tag => Tag}}}});
maybe_migrate_resource({crypto_wallet, #{id := ID, currency := Currency}}) when is_atom(Currency) ->
    maybe_migrate_resource({crypto_wallet, #{id => ID, currency => {Currency, #{}}}});
maybe_migrate_resource({crypto_wallet, #{id := _ID} = CryptoWallet}) ->
    maybe_migrate_resource({crypto_wallet, #{crypto_wallet => CryptoWallet}});
maybe_migrate_resource({bank_card, #{token := _Token} = BankCard}) ->
    maybe_migrate_resource({bank_card, #{bank_card => BankCard}});
maybe_migrate_resource(Resource) ->
    Resource.

maybe_migrate_name(Name) ->
    re:replace(Name, "\\d{12,19}", <<"">>, [global, {return, binary}]).

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec v1_created_migration_test() -> _.

v1_created_migration_test() ->
    CreatedAt = ff_time:now(),
    LegacyEvent =
        {created, #{
            version => 1,
            resource => {crypto_wallet, #{crypto_wallet => #{}}},
            name => <<"some name">>,
            external_id => genlib:unique()
        }},

    {created, #{version := Version}} = maybe_migrate(LegacyEvent, #{
        timestamp => ff_codec:unmarshal(timestamp, ff_codec:marshal(timestamp_ms, CreatedAt))
    }),
    ?assertEqual(5, Version).

-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    CreatedAt = ff_time:now(),
    LegacyEvent =
        {created, #{
            version => 2,
            resource => {crypto_wallet, #{crypto_wallet => #{}}},
            name => <<"some name">>,
            external_id => genlib:unique(),
            created_at => CreatedAt
        }},
    {created, #{version := Version, metadata := Metadata}} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.valitydev.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(5, Version),
    ?assertEqual(#{<<"some key">> => <<"some val">>}, Metadata).

-spec v4_created_migration_new_test() -> _.
v4_created_migration_new_test() ->
    CreatedAt = ff_time:now(),
    BankCard = #{
        token => <<"token">>,
        payment_system => #{id => <<"VISA">>},
        bin => <<"12345">>,
        masked_pan => <<"7890">>,
        bank_name => <<"bank">>,
        issuer_country => zmb,
        card_type => credit_or_debit,
        exp_date => {12, 3456},
        cardholder_name => <<"name">>,
        bin_data_id => #{<<"foo">> => 1}
    },
    LegacyEvent =
        {created, #{
            version => 4,
            resource => {bank_card, #{bank_card => BankCard}},
            name => <<"some name">>,
            external_id => genlib:unique(),
            created_at => CreatedAt
        }},
    {created, #{version := Version, resource := {bank_card, #{bank_card := NewBankCard}}}} = maybe_migrate(
        LegacyEvent, #{}
    ),
    ?assertEqual(5, Version),
    ?assertEqual(#{id => <<"VISA">>}, maps:get(payment_system, NewBankCard)).

-spec name_migration_test() -> _.
name_migration_test() ->
    ?assertEqual(<<"sd">>, maybe_migrate_name(<<"sd123123123123123">>)),
    ?assertEqual(<<"sd1231231231sd23123">>, maybe_migrate_name(<<"sd1231231231sd23123">>)),
    ?assertEqual(<<"sdds123sd">>, maybe_migrate_name(<<"sd123123123123ds123sd">>)),
    ?assertEqual(<<"sdsd">>, maybe_migrate_name(<<"sd123123123123123sd">>)),
    ?assertEqual(<<"sd">>, maybe_migrate_name(<<"123123123123123sd">>)).

-endif.
