%%%
%%% Identity
%%%
%%% Essentially a contract + a number of identity claims.
%%%  * What Payment Institution? Why does it matter?
%%%
%%% We should know:
%%%  * What are the fees?
%%%  * What are the limits?
%%%  * Who will sell us e-money? This is a party + shop pair probably.
%%%  * Who will provide us withdrawals? This is a party + shop pair probably.
%%%

-module(ff_identity).

%% API

-type id() :: binary().
-type name() :: binary().
-type external_id() :: id() | undefined.
-type party_id() :: ff_party:id().
-type provider_id() :: ff_provider:id().
-type contract_id() :: ff_party:contract_id().
-type blocking() :: unblocked | blocked.
-type metadata() :: ff_entity_context:md().

-define(ACTUAL_FORMAT_VERSION, 2).

-type identity_state() :: #{
    id := id(),
    name := name(),
    party := party_id(),
    provider := provider_id(),
    contract := contract_id(),
    external_id => id(),
    blocking => blocking(),
    metadata => metadata(),
    created_at => ff_time:timestamp_ms()
}.

-type identity() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    name := name(),
    party := party_id(),
    provider := provider_id(),
    contract := contract_id(),
    external_id => id(),
    metadata => metadata(),
    created_at => ff_time:timestamp_ms()
}.

-type event() ::
    {created, identity()}
    | {level_changed, level_id()}
    | {effective_challenge_changed, challenge_id()}
    | {{challenge, challenge_id()}, challenge_event()}.

-type level_id() :: binary().
-type challenge_id() :: id().
-type challenge_event() ::
    {created, any()}
    | {status_changed, any()}.

-type params() :: #{
    id := id(),
    name := name(),
    party := ff_party:id(),
    provider := ff_provider:id(),
    external_id => id(),
    metadata => metadata()
}.

-type check_params() :: #{
    party := ff_party:id(),
    provider := ff_provider:id()
}.

-type create_error() ::
    {provider, notfound}
    | {party, notfound | ff_party:inaccessibility()}
    | invalid.

-type check_error() ::
    {provider, notfound}
    | {party, notfound | ff_party:inaccessibility()}.

-type get_terms_params() :: #{
    party_revision => ff_party:revision(),
    domain_revision => ff_domain_config:revision(),
    timestamp => ff_time:timestamp_ms(),
    varset => ff_varset:varset()
}.

-export_type([identity/0]).
-export_type([identity_state/0]).
-export_type([event/0]).
-export_type([id/0]).
-export_type([create_error/0]).
-export_type([params/0]).
-export_type([get_terms_params/0]).

-export([id/1]).
-export([name/1]).
-export([provider/1]).
-export([party/1]).
-export([contract/1]).
-export([external_id/1]).
-export([blocking/1]).
-export([created_at/1]).
-export([metadata/1]).

-export([is_accessible/1]).
-export([set_blocking/1]).

-export([create/1]).
-export([get_withdrawal_methods/1]).
-export([get_withdrawal_methods/2]).
-export([get_terms/2]).
-export([check_identity_creation/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(identity_state()) -> id().
-spec name(identity_state()) -> name().
-spec provider(identity_state()) -> provider_id().
-spec party(identity_state()) -> party_id().
-spec contract(identity_state()) -> contract_id().
-spec blocking(identity_state()) -> boolean() | undefined.
-spec external_id(identity_state()) -> external_id().
-spec created_at(identity_state()) -> ff_time:timestamp_ms() | undefined.
-spec metadata(identity_state()) -> metadata() | undefined.

id(#{id := V}) ->
    V.

name(#{name := V}) ->
    V.

provider(#{provider := V}) ->
    V.

party(#{party := V}) ->
    V.

contract(#{contract := V}) ->
    V.

blocking(Identity) ->
    maps:get(blocking, Identity, undefined).

external_id(Identity) ->
    maps:get(external_id, Identity, undefined).

created_at(Identity) ->
    maps:get(created_at, Identity, undefined).

metadata(Identity) ->
    maps:get(metadata, Identity, undefined).

-spec is_accessible(identity_state()) ->
    {ok, accessible}
    | {error, ff_party:inaccessibility()}.
is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

-spec set_blocking(identity_state()) -> identity_state().
set_blocking(Identity) ->
    Blocking =
        case {ok, accessible} =:= is_accessible(Identity) of
            true ->
                unblocked;
            false ->
                blocked
        end,
    maps:put(blocking, Blocking, Identity).

%% Constructor

-spec create(params()) ->
    {ok, [event()]}
    | {error, create_error()}.
create(#{id := ID, name := Name, party := Party, provider := ProviderID} = Params) ->
    do(fun() ->
        Provider = unwrap(check_identity_creation(#{party => Party, provider => ProviderID})),
        Contract = unwrap(
            ff_party:create_contract(Party, #{
                payinst => ff_provider:payinst(Provider),
                contract_template => ff_provider:contract_template(Provider),
                contractor_level => ff_provider:contractor_level(Provider)
            })
        ),
        [
            {created,
                genlib_map:compact(#{
                    version => ?ACTUAL_FORMAT_VERSION,
                    id => ID,
                    name => Name,
                    party => Party,
                    provider => ProviderID,
                    contract => Contract,
                    created_at => ff_time:now(),
                    external_id => maps:get(external_id, Params, undefined),
                    metadata => maps:get(metadata, Params, undefined)
                })}
        ]
    end).

-spec check_identity_creation(check_params()) ->
    {ok, ff_provider:provider()}
    | {error, check_error()}.

check_identity_creation(#{party := Party, provider := ProviderID}) ->
    do(fun() ->
        accessible = unwrap(party, ff_party:is_accessible(Party)),
        unwrap(provider, ff_provider:get(ProviderID))
    end).

-spec get_withdrawal_methods(identity_state()) ->
    ordsets:ordset(ff_party:method_ref()).
get_withdrawal_methods(Identity) ->
    get_withdrawal_methods(Identity, #{}).

-spec get_withdrawal_methods(identity_state(), get_terms_params()) ->
    ordsets:ordset(ff_party:method_ref()).
get_withdrawal_methods(Identity, Params) ->
    ff_party:get_withdrawal_methods(get_terms(Identity, Params)).

-spec get_terms(identity_state(), get_terms_params()) ->
    ff_party:terms().
get_terms(Identity, Params) ->
    PartyID = ff_identity:party(Identity),
    ContractID = ff_identity:contract(Identity),
    PartyRevision =
        case maps:get(party_revision, Params, undefined) of
            Revision when Revision =/= undefined ->
                Revision;
            _ ->
                {ok, PartyRevisionDef} = ff_party:get_revision(PartyID),
                PartyRevisionDef
        end,
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID,
        ContractID,
        maps:get(varset, Params, #{}),
        maps:get(timestamp, Params, ff_time:now()),
        PartyRevision,
        maps:get(domain_revision, Params, ff_domain_config:head())
    ),
    Terms.

%%

-spec apply_event(event(), ff_maybe:'maybe'(identity_state())) -> identity_state().
apply_event({created, Identity}, undefined) ->
    Identity;
apply_event({level_changed, _L}, Identity) ->
    Identity;
apply_event({effective_challenge_changed, _ID}, Identity) ->
    Identity;
apply_event({{challenge, _ID}, _Ev}, Identity) ->
    Identity.
