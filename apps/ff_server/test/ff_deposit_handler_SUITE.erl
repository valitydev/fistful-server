-module(ff_deposit_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/fistful_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/fistful_deposit_thrift.hrl").
-include_lib("fistful_proto/include/fistful_deposit_status_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_thrift.hrl").

%% Common test API

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests

-export([create_bad_amount_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_source_notfound_test/1]).
-export([create_wallet_notfound_test/1]).
-export([create_ok_test/1]).
-export([create_negative_ok_test/1]).
-export([unknown_test/1]).
-export([get_context_test/1]).
-export([get_events_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_bad_amount_test,
            create_currency_validation_error_test,
            create_source_notfound_test,
            create_wallet_notfound_test,
            create_ok_test,
            create_negative_ok_test,
            unknown_test,
            get_context_test,
            get_events_test
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_, _) ->
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%% Tests

-spec create_bad_amount_test(config()) -> test_return().
create_bad_amount_test(C) ->
    Body = make_cash({0, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = SourceID,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', {Params, #{}}),
    ExpectedError = #fistful_InvalidOperationAmount{
        amount = Body
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = make_cash({5000, <<"EUR">>}),
        source_id = SourceID,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', {Params, #{}}),
    ExpectedError = #fistful_ForbiddenOperationCurrency{
        currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"EUR">>},
        allowed_currencies = [
            #'fistful_base_CurrencyRef'{symbolic_code = <<"RUB">>},
            #'fistful_base_CurrencyRef'{symbolic_code = <<"USD">>}
        ]
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_source_notfound_test(config()) -> test_return().
create_source_notfound_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = <<"unknown_source">>,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', {Params, #{}}),
    ExpectedError = #fistful_SourceNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_wallet_notfound_test(config()) -> test_return().
create_wallet_notfound_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = SourceID,
        wallet_id = <<"unknown_wallet">>
    },
    Result = call_deposit('Create', {Params, #{}}),
    ExpectedError = #fistful_WalletNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    DepositID = generate_id(),
    ExternalID = generate_id(),
    Context = #{<<"NS">> => #{generate_id() => generate_id()}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Description = <<"testDesc">>,
    Params = #deposit_DepositParams{
        id = DepositID,
        body = Body,
        source_id = SourceID,
        wallet_id = WalletID,
        metadata = Metadata,
        external_id = ExternalID,
        description = Description
    },
    {ok, DepositState} = call_deposit('Create', {Params, ff_entity_context_codec:marshal(Context)}),
    Expected = get_deposit(DepositID),
    ?assertEqual(DepositID, DepositState#deposit_DepositState.id),
    ?assertEqual(WalletID, DepositState#deposit_DepositState.wallet_id),
    ?assertEqual(SourceID, DepositState#deposit_DepositState.source_id),
    ?assertEqual(ExternalID, DepositState#deposit_DepositState.external_id),
    ?assertEqual(Body, DepositState#deposit_DepositState.body),
    ?assertEqual(Metadata, DepositState#deposit_DepositState.metadata),
    ?assertEqual(Description, DepositState#deposit_DepositState.description),
    ?assertEqual(
        ff_deposit:domain_revision(Expected),
        DepositState#deposit_DepositState.domain_revision
    ),
    ?assertEqual(
        ff_deposit:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, DepositState#deposit_DepositState.created_at)
    ).

-spec create_negative_ok_test(config()) -> test_return().
create_negative_ok_test(C) ->
    EnvBody = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(EnvBody, C),
    _ = process_deposit(WalletID, SourceID, EnvBody),
    Body = make_cash({-100, <<"RUB">>}),
    {DepositState, DepositID, ExternalID, _} = process_deposit(WalletID, SourceID, Body),
    Expected = get_deposit(DepositID),
    ?assertEqual(DepositID, DepositState#deposit_DepositState.id),
    ?assertEqual(WalletID, DepositState#deposit_DepositState.wallet_id),
    ?assertEqual(SourceID, DepositState#deposit_DepositState.source_id),
    ?assertEqual(ExternalID, DepositState#deposit_DepositState.external_id),
    ?assertEqual(Body, DepositState#deposit_DepositState.body),
    ?assertEqual(
        ff_deposit:domain_revision(Expected),
        DepositState#deposit_DepositState.domain_revision
    ),
    ?assertEqual(
        ff_deposit:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, DepositState#deposit_DepositState.created_at)
    ).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    Result = call_deposit('Get', {DepositID, #'fistful_base_EventRange'{}}),
    ExpectedError = #fistful_DepositNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec get_context_test(config()) -> test_return().
get_context_test(C) ->
    #{
        deposit_id := DepositID,
        context := Context
    } = prepare_standard_environment_with_deposit(C),
    {ok, EncodedContext} = call_deposit('GetContext', {DepositID}),
    ?assertEqual(Context, ff_entity_context_codec:unmarshal(EncodedContext)).

-spec get_events_test(config()) -> test_return().
get_events_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(C),
    Range = {undefined, undefined},
    EncodedRange = ff_codec:marshal(event_range, Range),
    {ok, Events} = call_deposit('GetEvents', {DepositID, EncodedRange}),
    {ok, ExpectedEvents} = ff_deposit_machine:events(DepositID, Range),
    EncodedEvents = [ff_deposit_codec:marshal(event, E) || E <- ExpectedEvents],
    ?assertEqual(EncodedEvents, Events).

%% Utils

call_deposit(Fun, Args) ->
    ServiceName = deposit_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

prepare_standard_environment(Body, C) ->
    #'fistful_base_Cash'{currency = #'fistful_base_CurrencyRef'{symbolic_code = Currency}} = Body,
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    SourceID = create_source(IdentityID, C),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID
    }.

prepare_standard_environment_with_deposit(C) ->
    Body = make_cash({100, <<"RUB">>}),
    Env = prepare_standard_environment_with_deposit(Body, C),
    Env#{body => Body}.

prepare_standard_environment_with_deposit(Body, C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = Env = prepare_standard_environment(Body, C),
    {_, DepositID, ExternalID, Context} = process_deposit(WalletID, SourceID, Body),
    Env#{
        deposit_id => DepositID,
        external_id => ExternalID,
        context => Context
    }.

process_deposit(WalletID, SourceID, Body) ->
    DepositID = generate_id(),
    ExternalID = generate_id(),
    Context = #{<<"NS">> => #{generate_id() => generate_id()}},
    EncodedContext = ff_entity_context_codec:marshal(Context),
    Params = #deposit_DepositParams{
        id = DepositID,
        wallet_id = WalletID,
        source_id = SourceID,
        body = Body,
        external_id = ExternalID
    },
    {ok, DepositState} = call_deposit('Create', {Params, EncodedContext}),
    succeeded = await_final_deposit_status(DepositID),
    {DepositState, DepositID, ExternalID, Context}.

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_deposit_status(DepositID) ->
    ff_deposit:status(get_deposit(DepositID)).

await_final_deposit_status(DepositID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            case ff_deposit:is_finished(Deposit) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_deposit_status(DepositID).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, C).

create_identity(Party, ProviderID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, C).

create_identity(Party, Name, ProviderID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID},
        #{<<"com.valitydev.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency},
        ff_entity_context:new()
    ),
    ID.

await_wallet_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun() -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_accounting:balance(Account),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_source(IID, _C) ->
    ID = generate_id(),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{
        id => ID,
        identity => IID,
        name => <<"XSource">>,
        currency => <<"RUB">>,
        resource => SrcResource
    },
    ok = ff_source_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, SrcM} = ff_source_machine:get(ID),
            Source = ff_source_machine:source(SrcM),
            ff_source:status(Source)
        end
    ),
    ID.

make_cash({Amount, Currency}) ->
    #'fistful_base_Cash'{
        amount = Amount,
        currency = #'fistful_base_CurrencyRef'{symbolic_code = Currency}
    }.
