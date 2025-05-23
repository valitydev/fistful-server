-module(ff_withdrawal_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/fistful_wthd_thrift.hrl").
-include_lib("fistful_proto/include/fistful_wthd_adj_thrift.hrl").
-include_lib("fistful_proto/include/fistful_wthd_status_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("fistful_proto/include/fistful_cashflow_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([session_unknown_test/1]).
-export([session_get_context_test/1]).
-export([session_get_events_test/1]).
-export([create_withdrawal_and_get_session_ok_test/1]).

-export([create_withdrawal_ok_test/1]).
-export([create_cashlimit_validation_error_test/1]).
-export([create_inconsistent_currency_validation_error_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_destination_resource_no_bindata_ok_test/1]).
-export([create_destination_resource_no_bindata_fail_test/1]).
-export([create_destination_notfound_test/1]).
-export([create_destination_generic_ok_test/1]).
-export([create_wallet_notfound_test/1]).
-export([unknown_test/1]).
-export([get_context_test/1]).
-export([get_events_test/1]).
-export([create_adjustment_ok_test/1]).
-export([create_adjustment_unavailable_status_error_test/1]).
-export([create_adjustment_already_has_status_error_test/1]).
-export([create_adjustment_already_has_data_revision_error_test/1]).
-export([withdrawal_state_content_test/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            session_unknown_test,
            session_get_context_test,
            session_get_events_test,
            create_withdrawal_and_get_session_ok_test,

            create_withdrawal_ok_test,
            create_cashlimit_validation_error_test,
            create_currency_validation_error_test,
            create_inconsistent_currency_validation_error_test,
            create_destination_resource_no_bindata_ok_test,
            create_destination_resource_no_bindata_fail_test,
            create_destination_notfound_test,
            create_destination_generic_ok_test,
            create_wallet_notfound_test,
            unknown_test,
            get_context_test,
            get_events_test,
            create_adjustment_ok_test,
            create_adjustment_unavailable_status_error_test,
            create_adjustment_already_has_status_error_test,
            create_adjustment_already_has_data_revision_error_test,
            withdrawal_state_content_test
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

-spec create_withdrawal_and_get_session_ok_test(config()) -> test_return().
create_withdrawal_and_get_session_ok_test(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, _WithdrawalState} = call_withdrawal('Create', {Params, Ctx}),

    succeeded = await_final_withdrawal_status(WithdrawalID),
    {ok, FinalWithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    [#wthd_SessionState{id = SessionID} | _Rest] = FinalWithdrawalState#wthd_WithdrawalState.sessions,
    {ok, _Session} = call_withdrawal_session('Get', {SessionID, #'fistful_base_EventRange'{}}).

-spec session_get_context_test(config()) -> test_return().
session_get_context_test(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, _WithdrawalState} = call_withdrawal('Create', {Params, Ctx}),

    succeeded = await_final_withdrawal_status(WithdrawalID),
    {ok, FinalWithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    [#wthd_SessionState{id = SessionID} | _Rest] = FinalWithdrawalState#wthd_WithdrawalState.sessions,
    {ok, _Session} = call_withdrawal_session('GetContext', {SessionID}).

-spec session_get_events_test(config()) -> test_return().
session_get_events_test(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, _WithdrawalState} = call_withdrawal('Create', {Params, Ctx}),

    succeeded = await_final_withdrawal_status(WithdrawalID),
    {ok, FinalWithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    [#wthd_SessionState{id = SessionID} | _Rest] = FinalWithdrawalState#wthd_WithdrawalState.sessions,

    Range = {undefined, undefined},
    EncodedRange = ff_codec:marshal(event_range, Range),
    {ok, Events} = call_withdrawal_session('GetEvents', {SessionID, EncodedRange}),
    {ok, ExpectedEvents} = ff_withdrawal_session_machine:events(SessionID, Range),
    EncodedEvents = lists:map(fun ff_withdrawal_session_codec:marshal_event/1, ExpectedEvents),
    ?assertEqual(EncodedEvents, Events).

-spec session_unknown_test(config()) -> test_return().
session_unknown_test(_C) ->
    WithdrawalSessionID = <<"unknown_withdrawal_session">>,
    Result = call_withdrawal_session('Get', {WithdrawalSessionID, #'fistful_base_EventRange'{}}),
    ExpectedError = #fistful_WithdrawalSessionNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_withdrawal_ok_test(config()) -> test_return().
create_withdrawal_ok_test(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, WithdrawalState} = call_withdrawal('Create', {Params, Ctx}),

    Expected = get_withdrawal(WithdrawalID),
    ?assertEqual(WithdrawalID, WithdrawalState#wthd_WithdrawalState.id),
    ?assertEqual(ExternalID, WithdrawalState#wthd_WithdrawalState.external_id),
    ?assertEqual(WalletID, WithdrawalState#wthd_WithdrawalState.wallet_id),
    ?assertEqual(DestinationID, WithdrawalState#wthd_WithdrawalState.destination_id),
    ?assertEqual(Cash, WithdrawalState#wthd_WithdrawalState.body),
    ?assertEqual(Metadata, WithdrawalState#wthd_WithdrawalState.metadata),
    ?assertEqual(
        ff_withdrawal:domain_revision(Expected),
        WithdrawalState#wthd_WithdrawalState.domain_revision
    ),
    ?assertEqual(
        ff_withdrawal:party_revision(Expected),
        WithdrawalState#wthd_WithdrawalState.party_revision
    ),
    ?assertEqual(
        ff_withdrawal:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, WithdrawalState#wthd_WithdrawalState.created_at)
    ),

    succeeded = await_final_withdrawal_status(WithdrawalID),
    {ok, FinalWithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    ?assertMatch(
        {succeeded, _},
        FinalWithdrawalState#wthd_WithdrawalState.status
    ).

-spec create_cashlimit_validation_error_test(config()) -> test_return().
create_cashlimit_validation_error_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = make_cash({20000000, <<"RUB">>})
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ExpectedError = #fistful_ForbiddenOperationAmount{
        amount = make_cash({20000000, <<"RUB">>}),
        allowed_range = #'fistful_base_CashRange'{
            lower = {inclusive, make_cash({0, <<"RUB">>})},
            upper = {exclusive, make_cash({10000001, <<"RUB">>})}
        }
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    Cash = make_cash({100, <<"USD">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ExpectedError = #fistful_ForbiddenOperationCurrency{
        currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"USD">>},
        allowed_currencies = [
            #'fistful_base_CurrencyRef'{symbolic_code = <<"RUB">>}
        ]
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_inconsistent_currency_validation_error_test(config()) -> test_return().
create_inconsistent_currency_validation_error_test(C) ->
    Cash = make_cash({100, <<"USD">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"USD_CURRENCY">>, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = make_cash({100, <<"RUB">>})
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ExpectedError = #wthd_InconsistentWithdrawalCurrency{
        withdrawal_currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"RUB">>},
        destination_currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"USD">>},
        wallet_currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"USD">>}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_destination_resource_no_bindata_fail_test(config()) -> test_return().
create_destination_resource_no_bindata_fail_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"TEST_NOTFOUND">>, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash
    },
    ?assertError(
        {woody_error, {external, result_unexpected, _}},
        call_withdrawal('Create', {Params, #{}})
    ).

-spec create_destination_resource_no_bindata_ok_test(config()) -> test_return().
create_destination_resource_no_bindata_ok_test(C) ->
    %% As per test terms this specific cash amount results in valid cashflow without bin data
    Cash = make_cash({424242, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"TEST_NOTFOUND">>, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ?assertMatch({ok, _}, Result).

-spec create_destination_notfound_test(config()) -> test_return().
create_destination_notfound_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Cash, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = WalletID,
        destination_id = <<"unknown_destination">>,
        body = Cash
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ExpectedError = #fistful_DestinationNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_destination_generic_ok_test(config()) -> test_return().
create_destination_generic_ok_test(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        identity_id := IdentityID
    } = prepare_standard_environment(Cash, C),
    DestinationID = create_generic_destination(<<"IND">>, IdentityID, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, WithdrawalState} = call_withdrawal('Create', {Params, Ctx}),

    Expected = get_withdrawal(WithdrawalID),
    ?assertEqual(WithdrawalID, WithdrawalState#wthd_WithdrawalState.id),
    ?assertEqual(ExternalID, WithdrawalState#wthd_WithdrawalState.external_id),
    ?assertEqual(WalletID, WithdrawalState#wthd_WithdrawalState.wallet_id),
    ?assertEqual(DestinationID, WithdrawalState#wthd_WithdrawalState.destination_id),
    ?assertEqual(Cash, WithdrawalState#wthd_WithdrawalState.body),
    ?assertEqual(Metadata, WithdrawalState#wthd_WithdrawalState.metadata),
    ?assertEqual(
        ff_withdrawal:domain_revision(Expected),
        WithdrawalState#wthd_WithdrawalState.domain_revision
    ),
    ?assertEqual(
        ff_withdrawal:party_revision(Expected),
        WithdrawalState#wthd_WithdrawalState.party_revision
    ),
    ?assertEqual(
        ff_withdrawal:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, WithdrawalState#wthd_WithdrawalState.created_at)
    ),

    succeeded = await_final_withdrawal_status(WithdrawalID),
    {ok, FinalWithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    ?assertMatch(
        {succeeded, _},
        FinalWithdrawalState#wthd_WithdrawalState.status
    ).

-spec create_wallet_notfound_test(config()) -> test_return().
create_wallet_notfound_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    Params = #wthd_WithdrawalParams{
        id = generate_id(),
        wallet_id = <<"unknown_wallet">>,
        destination_id = DestinationID,
        body = Cash
    },
    Result = call_withdrawal('Create', {Params, #{}}),
    ExpectedError = #fistful_WalletNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    WithdrawalID = <<"unknown_withdrawal">>,
    Result = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    ExpectedError = #fistful_WithdrawalNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec get_context_test(config()) -> test_return().
get_context_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        context := Context
    } = prepare_standard_environment_with_withdrawal(C),
    {ok, EncodedContext} = call_withdrawal('GetContext', {WithdrawalID}),
    ?assertEqual(Context, ff_entity_context_codec:unmarshal(EncodedContext)).

-spec get_events_test(config()) -> test_return().
get_events_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    Range = {undefined, undefined},
    EncodedRange = ff_codec:marshal(event_range, Range),
    {ok, Events} = call_withdrawal('GetEvents', {WithdrawalID, EncodedRange}),
    {ok, ExpectedEvents} = ff_withdrawal_machine:events(WithdrawalID, Range),
    EncodedEvents = lists:map(fun ff_withdrawal_codec:marshal_event/1, ExpectedEvents),
    ?assertEqual(EncodedEvents, Events).

-spec create_adjustment_ok_test(config()) -> test_return().
create_adjustment_ok_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    AdjustmentID = generate_id(),
    ExternalID = generate_id(),
    Params = #wthd_adj_AdjustmentParams{
        id = AdjustmentID,
        change =
            {change_status, #wthd_adj_ChangeStatusRequest{
                new_status = {failed, #wthd_status_Failed{failure = #'fistful_base_Failure'{code = <<"Ooops">>}}}
            }},
        external_id = ExternalID
    },
    {ok, AdjustmentState} = call_withdrawal('CreateAdjustment', {WithdrawalID, Params}),
    ExpectedAdjustment = get_adjustment(WithdrawalID, AdjustmentID),

    ?assertEqual(AdjustmentID, AdjustmentState#wthd_adj_AdjustmentState.id),
    ?assertEqual(ExternalID, AdjustmentState#wthd_adj_AdjustmentState.external_id),
    ?assertEqual(
        ff_adjustment:created_at(ExpectedAdjustment),
        ff_codec:unmarshal(timestamp_ms, AdjustmentState#wthd_adj_AdjustmentState.created_at)
    ),
    ?assertEqual(
        ff_adjustment:domain_revision(ExpectedAdjustment),
        AdjustmentState#wthd_adj_AdjustmentState.domain_revision
    ),
    ?assertEqual(
        ff_adjustment:party_revision(ExpectedAdjustment),
        AdjustmentState#wthd_adj_AdjustmentState.party_revision
    ),
    ?assertEqual(
        ff_withdrawal_adjustment_codec:marshal(changes_plan, ff_adjustment:changes_plan(ExpectedAdjustment)),
        AdjustmentState#wthd_adj_AdjustmentState.changes_plan
    ).

-spec create_adjustment_unavailable_status_error_test(config()) -> test_return().
create_adjustment_unavailable_status_error_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    Params = #wthd_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #wthd_adj_ChangeStatusRequest{
                new_status = {pending, #wthd_status_Pending{}}
            }}
    },
    Result = call_withdrawal('CreateAdjustment', {WithdrawalID, Params}),
    ExpectedError = #wthd_ForbiddenStatusChange{
        target_status = {pending, #wthd_status_Pending{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_adjustment_already_has_status_error_test(config()) -> test_return().
create_adjustment_already_has_status_error_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    Params = #wthd_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #wthd_adj_ChangeStatusRequest{
                new_status = {succeeded, #wthd_status_Succeeded{}}
            }}
    },
    Result = call_withdrawal('CreateAdjustment', {WithdrawalID, Params}),
    ExpectedError = #wthd_AlreadyHasStatus{
        withdrawal_status = {succeeded, #wthd_status_Succeeded{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_adjustment_already_has_data_revision_error_test(config()) -> test_return().
create_adjustment_already_has_data_revision_error_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    Withdrawal = get_withdrawal(WithdrawalID),
    DomainRevision = ff_withdrawal:domain_revision(Withdrawal),
    Params = #wthd_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_cash_flow, #wthd_adj_ChangeCashFlowRequest{
                domain_revision = DomainRevision
            }}
    },
    Result = call_withdrawal('CreateAdjustment', {WithdrawalID, Params}),
    ExpectedError = #wthd_AlreadyHasDataRevision{
        domain_revision = DomainRevision
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec withdrawal_state_content_test(config()) -> test_return().
withdrawal_state_content_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment_with_withdrawal(C),
    Params = #wthd_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #wthd_adj_ChangeStatusRequest{
                new_status = {failed, #wthd_status_Failed{failure = #'fistful_base_Failure'{code = <<"Ooops">>}}}
            }}
    },
    {ok, _AdjustmentState} = call_withdrawal('CreateAdjustment', {WithdrawalID, Params}),
    {ok, WithdrawalState} = call_withdrawal('Get', {WithdrawalID, #'fistful_base_EventRange'{}}),
    ?assertMatch([_], WithdrawalState#wthd_WithdrawalState.sessions),
    ?assertMatch([_], WithdrawalState#wthd_WithdrawalState.adjustments),
    ?assertNotEqual(
        #cashflow_FinalCashFlow{postings = []},
        WithdrawalState#wthd_WithdrawalState.effective_final_cash_flow
    ),
    ?assertNotEqual(undefined, WithdrawalState#wthd_WithdrawalState.effective_route),
    ?assertNotEqual(undefined, WithdrawalState#wthd_WithdrawalState.status).

%%  Internals

call_withdrawal_session(Fun, Args) ->
    ServiceName = withdrawal_session_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

call_withdrawal(Fun, Args) ->
    ServiceName = withdrawal_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

prepare_standard_environment(Body, C) ->
    prepare_standard_environment(Body, undefined, C).

prepare_standard_environment(Body, Token, C) ->
    prepare_standard_environment(Body, Token, #{}, C).

prepare_standard_environment(Body, Token, AuthData, C) ->
    #'fistful_base_Cash'{
        amount = Amount,
        currency = #'fistful_base_CurrencyRef'{symbolic_code = Currency}
    } = Body,
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, Token, AuthData, C),
    ok = set_wallet_balance({Amount, Currency}, WalletID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID
    }.

prepare_standard_environment_with_withdrawal(C) ->
    Cash = make_cash({1000, <<"RUB">>}),
    Env = prepare_standard_environment_with_withdrawal(Cash, C),
    Env#{body => Cash}.

prepare_standard_environment_with_withdrawal(Cash, C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = Env = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    ExternalID = generate_id(),
    Context = #{<<"NS">> => #{generate_id() => generate_id()}},
    EncodedContext = ff_entity_context_codec:marshal(Context),
    Params = #wthd_WithdrawalParams{
        id = WithdrawalID,
        wallet_id = WalletID,
        destination_id = DestinationID,
        body = Cash,
        external_id = ExternalID
    },
    {ok, _WithdrawalState} = call_withdrawal('Create', {Params, EncodedContext}),
    succeeded = await_final_withdrawal_status(WithdrawalID),
    Env#{
        withdrawal_id => WithdrawalID,
        external_id => ExternalID,
        context => Context
    }.

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_withdrawal_status(WithdrawalID) ->
    ff_withdrawal:status(get_withdrawal(WithdrawalID)).

get_adjustment(WithdrawalID, AdjustmentID) ->
    {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, get_withdrawal(WithdrawalID)),
    Adjustment.

await_final_withdrawal_status(WithdrawalID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            case ff_withdrawal:is_finished(Withdrawal) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(10, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

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

create_destination(IID, <<"USD_CURRENCY">>, AuthData, C) ->
    create_destination(IID, <<"USD">>, undefined, AuthData, C);
create_destination(IID, Token, AuthData, C) ->
    create_destination(IID, <<"RUB">>, Token, AuthData, C).

create_destination(IID, Currency, Token, AuthData, C) ->
    ID = generate_id(),
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    NewStoreResource =
        case Token of
            undefined ->
                StoreSource;
            Token ->
                StoreSource#{token => Token}
        end,
    Resource = {bank_card, #{bank_card => NewStoreResource}},
    Params0 = #{id => ID, identity => IID, name => <<"XDesination">>, currency => Currency, resource => Resource},
    Params = maps:merge(AuthData, Params0),
    ok = ff_destination_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, Machine} = ff_destination_machine:get(ID),
            Destination = ff_destination_machine:destination(Machine),
            ff_destination:status(Destination)
        end
    ),
    ID.

create_generic_destination(Provider, IID, _C) ->
    ID = generate_id(),
    Resource =
        {generic, #{
            generic => #{
                provider => #{id => Provider},
                data => #{type => <<"application/json">>, data => <<"{}">>}
            }
        }},
    Params = #{
        id => ID, identity => IID, name => <<"GenericDestination">>, currency => <<"RUB">>, resource => Resource
    },
    ok = ff_destination_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, Machine} = ff_destination_machine:get(ID),
            Destination = ff_destination_machine:destination(Machine),
            ff_destination:status(Destination)
        end
    ),
    ID.

set_wallet_balance({Amount, Currency}, ID) ->
    TransactionID = generate_id(),
    {ok, Machine} = ff_wallet_machine:get(ID),
    Account = ff_wallet:account(ff_wallet_machine:wallet(Machine)),
    AccounterID = ff_account:accounter_account_id(Account),
    {CurrentAmount, _, Currency} = get_account_balance(Account),
    {ok, AnotherAccounterID} = ct_helper:create_account(Currency),
    Postings = [{AnotherAccounterID, AccounterID, {Amount - CurrentAmount, Currency}}],
    {ok, _} = ff_accounting:prepare_trx(TransactionID, Postings),
    {ok, _} = ff_accounting:commit_trx(TransactionID, Postings),
    ok.

make_cash({Amount, Currency}) ->
    #'fistful_base_Cash'{
        amount = Amount,
        currency = #'fistful_base_CurrencyRef'{symbolic_code = Currency}
    }.
