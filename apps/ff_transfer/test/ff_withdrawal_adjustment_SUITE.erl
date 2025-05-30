-module(ff_withdrawal_adjustment_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

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

-export([adjustment_can_change_status_to_failed_test/1]).
-export([adjustment_can_change_failure_test/1]).
-export([adjustment_can_change_status_to_succeeded_test/1]).
-export([adjustment_can_not_change_status_to_pending_test/1]).
-export([adjustment_can_not_change_status_to_same/1]).
-export([adjustment_sequence_test/1]).
-export([adjustment_idempotency_test/1]).
-export([no_parallel_adjustments_test/1]).
-export([no_pending_withdrawal_adjustments_test/1]).
-export([unknown_withdrawal_test/1]).
-export([adjustment_can_not_change_domain_revision_to_same/1]).
-export([adjustment_can_not_change_domain_revision_with_failed_status/1]).
-export([adjustment_can_change_domain_revision_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% Macro helpers

-define(FINAL_BALANCE(Amount, Currency), {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency}).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default},
        {group, non_parallel}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            adjustment_can_change_status_to_failed_test,
            adjustment_can_change_failure_test,
            adjustment_can_change_status_to_succeeded_test,
            adjustment_can_not_change_status_to_pending_test,
            adjustment_can_not_change_status_to_same,
            adjustment_sequence_test,
            adjustment_idempotency_test,
            no_parallel_adjustments_test,
            no_pending_withdrawal_adjustments_test,
            unknown_withdrawal_test,
            adjustment_can_not_change_domain_revision_to_same,
            adjustment_can_not_change_domain_revision_with_failed_status
        ]},
        {non_parallel, [], [
            adjustment_can_change_domain_revision_test
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

-spec adjustment_can_change_status_to_failed_test(config()) -> test_return().
adjustment_can_change_status_to_failed_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(WithdrawalID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(WithdrawalID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure}, get_withdrawal_status(WithdrawalID)),
    assert_adjustment_same_revisions(WithdrawalID, AdjustmentID),
    ?assertEqual(?FINAL_BALANCE(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure1 = #{code => <<"one">>},
    AdjustmentID1 = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1}, get_withdrawal_status(WithdrawalID)),
    assert_adjustment_same_revisions(WithdrawalID, AdjustmentID1),
    ?assertEqual(?FINAL_BALANCE(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure2 = #{code => <<"two">>},
    AdjustmentID2 = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2}, get_withdrawal_status(WithdrawalID)),
    assert_adjustment_same_revisions(WithdrawalID, AdjustmentID2),
    ?assertEqual(?FINAL_BALANCE(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    WithdrawalID = genlib:bsuuid(),
    Params = #{
        id => WithdrawalID,
        wallet_id => WalletID,
        destination_id => DestinationID,
        party_id => PartyID,
        body => {1000, <<"RUB">>}
    },
    ok = ff_withdrawal_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_withdrawal_status(WithdrawalID)),
    AdjustmentID = process_adjustment(WithdrawalID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(WithdrawalID, AdjustmentID)),
    ?assertMatch(succeeded, get_withdrawal_status(WithdrawalID)),
    assert_adjustment_same_revisions(WithdrawalID, AdjustmentID),
    ?assertEqual(?FINAL_BALANCE(-1000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(880, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => genlib:bsuuid(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => genlib:bsuuid(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    MakeFailed = fun() ->
        _ = process_adjustment(WithdrawalID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        }),
        ?assertEqual(?FINAL_BALANCE(100, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_destination_balance(DestinationID))
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(WithdrawalID, #{
            change => {change_status, succeeded}
        }),
        ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID))
    end,
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed().

-spec adjustment_idempotency_test(config()) -> test_return().
adjustment_idempotency_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Params = #{
        id => genlib:bsuuid(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertMatch([_], ff_withdrawal:adjustments(Withdrawal)),
    ?assertEqual(?FINAL_BALANCE(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Withdrawal0 = get_withdrawal(WithdrawalID),
    AdjustmentID0 = genlib:bsuuid(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = ff_withdrawal:start_adjustment(Params0, Withdrawal0),
    Withdrawal1 = lists:foldl(fun ff_withdrawal:apply_event/2, Withdrawal0, Events0),
    Params1 = #{
        id => genlib:bsuuid(),
        change => {change_status, succeeded}
    },
    Result = ff_withdrawal:start_adjustment(Params1, Withdrawal1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_withdrawal_adjustments_test(config()) -> test_return().
no_pending_withdrawal_adjustments_test(C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    {ok, Events0} = ff_withdrawal:create(#{
        id => genlib:bsuuid(),
        wallet_id => WalletID,
        destination_id => DestinationID,
        party_id => PartyID,
        body => {100, <<"RUB">>}
    }),
    Withdrawal1 = lists:foldl(fun ff_withdrawal:apply_event/2, undefined, Events0),
    Params1 = #{
        id => genlib:bsuuid(),
        change => {change_status, succeeded}
    },
    Result = ff_withdrawal:start_adjustment(Params1, Withdrawal1),
    ?assertMatch({error, {invalid_withdrawal_status, pending}}, Result).

-spec unknown_withdrawal_test(config()) -> test_return().
unknown_withdrawal_test(_C) ->
    WithdrawalID = <<"unknown_withdrawal">>,
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => genlib:bsuuid(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_withdrawal, WithdrawalID}}, Result).

-spec adjustment_can_not_change_domain_revision_to_same(config()) -> test_return().
adjustment_can_not_change_domain_revision_to_same(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Withdrawal = get_withdrawal(WithdrawalID),
    DomainRevision = ff_withdrawal:domain_revision(Withdrawal),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => genlib:bsuuid(),
        change => {change_cash_flow, DomainRevision}
    }),
    ?assertMatch({error, {invalid_cash_flow_change, {already_has_domain_revision, DomainRevision}}}, Result).

-spec adjustment_can_not_change_domain_revision_with_failed_status(config()) -> test_return().
adjustment_can_not_change_domain_revision_with_failed_status(C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    WithdrawalID = genlib:bsuuid(),
    Params = #{
        id => WithdrawalID,
        wallet_id => WalletID,
        destination_id => DestinationID,
        party_id => PartyID,
        body => {1000, <<"RUB">>}
    },
    ok = ff_withdrawal_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_withdrawal_status(WithdrawalID)),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => genlib:bsuuid(),
        change => {change_cash_flow, ct_domain_config:head() - 1}
    }),
    ?assertMatch({error, {invalid_cash_flow_change, {unavailable_status, {failed, #{code := _}}}}}, Result).

-spec adjustment_can_change_domain_revision_test(config()) -> test_return().
adjustment_can_change_domain_revision_test(C) ->
    ProviderID = 1,
    ?FINAL_BALANCE(StartProviderAmount, <<"RUB">>) = get_provider_balance(ProviderID, ct_domain_config:head()),
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Withdrawal = get_withdrawal(WithdrawalID),
    #{provider_id := ProviderID} = ff_withdrawal:route(Withdrawal),
    DomainRevision = ff_withdrawal:domain_revision(Withdrawal),
    ?assertEqual(?FINAL_BALANCE(StartProviderAmount + 5, <<"RUB">>), get_provider_balance(ProviderID, DomainRevision)),
    _OtherWalletToChangeDomain = ct_objects:create_wallet(
        PartyID, <<"RUB">>, #domain_TermSetHierarchyRef{id = 1}, #domain_PaymentInstitutionRef{id = 1}
    ),
    AdjustmentID = process_adjustment(WithdrawalID, #{
        change => {change_cash_flow, ct_domain_config:head()},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(WithdrawalID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(WithdrawalID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual(succeeded, get_withdrawal_status(WithdrawalID)),
    assert_adjustment_same_revisions(WithdrawalID, AdjustmentID),
    ?assertEqual(?FINAL_BALANCE(StartProviderAmount + 5, <<"RUB">>), get_provider_balance(ProviderID, DomainRevision)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?FINAL_BALANCE(80, <<"RUB">>), get_destination_balance(DestinationID)).

%% Utils

prepare_standard_environment({_Amount, Currency} = WithdrawalCash, _C) ->
    PartyID = ct_objects:create_party(),
    WalletID = ct_objects:create_wallet(
        PartyID, Currency, #domain_TermSetHierarchyRef{id = 1}, #domain_PaymentInstitutionRef{id = 1}
    ),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = ct_objects:create_destination(PartyID, undefined),
    SourceID = ct_objects:create_source(PartyID, Currency),
    {_DepositID, _} = ct_objects:create_deposit(PartyID, WalletID, SourceID, WithdrawalCash),
    ok = await_wallet_balance(WithdrawalCash, WalletID),
    WithdrawalID = process_withdrawal(#{
        destination_id => DestinationID,
        wallet_id => WalletID,
        party_id => PartyID,
        body => WithdrawalCash
    }),
    #{
        party_id => PartyID,
        wallet_id => WalletID,
        destination_id => DestinationID,
        withdrawal_id => WithdrawalID
    }.

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_adjustment(WithdrawalID, AdjustmentID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, Withdrawal),
    Adjustment.

process_withdrawal(WithdrawalParams) ->
    WithdrawalID = genlib:bsuuid(),
    ok = ff_withdrawal_machine:create(WithdrawalParams#{id => WithdrawalID}, ff_entity_context:new()),
    succeeded = await_final_withdrawal_status(WithdrawalID),
    WithdrawalID.

process_adjustment(WithdrawalID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => genlib:bsuuid()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = ff_withdrawal_machine:start_adjustment(WithdrawalID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(WithdrawalID, AdjustmentID),
    AdjustmentID.

get_withdrawal_status(WithdrawalID) ->
    ff_withdrawal:status(get_withdrawal(WithdrawalID)).

get_adjustment_status(WithdrawalID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(WithdrawalID, AdjustmentID)).

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
        genlib_retry:linear(90, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

await_final_adjustment_status(WithdrawalID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, Withdrawal),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_adjustment_status(WithdrawalID, AdjustmentID).

assert_adjustment_same_revisions(WithdrawalID, AdjustmentID) ->
    Adjustment = get_adjustment(WithdrawalID, AdjustmentID),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertEqual(ff_withdrawal:final_domain_revision(Withdrawal), ff_adjustment:domain_revision(Adjustment)),
    ?assertEqual(ff_withdrawal:created_at(Withdrawal), ff_adjustment:operation_timestamp(Adjustment)),
    ok.

get_wallet_balance(ID) ->
    ct_objects:get_wallet_balance(ID).

get_destination_balance(ID) ->
    {ok, Machine} = ff_destination_machine:get(ID),
    Destination = ff_destination_machine:destination(Machine),
    get_account_balance(ff_destination:account(Destination)).

get_provider_balance(ProviderID, DomainRevision) ->
    {ok, Provider} = ff_payouts_provider:get(ProviderID, DomainRevision),
    ProviderAccounts = ff_payouts_provider:accounts(Provider),
    ProviderAccount = maps:get(<<"RUB">>, ProviderAccounts, undefined),
    get_account_balance(ProviderAccount).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_accounting:balance(Account),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

await_wallet_balance({Amount, Currency}, ID) ->
    ct_objects:await_wallet_balance({Amount, Currency}, ID).
