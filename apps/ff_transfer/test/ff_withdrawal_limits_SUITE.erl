-module(ff_withdrawal_limits_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("fistful_proto/include/fistful_wthd_session_thrift.hrl").
-include_lib("fistful_proto/include/fistful_wthd_thrift.hrl").
-include_lib("fistful_proto/include/fistful_wthd_status_thrift.hrl").
-include_lib("fistful_proto/include/fistful_repairer_thrift.hrl").

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
-export([create_ok_test/1]).
-export([limit_success/1]).
-export([limit_overflow/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% Macro helpers

-define(FINAL_BALANCE(Cash), {
    element(1, Cash),
    {
        {inclusive, element(1, Cash)},
        {inclusive, element(1, Cash)}
    },
    element(2, Cash)
}).

-define(FINAL_BALANCE(Amount, Currency), ?FINAL_BALANCE({Amount, Currency})).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_ok_test,
            limit_success,
            limit_overflow
            % payment_limit_other_paytool_success,
            % payment_partial_capture_limit_success,
            % switch_provider_after_limit_overflow,
            % limit_not_found,
            % refund_limit_success
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C0) ->
    C1 = ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C0
    ),
    _ = ff_limiter_helper:init_per_suite(C1),
    C1.

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
    C1 = ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(Name),
            ct_helper:woody_ctx()
        ],
        C
    ),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%% Tests

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)),
    ?assertEqual(?FINAL_BALANCE(0, <<"RUB">>), get_wallet_balance(WalletID)),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertEqual(WalletID, ff_withdrawal:wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, ff_withdrawal:destination_id(Withdrawal)),
    ?assertEqual(Cash, ff_withdrawal:body(Withdrawal)),
    ?assertEqual(WithdrawalID, ff_withdrawal:external_id(Withdrawal)).

-spec limit_success(config()) -> test_return().
limit_success(C) ->
    Cash = {800800, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)),
    Withdrawal = get_withdrawal(WithdrawalID),
    ok = ff_limiter_helper:assert_payment_limit_amount(1, Withdrawal).

-spec limit_overflow(config()) -> test_return().
limit_overflow(C) ->
    Cash = {900900, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"test_error">>}}, Result),
    Withdrawal = get_withdrawal(WithdrawalID),
    ok = ff_limiter_helper:assert_payment_limit_amount(1, Withdrawal).

%% Utils

prepare_standard_environment(WithdrawalCash, C) ->
    prepare_standard_environment(WithdrawalCash, undefined, C).

prepare_standard_environment({_Amount, Currency} = WithdrawalCash, Token, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, Token, C),
    ok = set_wallet_balance(WithdrawalCash, WalletID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID
    }.

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_withdrawal_status(WithdrawalID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    ff_withdrawal:status(Withdrawal).

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
        genlib_retry:linear(20, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, C).

create_identity(Party, ProviderID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, C).

create_identity(Party, Name, ProviderID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
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

create_destination(IID, <<"USD_CURRENCY">>, C) ->
    create_destination(IID, <<"USD">>, undefined, C);
create_destination(IID, Token, C) ->
    create_destination(IID, <<"RUB">>, Token, C).

create_destination(IID, Currency, Token, C) ->
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
    Params = #{id => ID, identity => IID, name => <<"XDesination">>, currency => Currency, resource => Resource},
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
