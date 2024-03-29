-module(ff_wallet_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/fistful_wallet_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("fistful_proto/include/fistful_account_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_ok/1]).
-export([create_error_identity_not_found/1]).
-export([create_error_currency_not_found/1]).
-export([create_error_party_blocked/1]).
-export([create_error_party_suspended/1]).
-export([get_account_balance/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_ok,
            create_error_identity_not_found,
            create_error_currency_not_found,
            create_error_party_blocked,
            create_error_party_suspended,
            get_account_balance
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

-spec create_ok(config()) -> test_return().
-spec create_error_identity_not_found(config()) -> test_return().
-spec create_error_currency_not_found(config()) -> test_return().
-spec create_error_party_blocked(config()) -> test_return().
-spec create_error_party_suspended(config()) -> test_return().
-spec get_account_balance(config()) -> test_return().

create_ok(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    IdentityID = create_identity(Party, C),
    Ctx = #{<<"TEST_NS">> => {obj, #{{str, <<"KEY">>} => {b, true}}}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata),
    CreateResult = call_service('Create', {Params, Ctx}),
    GetResult = call_service('Get', {ID, #'fistful_base_EventRange'{}}),
    {ok, Wallet} = GetResult,
    Account = Wallet#wallet_WalletState.account,
    CurrencyRef = Account#account_Account.currency,
    ?assertMatch(CreateResult, GetResult),
    ?assertMatch(<<"Valet">>, Wallet#wallet_WalletState.name),
    ?assertMatch(unblocked, Wallet#wallet_WalletState.blocking),
    ?assertMatch(ExternalID, Wallet#wallet_WalletState.external_id),
    ?assertMatch(Metadata, Wallet#wallet_WalletState.metadata),
    ?assertMatch(Ctx, Wallet#wallet_WalletState.context),
    ?assertMatch(IdentityID, Account#account_Account.identity),
    ?assertMatch(Currency, CurrencyRef#'fistful_base_CurrencyRef'.symbolic_code).

create_error_identity_not_found(_C) ->
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    IdentityID = genlib:unique(),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_IdentityNotFound{}}, Result).

create_error_currency_not_found(C) ->
    Party = create_party(C),
    Currency = <<"RBK.MONEY">>,
    ID = genlib:unique(),
    IdentityID = create_identity(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_CurrencyNotFound{}}, Result).

create_error_party_blocked(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    IdentityID = create_identity(Party, C),
    ok = block_party(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_PartyInaccessible{}}, Result).

create_error_party_suspended(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    IdentityID = create_identity(Party, C),
    ok = suspend_party(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_PartyInaccessible{}}, Result).

get_account_balance(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    IdentityID = create_identity(Party, C),
    Ctx = #{<<"TEST_NS">> => {obj, #{{str, <<"KEY">>} => {b, true}}}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata),
    {ok, Wallet} = call_service('Create', {Params, Ctx}),
    WalletID = Wallet#wallet_WalletState.id,
    {ok, AccountBalance} = call_service('GetAccountBalance', {WalletID}),
    CurrencyRef = AccountBalance#account_AccountBalance.currency,
    Account = Wallet#wallet_WalletState.account,
    AccountID = Account#account_Account.id,
    ?assertMatch(AccountID, AccountBalance#account_AccountBalance.id),
    ?assertMatch(Currency, CurrencyRef#'fistful_base_CurrencyRef'.symbolic_code),
    ?assertMatch(0, AccountBalance#account_AccountBalance.expected_min),
    ?assertMatch(0, AccountBalance#account_AccountBalance.current),
    ?assertMatch(0, AccountBalance#account_AccountBalance.expected_max).

%%-----------
%%  Internal
%%-----------
call_service(Fun, Args) ->
    Service = {fistful_wallet_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/wallet">>,
        event_handler => ff_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

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

suspend_party(Party, C) ->
    Service = {dmsl_payproc_thrift, 'PartyManagement'},
    Args = {Party},
    Request = {Service, 'Suspend', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

block_party(Party, C) ->
    Service = {dmsl_payproc_thrift, 'PartyManagement'},
    Args = {Party, <<"BECAUSE">>},
    Request = {Service, 'Block', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

construct_wallet_params(ID, IdentityID, Currency) ->
    #wallet_WalletParams{
        id = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.

construct_wallet_params(ID, IdentityID, Currency, ExternalID) ->
    #wallet_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalID,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.

construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata) ->
    #wallet_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalID,
        metadata = Metadata,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.
