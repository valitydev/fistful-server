-module(ff_limiter_helper).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_payproc_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_withdrawal_thrift.hrl").
-include_lib("limiter_proto/include/limproto_config_thrift.hrl").
-include_lib("limiter_proto/include/limproto_configurator_thrift.hrl").
-include_lib("limiter_proto/include/limproto_timerange_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_domain_thrift.hrl").

-include_lib("stdlib/include/assert.hrl").

-export([init_per_suite/1]).
-export([assert_payment_limit_amount/2]).
-export([get_payment_limit_amount/2]).

-type config() :: ct_suite:ct_config().

-define(LIMIT_ID, <<"ID">>).
-define(LIMIT_ID2, <<"ID2">>).
-define(LIMIT_ID3, <<"ID3">>).

-spec init_per_suite(config()) -> _.
init_per_suite(_Config) ->
    {ok, #config_LimitConfig{}} = ff_dummy_limiter:create_config(
        limiter_create_params(?LIMIT_ID),
        ff_dummy_limiter:new()
    ),
    {ok, #config_LimitConfig{}} = ff_dummy_limiter:create_config(
        limiter_create_params(?LIMIT_ID2),
        ff_dummy_limiter:new()
    ),
    {ok, #config_LimitConfig{}} = ff_dummy_limiter:create_config(
        limiter_create_params(?LIMIT_ID3),
        ff_dummy_limiter:new()
    ).

-spec assert_payment_limit_amount(_, _) -> _.
assert_payment_limit_amount(AssertAmount, Withdrawal) ->
    {ok, Limit} = get_payment_limit_amount(?LIMIT_ID, Withdrawal),
    ?assertMatch(#limiter_Limit{amount = AssertAmount}, Limit).

-spec get_payment_limit_amount(_, _) -> _.
get_payment_limit_amount(LimitId, Withdrawal) ->
    MarshaledWithdrawal = ff_limiter:marshal_withdrawal(Withdrawal),
    Context = #limiter_LimitContext{
        withdrawal_processing = #context_withdrawal_Context{
            op = {withdrawal, #context_withdrawal_OperationWithdrawal{}},
            withdrawal = #context_withdrawal_Withdrawal{withdrawal = MarshaledWithdrawal}
        }
    },
    ff_dummy_limiter:get(LimitId, Context, ff_dummy_limiter:new()).

limiter_create_params(LimitID) ->
    #config_LimitConfigParams{
        id = LimitID,
        started_at = <<"2000-01-01T00:00:00Z">>,
        shard_size = 12,
        time_range_type = {calendar, {month, #timerange_TimeRangeTypeCalendarMonth{}}},
        context_type = {withdrawal_processing, #config_LimitContextTypeWithdrawalProcessing{}},
        type = {turnover, #config_LimitTypeTurnover{}},
        scope = {single, {payment_tool, #config_LimitScopeEmptyDetails{}}},
        description = <<"description">>,
        op_behaviour = #config_OperationLimitBehaviour{
            invoice_payment_refund = {subtraction, #config_Subtraction{}}
        }
    }.
