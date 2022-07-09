-module(ff_limiter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_domain_thrift.hrl").
-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_withdrawal_thrift.hrl").

-type turnover_selector() :: dmsl_domain_thrift:'TurnoverLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().
-type withdrawal() :: ff_withdrawal:withdrawal_state().
-type route() :: ff_routing_rule:route().

-export([get_turnover_limits/1]).
-export([check_limits/2]).

-export([hold_withdrawal_limits/3]).
-export([commit_withdrawal_limits/3]).
-export([rollback_withdrawal_limits/3]).

-spec get_turnover_limits(turnover_selector() | undefined) -> [turnover_limit()].
get_turnover_limits(undefined) ->
    logger:info("Operation limits haven't been set on provider terms."),
    [];
get_turnover_limits({value, Limits}) ->
    Limits;
get_turnover_limits(Ambiguous) ->
    error({misconfiguration, {'Could not reduce selector to a value', Ambiguous}}).

-spec check_limits([turnover_limit()], withdrawal()) ->
    {ok, [ff_limiter_client:limit()]}
    | {error, {limit_overflow, [binary()]}}.
check_limits(TurnoverLimits, Withdrawal) ->
    Context = gen_limit_context(Withdrawal),
    try
        check_limits_(TurnoverLimits, Context, [])
    catch
        throw:limit_overflow ->
            IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
            {error, {limit_overflow, IDs}}
    end.

check_limits_([], _, Limits) ->
    {ok, Limits};
check_limits_([T | TurnoverLimits], Context, Acc) ->
    #domain_TurnoverLimit{id = LimitID} = T,
    Clock = get_latest_clock(),
    Limit = ff_limiter_client:get(LimitID, Clock, Context),
    #limiter_Limit{
        amount = LimiterAmount
    } = Limit,
    UpperBoundary = T#domain_TurnoverLimit.upper_boundary,
    case LimiterAmount < UpperBoundary of
        true ->
            check_limits_(TurnoverLimits, Context, [Limit | Acc]);
        false ->
            logger:info("Limit with id ~p overflowed, amount ~p upper boundary ~p", [
                LimitID,
                LimiterAmount,
                UpperBoundary
            ]),
            throw(limit_overflow)
    end.

-spec hold_withdrawal_limits([turnover_limit()], route(), withdrawal()) -> ok.
hold_withdrawal_limits(TurnoverLimits, Route, Withdrawal) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_payment_changes(IDs, Route, Withdrawal),
    Context = gen_limit_context(Withdrawal),
    hold(LimitChanges, get_latest_clock(), Context).

-spec commit_withdrawal_limits([turnover_limit()], route(), withdrawal()) -> ok.
commit_withdrawal_limits(TurnoverLimits, Route, Withdrawal) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_payment_changes(IDs, Route, Withdrawal),
    Context = gen_limit_context(Withdrawal),
    commit(LimitChanges, get_latest_clock(), Context).

-spec rollback_withdrawal_limits([turnover_limit()], route(), withdrawal()) -> ok.
rollback_withdrawal_limits(TurnoverLimits, Route, Withdrawal) ->
    {_, Currency} = ff_withdrawal:body(Withdrawal),
    commit_withdrawal_limits(TurnoverLimits, Route, ff_withdrawal:set_body({0, Currency}, Withdrawal)).

-spec hold([ff_limiter_client:limit_change()], ff_limiter_client:clock(), ff_limiter_client:context()) -> ok.
hold(LimitChanges, Clock, Context) ->
    lists:foreach(
        fun(LimitChange) ->
            ff_limiter_client:hold(LimitChange, Clock, Context)
        end,
        LimitChanges
    ).

-spec commit([ff_limiter_client:limit_change()], ff_limiter_client:clock(), ff_limiter_client:context()) -> ok.
commit(LimitChanges, Clock, Context) ->
    lists:foreach(
        fun(LimitChange) ->
            ff_limiter_client:commit(LimitChange, Clock, Context)
        end,
        LimitChanges
    ).

gen_limit_context(Withdrawal) ->
    MarshaledWithdrawal = marshal_withdrawal(Withdrawal),
    #limiter_LimitContext{
        withdrawal_processing = #context_withdrawal_Context{
            op = {withdrawal, #context_withdrawal_OperationWithdrawal{}},
            withdrawal = #context_withdrawal_Withdrawal{withdrawal = MarshaledWithdrawal}
        }
    }.

gen_limit_payment_changes(LimitIDs, Route, Withdrawal) ->
    [
        #limiter_LimitChange{
            id = ID,
            change_id = construct_limit_change_id(ID, Route, Withdrawal)
        }
     || ID <- LimitIDs
    ].

construct_limit_change_id(LimitID, #{terminal_ref := TerminalRef, provider_ref := ProviderRef}, Withdrawal) ->
    ComplexID = construct_complex_id([
        LimitID,
        genlib:to_binary(get_provider_id(ProviderRef)),
        genlib:to_binary(get_terminal_id(TerminalRef)),
        ff_withdrawal:id(Withdrawal)
    ]),
    genlib_string:join($., [<<"limiter">>, ComplexID]).

get_provider_id(#domain_ProviderRef{id = ID}) ->
    ID.

get_terminal_id(#domain_TerminalRef{id = ID}) ->
    ID.

get_latest_clock() ->
    {latest, #limiter_LatestClock{}}.

-spec construct_complex_id([binary() | {atom(), binary()}]) -> binary().
construct_complex_id(L) ->
    genlib_string:join(
        $.,
        lists:map(
            fun
                ({Tag, ID}) -> [atom_to_binary(Tag, utf8), <<"-">>, ID];
                (ID) -> ID
            end,
            L
        )
    ).

marshal_withdrawal(Withdrawal) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = ff_withdrawal:params(Withdrawal),
    {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    WalletAccount = ff_wallet:account(Wallet),

    {ok, DestinationMachine} = ff_destination_machine:get(DestinationID),
    Destination = ff_destination_machine:destination(DestinationMachine),
    DestinationAccount = ff_destination:account(Destination),

    {ok, SenderSt} = ff_identity_machine:get(ff_account:identity(WalletAccount)),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_account:identity(DestinationAccount)),
    SenderIdentity = ff_identity_machine:identity(SenderSt),
    ReceiverIdentity = ff_identity_machine:identity(ReceiverSt),

    Resource = ff_withdrawal:destination_resource(Withdrawal),
    MarshaledResource = ff_adapter_withdrawal_codec:marshal(resource, Resource),
    #wthd_domain_Withdrawal{
        body = ff_dmsl_codec:marshal(cash, ff_withdrawal:body(Withdrawal)),
        destination = MarshaledResource,
        sender = ff_adapter_withdrawal_codec:marshal(identity, #{id => ff_identity:id(SenderIdentity)}),
        receiver = ff_adapter_withdrawal_codec:marshal(identity, #{id => ff_identity:id(ReceiverIdentity)})
    }.
