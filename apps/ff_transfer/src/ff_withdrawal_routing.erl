-module(ff_withdrawal_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_routes/2]).
-export([prepare_routes/3]).
-export([gather_routes/2]).
-export([filter_limit_overflow_routes/3]).
-export([rollback_routes_limits/3]).
-export([commit_routes_limits/3]).
-export([convert/1]).
-export([make_route/2]).
-export([get_provider/1]).
-export([get_terminal/1]).
-export([merge_withdrawal_terms/2]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    version := 1,
    provider_id := provider_id(),
    terminal_id := terminal_id(),
    provider_id_legacy => provider_id()
}.

-type routing_context() :: #{
    domain_revision := domain_revision(),
    identity := identity(),
    withdrawal => withdrawal(),
    route => route()
}.

-export_type([route/0]).
-export_type([routing_context/0]).

-type identity() :: ff_identity:identity_state().
-type withdrawal() :: ff_withdrawal:withdrawal_state().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset() :: ff_varset:varset().

-type provider_id() :: ff_payouts_provider:id().

-type terminal_id() :: ff_payouts_terminal:id().

-type routing_rule_route() :: ff_routing_rule:route().
-type reject_context() :: ff_routing_rule:reject_context().

-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().
-type currency_selector() :: dmsl_domain_thrift:'CurrencySelector'().
-type cash_limit_selector() :: dmsl_domain_thrift:'CashLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().

%%

-spec prepare_routes(party_varset(), identity(), domain_revision()) ->
    {ok, [routing_rule_route()]} | {error, route_not_found}.
prepare_routes(PartyVarset, Identity, DomainRevision) ->
    prepare_routes(PartyVarset, #{identity => Identity, domain_revision => DomainRevision}).

-spec prepare_routes(party_varset(), routing_context()) -> {ok, [routing_rule_route()]} | {error, route_not_found}.
prepare_routes(PartyVarset, Context) ->
    handle_process_routes(gather_routes(PartyVarset, Context)).

-spec gather_routes(party_varset(), routing_context()) ->
    {[routing_rule_route()], reject_context()}.
gather_routes(PartyVarset, Context = #{identity := Identity, domain_revision := DomainRevision}) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, PartyVarset, DomainRevision),
    {Routes, RejectContext0} = ff_routing_rule:gather_routes(
        PaymentInstitution,
        withdrawal_routing_rules,
        PartyVarset,
        DomainRevision
    ),
    filter_valid_routes(Routes, RejectContext0, PartyVarset, Context).

-spec make_route(provider_id(), terminal_id() | undefined) -> route().
make_route(ProviderID, TerminalID) ->
    genlib_map:compact(#{
        version => 1,
        provider_id => ProviderID,
        terminal_id => TerminalID
    }).

-spec get_provider(route()) -> provider_id().
get_provider(#{provider_id := ProviderID}) ->
    ProviderID.

-spec get_terminal(route()) -> ff_maybe:maybe(terminal_id()).
get_terminal(Route) ->
    maps:get(terminal_id, Route, undefined).

-spec merge_withdrawal_terms(
    ff_payouts_provider:provision_terms() | undefined,
    ff_payouts_terminal:provision_terms() | undefined
) -> ff_maybe:maybe(withdrawal_provision_terms()).
merge_withdrawal_terms(
    #domain_WithdrawalProvisionTerms{
        currencies = PCurrencies,
        payout_methods = PPayoutMethods,
        cash_limit = PCashLimit,
        cash_flow = PCashflow
    },
    #domain_WithdrawalProvisionTerms{
        currencies = TCurrencies,
        payout_methods = TPayoutMethods,
        cash_limit = TCashLimit,
        cash_flow = TCashflow
    }
) ->
    #domain_WithdrawalProvisionTerms{
        currencies = ff_maybe:get_defined(TCurrencies, PCurrencies),
        payout_methods = ff_maybe:get_defined(TPayoutMethods, PPayoutMethods),
        cash_limit = ff_maybe:get_defined(TCashLimit, PCashLimit),
        cash_flow = ff_maybe:get_defined(TCashflow, PCashflow)
    };
merge_withdrawal_terms(ProviderTerms, TerminalTerms) ->
    ff_maybe:get_defined(TerminalTerms, ProviderTerms).

%%

-spec filter_valid_routes([routing_rule_route()], reject_context(), party_varset(), routing_context()) ->
    {[routing_rule_route()], reject_context()}.
filter_valid_routes(Routes, RejectContext, PartyVarset, RoutingContext) ->
    validate_routes_with(
        fun do_validate_terms/3,
        {Routes, RejectContext},
        PartyVarset,
        RoutingContext
    ).

-spec filter_limit_overflow_routes({[routing_rule_route()], reject_context()}, party_varset(), routing_context()) ->
    {ok, [routing_rule_route()]} | {error, route_not_found}.
filter_limit_overflow_routes({Routes, RejectContext}, PartyVarset, RoutingContext) ->
    handle_process_routes(
        validate_routes_with(
            fun do_validate_limits/3,
            {Routes, RejectContext},
            PartyVarset,
            RoutingContext
        )
    ).

-spec rollback_routes_limits([route()], party_varset(), routing_context()) ->
    ok.
rollback_routes_limits(Routes, PartyVarset, RoutingContext) ->
    process_routes_with(
        fun do_rollback_limits/3,
        Routes,
        PartyVarset,
        RoutingContext
    ).

-spec commit_routes_limits([route()], party_varset(), routing_context()) ->
    ok.
commit_routes_limits(Routes, PartyVarset, RoutingContext) ->
    process_routes_with(
        fun do_commit_limits/3,
        Routes,
        PartyVarset,
        RoutingContext
    ).

handle_process_routes({Routes = [_ | _], _RejectContext1}) ->
    {ok, Routes};
handle_process_routes({[], RejectContext1}) ->
    ff_routing_rule:log_reject_context(RejectContext1),
    {error, route_not_found}.

-spec process_routes_with(function(), [route()], party_varset(), routing_context()) ->
    ok.
process_routes_with(Func, Routes, PartyVarset, RoutingContext0) ->
    lists:foreach(
        fun(Route) ->
            ProviderID = maps:get(provider_id, Route),
            TerminalID = maps:get(terminal_id, Route),
            ProviderRef = #domain_ProviderRef{id = ProviderID},
            TerminalRef = #domain_TerminalRef{id = TerminalID},
            #{domain_revision := DomainRevision} = RoutingContext0,
            RoutingContext1 = RoutingContext0#{route => Route},
            {ok, WithdrawalProvisionTerms} = get_route_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision),
            Func(WithdrawalProvisionTerms, PartyVarset, RoutingContext1)
        end,
        Routes
    ).

-spec validate_routes_with(function(), {[routing_rule_route()], reject_context()}, party_varset(), routing_context()) ->
    {[routing_rule_route()], reject_context()}.
validate_routes_with(Func, {Routes, RejectContext}, PartyVarset, RoutingContext0) ->
    lists:foldl(
        fun(Route, {ValidRoutes0, RejectContext0}) ->
            ProviderRef = maps:get(provider_ref, Route),
            TerminalRef = maps:get(terminal_ref, Route),
            #{domain_revision := DomainRevision} = RoutingContext0,
            RoutingContext1 = RoutingContext0#{
                route => make_route(ProviderRef#domain_ProviderRef.id, TerminalRef#domain_TerminalRef.id)
            },

            {ok, WithdrawalProvisionTerms} = get_route_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision),
            case Func(WithdrawalProvisionTerms, PartyVarset, RoutingContext1) of
                {ok, valid} ->
                    ValidRoutes1 = [Route | ValidRoutes0],
                    {ValidRoutes1, RejectContext0};
                {error, RejectReason} ->
                    RejectedRoutes0 = maps:get(rejected_routes, RejectContext0),
                    RejectedRoutes1 = [{ProviderRef, TerminalRef, RejectReason} | RejectedRoutes0],
                    RejectContext1 = maps:put(rejected_routes, RejectedRoutes1, RejectContext0),
                    {ValidRoutes0, RejectContext1}
            end
        end,
        {[], RejectContext},
        Routes
    ).

get_route_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) ->
    case ff_party:compute_provider_terminal_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) of
        {ok, #domain_ProvisionTermSet{
            wallet = #domain_WalletProvisionTerms{
                withdrawals = WithdrawalProvisionTerms
            }
        }} ->
            {ok, WithdrawalProvisionTerms};
        {error, Error} ->
            %% TODO: test for provision_termset_undefined error after routing migration
            {error, Error}
    end.

-spec do_rollback_limits(withdrawal_provision_terms(), party_varset(), routing_context()) ->
    ok.
do_rollback_limits(CombinedTerms, _PartyVarset, #{withdrawal := Withdrawal, route := Route}) ->
    #domain_WithdrawalProvisionTerms{
        turnover_limit = TurnoverLimit
    } = CombinedTerms,
    Limits = ff_limiter:get_turnover_limits(TurnoverLimit),
    ff_limiter:hold_withdrawal_limits(Limits, Route, Withdrawal).

-spec do_commit_limits(withdrawal_provision_terms(), party_varset(), routing_context()) ->
    ok.
do_commit_limits(CombinedTerms, PartyVarset, #{withdrawal := Withdrawal, route := Route, domain_revision := DomainRevision}) ->
    #domain_WithdrawalProvisionTerms{
        turnover_limit = TurnoverLimit
    } = CombinedTerms,
    Limits = ff_limiter:get_turnover_limits(TurnoverLimit),
    TerminalID = maps:get(terminal_id, Route),
    TerminalRef = #domain_TerminalRef{id = TerminalID},
    {ok, Terminal} = ff_domain_config:object(DomainRevision, {terminal, TerminalRef}),
    error({test, Route, CombinedTerms, PartyVarset, Terminal}),
    ff_limiter:commit_withdrawal_limits(Limits, Route, Withdrawal).

-spec do_validate_limits(withdrawal_provision_terms(), party_varset(), routing_context()) ->
    {ok, valid}
    | {error, Error :: term()}.
do_validate_limits(CombinedTerms, PartyVarset, RoutingContext) ->
    do(fun() ->
        #domain_WithdrawalProvisionTerms{
            turnover_limit = TurnoverLimit
        } = CombinedTerms,
        Limits = ff_limiter:get_turnover_limits(TurnoverLimit),
        valid = unwrap(validate_turnover_limits(Limits, PartyVarset, RoutingContext))
    end).

-spec do_validate_terms(withdrawal_provision_terms(), party_varset(), routing_context()) ->
    {ok, valid}
    | {error, Error :: term()}.
do_validate_terms(CombinedTerms, PartyVarset, _RoutingContext) ->
    do(fun() ->
        #domain_WithdrawalProvisionTerms{
            currencies = CurrenciesSelector,
            %% PayoutMethodsSelector is useless for withdrawals
            %% so we can just ignore it
            %% payout_methods = PayoutMethodsSelector,
            cash_limit = CashLimitSelector
        } = CombinedTerms,
        valid = unwrap(validate_selectors_defined(CombinedTerms)),
        valid = unwrap(validate_currencies(CurrenciesSelector, PartyVarset)),
        valid = unwrap(validate_cash_limit(CashLimitSelector, PartyVarset))
    end).

-spec validate_selectors_defined(withdrawal_provision_terms()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_selectors_defined(Terms) ->
    Selectors = [
        Terms#domain_WithdrawalProvisionTerms.currencies,
        Terms#domain_WithdrawalProvisionTerms.payout_methods,
        Terms#domain_WithdrawalProvisionTerms.cash_limit,
        Terms#domain_WithdrawalProvisionTerms.cash_flow
    ],
    case lists:any(fun(Selector) -> Selector =:= undefined end, Selectors) of
        false ->
            {ok, valid};
        true ->
            {error, terms_undefined}
    end.

-spec validate_currencies(currency_selector(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_currencies({value, Currencies}, #{currency := CurrencyRef}) ->
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end;
validate_currencies(_NotReducedSelector, _VS) ->
    {error, {misconfiguration, {not_reduced_termset, currencies}}}.

-spec validate_cash_limit(cash_limit_selector(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_cash_limit({value, CashRange}, #{cost := Cash}) ->
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end;
validate_cash_limit(_NotReducedSelector, _VS) ->
    {error, {misconfiguration, {not_reduced_termset, cash_range}}}.

-spec validate_turnover_limits([turnover_limit()], party_varset(), routing_context()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_turnover_limits(TurnoverLimits, _VS, #{withdrawal := Withdrawal, route := Route}) ->
    ok = ff_limiter:hold_withdrawal_limits(TurnoverLimits, Route, Withdrawal),
    case ff_limiter:check_limits(TurnoverLimits, Withdrawal) of
        {ok, _} ->
            {ok, valid};
        {error, Error} ->
            {error, {terms_violation, Error}}
    end;
validate_turnover_limits(NotReducedSelector, _VS, _RoutingContext) ->
    {error, {misconfiguration, {'Could not reduce selector to a value', NotReducedSelector}}}.

-spec convert([routing_rule_route()]) -> [route()].
convert(RoutingRuleRoutes) ->
    ProviderTerminalMap = lists:foldl(
        fun(#{provider_ref := ProviderRef, terminal_ref := TerminalRef, priority := Priority}, Acc0) ->
            TerminalID = TerminalRef#domain_TerminalRef.id,
            ProviderID = ProviderRef#domain_ProviderRef.id,
            Routes = maps:get(Priority, Acc0, []),
            Acc1 = maps:put(Priority, [{ProviderID, TerminalID} | Routes], Acc0),
            Acc1
        end,
        #{},
        RoutingRuleRoutes
    ),
    lists:foldl(
        fun({_, Data}, Acc) ->
            SortedRoutes = [make_route(P, T) || {P, T} <- lists:sort(Data)],
            SortedRoutes ++ Acc
        end,
        [],
        lists:keysort(1, maps:to_list(ProviderTerminalMap))
    ).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec convert_to_route_test() -> _.
convert_to_route_test() ->
    ?assertEqual(
        [],
        convert([])
    ),
    ?assertEqual(
        [
            #{provider_id => 100, terminal_id => 2000, version => 1},
            #{provider_id => 100, terminal_id => 2001, version => 1},
            #{provider_id => 200, terminal_id => 2100, version => 1},
            #{provider_id => 200, terminal_id => 2101, version => 1},
            #{provider_id => 300, terminal_id => 2200, version => 1}
        ],
        convert([
            #{
                provider_ref => #domain_ProviderRef{id = 100},
                terminal_ref => #domain_TerminalRef{id = 2000},
                priority => 1000
            },
            #{
                provider_ref => #domain_ProviderRef{id = 100},
                terminal_ref => #domain_TerminalRef{id = 2001},
                priority => 1000
            },
            #{
                provider_ref => #domain_ProviderRef{id = 200},
                terminal_ref => #domain_TerminalRef{id = 2100},
                priority => 900
            },
            #{
                provider_ref => #domain_ProviderRef{id = 200},
                terminal_ref => #domain_TerminalRef{id = 2101},
                priority => 900
            },
            #{
                provider_ref => #domain_ProviderRef{id = 300},
                terminal_ref => #domain_TerminalRef{id = 2200},
                priority => 100
            }
        ])
    ).

-endif.
