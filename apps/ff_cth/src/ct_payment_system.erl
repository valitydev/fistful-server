-module(ct_payment_system).

-export([setup/0]).
-export([setup/1]).
-export([shutdown/1]).

%% API types

-type options() :: #{
    identity_provider_config => map(),
    services => map(),
    domain_config => list(),
    default_termset => dmsl_domain_thrift:'TermSet'(),
    company_termset => dmsl_domain_thrift:'TermSet'(),
    payment_inst_identity_id => id(),
    dummy_payment_inst_identity_id => id(),
    provider_identity_id => id(),
    dummy_provider_identity_id => id(),
    optional_apps => list()
}.

-opaque system() :: #{
    started_apps := [atom()],
    suite_sup := pid()
}.

-export_type([options/0]).
-export_type([system/0]).

%% Internal types

-type id() :: binary().
-type config() :: ct_helper:config().

%% API

-spec setup() -> fun((config()) -> config()).
setup() ->
    setup(#{}).

-spec setup(options()) -> fun((config()) -> config()).
setup(Options) ->
    fun(C) -> do_setup(Options, C) end.

-spec shutdown(config()) -> ok.
shutdown(C) ->
    #{started_apps := Apps, suite_sup := Sup} = ct_helper:cfg(payment_system, C),
    ok = ct_sup:stop(Sup),
    ok = ct_helper:stop_apps(Apps).

%% Internals

-spec do_setup(options(), config()) -> config().
do_setup(Options0, C0) ->
    Options = Options0#{
        payment_inst_identity_id => genlib:unique(),
        dummy_payment_inst_identity_id => genlib:unique(),
        provider_identity_id => genlib:unique(),
        dummy_provider_identity_id => genlib:unique()
    },
    {ok, Processing0} = start_processing_apps(Options),
    C1 = ct_helper:makeup_cfg([ct_helper:woody_ctx()], [{services, services(Options)} | C0]),
    ok = ct_helper:set_context(C1),
    ok = setup_dominant(Options),
    ok = configure_processing_apps(Options),
    ok = ct_helper:unset_context(),
    [{payment_system, Processing0} | C1].

start_processing_apps(Options) ->
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, services(Options)},
            {providers, identity_provider_config(Options)}
        ]},
        ff_server,
        bender_client
    ]),
    SuiteSup = ct_sup:start(),
    {ok, _} = supervisor:start_child(
        SuiteSup,
        woody_server:child_spec(
            ?MODULE,
            #{
                ip => {127, 0, 0, 1},
                port => 8222,
                handlers => [
                    {
                        <<"/bank">>,
                        {{dmsl_wthd_provider_thrift, 'Adapter'}, {ff_ct_provider_handler, []}}
                    },
                    {
                        <<"/quotebank">>,
                        {{dmsl_wthd_provider_thrift, 'Adapter'}, {ff_ct_provider_handler, []}}
                    },
                    {
                        <<"/downbank">>,
                        {
                            {dmsl_wthd_provider_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_fail_provider}]}
                        }
                    },
                    {
                        <<"/downbank2">>,
                        {
                            {dmsl_wthd_provider_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_unknown_failure_provider}]}
                        }
                    },
                    {
                        <<"/sleepybank">>,
                        {
                            {dmsl_wthd_provider_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_sleepy_provider}]}
                        }
                    },
                    {
                        <<"/binbase">>,
                        {{binbase_binbase_thrift, 'Binbase'}, {ff_ct_binbase_handler, []}}
                    }
                ],
                event_handler => scoper_woody_event_handler
            }
        )
    ),
    Processing = #{
        started_apps => StartedApps ++ start_optional_apps(Options),
        suite_sup => SuiteSup
    },
    {ok, Processing}.

start_optional_apps(#{optional_apps := Apps}) ->
    {StartedApps, _StartupCtx} = ct_helper:start_apps(Apps),
    StartedApps;
start_optional_apps(_) ->
    [].

setup_dominant(Options) ->
    DomainConfig = domain_config(Options),
    _ = ct_domain_config:upsert(DomainConfig),
    DomainConfigUpdate = domain_config_add_version(Options),
    _ = ct_domain_config:upsert(DomainConfigUpdate),
    ok.

configure_processing_apps(Options) ->
    ok = set_app_env(
        [ff_transfer, withdrawal, system, accounts, settlement, <<"RUB">>],
        create_company_account()
    ),
    ok = set_app_env(
        [ff_transfer, withdrawal, system, accounts, subagent, <<"RUB">>],
        create_company_account()
    ),
    ok = set_app_env(
        [ff_transfer, withdrawal, provider, <<"mocketbank">>, accounts, <<"RUB">>],
        create_company_account()
    ),
    ok = create_crunch_identity(
        payment_inst_identity_id(Options),
        provider_identity_id(Options),
        <<"good-one">>
    ),
    ok = create_crunch_identity(
        dummy_payment_inst_identity_id(Options),
        dummy_provider_identity_id(Options),
        <<"good-two">>
    ).

create_crunch_identity(PayInstIID, ProviderIID, ProviderID) ->
    PartyID = create_party(),
    PayInstIID = create_identity(PayInstIID, <<"ChurchPI">>, PartyID, ProviderID),
    ProviderIID = create_identity(ProviderIID, <<"ChurchPR">>, PartyID, ProviderID),
    ok.

create_company_account() ->
    PartyID = create_party(),
    IdentityID = create_identity(PartyID, <<"good-one">>),
    {ok, Currency} = ff_currency:get(<<"RUB">>),
    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    Identity = ff_identity_machine:identity(IdentityMachine),
    {ok, [{created, Account}]} = ff_account:create(PartyID, Identity, Currency),
    Account.

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(PartyID, ProviderID) ->
    ID = genlib:unique(),
    Name = <<"Test Identity">>,
    create_identity(ID, Name, PartyID, ProviderID).

create_identity(ID, Name, PartyID, ProviderID) ->
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => PartyID, provider => ProviderID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

set_app_env([App, Key | Path], Value) ->
    Env = genlib_app:env(App, Key, #{}),
    NewEnv = do_set_env(Path, Value, Env),
    application:set_env(App, Key, NewEnv).

do_set_env([], Value, _Env) ->
    Value;
do_set_env([Key | Path], Value, Env) ->
    SubEnv = maps:get(Key, Env, #{}),
    Env#{Key => do_set_env(Path, Value, SubEnv)}.

%% Default options
identity_provider_config(Options) ->
    Default = #{
        <<"good-one">> => #{
            payment_institution_id => 1,
            contract_template_id => 1,
            contractor_level => full
        },
        <<"good-two">> => #{
            payment_institution_id => 2,
            contract_template_id => 1,
            contractor_level => full
        }
    },
    maps:get(identity_provider_config, Options, Default).

services(Options) ->
    Default = #{
        ff_withdrawal_adapter_host => "http://fistful-server:8022/v1/ff_withdrawal_adapter_host",
        eventsink => "http://machinegun:8022/v1/event_sink",
        automaton => "http://machinegun:8022/v1/automaton",
        accounter => "http://shumway:8022/accounter",
        partymgmt => "http://party-management:8022/v1/processing/partymgmt",
        binbase => "http://localhost:8222/binbase",
        limiter => "http://limiter:8022/v1/limiter"
    },
    maps:get(services, Options, Default).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

% NOTE
% Allocate those domain object identifiers at least 100 apart from each other.
% This space might be used to define additional object in place (see below).
-define(EMPTY_ROUTING_RULESET, 0).
-define(PAYINST1_ROUTING_POLICIES, 100).
-define(PAYINST1_ROUTING_PROHIBITIONS, 200).
-define(PAYINST2_ROUTING_POLICIES, 300).

payment_inst_identity_id(Options) ->
    maps:get(payment_inst_identity_id, Options).

provider_identity_id(Options) ->
    maps:get(provider_identity_id, Options).

dummy_payment_inst_identity_id(Options) ->
    maps:get(dummy_payment_inst_identity_id, Options).

dummy_provider_identity_id(Options) ->
    maps:get(dummy_provider_identity_id, Options).

domain_config_add_version(Options) ->
    ProviderTermSet = #domain_ProvisionTermSet{
        wallet = #domain_WalletProvisionTerms{
            withdrawals = #domain_WithdrawalProvisionTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                payout_methods = {value, ?ordset([])},
                cash_limit =
                    {value,
                        ?cashrng(
                            {inclusive, ?cash(0, <<"RUB">>)},
                            {exclusive, ?cash(10000000, <<"RUB">>)}
                        )},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {provider, settlement},
                                        {product,
                                            {min_of,
                                                ?ordset([
                                                    ?fixed(10, <<"RUB">>),
                                                    ?share(5, 100, operation_amount, round_half_towards_zero)
                                                ])}}
                                    )
                                ]}
                        }
                    ]}
            }
        }
    },
    [ct_domain:withdrawal_provider(?prv(1), ?prx(2), provider_identity_id(Options), ProviderTermSet)].

domain_config(Options) ->
    ProviderTermSet = #domain_ProvisionTermSet{
        wallet = #domain_WalletProvisionTerms{
            withdrawals = #domain_WithdrawalProvisionTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                payout_methods = {value, ?ordset([])},
                cash_limit =
                    {value,
                        ?cashrng(
                            {inclusive, ?cash(0, <<"RUB">>)},
                            {exclusive, ?cash(10000000, <<"RUB">>)}
                        )},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {provider, settlement},
                                        ?fixed(20, <<"RUB">>)
                                    )
                                ]}
                        }
                    ]}
            }
        }
    },
    Default = [
        ct_domain:globals(?eas(1), [?payinst(1)]),
        ct_domain:external_account_set(?eas(1), <<"Default">>, ?cur(<<"RUB">>)),

        routing_ruleset(
            ?ruleset(?EMPTY_ROUTING_RULESET),
            <<"Empty Ruleset">>,
            {candidates, []}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES),
            <<"PayInst1 Withdrawal Ruleset">>,
            {delegates, [
                delegate(condition(party, <<"12345">>), ?ruleset(?PAYINST1_ROUTING_POLICIES + 1)),
                delegate(condition(party, <<"67890">>), ?ruleset(?PAYINST1_ROUTING_POLICIES + 3)),
                delegate({constant, true}, ?ruleset(?PAYINST1_ROUTING_POLICIES + 4))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 1),
            {delegates, [
                delegate(condition(cost_in, {0, 1000, <<"RUB">>}), ?ruleset(?PAYINST1_ROUTING_POLICIES + 2))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 2),
            {candidates, [
                candidate({constant, true}, ?trm(1)),
                candidate({constant, true}, ?trm(2))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 3),
            {candidates, [
                candidate({constant, true}, ?trm(3)),
                candidate({constant, true}, ?trm(4)),
                candidate({constant, true}, ?trm(5))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 4),
            {delegates, [
                delegate(
                    condition(cost_in, {300, 302, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 5)
                ),
                delegate(
                    condition(cost_in, {123123, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 6)
                ),
                delegate(
                    condition(cost_in, {100500, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 10)
                ),
                delegate(
                    condition(cost_in, {500100, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 11)
                ),
                delegate(
                    condition(cost_in, {500500, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 12)
                ),
                delegate(
                    condition(cost_in, {700700, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 13)
                ),
                delegate(
                    condition(cost_in, {800800, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 16)
                ),
                delegate(
                    condition(cost_in, {900900, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 17)
                ),
                delegate(
                    condition(cost_in, {901000, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 18)
                ),
                delegate(
                    condition(cost_in, {902000, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 19)
                ),
                delegate(
                    condition(cost_in, {903000, <<"RUB">>}),
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 19)
                ),
                delegate(
                    {condition,
                        {payment_tool,
                            {bank_card, #domain_BankCardCondition{
                                definition = {issuer_country_is, 'rus'}
                            }}}},
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 14)
                ),
                delegate(
                    {condition, {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}},
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 15)
                ),
                delegate(
                    {condition,
                        {payment_tool,
                            {generic,
                                {payment_service_is, #domain_PaymentServiceRef{
                                    id = <<"IND">>
                                }}}}},
                    ?ruleset(?PAYINST1_ROUTING_POLICIES + 15)
                )
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 5),
            {candidates, [
                candidate(condition(cost_in, {300, <<"RUB">>}), ?trm(1701)),
                candidate(condition(cost_in, {301, <<"RUB">>}), ?trm(1708))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 6),
            {candidates, [
                candidate({constant, true}, ?trm(6))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 10),
            {candidates, [
                % provider 4 will be discarded by proxy 6
                candidate({constant, true}, ?trm(401)),
                candidate({constant, true}, ?trm(501))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 11),
            {candidates, [
                candidate({constant, true}, ?trm(401)),
                candidate({constant, true}, ?trm(601)),
                candidate({constant, true}, ?trm(701)),
                candidate({constant, true}, ?trm(801))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 12),
            {candidates, [
                candidate({constant, true}, ?trm(901), 500),
                candidate({constant, true}, ?trm(1001), 1000)
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 13),
            {candidates, [
                candidate({constant, true}, ?trm(1101))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 14),
            {candidates, [
                candidate(
                    condition(cost_in, {0, 1000000, <<"RUB">>}),
                    ?trm(1)
                ),
                candidate(
                    condition(cost_in, {3000000, 10000000, <<"RUB">>}),
                    ?trm(307)
                )
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 15),
            {candidates, [
                candidate(
                    condition(cost_in, {0, 1000000, <<"RUB">>}),
                    ?trm(201)
                ),
                candidate(
                    condition(cost_in, {3000000, 10000000, <<"RUB">>}),
                    ?trm(307)
                )
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 16),
            {candidates, [
                candidate({constant, true}, ?trm(1800))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 17),
            {candidates, [
                candidate({constant, true}, ?trm(1900))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 18),
            {candidates, [
                candidate({constant, true}, ?trm(2000), 1000),
                candidate({constant, true}, ?trm(1900), 4000)
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_POLICIES + 19),
            {candidates, [
                candidate({constant, true}, ?trm(2200), 1000),
                candidate({constant, true}, ?trm(2100), 4000)
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST1_ROUTING_PROHIBITIONS),
            <<"PayInst1 Withdrawal Prohibitions">>,
            {candidates, [
                candidate({constant, true}, ?trm(4))
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST2_ROUTING_POLICIES),
            <<"PayInst2 Withdrawal Ruleset">>,
            {delegates, [
                delegate(
                    condition(cost_in, {123, <<"RUB">>}),
                    ?ruleset(?PAYINST2_ROUTING_POLICIES + 1)
                ),
                delegate(
                    {condition,
                        {payment_tool,
                            {crypto_currency, #domain_CryptoCurrencyCondition{
                                definition = {crypto_currency_is, ?crptcur(<<"Litecoin">>)}
                            }}}},
                    ?ruleset(?PAYINST2_ROUTING_POLICIES + 1)
                ),
                delegate(
                    {condition,
                        {payment_tool,
                            {digital_wallet, #domain_DigitalWalletCondition{
                                definition = {payment_service_is, ?pmtsrv(<<"webmoney">>)}
                            }}}},
                    ?ruleset(?PAYINST2_ROUTING_POLICIES + 1)
                ),
                delegate(
                    {condition,
                        {payment_tool,
                            {bank_card, #domain_BankCardCondition{
                                definition = {issuer_country_is, 'rus'}
                            }}}},
                    ?ruleset(?PAYINST2_ROUTING_POLICIES + 1)
                )
            ]}
        ),

        routing_ruleset(
            ?ruleset(?PAYINST2_ROUTING_POLICIES + 1),
            <<"PayInst2 Withdrawal Ruleset #1">>,
            {candidates, [
                candidate(
                    condition(cost_in, {0, 1000000, <<"RUB">>}),
                    ?trm(301)
                ),
                candidate(
                    condition(cost_in, {3000000, 10000000, <<"RUB">>}),
                    ?trm(307)
                )
            ]}
        ),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Generic Payment Institution">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                withdrawal_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(?PAYINST1_ROUTING_POLICIES),
                    prohibitions = ?ruleset(?PAYINST1_ROUTING_PROHIBITIONS)
                },
                inspector = {value, ?insp(1)},
                residences = ['rus'],
                realm = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity = payment_inst_identity_id(Options),
                payment_system =
                    {decisions, [
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"uber">>}
                                            }}},
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"sber">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"VISA">>)}
                        },
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"NSPK MIR">>},
                                                bank_name = {equals, <<"poopa">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"NSPK MIR">>)}
                        }
                    ]}
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Generic Payment Institution">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = ['rus'],
                realm = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity = dummy_payment_inst_identity_id(Options),
                withdrawal_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(?PAYINST2_ROUTING_POLICIES),
                    prohibitions = ?ruleset(?EMPTY_ROUTING_RULESET)
                },
                payment_system =
                    {decisions, [
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"uber">>}
                                            }}},
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"sber">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"VISA">>)}
                        },
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"NSPK MIR">>},
                                                bank_name = {equals, <<"poopa">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"NSPK MIR">>)}
                        }
                    ]}
            }
        }},

        ct_domain:system_account_set(?sas(1), <<"System">>, ?cur(<<"RUB">>)),

        ct_domain:inspector(?insp(1), <<"Low Life">>, ?prx(1), #{<<"risk_score">> => <<"low">>}),
        ct_domain:proxy(?prx(1), <<"Inspector proxy">>),
        ct_domain:proxy(?prx(2), <<"Mocket proxy">>, <<"http://localhost:8222/bank">>),
        ct_domain:proxy(?prx(3), <<"Quote proxy">>, <<"http://localhost:8222/quotebank">>),
        ct_domain:proxy(?prx(6), <<"Down proxy">>, <<"http://localhost:8222/downbank">>),
        ct_domain:proxy(?prx(7), <<"Another down proxy">>, <<"http://localhost:8222/downbank2">>),
        ct_domain:proxy(?prx(8), <<"Sleep proxy">>, <<"http://localhost:8222/sleepybank">>),

        ct_domain:withdrawal_provider(?prv(1), ?prx(2), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(2), ?prx(2), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(3), ?prx(3), dummy_provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(4), ?prx(6), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(5), ?prx(2), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(6), ?prx(6), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(7), ?prx(6), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(8), ?prx(2), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(9), ?prx(7), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(10), ?prx(6), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(11), ?prx(8), provider_identity_id(Options), ProviderTermSet),
        ct_domain:withdrawal_provider(?prv(16), ?prx(2), provider_identity_id(Options), undefined),
        ct_domain:withdrawal_provider(?prv(17), ?prx(2), provider_identity_id(Options), ProviderTermSet),

        ct_domain:contract_template(?tmpl(1), ?trms(1)),
        ct_domain:term_set_hierarchy(?trms(1), [ct_domain:timed_term_set(default_termset(Options))]),
        ct_domain:contract_template(?tmpl(2), ?trms(2)),
        ct_domain:term_set_hierarchy(?trms(2), [ct_domain:timed_term_set(company_termset(Options))]),

        ct_domain:withdrawal_terminal(?trm(1), ?prv(1)),
        ct_domain:withdrawal_terminal(?trm(2), ?prv(1)),
        ct_domain:withdrawal_terminal(?trm(3), ?prv(1)),
        ct_domain:withdrawal_terminal(?trm(4), ?prv(1)),
        ct_domain:withdrawal_terminal(?trm(5), ?prv(1)),

        ct_domain:withdrawal_terminal(?trm(6), ?prv(16)),

        ct_domain:withdrawal_terminal(?trm(201), ?prv(2)),

        ct_domain:withdrawal_terminal(?trm(301), ?prv(3)),
        ct_domain:withdrawal_terminal(
            ?trm(307),
            ?prv(3),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        currencies = {value, ?ordset([?cur(<<"BTC">>)])},
                        payout_methods = {value, ?ordset([])},
                        cash_limit =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(1000000, <<"BTC">>)},
                                    {exclusive, ?cash(10000000, <<"BTC">>)}
                                )},
                        cash_flow = {value, ?ordset([])}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(?trm(401), ?prv(4)),
        ct_domain:withdrawal_terminal(?trm(501), ?prv(5)),
        ct_domain:withdrawal_terminal(?trm(601), ?prv(6)),
        ct_domain:withdrawal_terminal(?trm(701), ?prv(7)),
        ct_domain:withdrawal_terminal(?trm(801), ?prv(8)),
        ct_domain:withdrawal_terminal(?trm(901), ?prv(9)),
        ct_domain:withdrawal_terminal(?trm(1001), ?prv(10)),
        ct_domain:withdrawal_terminal(?trm(1101), ?prv(11)),

        ct_domain:withdrawal_terminal(?trm(1701), ?prv(17)),
        ct_domain:withdrawal_terminal(
            ?trm(1708),
            ?prv(17),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        cash_flow =
                            {decisions, [
                                #domain_CashFlowDecision{
                                    if_ = {constant, true},
                                    then_ =
                                        {value, [
                                            ?cfpost(
                                                {system, settlement},
                                                {provider, settlement},
                                                ?fixed(16, <<"RUB">>)
                                            )
                                        ]}
                                }
                            ]}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(
            ?trm(1800),
            ?prv(1),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"BTC">>)])},
                        turnover_limit =
                            {value, [
                                ?trnvrlimit(?LIMIT_TURNOVER_NUM_PAYTOOL_ID1, 1000)
                            ]}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(
            ?trm(1900),
            ?prv(1),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        turnover_limit =
                            {value, [
                                ?trnvrlimit(?LIMIT_TURNOVER_NUM_PAYTOOL_ID2, 0)
                            ]}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(
            ?trm(2000),
            ?prv(1),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        turnover_limit =
                            {value, [
                                ?trnvrlimit(?LIMIT_TURNOVER_NUM_PAYTOOL_ID2, 1000)
                            ]}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(
            ?trm(2100),
            ?prv(1),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        turnover_limit =
                            {value, [
                                ?trnvrlimit(?LIMIT_TURNOVER_AMOUNT_PAYTOOL_ID1, 1804000)
                            ]}
                    }
                }
            }
        ),

        ct_domain:withdrawal_terminal(
            ?trm(2200),
            ?prv(1),
            #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        turnover_limit =
                            {value, [
                                ?trnvrlimit(?LIMIT_TURNOVER_AMOUNT_PAYTOOL_ID2, 903000)
                            ]}
                    }
                }
            }
        ),

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),
        ct_domain:currency(?cur(<<"EUR">>)),
        ct_domain:currency(?cur(<<"BTC">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_BANK_CARD(<<"VISA">>))),
        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_GENERIC(<<"IND">>))),
        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_DIGITAL_WALLET(<<"webmoney">>))),
        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"Litecoin">>))),
        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"bitcoin_cash">>))),
        ct_domain:payment_method(?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"ripple">>))),

        ct_domain:payment_system(?pmtsys(<<"VISA">>), <<"VISA">>),
        ct_domain:payment_system(?pmtsys(<<"NSPK MIR">>), <<"NSPK MIR">>),

        ct_domain:payment_service(?pmtsrv(<<"webmoney">>), <<"Webmoney">>),
        ct_domain:payment_service(?pmtsrv(<<"qiwi">>), <<"Qiwi">>),
        ct_domain:payment_service(?pmtsrv(<<"IND">>), <<"INDbank">>),
        ct_domain:crypto_currency(?crptcur(<<"Litecoin">>), <<"Litecoin">>),
        ct_domain:crypto_currency(?crptcur(<<"bitcoin_cash">>), <<"bitcoin_cash">>),
        ct_domain:crypto_currency(?crptcur(<<"ripple">>), <<"ripple">>)
    ],
    maps:get(domain_config, Options, Default).

default_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(5000001, <<"RUB">>)}
                                )}
                    },
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"USD">>)},
                                    {exclusive, ?cash(10000001, <<"USD">>)}
                                )}
                    }
                ]},
            withdrawals = #domain_WithdrawalServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                attempt_limit = {value, #domain_AttemptLimit{attempts = 3}},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"EUR">>)},
                                        {exclusive, ?cash(10000001, <<"EUR">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"USD">>)},
                                        {exclusive, ?cash(10000001, <<"USD">>)}
                                    )}
                        }
                    ]},
                methods =
                    {value,
                        ?ordset([
                            ?pmt(?PAYMENT_METHOD_BANK_CARD(<<"VISA">>)),
                            ?pmt(?PAYMENT_METHOD_GENERIC(<<"IND">>)),
                            ?pmt(?PAYMENT_METHOD_DIGITAL_WALLET(<<"webmoney">>)),
                            ?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"Litecoin">>)),
                            ?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"bitcoin_cash">>)),
                            ?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(<<"ripple">>))
                        ])},
                cash_flow =
                    {decisions, [
                        % this is impossible cash flow decision to check
                        % if withdrawals cash flow calculates properly
                        #domain_CashFlowDecision{
                            if_ = {
                                condition,
                                {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{}}}
                            },
                            then_ = {value, []}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {any_of,
                                    ?ordset([
                                        {all_of,
                                            ?ordset([
                                                {condition, {currency_is, ?cur(<<"RUB">>)}},
                                                {condition,
                                                    {payment_tool,
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is = #domain_PaymentSystemRef{
                                                                        id = <<"VISA">>
                                                                    }
                                                                }}
                                                        }}}}
                                            ])},
                                        {all_of,
                                            ?ordset([
                                                condition(cost_in, {424242, <<"RUB">>})
                                            ])}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"EUR">>)}},
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is = #domain_PaymentSystemRef{
                                                                id = <<"VISA">>
                                                            }
                                                        }}
                                                }}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"USD">>)}},
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is = #domain_PaymentSystemRef{
                                                                id = <<"VISA">>
                                                            }
                                                        }}
                                                }}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition,
                                            {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition, {payment_tool, {digital_wallet, #domain_DigitalWalletCondition{}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition,
                                            {payment_tool,
                                                {generic,
                                                    {payment_service_is, #domain_PaymentServiceRef{
                                                        id = <<"IND">>
                                                    }}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition,
                                            {payment_tool,
                                                {generic,
                                                    {payment_service_is, #domain_PaymentServiceRef{
                                                        id = <<"qiwi">>
                                                    }}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        }
                    ]}
            },
            w2w = #domain_W2WServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
                allow = {constant, true},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"EUR">>)},
                                        {exclusive, ?cash(10000001, <<"EUR">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"USD">>)},
                                        {exclusive, ?cash(10000001, <<"USD">>)}
                                    )}
                        }
                    ]},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        }
                    ]},
                fees =
                    {decisions, [
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        }
                    ]}
            }
        }
    },
    maps:get(default_termset, Options, Default).

company_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(5000000, <<"RUB">>)}
                                )}
                    },
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"USD">>)},
                                    {exclusive, ?cash(5000000, <<"USD">>)}
                                )}
                    }
                ]}
        }
    },
    maps:get(company_termset, Options, Default).

routing_ruleset(?ruleset(ID) = Ref, Decisions) ->
    routing_ruleset(Ref, genlib:format("Withdrawal Ruleset #~B", [ID]), Decisions).

routing_ruleset(Ref, Name, Decisions) ->
    {routing_rules, #domain_RoutingRulesObject{
        ref = Ref,
        data = #domain_RoutingRuleset{
            name = Name,
            decisions = Decisions
        }
    }}.

condition(cost_in, {CostExact, Currency}) ->
    {condition,
        {cost_in,
            ?cashrng(
                {inclusive, ?cash(CostExact, Currency)},
                {inclusive, ?cash(CostExact, Currency)}
            )}};
condition(cost_in, {Min, Max, Currency}) ->
    {condition,
        {cost_in,
            ?cashrng(
                {inclusive, ?cash(Min, Currency)},
                {exclusive, ?cash(Max, Currency)}
            )}};
condition(party, ID) ->
    {condition, {party, #domain_PartyCondition{id = ID}}}.

delegate(Allowed, RuleSetRef) ->
    #domain_RoutingDelegate{
        allowed = Allowed,
        ruleset = RuleSetRef
    }.

candidate(Allowed, Terminal) ->
    #domain_RoutingCandidate{
        allowed = Allowed,
        terminal = Terminal
    }.

candidate(Allowed, Terminal, Prio) ->
    #domain_RoutingCandidate{
        allowed = Allowed,
        terminal = Terminal,
        priority = Prio
    }.
