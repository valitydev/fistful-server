%%%
%%% Managed party
%%%
%%% TODOs
%%%
%%%  - We expect party to exist, which is certainly not the general case.
%%%

-module(ff_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-type id() :: dmsl_domain_thrift:'PartyID'().
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type wallet_id() :: dmsl_domain_thrift:'WalletID'().
-type revision() :: dmsl_domain_thrift:'PartyRevision'().
-type terms() :: dmsl_domain_thrift:'TermSet'().
-type attempt_limit() :: integer().

-type party_params() :: #{
    email := binary()
}.

-type validate_account_creation_error() ::
    currency_validation_error().

-type validate_deposit_creation_error() ::
    currency_validation_error()
    | {bad_deposit_amount, Cash :: cash()}.

-type get_contract_terms_error() ::
    {party_not_found, id()}
    | {contract_not_found, id()}
    | {party_not_exists_yet, id()}.

-type validate_destination_creation_error() ::
    withdrawal_method_validation_error().

-type validate_withdrawal_creation_error() ::
    currency_validation_error()
    | withdrawal_method_validation_error()
    | cash_range_validation_error().

-type validate_w2w_transfer_creation_error() ::
    w2w_forbidden_error()
    | currency_validation_error()
    | {bad_w2w_transfer_amount, Cash :: cash()}
    | invalid_w2w_terms_error().

-export_type([id/0]).
-export_type([revision/0]).
-export_type([terms/0]).
-export_type([contract_id/0]).
-export_type([wallet_id/0]).
-export_type([party_params/0]).
-export_type([validate_deposit_creation_error/0]).
-export_type([validate_account_creation_error/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_destination_creation_error/0]).
-export_type([validate_withdrawal_creation_error/0]).
-export_type([withdrawal_method_validation_error/0]).
-export_type([validate_w2w_transfer_creation_error/0]).
-export_type([cash/0]).
-export_type([cash_range/0]).
-export_type([attempt_limit/0]).
-export_type([provision_term_set/0]).
-export_type([method_ref/0]).

-type inaccessibility() ::
    {inaccessible, blocked | suspended}.

-export_type([inaccessibility/0]).

-export([create/1]).
-export([create/2]).
-export([is_accessible/1]).
-export([create_contract/2]).
-export([get_revision/1]).
-export([change_contractor_level/3]).
-export([validate_account_creation/2]).
-export([validate_destination_creation/2]).
-export([get_withdrawal_methods/1]).
-export([validate_withdrawal_creation/3]).
-export([validate_deposit_creation/2]).
-export([validate_w2w_transfer_creation/2]).
-export([validate_wallet_limits/2]).
-export([get_contract_terms/6]).
-export([compute_payment_institution/3]).
-export([compute_routing_ruleset/3]).
-export([compute_provider_terminal_terms/4]).
-export([get_withdrawal_cash_flow_plan/1]).
-export([get_w2w_cash_flow_plan/1]).
-export([get_identity_payment_institution_id/1]).

%% Internal types
-type cash() :: ff_cash:cash().
-type method() :: ff_resource:method().
-type wallet_terms() :: dmsl_domain_thrift:'WalletServiceTerms'().
-type withdrawal_terms() :: dmsl_domain_thrift:'WithdrawalServiceTerms'().
-type w2w_terms() :: dmsl_domain_thrift:'W2WServiceTerms'().
-type currency_id() :: ff_currency:id().
-type currency_ref() :: dmsl_domain_thrift:'CurrencyRef'().
-type domain_cash() :: dmsl_domain_thrift:'Cash'().
-type domain_cash_range() :: dmsl_domain_thrift:'CashRange'().
-type domain_revision() :: ff_domain_config:revision().
-type timestamp() :: ff_time:timestamp_ms().
-type wallet() :: ff_wallet:wallet_state().
-type payinst_ref() :: ff_payment_institution:payinst_ref().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payment_institution_id() :: ff_payment_institution:id().
-type routing_ruleset_ref() :: dmsl_domain_thrift:'RoutingRulesetRef'().
-type routing_ruleset() :: dmsl_domain_thrift:'RoutingRuleset'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type method_ref() :: dmsl_domain_thrift:'PaymentMethodRef'().
-type provision_term_set() :: dmsl_domain_thrift:'ProvisionTermSet'().
-type bound_type() :: 'exclusive' | 'inclusive'.
-type cash_range() :: {{bound_type(), cash()}, {bound_type(), cash()}}.

-type currency_validation_error() ::
    {terms_violation, {not_allowed_currency, {currency_ref(), ordsets:ordset(currency_ref())}}}.

-type cash_range_validation_error() :: {terms_violation, {cash_range, {cash(), cash_range()}}}.
-type w2w_forbidden_error() :: {terms_violation, w2w_forbidden}.
-type attempt_limit_error() :: {terms_violation, {attempt_limit, attempt_limit()}}.

-type not_reduced_error() :: {not_reduced, {Name :: atom(), TermsPart :: any()}}.

-type invalid_withdrawal_terms_error() ::
    invalid_wallet_terms_error()
    | {invalid_terms, not_reduced_error()}
    | {invalid_terms, {undefined_withdrawal_terms, wallet_terms()}}.

-type invalid_wallet_terms_error() ::
    {invalid_terms, not_reduced_error()}
    | {invalid_terms, undefined_wallet_terms}.

-type invalid_w2w_terms_error() ::
    {invalid_terms, not_reduced_error()}
    | {invalid_terms, undefined_wallet_terms}
    | {invalid_terms, {undefined_w2w_terms, wallet_terms()}}.

-type withdrawal_method_validation_error() ::
    {terms_violation, {not_allowed_withdrawal_method, {method_ref(), ordsets:ordset(method_ref())}}}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec create(id()) ->
    ok
    | {error, exists}.
create(ID) ->
    create(ID, #{email => <<"bob@example.org">>}).

-spec create(id(), party_params()) ->
    ok
    | {error, exists}.
create(ID, Params) ->
    do_create_party(ID, Params).

-spec is_accessible(id()) ->
    {ok, accessible}
    | {error, inaccessibility()}
    | {error, notfound}.
is_accessible(ID) ->
    case do_get_party(ID) of
        #domain_Party{blocking = {blocked, _}} ->
            {error, {inaccessible, blocked}};
        #domain_Party{suspension = {suspended, _}} ->
            {error, {inaccessible, suspended}};
        #domain_Party{} ->
            {ok, accessible};
        #payproc_PartyNotFound{} ->
            {error, notfound}
    end.

-spec get_revision(id()) -> {ok, revision()} | {error, {party_not_found, id()}}.
get_revision(ID) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:get_revision(ID, Client, Context) of
        {ok, Revision} ->
            {ok, Revision};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}}
    end.

%%

-type contract_prototype() :: #{
    payinst := dmsl_domain_thrift:'PaymentInstitutionRef'(),
    contract_template := dmsl_domain_thrift:'ContractTemplateRef'(),
    contractor_level := dmsl_domain_thrift:'ContractorIdentificationLevel'()
}.

-spec create_contract(id(), contract_prototype()) ->
    {ok, contract_id()}
    | {error, inaccessibility()}
    | {error, invalid}.
create_contract(ID, Prototype) ->
    do(fun() ->
        ContractID = generate_contract_id(),
        Changeset = construct_contract_changeset(ContractID, Prototype),
        Claim = unwrap(do_create_claim(ID, Changeset)),
        accepted = do_accept_claim(ID, Claim),
        ContractID
    end).

%%

-spec change_contractor_level(id(), contract_id(), dmsl_domain_thrift:'ContractorIdentificationLevel'()) ->
    ok
    | {error, inaccessibility()}
    | {error, invalid}.
change_contractor_level(ID, ContractID, ContractorLevel) ->
    do(fun() ->
        Changeset = construct_level_changeset(ContractID, ContractorLevel),
        Claim = unwrap(do_create_claim(ID, Changeset)),
        accepted = do_accept_claim(ID, Claim),
        ok
    end).

-spec get_identity_payment_institution_id(ff_identity:identity_state()) -> Result when
    Result :: {ok, payment_institution_id()} | {error, Error},
    Error ::
        {party_not_found, id()}
        | {contract_not_found, id()}
        | no_return().
get_identity_payment_institution_id(Identity) ->
    do(fun() ->
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        Contract = unwrap(do_get_contract(PartyID, ContractID)),
        #domain_PaymentInstitutionRef{id = ID} = Contract#domain_Contract.payment_institution,
        ID
    end).

-spec get_contract_terms(PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision) -> Result when
    PartyID :: id(),
    ContractID :: contract_id(),
    Varset :: ff_varset:varset(),
    Timestamp :: timestamp(),
    PartyRevision :: revision(),
    DomainRevision :: domain_revision(),
    Result :: {ok, terms()} | {error, Error},
    Error :: get_contract_terms_error().
get_contract_terms(PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision) ->
    DomainVarset = ff_varset:encode_contract_terms_varset(Varset),
    TimestampStr = ff_time:to_rfc3339(Timestamp),
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_contract_terms(
        PartyID,
        ContractID,
        TimestampStr,
        {revision, PartyRevision},
        DomainRevision,
        DomainVarset,
        Client,
        Context
    ),
    case Result of
        {ok, Terms} ->
            {ok, Terms};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, PartyID}};
        {error, #payproc_ContractNotFound{}} ->
            {error, {contract_not_found, ContractID}};
        {error, #payproc_PartyNotExistsYet{}} ->
            {error, {party_not_exists_yet, PartyID}}
    end.

-spec compute_payment_institution(PaymentInstitutionRef, Varset, DomainRevision) -> Result when
    PaymentInstitutionRef :: payinst_ref(),
    Varset :: ff_varset:varset(),
    DomainRevision :: domain_revision(),
    Result :: {ok, payment_institution()} | {error, payinst_not_found}.
compute_payment_institution(PaymentInstitutionRef, Varset, DomainRevision) ->
    DomainVarset = ff_varset:encode(Varset),
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_payment_institution(
        PaymentInstitutionRef,
        DomainRevision,
        DomainVarset,
        Client,
        Context
    ),
    case Result of
        {ok, PaymentInstitution} ->
            {ok, PaymentInstitution};
        {error, #payproc_PaymentInstitutionNotFound{}} ->
            {error, payinst_not_found}
    end.

-spec compute_routing_ruleset(RoutingRulesetRef, Varset, DomainRevision) -> Result when
    RoutingRulesetRef :: routing_ruleset_ref(),
    Varset :: ff_varset:varset(),
    DomainRevision :: domain_revision(),
    Result :: {ok, routing_ruleset()} | {error, ruleset_not_found}.
compute_routing_ruleset(RoutingRulesetRef, Varset, DomainRevision) ->
    DomainVarset = ff_varset:encode(Varset),
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_routing_ruleset(
        RoutingRulesetRef,
        DomainRevision,
        DomainVarset,
        Client,
        Context
    ),
    case Result of
        {ok, RoutingRuleset} ->
            {ok, RoutingRuleset};
        {error, #payproc_RuleSetNotFound{}} ->
            {error, ruleset_not_found}
    end.

-spec compute_provider_terminal_terms(ProviderRef, TerminalRef, Varset, DomainRevision) -> Result when
    ProviderRef :: provider_ref(),
    TerminalRef :: terminal_ref(),
    Varset :: ff_varset:varset(),
    DomainRevision :: domain_revision(),
    Result :: {ok, provision_term_set()} | {error, provider_not_found} | {error, terminal_not_found}.
compute_provider_terminal_terms(ProviderRef, TerminalRef, Varset, DomainRevision) ->
    DomainVarset = ff_varset:encode(Varset),
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_provider_terminal_terms(
        ProviderRef,
        TerminalRef,
        DomainRevision,
        DomainVarset,
        Client,
        Context
    ),
    case Result of
        {ok, RoutingRuleset} ->
            {ok, RoutingRuleset};
        {error, #payproc_ProviderNotFound{}} ->
            {error, provider_not_found};
        {error, #payproc_TerminalNotFound{}} ->
            {error, terminal_not_found};
        {error, #payproc_ProvisionTermSetUndefined{}} ->
            {error, provision_termset_undefined}
    end.

-spec validate_account_creation(terms(), currency_id()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: currency_validation_error().
validate_account_creation(Terms, CurrencyID) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun() ->
        {ok, valid} = validate_wallet_currencies_term_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms))
    end).

-spec get_withdrawal_methods(terms()) ->
    ordsets:ordset(method_ref()).
get_withdrawal_methods(Terms) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    #domain_WalletServiceTerms{withdrawals = WithdrawalTerms} = WalletTerms,
    #domain_WithdrawalServiceTerms{methods = MethodsSelector} = WithdrawalTerms,
    {ok, valid} = do_validate_terms_is_reduced([{withdrawal_methods, MethodsSelector}]),
    {value, Methods} = MethodsSelector,
    Methods.

-spec validate_destination_creation(terms(), method()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_destination_creation_error().
validate_destination_creation(Terms, Method) ->
    Methods = get_withdrawal_methods(Terms),
    validate_withdrawal_terms_method(Method, Methods).

-spec validate_withdrawal_creation(terms(), cash(), method()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_withdrawal_creation_error().
validate_withdrawal_creation(Terms, {_, CurrencyID} = Cash, Method) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun() ->
        {ok, valid} = validate_withdrawal_terms_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms)),
        #domain_WalletServiceTerms{withdrawals = WithdrawalTerms} = WalletTerms,
        valid = unwrap(validate_withdrawal_terms_currency(CurrencyID, WithdrawalTerms)),
        valid = unwrap(validate_withdrawal_cash_limit(Cash, WithdrawalTerms)),
        valid = unwrap(validate_withdrawal_attempt_limit(WithdrawalTerms)),
        #domain_WithdrawalServiceTerms{methods = {value, Methods}} = WithdrawalTerms,
        valid = unwrap(validate_withdrawal_terms_method(Method, Methods))
    end).

-spec validate_deposit_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_deposit_creation_error().
validate_deposit_creation(_Terms, {Amount, _Currency} = Cash) when Amount == 0 ->
    {error, {bad_deposit_amount, Cash}};
validate_deposit_creation(Terms, {_Amount, CurrencyID} = _Cash) ->
    do(fun() ->
        #domain_TermSet{wallets = WalletTerms} = Terms,
        {ok, valid} = validate_wallet_currencies_term_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms))
    end).

-spec validate_w2w_transfer_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_w2w_transfer_creation_error().
validate_w2w_transfer_creation(_Terms, {Amount, _Currency} = Cash) when Amount < 1 ->
    {error, {bad_w2w_transfer_amount, Cash}};
validate_w2w_transfer_creation(Terms, {_Amount, CurrencyID} = Cash) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun() ->
        {ok, valid} = validate_w2w_terms_is_reduced(WalletTerms),
        #domain_WalletServiceTerms{w2w = W2WServiceTerms} = WalletTerms,
        valid = unwrap(validate_w2w_terms_currency(CurrencyID, W2WServiceTerms)),
        valid = unwrap(validate_w2w_cash_limit(Cash, W2WServiceTerms)),
        valid = unwrap(validate_w2w_allow(W2WServiceTerms))
    end).

-spec get_withdrawal_cash_flow_plan(terms()) -> {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_withdrawal_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            withdrawals = #domain_WithdrawalServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

-spec get_w2w_cash_flow_plan(terms()) -> {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_w2w_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            w2w = #domain_W2WServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

%% Internal functions

generate_contract_id() ->
    generate_uuid().

generate_uuid() ->
    % TODO
    %  - Snowflake, anyone?
    uuid:uuid_to_string(uuid:get_v4(), binary_nodash).

%% Party management client

do_create_party(ID, Params) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:create(ID, construct_party_params(Params), Client, Context) of
        ok ->
            ok;
        {error, #payproc_PartyExists{}} ->
            {error, exists}
    end.

do_get_party(ID) ->
    {Client, Context} = get_party_client(),
    Result = do(fun() ->
        Revision = unwrap(party_client_thrift:get_revision(ID, Client, Context)),
        unwrap(party_client_thrift:checkout(ID, {revision, Revision}, Client, Context))
    end),
    case Result of
        {ok, Party} ->
            Party;
        {error, #payproc_PartyNotFound{} = Reason} ->
            Reason
    end.

do_get_contract(ID, ContractID) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:get_contract(ID, ContractID, Client, Context) of
        {ok, #domain_Contract{} = Contract} ->
            {ok, Contract};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}};
        {error, #payproc_ContractNotFound{}} ->
            {error, {contract_not_found, ContractID}}
    end.

do_create_claim(ID, Changeset) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:create_claim(ID, Changeset, Client, Context) of
        {ok, Claim} ->
            {ok, Claim};
        {error, #payproc_InvalidChangeset{
            reason = {invalid_wallet, #payproc_InvalidWallet{reason = {contract_terms_violated, _}}}
        }} ->
            {error, invalid};
        {error, #payproc_InvalidPartyStatus{status = Status}} ->
            {error, construct_inaccessibilty(Status)}
    end.

do_accept_claim(ID, Claim) ->
    % TODO
    %  - We assume here that there's only one actor (identity machine) acting in
    %    such a way which may cause conflicts.
    ClaimID = Claim#payproc_Claim.id,
    Revision = Claim#payproc_Claim.revision,
    {Client, Context} = get_party_client(),
    case party_client_thrift:accept_claim(ID, ClaimID, Revision, Client, Context) of
        ok ->
            accepted;
        {error, #payproc_InvalidClaimStatus{status = {accepted, _}}} ->
            accepted
    end.

get_party_client() ->
    Context = ff_context:load(),
    Client = ff_context:get_party_client(Context),
    ClientContext = ff_context:get_party_client_context(Context),
    {Client, ClientContext}.

construct_inaccessibilty({blocking, _}) ->
    {inaccessible, blocked};
construct_inaccessibilty({suspension, _}) ->
    {inaccessible, suspended}.

%%

-define(CONTRACTOR_MOD(ID, Mod),
    {contractor_modification, #payproc_ContractorModificationUnit{id = ID, modification = Mod}}
).

-define(CONTRACT_MOD(ID, Mod),
    {contract_modification, #payproc_ContractModificationUnit{id = ID, modification = Mod}}
).

construct_party_params(#{email := Email}) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            registration_email = Email
        }
    }.

construct_contract_changeset(ContractID, #{
    payinst := PayInstRef,
    contract_template := ContractTemplateRef,
    contractor_level := ContractorLevel
}) ->
    [
        ?CONTRACTOR_MOD(
            ContractID,
            {creation,
                {private_entity,
                    {russian_private_entity, #domain_RussianPrivateEntity{
                        % TODO
                        first_name = <<>>,
                        second_name = <<>>,
                        middle_name = <<>>,
                        contact_info = #domain_ContactInfo{}
                    }}}}
        ),
        ?CONTRACTOR_MOD(
            ContractID,
            {identification_level_modification, ContractorLevel}
        ),
        ?CONTRACT_MOD(
            ContractID,
            {creation, #payproc_ContractParams{
                contractor_id = ContractID,
                payment_institution = PayInstRef,
                template = ContractTemplateRef
            }}
        )
    ].

construct_level_changeset(ContractID, ContractorLevel) ->
    [
        ?CONTRACTOR_MOD(
            ContractID,
            {identification_level_modification, ContractorLevel}
        )
    ].

%% Terms stuff

-spec validate_wallet_currencies_term_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.
validate_wallet_currencies_term_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_wallet_currencies_term_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = CurrenciesSelector
    } = Terms,
    do_validate_terms_is_reduced([
        {wallet_currencies, CurrenciesSelector}
    ]).

-spec validate_withdrawal_terms_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, invalid_withdrawal_terms_error()}.
validate_withdrawal_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_withdrawal_terms_is_reduced(#domain_WalletServiceTerms{withdrawals = undefined} = WalletTerms) ->
    {error, {invalid_terms, {undefined_withdrawal_terms, WalletTerms}}};
validate_withdrawal_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = WalletCurrenciesSelector,
        withdrawals = WithdrawalTerms
    } = Terms,
    #domain_WithdrawalServiceTerms{
        currencies = WithdrawalCurrenciesSelector,
        cash_limit = CashLimitSelector,
        cash_flow = CashFlowSelector,
        attempt_limit = AttemptLimitSelector,
        methods = MethodsSelector
    } = WithdrawalTerms,
    do_validate_terms_is_reduced([
        {wallet_currencies, WalletCurrenciesSelector},
        {withdrawal_currencies, WithdrawalCurrenciesSelector},
        {withdrawal_cash_limit, CashLimitSelector},
        {withdrawal_cash_flow, CashFlowSelector},
        {withdrawal_attempt_limit, AttemptLimitSelector},
        {withdrawal_methods, MethodsSelector}
    ]).

-spec validate_w2w_terms_is_reduced(wallet_terms() | undefined) -> {ok, valid} | {error, invalid_w2w_terms_error()}.
validate_w2w_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_w2w_terms_is_reduced(#domain_WalletServiceTerms{w2w = undefined} = WalletTerms) ->
    {error, {invalid_terms, {undefined_w2w_terms, WalletTerms}}};
validate_w2w_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        w2w = W2WServiceTerms
    } = Terms,
    #domain_W2WServiceTerms{
        currencies = W2WCurrenciesSelector,
        cash_limit = CashLimitSelector,
        cash_flow = CashFlowSelector,
        fees = FeeSelector
    } = W2WServiceTerms,
    do_validate_terms_is_reduced([
        {w2w_currencies, W2WCurrenciesSelector},
        {w2w_cash_limit, CashLimitSelector},
        {w2w_cash_flow, CashFlowSelector},
        {w2w_fee, FeeSelector}
    ]).

-spec do_validate_terms_is_reduced([{atom(), Selector :: any()}]) ->
    {ok, valid} | {error, {invalid_terms, not_reduced_error()}}.
do_validate_terms_is_reduced([]) ->
    {ok, valid};
do_validate_terms_is_reduced([{Name, Terms} | TermsTail]) ->
    case selector_is_reduced(Terms) of
        Result when Result =:= reduced orelse Result =:= is_undefined ->
            do_validate_terms_is_reduced(TermsTail);
        not_reduced ->
            {error, {invalid_terms, {not_reduced, {Name, Terms}}}}
    end.

selector_is_reduced(undefined) ->
    is_undefined;
selector_is_reduced({value, _Value}) ->
    reduced;
selector_is_reduced({decisions, _Decisions}) ->
    not_reduced.

-spec validate_wallet_terms_currency(currency_id(), wallet_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_wallet_terms_currency(CurrencyID, Terms) ->
    #domain_WalletServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_wallet_limits(terms(), wallet()) ->
    {ok, valid}
    | {error, invalid_wallet_terms_error()}
    | {error, cash_range_validation_error()}.
validate_wallet_limits(Terms, Wallet) ->
    do(fun() ->
        #domain_TermSet{wallets = WalletTerms} = Terms,
        valid = unwrap(validate_wallet_limits_terms_is_reduced(WalletTerms)),
        #domain_WalletServiceTerms{
            wallet_limit = {value, CashRange}
        } = WalletTerms,
        Account = ff_wallet:account(Wallet),
        valid = unwrap(validate_account_balance(Account, CashRange))
    end).

-spec validate_wallet_limits_terms_is_reduced(wallet_terms()) -> {ok, valid} | {error, {invalid_terms, _Details}}.
validate_wallet_limits_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        wallet_limit = WalletLimitSelector
    } = Terms,
    do_validate_terms_is_reduced([
        {wallet_limit, WalletLimitSelector}
    ]).

-spec validate_withdrawal_terms_currency(currency_id(), withdrawal_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_withdrawal_terms_currency(CurrencyID, Terms) ->
    #domain_WithdrawalServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_withdrawal_cash_limit(cash(), withdrawal_terms()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_withdrawal_cash_limit(Cash, Terms) ->
    #domain_WithdrawalServiceTerms{
        cash_limit = {value, CashRange}
    } = Terms,
    validate_cash_range(ff_dmsl_codec:marshal(cash, Cash), CashRange).

-spec validate_withdrawal_attempt_limit(withdrawal_terms()) -> {ok, valid} | {error, attempt_limit_error()}.
validate_withdrawal_attempt_limit(Terms) ->
    #domain_WithdrawalServiceTerms{
        attempt_limit = AttemptLimit
    } = Terms,
    case AttemptLimit of
        undefined ->
            {ok, valid};
        {value, Limit} ->
            validate_attempt_limit(ff_dmsl_codec:unmarshal(attempt_limit, Limit))
    end.

-spec validate_withdrawal_terms_method(method() | undefined, ordsets:ordset(method_ref())) ->
    {ok, valid} | {error, withdrawal_method_validation_error()}.
validate_withdrawal_terms_method(undefined, _MethodRefs) ->
    %# TODO: remove this when work on TD-234
    {ok, valid};
validate_withdrawal_terms_method(Method, MethodRefs) ->
    MethodRef = ff_dmsl_codec:marshal(payment_method_ref, #{id => Method}),
    case ordsets:is_element(MethodRef, MethodRefs) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_withdrawal_method, {MethodRef, MethodRefs}}}}
    end.

-spec validate_w2w_terms_currency(currency_id(), w2w_terms()) -> {ok, valid} | {error, currency_validation_error()}.
validate_w2w_terms_currency(CurrencyID, Terms) ->
    #domain_W2WServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_w2w_cash_limit(cash(), w2w_terms()) -> {ok, valid} | {error, cash_range_validation_error()}.
validate_w2w_cash_limit(Cash, Terms) ->
    #domain_W2WServiceTerms{
        cash_limit = {value, CashRange}
    } = Terms,
    validate_cash_range(ff_dmsl_codec:marshal(cash, Cash), CashRange).

-spec validate_w2w_allow(w2w_terms()) -> {ok, valid} | {error, w2w_forbidden_error()}.
validate_w2w_allow(W2WServiceTerms) ->
    #domain_W2WServiceTerms{allow = Constant} = W2WServiceTerms,
    case Constant of
        {constant, true} ->
            {ok, valid};
        {constant, false} ->
            {error, {terms_violation, w2w_forbidden}}
    end.

-spec validate_currency(currency_id(), ordsets:ordset(currency_ref())) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_currency(CurrencyID, Currencies) ->
    CurrencyRef = #domain_CurrencyRef{symbolic_code = CurrencyID},
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.

-spec validate_account_balance(ff_account:account(), domain_cash_range()) ->
    {ok, valid}
    | {error, cash_range_validation_error()}.
validate_account_balance(Account, CashRange) ->
    do(fun() ->
        {Amounts, CurrencyID} = unwrap(ff_accounting:balance(Account)),
        ExpMinCash = ff_dmsl_codec:marshal(cash, {ff_indef:expmin(Amounts), CurrencyID}),
        ExpMaxCash = ff_dmsl_codec:marshal(cash, {ff_indef:expmax(Amounts), CurrencyID}),
        valid = unwrap(validate_cash_range(ExpMinCash, CashRange)),
        valid = unwrap(validate_cash_range(ExpMaxCash, CashRange))
    end).

-spec validate_cash_range(domain_cash(), domain_cash_range()) -> {ok, valid} | {error, cash_range_validation_error()}.
validate_cash_range(Cash, CashRange) ->
    case is_inside(Cash, CashRange) of
        true ->
            {ok, valid};
        _ ->
            DecodedCash = ff_dmsl_codec:unmarshal(cash, Cash),
            DecodedCashRange = ff_dmsl_codec:unmarshal(cash_range, CashRange),
            {error, {terms_violation, {cash_range, {DecodedCash, DecodedCashRange}}}}
    end.

is_inside(Cash, #domain_CashRange{lower = Lower, upper = Upper}) ->
    compare_cash(fun erlang:'>'/2, Cash, Lower) andalso
        compare_cash(fun erlang:'<'/2, Cash, Upper).

compare_cash(_Fun, V, {inclusive, V}) ->
    true;
compare_cash(
    Fun,
    #domain_Cash{amount = A, currency = C},
    {_, #domain_Cash{amount = Am, currency = C}}
) ->
    Fun(A, Am).

-spec validate_attempt_limit(attempt_limit()) -> {ok, valid} | {error, attempt_limit_error()}.
validate_attempt_limit(AttemptLimit) when AttemptLimit > 0 ->
    {ok, valid};
validate_attempt_limit(AttemptLimit) ->
    {error, {terms_violation, {attempt_limit, AttemptLimit}}}.
