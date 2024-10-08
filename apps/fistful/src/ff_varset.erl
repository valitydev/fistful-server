-module(ff_varset).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export_type([varset/0]).
-export_type([encoded_varset/0]).

-export([encode/1]).
-export([encode_contract_terms_varset/1]).

-type varset() :: #{
    category => dmsl_domain_thrift:'CategoryRef'(),
    currency => dmsl_domain_thrift:'CurrencyRef'(),
    cost => dmsl_domain_thrift:'Cash'(),
    payment_tool => dmsl_domain_thrift:'PaymentTool'(),
    party_id => dmsl_domain_thrift:'PartyID'(),
    shop_id => dmsl_domain_thrift:'ShopID'(),
    risk_score => dmsl_domain_thrift:'RiskScore'(),
    flow => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()},
    wallet_id => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    bin_data => dmsl_domain_thrift:'BinData'()
}.

-type encoded_varset() :: dmsl_payproc_thrift:'Varset'().

-spec encode(varset()) -> encoded_varset().
encode(Varset) ->
    PaymentTool = genlib_map:get(payment_tool, Varset),
    #payproc_Varset{
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        payment_tool = PaymentTool,
        payment_method = encode_payment_method(PaymentTool),
        identification_level = genlib_map:get(identification_level, Varset),
        party_id = genlib_map:get(party_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec encode_contract_terms_varset(varset()) -> dmsl_payproc_thrift:'ComputeContractTermsVarset'().
encode_contract_terms_varset(Varset) ->
    #payproc_ComputeContractTermsVarset{
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        shop_id = genlib_map:get(shop_id, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec encode_payment_method(ff_destination:resource_params() | undefined) ->
    dmsl_domain_thrift:'PaymentMethodRef'() | undefined.
encode_payment_method(undefined) ->
    undefined;
encode_payment_method({bank_card, #domain_BankCard{payment_system = PaymentSystem}}) ->
    #domain_PaymentMethodRef{
        id = {bank_card, #domain_BankCardPaymentMethod{payment_system = PaymentSystem}}
    };
encode_payment_method({crypto_currency, CryptoCurrency}) ->
    #domain_PaymentMethodRef{
        id = {crypto_currency, CryptoCurrency}
    };
encode_payment_method({digital_wallet, #domain_DigitalWallet{payment_service = PaymentService}}) ->
    #domain_PaymentMethodRef{
        id = {digital_wallet, PaymentService}
    };
encode_payment_method({generic, #domain_GenericPaymentTool{payment_service = PaymentService}}) ->
    #domain_PaymentMethodRef{
        id = {generic, #domain_GenericPaymentMethod{payment_service = PaymentService}}
    }.
