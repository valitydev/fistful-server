-module(ff_resource).

-type resource() ::
    {bank_card, resource_bank_card()}
    | {crypto_wallet, resource_crypto_wallet()}
    | {digital_wallet, resource_digital_wallet()}
    | {generic, resource_generic()}.

-type resource_bank_card() :: #{
    bank_card := bank_card(),
    auth_data => bank_card_auth_data()
}.

-type resource_crypto_wallet() :: #{
    crypto_wallet := crypto_wallet()
}.

-type resource_digital_wallet() :: #{
    digital_wallet := digital_wallet()
}.

-type resource_generic() :: #{
    generic := generic_resource()
}.

-type resource_params() ::
    {bank_card, resource_bank_card_params()}
    | {crypto_wallet, resource_crypto_wallet_params()}
    | {digital_wallet, resource_digital_wallet_params()}
    | {generic, resource_generic_params()}.

-type resource_bank_card_params() :: #{
    bank_card := bank_card_params(),
    auth_data => bank_card_auth_data()
}.

-type bank_card_params() :: #{
    token := binary(),
    bin => binary(),
    masked_pan => binary(),
    cardholder_name => cardholder_name(),
    exp_date => exp_date()
}.

-type resource_crypto_wallet_params() :: #{
    crypto_wallet := crypto_wallet_params()
}.

-type crypto_wallet_params() :: #{
    id := binary(),
    currency := crypto_currency(),
    tag => binary()
}.

-type resource_digital_wallet_params() :: #{
    digital_wallet := digital_wallet_params()
}.

-type digital_wallet_params() :: #{
    id := binary(),
    payment_service := payment_service(),
    token => binary(),
    account_name => binary(),
    account_identity_number => binary()
}.

-type resource_generic_params() :: #{
    generic := generic_resource_params()
}.

-type generic_resource_params() :: generic_resource().

-type bank_card() :: #{
    token := token(),
    bin => bin(),
    payment_system => payment_system(),
    masked_pan => masked_pan(),
    bank_name => bank_name(),
    issuer_country => issuer_country(),
    card_type => card_type(),
    cardholder_name => binary(),
    exp_date => exp_date(),
    bin_data_id => bin_data_id(),
    category => binary()
}.

-type token() :: binary().
-type bin() :: binary().
-type payment_system() :: #{
    id := binary()
}.
-type payment_service() :: #{
    id := binary()
}.
-type crypto_currency() :: #{
    id := binary()
}.

-type method() ::
    {bank_card, #{payment_system := payment_system()}}
    | {digital_wallet, payment_service()}
    | {crypto_currency, crypto_currency()}
    | {generic, #{payment_service := payment_service()}}.

-type masked_pan() :: binary().
-type bank_name() :: binary().
-type issuer_country() :: ff_bin_data:issuer_country().
-type card_type() :: charge_card | credit | debit | credit_or_debit.
-type cardholder_name() :: binary().
-type exp_date() :: {month(), year()}.
-type month() :: integer().
-type year() :: integer().
-type bin_data() :: ff_bin_data:bin_data().
-type bin_data_id() :: ff_bin_data:bin_data_id().
-type bin_data_error() :: ff_bin_data:bin_data_error().
-type category() :: binary().

-type resource_descriptor() :: {bank_card, bin_data_id()}.

-type bank_card_auth_data() ::
    {session, session_auth_data()}.

-type session_auth_data() :: #{
    session_id := binary()
}.

-type crypto_wallet() :: #{
    id := binary(),
    currency := crypto_currency(),
    tag => binary()
}.

-type digital_wallet() :: #{
    id := binary(),
    payment_service := payment_service(),
    token => binary(),
    account_name => binary(),
    account_identity_number => binary()
}.

-type generic_resource() :: #{
    provider := payment_service(),
    data => generic_resource_data()
}.

-type generic_resource_data() :: #{
    type := binary(),
    data := binary()
}.

-export_type([resource_descriptor/0]).
-export_type([resource/0]).
-export_type([resource_params/0]).
-export_type([method/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).
-export_type([digital_wallet/0]).
-export_type([generic_resource/0]).

-export_type([token/0]).
-export_type([bin/0]).
-export_type([payment_system/0]).
-export_type([masked_pan/0]).
-export_type([bank_name/0]).
-export_type([issuer_country/0]).
-export_type([category/0]).
-export_type([card_type/0]).

-export([get_bin_data/2]).
-export([check_resource/1]).
-export([check_resource/2]).
-export([create_resource/1]).
-export([create_resource/2]).
-export([create_bank_card_basic/3]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([issuer_country/1]).
-export([category/1]).
-export([bank_name/1]).
-export([exp_date/1]).
-export([card_type/1]).
-export([cardholder_name/1]).
-export([resource_descriptor/1]).
-export([method/1]).

%% Pipeline

-spec token(bank_card()) -> token().
token(#{token := Token}) ->
    Token.

-spec bin(bank_card()) -> ff_maybe:'maybe'(bin()).
bin(BankCard) ->
    maps:get(bin, BankCard, undefined).

-spec bin_data_id(bank_card()) -> ff_maybe:'maybe'(bin_data_id()).
bin_data_id(BankCard) ->
    maps:get(bin_data_id, BankCard, undefined).

-spec masked_pan(bank_card()) -> ff_maybe:'maybe'(masked_pan()).
masked_pan(BankCard) ->
    maps:get(masked_pan, BankCard, undefined).

-spec payment_system(bank_card()) -> ff_maybe:'maybe'(payment_system()).
payment_system(BankCard) ->
    maps:get(payment_system, BankCard, undefined).

-spec issuer_country(bank_card()) -> ff_maybe:'maybe'(issuer_country()).
issuer_country(BankCard) ->
    maps:get(issuer_country, BankCard, undefined).

-spec category(bank_card()) -> ff_maybe:'maybe'(category()).
category(BankCard) ->
    maps:get(category, BankCard, undefined).

-spec bank_name(bank_card()) -> ff_maybe:'maybe'(bank_name()).
bank_name(BankCard) ->
    maps:get(bank_name, BankCard, undefined).

-spec exp_date(bank_card()) -> ff_maybe:'maybe'(exp_date()).
exp_date(BankCard) ->
    maps:get(exp_date, BankCard, undefined).

-spec card_type(bank_card()) -> ff_maybe:'maybe'(card_type()).
card_type(BankCard) ->
    maps:get(card_type, BankCard, undefined).

-spec cardholder_name(bank_card()) -> ff_maybe:'maybe'(cardholder_name()).
cardholder_name(BankCard) ->
    maps:get(cardholder_name, BankCard, undefined).

-spec resource_descriptor(ff_maybe:'maybe'(resource())) -> ff_maybe:'maybe'(resource_descriptor()).
resource_descriptor({bank_card, #{bank_card := #{bin_data_id := ID}}}) ->
    {bank_card, ID};
resource_descriptor(_) ->
    undefined.

-spec method(resource()) -> ff_maybe:'maybe'(method()).
method({bank_card, #{bank_card := #{payment_system := PaymentSystem}}}) ->
    {bank_card, #{payment_system => PaymentSystem}};
method({digital_wallet, #{digital_wallet := #{payment_service := PaymentService}}}) ->
    {digital_wallet, PaymentService};
method({crypto_wallet, #{crypto_wallet := #{currency := CryptoCurrency}}}) ->
    {crypto_currency, CryptoCurrency};
method({generic, #{generic := #{provider := PaymentService}}}) ->
    {generic, #{payment_service => PaymentService}};
method(_) ->
    undefined.

-spec get_bin_data(binary(), ff_maybe:'maybe'(resource_descriptor())) ->
    {ok, bin_data()}
    | {error, bin_data_error()}.
get_bin_data(Token, undefined) ->
    ff_bin_data:get(Token, undefined);
get_bin_data(Token, {bank_card, ResourceID}) ->
    ff_bin_data:get(Token, ResourceID).

-spec check_resource(resource()) ->
    valid | no_return().
check_resource(Resource) ->
    check_resource(ff_domain_config:head(), Resource).

-spec check_resource(ff_domain_config:revision(), resource()) ->
    valid | no_return().
check_resource(Revision, {digital_wallet, #{digital_wallet := #{payment_service := PaymentService}}}) ->
    MarshalledPaymentService = ff_dmsl_codec:marshal(payment_service, PaymentService),
    {ok, _} = ff_domain_config:object(Revision, {payment_service, MarshalledPaymentService}),
    valid;
check_resource(Revision, {crypto_wallet, #{crypto_wallet := #{currency := CryptoCurrency}}}) ->
    MarshalledCryptoCurrency = ff_dmsl_codec:marshal(crypto_currency, CryptoCurrency),
    {ok, _} = ff_domain_config:object(Revision, {crypto_currency, MarshalledCryptoCurrency}),
    valid;
check_resource(Revision, {generic, #{generic := #{provider := PaymentService}}}) ->
    MarshalledPaymentService = ff_dmsl_codec:marshal(payment_service, PaymentService),
    {ok, _} = ff_domain_config:object(Revision, {payment_service, MarshalledPaymentService}),
    valid;
check_resource(Revision, {bank_card, #{bank_card := #{payment_system := PaymentSystem}}}) ->
    MarshalledPaymentSystem = ff_dmsl_codec:marshal(payment_system, PaymentSystem),
    {ok, _} = ff_domain_config:object(Revision, {payment_system, MarshalledPaymentSystem}),
    valid;
%% For bank cards struct with token only
check_resource(_, _) ->
    valid.

-spec create_resource(resource_params()) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_resource(ResourceParams) ->
    create_resource(ResourceParams, undefined).

-spec create_resource(resource_params(), resource_descriptor() | undefined) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_resource({bank_card, ResourceBankCardParams}, ResourceDescriptor) ->
    create_bank_card(ResourceBankCardParams, ResourceDescriptor);
create_resource({crypto_wallet, ResourceCryptoWalletParams}, _ResourceDescriptor) ->
    create_crypto_wallet(ResourceCryptoWalletParams);
create_resource({digital_wallet, ResourceDigitalWalletParams}, _ResourceDescriptor) ->
    create_digital_wallet(ResourceDigitalWalletParams);
create_resource({generic, ResourceGenericParams}, _ResourceDescriptor) ->
    create_generic_resource(ResourceGenericParams).

-spec create_bank_card(resource_bank_card_params(), resource_descriptor() | undefined) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_bank_card(#{bank_card := #{token := Token}} = ResourceBankCardParams, ResourceDescriptor) ->
    case get_bin_data(Token, ResourceDescriptor) of
        {ok, BinData} ->
            create_bank_card_basic(ResourceBankCardParams, BinData, undefined);
        {error, Error} ->
            {error, {bin_data, Error}}
    end.

-spec create_bank_card_basic(resource_bank_card_params(), bin_data() | undefined, payment_system() | undefined) ->
    {ok, resource()}.
create_bank_card_basic(#{bank_card := BankCardParams0} = ResourceBankCardParams, BinData, PaymentSystem) ->
    ExtendData = create_extended_data(BinData),
    BankCardParams1 = genlib_map:compact(BankCardParams0#{payment_system => PaymentSystem}),
    {ok,
        {bank_card,
            genlib_map:compact(#{
                bank_card => maps:merge(BankCardParams1, ExtendData),
                auth_data => maps:get(auth_data, ResourceBankCardParams, undefined)
            })}}.

-spec create_crypto_wallet(resource_crypto_wallet_params()) -> {ok, resource()}.
create_crypto_wallet(#{
    crypto_wallet := CryptoWallet = #{
        id := ID,
        currency := Currency
    }
}) ->
    {ok,
        {crypto_wallet, #{
            crypto_wallet => genlib_map:compact(#{
                id => ID,
                currency => Currency,
                tag => maps:get(tag, CryptoWallet, undefined)
            })
        }}}.

-spec create_digital_wallet(resource_digital_wallet_params()) -> {ok, resource()}.
create_digital_wallet(#{
    digital_wallet := Wallet = #{
        id := ID,
        payment_service := PaymentService
    }
}) ->
    Token = maps:get(token, Wallet, undefined),
    {ok,
        {digital_wallet, #{
            digital_wallet => genlib_map:compact(#{
                id => ID,
                payment_service => PaymentService,
                token => Token,
                account_name => maps:get(account_name, Wallet, undefined),
                account_identity_number => maps:get(account_identity_number, Wallet, undefined)
            })
        }}}.

-spec create_generic_resource(resource_generic_params()) -> {ok, resource()}.
create_generic_resource(#{generic := Generic}) ->
    {ok, {generic, #{generic => Generic}}}.

create_extended_data(undefined) ->
    #{};
create_extended_data(BinData) ->
    KeyList = [bank_name, issuer_country, card_type, category],
    ExtendData0 = maps:with(KeyList, BinData),
    ExtendData0#{bin_data_id => ff_bin_data:id(BinData)}.
