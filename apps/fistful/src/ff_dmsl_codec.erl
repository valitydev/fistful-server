-module(ff_dmsl_codec).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-export([unmarshal/2]).
-export([maybe_unmarshal/2]).
-export([marshal/2]).
-export([maybe_marshal/2]).

%% Types

-type type_name() :: atom() | {list, atom()}.
-type codec() :: module().

-type encoded_value() :: encoded_value(any()).
-type encoded_value(T) :: T.

-type decoded_value() :: decoded_value(any()).
-type decoded_value(T) :: T.

-export_type([codec/0]).
-export_type([type_name/0]).
-export_type([encoded_value/0]).
-export_type([encoded_value/1]).
-export_type([decoded_value/0]).
-export_type([decoded_value/1]).

-spec unmarshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:encoded_value()) -> ff_dmsl_codec:decoded_value().
unmarshal(transaction_info, #domain_TransactionInfo{
    id = ID,
    timestamp = Timestamp,
    extra = Extra,
    additional_info = AddInfo
}) ->
    genlib_map:compact(#{
        id => unmarshal(string, ID),
        timestamp => maybe_unmarshal(string, Timestamp),
        extra => Extra,
        additional_info => maybe_unmarshal(additional_transaction_info, AddInfo)
    });
unmarshal(additional_transaction_info, #domain_AdditionalTransactionInfo{
    rrn = RRN,
    approval_code = ApprovalCode,
    acs_url = AcsURL,
    pareq = Pareq,
    md = MD,
    term_url = TermURL,
    pares = Pares,
    eci = ECI,
    cavv = CAVV,
    xid = XID,
    cavv_algorithm = CAVVAlgorithm,
    three_ds_verification = ThreeDSVerification
}) ->
    genlib_map:compact(#{
        rrn => maybe_unmarshal(string, RRN),
        approval_code => maybe_unmarshal(string, ApprovalCode),
        acs_url => maybe_unmarshal(string, AcsURL),
        pareq => maybe_unmarshal(string, Pareq),
        md => maybe_unmarshal(string, MD),
        term_url => maybe_unmarshal(string, TermURL),
        pares => maybe_unmarshal(string, Pares),
        eci => maybe_unmarshal(string, ECI),
        cavv => maybe_unmarshal(string, CAVV),
        xid => maybe_unmarshal(string, XID),
        cavv_algorithm => maybe_unmarshal(string, CAVVAlgorithm),
        three_ds_verification => maybe_unmarshal(three_ds_verification, ThreeDSVerification)
    });
unmarshal(three_ds_verification, Value) when
    Value =:= authentication_successful orelse
        Value =:= attempts_processing_performed orelse
        Value =:= authentication_failed orelse
        Value =:= authentication_could_not_be_performed
->
    Value;
unmarshal(failure, #domain_Failure{
    code = Code,
    reason = Reason,
    sub = SubFailure
}) ->
    genlib_map:compact(#{
        code => unmarshal(string, Code),
        reason => maybe_unmarshal(string, Reason),
        sub => maybe_unmarshal(sub_failure, SubFailure)
    });
unmarshal(sub_failure, #domain_SubFailure{
    code = Code,
    sub = SubFailure
}) ->
    genlib_map:compact(#{
        code => unmarshal(string, Code),
        sub => maybe_unmarshal(sub_failure, SubFailure)
    });
unmarshal(payment_method_ref, #domain_PaymentMethodRef{
    id = PaymentMethod
}) ->
    #{id => unmarshal(payment_method, PaymentMethod)};
unmarshal(payment_method, {generic, #domain_GenericPaymentMethod{payment_service = PaymentService}}) ->
    {generic, #{payment_service => unmarshal(payment_service, PaymentService)}};
unmarshal(payment_method, {digital_wallet, PaymentServiceRef}) ->
    {digital_wallet, unmarshal(payment_service, PaymentServiceRef)};
unmarshal(payment_method, {crypto_currency, CryptoCurrencyRef}) ->
    {crypto_currency, unmarshal(crypto_currency, CryptoCurrencyRef)};
unmarshal(payment_method, {bank_card, #domain_BankCardPaymentMethod{payment_system = PaymentSystem}}) ->
    {bank_card, #{payment_system => unmarshal(payment_system, PaymentSystem)}};
unmarshal(cash, #domain_Cash{
    amount = Amount,
    currency = CurrencyRef
}) ->
    {unmarshal(amount, Amount), unmarshal(currency_ref, CurrencyRef)};
unmarshal(cash_range, #domain_CashRange{
    lower = {BoundLower, CashLower},
    upper = {BoundUpper, CashUpper}
}) ->
    {
        {BoundLower, unmarshal(cash, CashLower)},
        {BoundUpper, unmarshal(cash, CashUpper)}
    };
unmarshal(currency_ref, #domain_CurrencyRef{symbolic_code = SymbolicCode}) ->
    unmarshal(string, SymbolicCode);
unmarshal(risk_score, low) ->
    low;
unmarshal(risk_score, high) ->
    high;
unmarshal(risk_score, fatal) ->
    fatal;
unmarshal(currency, #domain_Currency{
    name = Name,
    symbolic_code = Symcode,
    numeric_code = Numcode,
    exponent = Exponent
}) ->
    #{
        name => Name,
        symcode => Symcode,
        numcode => Numcode,
        exponent => Exponent
    };
unmarshal(user_interaction, {redirect, {get_request, #'user_interaction_BrowserGetRequest'{uri = URI}}}) ->
    {redirect, #{content => {get, URI}}};
unmarshal(
    user_interaction,
    {redirect, {post_request, #'user_interaction_BrowserPostRequest'{uri = URI, form = Form}}}
) ->
    {redirect, #{content => {post, URI, Form}}};
unmarshal(
    resource,
    {disposable, #domain_DisposablePaymentResource{
        payment_tool =
            {bank_card, #domain_BankCard{
                token = Token,
                payment_system = PaymentSystem,
                issuer_country = IssuerCountry,
                bin = Bin,
                last_digits = LastDigits,
                exp_date = ExpDate,
                cardholder_name = CardholderName
            }},
        payment_session_id = ID
    }}
) ->
    AuthData =
        case ID of
            undefined ->
                undefined;
            ID ->
                {session, #{session_id => unmarshal(string, ID)}}
        end,
    {bank_card,
        genlib_map:compact(#{
            bank_card => #{
                token => Token,
                payment_system => maybe_unmarshal(payment_system, PaymentSystem),
                issuer_country => maybe_unmarshal(issuer_country, IssuerCountry),
                bin => Bin,
                masked_pan => LastDigits,
                exp_date => maybe_unmarshal(exp_date, ExpDate),
                cardholder_name => maybe_unmarshal(string, CardholderName)
            },
            auth_data => AuthData
        })};
unmarshal(exp_date, #'domain_BankCardExpDate'{
    month = Month,
    year = Year
}) ->
    {unmarshal(integer, Month), unmarshal(integer, Year)};
unmarshal(payment_system, #'domain_PaymentSystemRef'{
    id = ID
}) ->
    #{
        id => unmarshal(string, ID)
    };
unmarshal(payment_service, #'domain_PaymentServiceRef'{
    id = ID
}) ->
    #{
        id => unmarshal(string, ID)
    };
unmarshal(crypto_currency, #'domain_CryptoCurrencyRef'{
    id = ID
}) ->
    #{
        id => unmarshal(string, ID)
    };
unmarshal(issuer_country, V) when is_atom(V) ->
    V;
unmarshal(attempt_limit, #domain_AttemptLimit{
    attempts = Attempts
}) ->
    unmarshal(integer, Attempts);
unmarshal(amount, V) ->
    unmarshal(integer, V);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(integer, V) when is_integer(V) ->
    V.

-spec maybe_unmarshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:encoded_value()) -> ff_dmsl_codec:decoded_value().
maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, V) ->
    unmarshal(Type, V).

-spec marshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:decoded_value()) -> ff_dmsl_codec:encoded_value().
marshal(transaction_info, #{id := ID} = V) ->
    #domain_TransactionInfo{
        id = marshal(string, ID),
        timestamp = maybe_marshal(string, maps:get(timestamp, V, undefined)),
        extra = maps:get(extra, V, #{}),
        additional_info = maybe_marshal(additional_transaction_info, maps:get(additional_info, V, undefined))
    };
marshal(additional_transaction_info, V) ->
    #domain_AdditionalTransactionInfo{
        rrn = maybe_marshal(string, maps:get(rrn, V, undefined)),
        approval_code = maybe_marshal(string, maps:get(approval_code, V, undefined)),
        acs_url = maybe_marshal(string, maps:get(acs_url, V, undefined)),
        pareq = maybe_marshal(string, maps:get(pareq, V, undefined)),
        md = maybe_marshal(string, maps:get(md, V, undefined)),
        term_url = maybe_marshal(string, maps:get(term_url, V, undefined)),
        pares = maybe_marshal(string, maps:get(pares, V, undefined)),
        eci = maybe_marshal(string, maps:get(eci, V, undefined)),
        cavv = maybe_marshal(string, maps:get(cavv, V, undefined)),
        xid = maybe_marshal(string, maps:get(xid, V, undefined)),
        cavv_algorithm = maybe_marshal(string, maps:get(cavv_algorithm, V, undefined)),
        three_ds_verification = maybe_marshal(three_ds_verification, maps:get(three_ds_verification, V, undefined))
        %% TODO 'short_payment_id' and 'extra_payment_info'
        %% short_payment_id = maybe_marshal(string, maps:get(short_payment_id, V, undefined)),
        %% extra_payment_info = maps:get(extra_payment_info, V, undefined)
    };
marshal(three_ds_verification, V) when
    V =:= authentication_successful orelse
        V =:= attempts_processing_performed orelse
        V =:= authentication_failed orelse
        V =:= authentication_could_not_be_performed
->
    V;
marshal(failure, #{code := Code} = V) ->
    #domain_Failure{
        code = marshal(string, Code),
        reason = maybe_marshal(string, maps:get(reason, V, undefined)),
        sub = maybe_marshal(sub_failure, maps:get(sub, V, undefined))
    };
marshal(sub_failure, #{code := Code} = V) ->
    #domain_SubFailure{
        code = marshal(string, Code),
        sub = maybe_marshal(sub_failure, maps:get(sub, V, undefined))
    };
marshal(cash, {Amount, CurrencyRef}) ->
    #domain_Cash{
        amount = marshal(amount, Amount),
        currency = marshal(currency_ref, CurrencyRef)
    };
marshal(cash_range, {{BoundLower, CashLower}, {BoundUpper, CashUpper}}) ->
    #domain_CashRange{
        lower = {BoundLower, marshal(cash, CashLower)},
        upper = {BoundUpper, marshal(cash, CashUpper)}
    };
marshal(currency_ref, CurrencyID) when is_binary(CurrencyID) ->
    #domain_CurrencyRef{
        symbolic_code = CurrencyID
    };
marshal(currency, #{
    name := Name,
    symcode := Symcode,
    numcode := Numcode,
    exponent := Exponent
}) ->
    #domain_Currency{
        name = Name,
        symbolic_code = Symcode,
        numeric_code = Numcode,
        exponent = Exponent
    };
marshal(payment_method_ref, #{id := PaymentMethod}) ->
    #domain_PaymentMethodRef{
        id = marshal(payment_method, PaymentMethod)
    };
marshal(payment_method, {generic, #{payment_service := PaymentService}}) ->
    {generic, #domain_GenericPaymentMethod{payment_service = marshal(payment_service, PaymentService)}};
marshal(payment_method, {digital_wallet, PaymentServiceRef}) ->
    {digital_wallet, marshal(payment_service, PaymentServiceRef)};
marshal(payment_method, {crypto_currency, CryptoCurrencyRef}) ->
    {crypto_currency, marshal(crypto_currency, CryptoCurrencyRef)};
marshal(payment_method, {bank_card, #{payment_system := PaymentSystem}}) ->
    {bank_card, #domain_BankCardPaymentMethod{payment_system = marshal(payment_system, PaymentSystem)}};
marshal(payment_resource_payer, #{resource := Resource} = Payer) ->
    ClientInfo = maps:get(client_info, Payer, undefined),
    ContactInfo = maps:get(contact_info, Payer, undefined),
    #domain_PaymentResourcePayer{
        resource = marshal(disposable_payment_resource, {Resource, ClientInfo}),
        contact_info = marshal(contact_info, ContactInfo)
    };
marshal(disposable_payment_resource, {Resource, ClientInfo}) ->
    #domain_DisposablePaymentResource{
        payment_tool = marshal(payment_tool, Resource),
        payment_session_id = try_get_session_auth_data(Resource),
        client_info = maybe_marshal(client_info, ClientInfo)
    };
marshal(payment_tool, {bank_card, #{bank_card := BankCard}}) ->
    {bank_card, marshal(bank_card, BankCard)};
marshal(payment_tool, {generic, #{generic := GenericResource = #{provider := PaymentService}}}) ->
    Data = maps:get(data, GenericResource, undefined),
    {generic, #domain_GenericPaymentTool{
        data = maybe_marshal(content, Data),
        payment_service = marshal(payment_service, PaymentService)
    }};
marshal(bin_data, #{payment_system := PaymentSystem} = BinData) ->
    BankName = maps:get(bank_name, BinData, undefined),
    #domain_BinData{
        payment_system = marshal(string, PaymentSystem),
        bank_name = maybe_marshal(string, BankName)
    };
marshal(bank_card, BankCard) ->
    ExpDate = ff_resource:exp_date(BankCard),
    PaymentSystem = ff_resource:payment_system(BankCard),
    #domain_BankCard{
        token = ff_resource:token(BankCard),
        bin = ff_resource:bin(BankCard),
        last_digits = ff_resource:masked_pan(BankCard),
        payment_system = maybe_marshal(payment_system, PaymentSystem),
        issuer_country = ff_resource:issuer_country(BankCard),
        bank_name = ff_resource:bank_name(BankCard),
        exp_date = maybe_marshal(exp_date, ExpDate),
        cardholder_name = ff_resource:cardholder_name(BankCard),
        category = ff_resource:category(BankCard)
    };
marshal(exp_date, {Month, Year}) ->
    #domain_BankCardExpDate{
        month = marshal(integer, Month),
        year = marshal(integer, Year)
    };
marshal(payment_system, #{id := ID}) ->
    #domain_PaymentSystemRef{
        id = marshal(string, ID)
    };
marshal(payment_service, #{id := ID}) ->
    #domain_PaymentServiceRef{
        id = marshal(string, ID)
    };
marshal(crypto_currency, #{id := ID}) ->
    #domain_CryptoCurrencyRef{
        id = marshal(string, ID)
    };
marshal(contact_info, undefined) ->
    #domain_ContactInfo{};
marshal(contact_info, ContactInfo) ->
    #domain_ContactInfo{
        phone_number = maps:get(phone_number, ContactInfo, undefined),
        email = maps:get(email, ContactInfo, undefined)
    };
marshal(client_info, ClientInfo) ->
    IPAddress = maps:get(ip_address, ClientInfo, undefined),
    Fingerprint = maps:get(fingerprint, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address = IPAddress,
        fingerprint = Fingerprint
    };
marshal(attempt_limit, Limit) ->
    #domain_AttemptLimit{
        attempts = Limit
    };
marshal(content, #{type := Type, data := Data}) ->
    #'base_Content'{
        type = marshal(string, Type),
        data = Data
    };
marshal(risk_score, low) ->
    low;
marshal(risk_score, high) ->
    high;
marshal(risk_score, fatal) ->
    fatal;
marshal(amount, V) ->
    marshal(integer, V);
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(_, Other) ->
    Other.

-spec maybe_marshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:decoded_value()) -> ff_dmsl_codec:encoded_value().
maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

try_get_session_auth_data({bank_card, #{auth_data := {session, #{session_id := ID}}}}) ->
    marshal(string, ID);
try_get_session_auth_data(_) ->
    undefined.
