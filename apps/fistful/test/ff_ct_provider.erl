-module(ff_ct_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_provider_thrift.hrl").

%% API
-export([start/0]).
-export([start/1]).

%% Processing callbacks
-export([process_withdrawal/3]).
-export([get_quote/2]).
-export([handle_callback/4]).

-define(DUMMY_QUOTE, {obj, #{{str, <<"test">>} => {str, <<"test">>}}}).
-define(DUMMY_QUOTE_ERROR, {obj, #{{str, <<"test">>} => {str, <<"error">>}}}).

%%
%% Internal types
%%

-type destination() :: dmsl_wthd_domain_thrift:'Destination'().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type currency() :: dmsl_domain_thrift:'Currency'().
-type domain_quote() :: dmsl_wthd_provider_thrift:'Quote'().

-type withdrawal() :: #{
    id => binary(),
    body => cash(),
    destination => destination(),
    sender => party_id(),
    receiver => party_id(),
    quote => domain_quote()
}.

-type quote_params() :: #{
    idempotency_id => binary(),
    currency_from := currency(),
    currency_to := currency(),
    exchange_cash := cash()
}.

-type quote() :: #{
    cash_from := cash(),
    cash_to := cash(),
    created_at := binary(),
    expires_on := binary(),
    quote_data := any()
}.

-type callback() :: ff_withdrawal_callback:callback().

-record(state, {}).

-type state() :: #state{}.

-type transaction_info() :: ff_adapter:transaction_info().

%%
%% API
%%

-spec start() -> {ok, pid()}.
start() ->
    start([]).

-spec start(list()) -> {ok, pid()}.
start(Opts) ->
    {ok, Pid} = supervisor:start_link(ff_ct_provider_sup, Opts),
    _ = erlang:unlink(Pid),
    {ok, Pid}.

%%
%% Processing callbacks
%%

-define(STRING, <<"STRING">>).
-define(TIMESTAMP, <<"2024-01-28T08:26:00.000000Z">>).

-define(TRX_INFO, #{
    id => ?STRING,
    timestamp => ?TIMESTAMP,
    extra => #{?STRING => ?STRING},
    additional_info => #{
        rrn => ?STRING,
        approval_code => ?STRING,
        acs_url => ?STRING,
        pareq => ?STRING,
        md => ?STRING,
        term_url => ?STRING,
        pares => ?STRING,
        eci => ?STRING,
        cavv => ?STRING,
        xid => ?STRING,
        cavv_algorithm => ?STRING,
        three_ds_verification => authentication_successful
    }
}).

-spec process_withdrawal(withdrawal(), state(), map()) ->
    {ok, #{
        intent := ff_adapter_withdrawal:intent(),
        next_state => state(),
        transaction_info => transaction_info()
    }}.
process_withdrawal(#{quote := #wthd_provider_Quote{quote_data = QuoteData}}, State, _Options) when
    QuoteData =:= ?DUMMY_QUOTE_ERROR
->
    {ok, #{
        intent => {finish, {failed, #{code => <<"test_error">>}}},
        next_state => State
    }};
process_withdrawal(#{quote := #wthd_provider_Quote{quote_data = QuoteData}}, State, _Options) when
    QuoteData =:= ?DUMMY_QUOTE
->
    {ok, #{
        intent => {finish, {success, ?TRX_INFO}},
        next_state => State
    }};
process_withdrawal(#{auth_data := #{sender := <<"SenderToken">>, receiver := <<"ReceiverToken">>}}, State, _Options) ->
    {ok, #{
        intent => {finish, {success, ?TRX_INFO}},
        next_state => State
    }};
process_withdrawal(#{auth_data := _AuthData}, State, _Options) ->
    {ok, #{
        intent => {finish, {failed, #{code => <<"auth_data_error">>}}},
        next_state => State
    }};
process_withdrawal(_Withdrawal, State, _Options) ->
    {ok, #{
        intent => {finish, {success, ?TRX_INFO}},
        next_state => State
    }}.

-dialyzer({nowarn_function, get_quote/2}).

-spec get_quote(quote_params(), map()) -> {ok, quote()}.
get_quote(
    #{
        currency_from := CurrencyFrom,
        currency_to := CurrencyTo,
        exchange_cash := #wthd_provider_Cash{amount = Amount, currency = Currency},
        destination := {DestinationName, _}
    },
    _Options
) ->
    {ok, #{
        cash_from => calc_cash(CurrencyFrom, Currency, Amount),
        cash_to => calc_cash(CurrencyTo, Currency, Amount),
        created_at => ff_time:to_rfc3339(ff_time:now()),
        expires_on => ff_time:to_rfc3339(ff_time:now() + 15 * 3600 * 1000),
        quote_data =>
            {obj, #{
                {str, <<"test">>} => {str, <<"test">>},
                {str, <<"destination">>} => {str, erlang:atom_to_binary(DestinationName)}
            }}
    }};
get_quote(
    #{
        currency_from := CurrencyFrom,
        currency_to := CurrencyTo,
        exchange_cash := #wthd_provider_Cash{amount = Amount, currency = Currency}
    },
    _Options
) ->
    {ok, #{
        cash_from => calc_cash(CurrencyFrom, Currency, Amount),
        cash_to => calc_cash(CurrencyTo, Currency, Amount),
        created_at => ff_time:to_rfc3339(ff_time:now()),
        expires_on => ff_time:to_rfc3339(ff_time:now() + 15 * 3600 * 1000),
        quote_data => ?DUMMY_QUOTE
    }}.

-dialyzer({nowarn_function, handle_callback/4}).

-spec handle_callback(callback(), withdrawal(), state(), map()) ->
    {ok, #{
        intent := ff_adapter_withdrawal:intent(),
        response := any(),
        next_state => state(),
        transaction_info => transaction_info()
    }}.
handle_callback(_Callback, _Withdrawal, _State, _Options) ->
    erlang:error(not_implemented).

calc_cash(Currency, Currency, Amount) ->
    #wthd_provider_Cash{amount = Amount, currency = Currency};
calc_cash(Currency, _, Amount) ->
    NewAmount = erlang:round(Amount / 2),
    #wthd_provider_Cash{amount = NewAmount, currency = Currency}.
