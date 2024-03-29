-module(ff_bin_data).

-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("msgpack_proto/include/msgp_msgpack_thrift.hrl").

-type token() :: binary().
-type issuer_country() :: atom().
-type payment_system() :: binary().

-type response_data() :: binbase_binbase_thrift:'ResponseData'().
-type bin_data() :: #{
    token := token(),
    id := bin_data_id(),
    payment_system := payment_system(),
    bank_name => binary(),
    issuer_country => issuer_country(),
    card_type => charge_card | credit | debit | credit_or_debit,
    version := integer()
}.

%% as stolen from `machinery_msgpack`
-type bin_data_id() ::
    nil
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [bin_data_id()]
    | #{bin_data_id() => bin_data_id()}.

-type bin_data_error() ::
    not_found
    | {unknown_residence, binary()}.

-export_type([bin_data/0]).
-export_type([bin_data_id/0]).
-export_type([bin_data_error/0]).
-export_type([issuer_country/0]).
-export_type([payment_system/0]).

-export([get/2]).
-export([id/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% API

-spec get(token(), bin_data_id() | undefined) -> {ok, bin_data()} | {error, bin_data_error()}.
get(Token, undefined) ->
    case call_binbase('GetByCardToken', {Token}) of
        {ok, Result} ->
            decode_result(Token, Result);
        {exception, #binbase_BinNotFound{}} ->
            {error, not_found}
    end;
get(Token, ID) ->
    case call_binbase('GetByBinDataId', {encode_msgpack(ID)}) of
        {ok, Result} ->
            decode_result(Token, Result);
        {exception, #binbase_BinNotFound{}} ->
            {error, not_found}
    end.

-spec id(bin_data()) -> bin_data_id().
id(Data) ->
    maps:get(id, Data).

%%

encode_msgpack(nil) ->
    {nl, #'msgpack_Nil'{}};
encode_msgpack(V) when is_boolean(V) ->
    {b, V};
encode_msgpack(V) when is_integer(V) ->
    {i, V};
encode_msgpack(V) when is_float(V) ->
    V;
% Assuming well-formed UTF-8 bytestring.
encode_msgpack(V) when is_binary(V) ->
    {str, V};
encode_msgpack({binary, V}) when is_binary(V) ->
    {bin, V};
encode_msgpack(V) when is_list(V) ->
    {arr, [encode_msgpack(ListItem) || ListItem <- V]};
encode_msgpack(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{encode_msgpack(Key) => encode_msgpack(Value)} end, #{}, V)}.

%%

-spec decode_result(token(), response_data()) -> {ok, bin_data()} | {error, bin_data_error()}.
decode_result(Token, #'binbase_ResponseData'{bin_data = Bindata, version = Version}) ->
    #'binbase_BinData'{
        payment_system = PaymentSystem,
        bank_name = BankName,
        iso_country_code = IsoCountryCode,
        card_type = CardType,
        category = Category,
        bin_data_id = BinDataID
    } = Bindata,
    do(fun() ->
        genlib_map:compact(#{
            token => Token,
            id => decode_msgpack(BinDataID),
            payment_system => unwrap(decode_payment_system(PaymentSystem)),
            bank_name => BankName,
            issuer_country => unwrap(decode_residence(IsoCountryCode)),
            card_type => decode_card_type(CardType),
            category => Category,
            version => Version
        })
    end).

decode_msgpack({nl, #'msgpack_Nil'{}}) ->
    nil;
decode_msgpack({b, V}) when is_boolean(V) ->
    V;
decode_msgpack({i, V}) when is_integer(V) ->
    V;
decode_msgpack({flt, V}) when is_float(V) ->
    V;
% Assuming well-formed UTF-8 bytestring.
decode_msgpack({str, V}) when is_binary(V) ->
    V;
decode_msgpack({bin, V}) when is_binary(V) ->
    {binary, V};
decode_msgpack({arr, V}) when is_list(V) ->
    [decode_msgpack(ListItem) || ListItem <- V];
decode_msgpack({obj, V}) when is_map(V) ->
    maps:fold(fun(Key, Value, Map) -> Map#{decode_msgpack(Key) => decode_msgpack(Value)} end, #{}, V).

decode_payment_system(PaymentSystem) when is_binary(PaymentSystem) ->
    {ok, PaymentSystem}.

decode_card_type(undefined) ->
    undefined;
decode_card_type(Type) ->
    Type.

decode_residence(undefined) ->
    {ok, undefined};
decode_residence(Residence) when is_binary(Residence) ->
    try
        {ok, list_to_existing_atom(string:to_lower(binary_to_list(Residence)))}
    catch
        error:badarg ->
            {error, {unknown_residence, Residence}}
    end.

call_binbase(Function, Args) ->
    Service = {binbase_binbase_thrift, 'Binbase'},
    ff_woody_client:call(binbase, {Service, Function, Args}).
