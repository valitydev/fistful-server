-module(ff_withdrawal_session_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/fistful_wthd_session_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("fistful_proto/include/fistful_destination_thrift.hrl").

-export([marshal_state/3]).
-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API
-spec marshal_state(ff_withdrawal_session:session_state(), ff_withdrawal_session:id(), ff_entity_context:context()) ->
    fistful_wthd_session_thrift:'SessionState'().
marshal_state(State, ID, Context) ->
    #wthd_session_SessionState{
        id = marshal(id, ID),
        status = marshal(session_status, ff_withdrawal_session:status(State)),
        withdrawal = marshal(withdrawal, ff_withdrawal_session:withdrawal(State)),
        route = marshal(route, ff_withdrawal_session:route(State)),
        context = marshal(ctx, Context)
    }.

-spec marshal_event(ff_withdrawal_machine:event()) -> fistful_wthd_session_thrift:'Event'().
marshal_event({EventID, {ev, Timestamp, Change}}) ->
    #wthd_session_Event{
        sequence = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        %% NOTE Each emitted session event contains single change
        changes = [marshal(change, Change)]
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];
marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #wthd_session_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };
marshal(change, {created, Session}) ->
    {created, marshal(session, Session)};
marshal(change, {next_state, AdapterState}) ->
    {next_state, marshal(msgpack, AdapterState)};
marshal(change, {transaction_bound, TransactionInfo}) ->
    {transaction_bound, #wthd_session_TransactionBoundChange{trx_info = marshal(transaction_info, TransactionInfo)}};
marshal(change, {finished, SessionResult}) ->
    {finished, marshal(session_result, SessionResult)};
marshal(change, {callback, CallbackChange}) ->
    {callback, marshal(callback_change, CallbackChange)};
marshal(session, Session) ->
    #{
        id := SessionID,
        status := SessionStatus,
        withdrawal := Withdrawal,
        route := Route
    } = Session,
    #wthd_session_Session{
        id = marshal(id, SessionID),
        status = marshal(session_status, SessionStatus),
        withdrawal = marshal(withdrawal, Withdrawal),
        route = marshal(route, Route)
    };
marshal(session_status, active) ->
    {active, #wthd_session_SessionActive{}};
marshal(session_status, {finished, Result}) ->
    {
        finished,
        #wthd_session_SessionFinished{status = marshal(session_finished_status, Result)}
    };
marshal(session_finished_status, success) ->
    {success, #wthd_session_SessionFinishedSuccess{}};
marshal(session_finished_status, {failed, Failure}) ->
    {failed, #wthd_session_SessionFinishedFailed{failure = marshal(failure, Failure)}};
marshal(
    withdrawal,
    #{
        id := WithdrawalID,
        resource := Resource,
        cash := Cash
    } = Params
) ->
    Sender = maps:get(sender, Params, undefined),
    Receiver = maps:get(receiver, Params, undefined),
    SessionID = maps:get(session_id, Params, undefined),
    Quote = maps:get(quote, Params, undefined),
    DestAuthData = maps:get(dest_auth_data, Params, undefined),
    #wthd_session_Withdrawal{
        id = marshal(id, WithdrawalID),
        destination_resource = marshal(resource, Resource),
        cash = marshal(cash, Cash),
        sender = marshal(id, Sender),
        receiver = marshal(id, Receiver),
        session_id = maybe_marshal(id, SessionID),
        quote = maybe_marshal(quote, Quote),
        auth_data = maybe_marshal(auth_data, DestAuthData)
    };
marshal(route, Route) ->
    #wthd_session_Route{
        provider_id = marshal(provider_id, maps:get(provider_id, Route)),
        terminal_id = maybe_marshal(terminal_id, genlib_map:get(terminal_id, Route))
    };
marshal(quote, #{
    cash_from := CashFrom,
    cash_to := CashTo,
    created_at := CreatedAt,
    expires_on := ExpiresOn,
    quote_data := Data
}) ->
    #wthd_session_Quote{
        cash_from = marshal(cash, CashFrom),
        cash_to = marshal(cash, CashTo),
        created_at = CreatedAt,
        expires_on = ExpiresOn,
        quote_data = maybe_marshal(msgpack, Data)
    };
marshal(auth_data, #{
    sender := SenderToken,
    receiver := ReceiverToken
}) ->
    {sender_receiver, #destination_SenderReceiverAuthData{
        sender = SenderToken,
        receiver = ReceiverToken
    }};
marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);
marshal(session_result, success) ->
    {success, #wthd_session_SessionResultSuccess{}};
marshal(session_result, {success, TransactionInfo}) ->
    %% for backward compatibility with events stored in DB - take TransactionInfo here.
    %% @see ff_adapter_withdrawal:rebind_transaction_info/1
    {success, #wthd_session_SessionResultSuccess{trx_info = marshal(transaction_info, TransactionInfo)}};
marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_session_SessionResultFailed{failure = ff_codec:marshal(failure, Failure)}};
marshal(callback_change, #{tag := Tag, payload := Payload}) ->
    #wthd_session_CallbackChange{
        tag = marshal(string, Tag),
        payload = marshal(callback_event, Payload)
    };
marshal(callback_event, {created, Callback}) ->
    {created, #wthd_session_CallbackCreatedChange{callback = marshal(callback, Callback)}};
marshal(callback_event, {status_changed, Status}) ->
    {status_changed, #wthd_session_CallbackStatusChange{status = marshal(callback_status, Status)}};
marshal(callback_event, {finished, #{payload := Response}}) ->
    {finished, #wthd_session_CallbackResultChange{payload = Response}};
marshal(callback, #{tag := Tag}) ->
    #wthd_session_Callback{tag = marshal(string, Tag)};
marshal(callback_status, pending) ->
    {pending, #wthd_session_CallbackStatusPending{}};
marshal(callback_status, succeeded) ->
    {succeeded, #wthd_session_CallbackStatusSucceeded{}};
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#wthd_session_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#wthd_session_TimestampedChange.change),
    {ev, Timestamp, Change};
unmarshal(repair_scenario, {add_events, #wthd_session_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events,
        genlib_map:compact(#{
            events => unmarshal({list, change}, Events),
            action => maybe_unmarshal(complex_action, Action)
        })};
unmarshal(repair_scenario, {set_session_result, #wthd_session_SetResultRepair{result = Result}}) ->
    {set_session_result, unmarshal(session_result, Result)};
unmarshal(change, {created, Session}) ->
    {created, unmarshal(session, Session)};
unmarshal(change, {next_state, AdapterState}) ->
    {next_state, unmarshal(msgpack, AdapterState)};
unmarshal(change, {transaction_bound, #wthd_session_TransactionBoundChange{trx_info = TransactionInfo}}) ->
    {transaction_bound, unmarshal(transaction_info, TransactionInfo)};
unmarshal(change, {finished, SessionResult}) ->
    {finished, unmarshal(session_result, SessionResult)};
unmarshal(change, {callback, #wthd_session_CallbackChange{tag = Tag, payload = Payload}}) ->
    {callback, #{
        tag => unmarshal(string, Tag),
        payload => unmarshal(callback_event, Payload)
    }};
unmarshal(session, #wthd_session_Session{
    id = SessionID,
    status = SessionStatus,
    withdrawal = Withdrawal,
    route = Route0
}) ->
    Route1 = unmarshal(route, Route0),
    genlib_map:compact(#{
        version => 5,
        id => unmarshal(id, SessionID),
        status => unmarshal(session_status, SessionStatus),
        withdrawal => unmarshal(withdrawal, Withdrawal),
        route => Route1
    });
unmarshal(session_status, {active, #wthd_session_SessionActive{}}) ->
    active;
unmarshal(session_status, {finished, #wthd_session_SessionFinished{status = Result}}) ->
    {finished, unmarshal(session_finished_status, Result)};
unmarshal(session_finished_status, {success, #wthd_session_SessionFinishedSuccess{}}) ->
    success;
unmarshal(session_finished_status, {failed, #wthd_session_SessionFinishedFailed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};
unmarshal(withdrawal, #wthd_session_Withdrawal{
    id = WithdrawalID,
    destination_resource = Resource,
    cash = Cash,
    sender = Sender,
    receiver = Receiver,
    session_id = SessionID,
    quote = Quote,
    auth_data = DestAuthData
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, WithdrawalID),
        resource => unmarshal(resource, Resource),
        cash => unmarshal(cash, Cash),
        sender => unmarshal(id, Sender),
        receiver => unmarshal(id, Receiver),
        session_id => maybe_unmarshal(id, SessionID),
        quote => maybe_unmarshal(quote, Quote),
        dest_auth_data => maybe_unmarshal(auth_data, DestAuthData)
    });
unmarshal(route, Route) ->
    genlib_map:compact(#{
        provider_id => unmarshal(provider_id, Route#wthd_session_Route.provider_id),
        terminal_id => maybe_unmarshal(terminal_id, Route#wthd_session_Route.terminal_id)
    });
unmarshal(quote, #wthd_session_Quote{
    cash_from = CashFrom,
    cash_to = CashTo,
    created_at = CreatedAt,
    expires_on = ExpiresOn,
    quote_data = Data
}) ->
    genlib_map:compact(#{
        cash_from => unmarshal(cash, CashFrom),
        cash_to => unmarshal(cash, CashTo),
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => maybe_unmarshal(msgpack, Data)
    });
unmarshal(
    auth_data,
    {sender_receiver, #destination_SenderReceiverAuthData{
        sender = SenderToken,
        receiver = ReceiverToken
    }}
) ->
    #{
        sender => SenderToken,
        receiver => ReceiverToken
    };
unmarshal(session_result, {success, #wthd_session_SessionResultSuccess{trx_info = undefined}}) ->
    success;
unmarshal(session_result, {success, #wthd_session_SessionResultSuccess{trx_info = TransactionInfo}}) ->
    %% for backward compatibility with events stored in DB - take TransactionInfo here.
    %% @see ff_adapter_withdrawal:rebind_transaction_info/1
    {success, unmarshal(transaction_info, TransactionInfo)};
unmarshal(session_result, {failed, #wthd_session_SessionResultFailed{failure = Failure}}) ->
    {failed, ff_codec:unmarshal(failure, Failure)};
unmarshal(callback_event, {created, #wthd_session_CallbackCreatedChange{callback = Callback}}) ->
    {created, unmarshal(callback, Callback)};
unmarshal(callback_event, {finished, #wthd_session_CallbackResultChange{payload = Response}}) ->
    {finished, #{payload => Response}};
unmarshal(callback_event, {status_changed, #wthd_session_CallbackStatusChange{status = Status}}) ->
    {status_changed, unmarshal(callback_status, Status)};
unmarshal(callback, #wthd_session_Callback{tag = Tag}) ->
    #{tag => unmarshal(string, Tag)};
unmarshal(callback_status, {pending, #wthd_session_CallbackStatusPending{}}) ->
    pending;
unmarshal(callback_status, {succeeded, #wthd_session_CallbackStatusSucceeded{}}) ->
    succeeded;
unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);
unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_change_test_() -> _.

marshal_change_test_() ->
    TransactionInfo = #{id => <<"ID">>, extra => #{<<"Hello">> => <<"World">>}},
    TransactionInfoThrift = marshal(transaction_info, TransactionInfo),
    Changes = [
        {finished, {success, TransactionInfo}},
        {finished, success},
        {transaction_bound, TransactionInfo}
    ],
    ChangesThrift = [
        {finished, {success, #wthd_session_SessionResultSuccess{trx_info = TransactionInfoThrift}}},
        {finished, {success, #wthd_session_SessionResultSuccess{}}},
        {transaction_bound, #wthd_session_TransactionBoundChange{trx_info = TransactionInfoThrift}}
    ],
    [
        ?_assertEqual(ChangesThrift, marshal({list, change}, Changes)),
        ?_assertEqual(Changes, unmarshal({list, change}, ChangesThrift))
    ].

-endif.
