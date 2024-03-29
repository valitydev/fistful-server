-module(ff_deposit_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/fistful_deposit_thrift.hrl").
-include_lib("fistful_proto/include/fistful_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_deposit:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(fistful_deposit_thrift:'SinkEvent'()).

%%
%% Internals
%%

-spec publish_events(list(event())) -> list(sinkevent()).
publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) -> sinkevent().
publish_event(#{
    id := ID,
    source_id := SourceID,
    event := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #deposit_SinkEvent{
        id = marshal(event_id, ID),
        created_at = marshal(timestamp, Dt),
        source = marshal(id, SourceID),
        payload = #deposit_EventSinkPayload{
            sequence = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes = [marshal(change, Payload)]
        }
    }.

%%

marshal(Type, Value) ->
    ff_deposit_codec:marshal(Type, Value).
