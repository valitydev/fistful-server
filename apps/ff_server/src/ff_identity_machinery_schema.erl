-module(ff_identity_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, 2).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().

-type event() :: ff_machine:timestamped_event(ff_identity:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().
-type context() :: machinery_mg_schema:context().

-type legacy_change() :: any().

-type timestamped_change() :: ff_proto_identity_thrift:'TimestampedChange'().
-type thrift_change() :: ff_proto_identity_thrift:'Change'().

-type data() ::
    aux_state()
    | event()
    | call_args()
    | call_response().

%% machinery_mg_schema callbacks

-spec get_version(value_type()) -> machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(type(), value(data()), context()) -> {machinery_msgpack:t(), context()}.
marshal({event, Format}, TimestampedChange, Context) ->
    marshal_event(Format, TimestampedChange, Context);
marshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:marshal(T, V, C).

-spec unmarshal(type(), machinery_msgpack:t(), context()) -> {data(), context()}.
unmarshal({event, FormatVersion}, EncodedChange, Context) ->
    unmarshal_event(FormatVersion, EncodedChange, Context);
unmarshal({aux_state, undefined} = T, V, C0) ->
    {AuxState, C1} = machinery_mg_schema_generic:unmarshal(T, V, C0),
    {AuxState, C1#{ctx => get_aux_state_ctx(AuxState)}};
unmarshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event(), context()) -> {machinery_msgpack:t(), context()}.
marshal_event(undefined = Version, TimestampedChange, Context) ->
    % TODO: Remove this clause after MSPF-561 finish
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(Version, TimestampedChange, Context) when Version =:= 1; Version =:= 2 ->
    ThriftChange = ff_identity_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(2, EncodedChange, Context) ->
    ThriftChange = unmashal_thrift_change(EncodedChange),
    {ff_identity_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(1, EncodedChange, Context) ->
    ThriftChange = unmashal_thrift_change(EncodedChange),
    MigratedChange = ThriftChange#idnt_TimestampedChange{
        change = maybe_migrate_thrift_change(ThriftChange#idnt_TimestampedChange.change, Context)
    },
    {ff_identity_codec:unmarshal(timestamped_change, MigratedChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context) ->
    {{ev, Timestamp, Change}, Context1} = machinery_mg_schema_generic:unmarshal(
        {event, Version},
        EncodedChange,
        Context
    ),
    {{ev, Timestamp, maybe_migrate_change(Change, Context1)}, Context1}.

-spec unmashal_thrift_change(machinery_msgpack:t()) -> timestamped_change().
unmashal_thrift_change(EncodedChange) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    ff_proto_utils:deserialize(Type, EncodedThriftChange).

-spec maybe_migrate_thrift_change(thrift_change(), context()) -> thrift_change().
maybe_migrate_thrift_change({created, #idnt_Identity{name = undefined} = Identity}, MigrateContext) ->
    Context = fetch_entity_context(Identity#idnt_Identity.id, MigrateContext),
    {created, Identity#idnt_Identity{name = get_legacy_name(Context)}};
maybe_migrate_thrift_change(Change, _MigrateContext) ->
    Change.

-spec maybe_migrate_change(legacy_change(), context()) -> ff_identity:event().
maybe_migrate_change(Event = {created, #{version := 2, name := _}}, _MigrateContext) ->
    Event;
maybe_migrate_change(Ev, _MigrateContext) ->
    Ev.

get_aux_state_ctx(AuxState) when is_map(AuxState) ->
    maps:get(ctx, AuxState, undefined);
get_aux_state_ctx(_) ->
    undefined.

fetch_entity_context(MachineID, MigrateContext) ->
    case maps:get(ctx, MigrateContext, undefined) of
        undefined ->
            {ok, State} = ff_machine:get(ff_identity, 'ff/identity', MachineID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined);
        Data ->
            Data
    end.

get_legacy_name(#{<<"com.rbkmoney.wapi">> := #{<<"name">> := Name}}) ->
    Name.
