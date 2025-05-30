%%%
%%% Generic machine
%%%
%%% TODOs
%%%
%%%  - Split ctx and time tracking into different machine layers.
%%%

-module(ff_machine).

-type ctx() :: ff_entity_context:context().
-type range() :: machinery:range().
-type id() :: machinery:id().
-type namespace() :: machinery:namespace().
-type timestamp() :: machinery:timestamp().

-type st(Model) :: #{
    model := Model,
    ctx := ctx(),
    times => {timestamp(), timestamp()}
}.

-type timestamped_event(T) ::
    {ev, timestamp(), T}.

-type auxst() :: #{ctx := ctx()}.

-type machine(T) ::
    machinery:machine(timestamped_event(T), auxst()).

-type result(T) ::
    machinery:result(timestamped_event(T), auxst()).

-type migrate_params() :: #{
    ctx => ctx(),
    timestamp => timestamp(),
    id => id()
}.

-export_type([st/1]).
-export_type([machine/1]).
-export_type([result/1]).
-export_type([timestamped_event/1]).
-export_type([auxst/0]).
-export_type([migrate_params/0]).

%% Accessors

-export([model/1]).
-export([ctx/1]).
-export([created/1]).
-export([updated/1]).

%% API

-export([get/3]).
-export([get/4]).

-export([collapse/2]).
-export([history/4]).

-export([emit_event/1]).
-export([emit_events/1]).

%% Machinery helpers

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).
-export([process_repair/4]).
-export([process_notification/4]).

%% Model callbacks

-callback init(machinery:args(_)) -> [event()].

-callback apply_event(event(), model()) -> model().

-callback maybe_migrate(event(), migrate_params()) -> event().

-callback process_call(machinery:args(_), st()) -> {machinery:response(_), [event()]}.

-callback process_repair(machinery:args(_), st()) ->
    {ok, machinery:response(_), [event()]} | {error, machinery:error(_)}.

-callback process_timeout(st()) -> [event()].

-optional_callbacks([maybe_migrate/2]).

%% Pipeline helpers

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type model() :: any().
-type event() :: any().
-type st() :: st(model()).
-type machine() :: machine(model()).
-type history() :: [machinery:event(timestamped_event(event()))].

%%

-spec model(st(Model)) -> Model.
-spec ctx(st(_)) -> ctx().
-spec created(st(_)) -> timestamp() | undefined.
-spec updated(st(_)) -> timestamp() | undefined.

model(#{model := V}) ->
    V.

ctx(#{ctx := V}) ->
    V.

created(St) ->
    erlang:element(1, times(St)).

updated(St) ->
    erlang:element(2, times(St)).

times(St) ->
    genlib_map:get(times, St, {undefined, undefined}).

%%

-spec get(module(), namespace(), id()) ->
    {ok, st()}
    | {error, notfound}.
get(Mod, NS, Ref) ->
    get(Mod, NS, Ref, {undefined, undefined, forward}).

-spec get(module(), namespace(), id(), range()) ->
    {ok, st()}
    | {error, notfound}.
get(Mod, NS, ID, Range) ->
    do(fun() ->
        Machine = unwrap(machinery:get(NS, ID, Range, fistful:backend(NS))),
        collapse(Mod, Machine)
    end).

-spec history(module(), namespace(), id(), range()) ->
    {ok, history()}
    | {error, notfound}.
history(Mod, NS, ID, Range) ->
    do(fun() ->
        Machine = unwrap(machinery:get(NS, ID, Range, fistful:backend(NS))),
        #{history := History} = migrate_machine(Mod, Machine),
        History
    end).

-spec collapse(module(), machine()) -> st().
collapse(Mod, Machine) ->
    collapse_(Mod, migrate_machine(Mod, Machine)).

-spec collapse_(module(), machine()) -> st().
collapse_(Mod, #{history := History, aux_state := #{ctx := Ctx}}) ->
    collapse_history(Mod, History, #{ctx => Ctx}).

collapse_history(Mod, History, St0) ->
    lists:foldl(fun(Ev, St) -> merge_event(Mod, Ev, St) end, St0, History).

-spec migrate_history(module(), history(), migrate_params()) -> history().
migrate_history(Mod, History, MigrateParams) ->
    [migrate_event(Mod, Ev, MigrateParams) || Ev <- History].

-spec emit_event(E) -> [timestamped_event(E)].
emit_event(Event) ->
    emit_events([Event]).

-spec emit_events([E]) -> [timestamped_event(E)].
emit_events(Events) ->
    emit_timestamped_events(Events, machinery_time:now()).

emit_timestamped_events(Events, Ts) ->
    [{ev, Ts, Body} || Body <- Events].

merge_event(Mod, {_ID, _Ts, TsEvent}, St0) ->
    {Ev, St1} = merge_timestamped_event(TsEvent, St0),
    Model1 = Mod:apply_event(Ev, maps:get(model, St1, undefined)),
    St1#{model => Model1}.

merge_timestamped_event({ev, Ts, Body}, #{times := {Created, _Updated}} = St) ->
    {Body, St#{times => {Created, Ts}}};
merge_timestamped_event({ev, Ts, Body}, #{} = St) ->
    {Body, St#{times => {Ts, Ts}}}.

-spec migrate_machine(module(), machine()) -> machine().
migrate_machine(Mod, #{history := History} = Machine) ->
    MigrateParams = #{
        ctx => maps:get(ctx, maps:get(aux_state, Machine, #{}), undefined),
        id => maps:get(id, Machine, undefined)
    },
    Machine#{history => migrate_history(Mod, History, MigrateParams)}.

migrate_event(Mod, {ID, Ts, {ev, EventTs, EventBody}} = Event, MigrateParams) ->
    case erlang:function_exported(Mod, maybe_migrate, 2) of
        true ->
            {ID, Ts, {ev, EventTs, Mod:maybe_migrate(EventBody, MigrateParams#{timestamp => EventTs})}};
        false ->
            Event
    end.

%%

-spec init({machinery:args(_), ctx()}, machinery:machine(E, A), module(), _) -> machinery:result(E, A).
init({Args, Ctx}, _Machine, Mod, _) ->
    Events = Mod:init(Args),
    #{
        events => emit_events(Events),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machinery:machine(E, A), module(), _) -> machinery:result(E, A).
process_timeout(Machine, Mod, _) ->
    Events = Mod:process_timeout(collapse(Mod, Machine)),
    #{
        events => emit_events(Events)
    }.

-spec process_call(machinery:args(_), machinery:machine(E, A), module(), _) ->
    {machinery:response(_), machinery:result(E, A)}.
process_call(Args, Machine, Mod, _) ->
    {Response, Events} = Mod:process_call(Args, collapse(Mod, Machine)),
    {Response, #{
        events => emit_events(Events)
    }}.

-spec process_repair(machinery:args(_), machinery:machine(E, A), module(), _) ->
    {ok, machinery:response(_), machinery:result(E, A)} | {error, machinery:error(_)}.
process_repair(Args, Machine, Mod, _) ->
    case Mod:process_repair(Args, collapse(Mod, Machine)) of
        {ok, Response, Events} ->
            {ok, Response, #{
                events => emit_events(Events)
            }};
        {error, _Reason} = Error ->
            Error
    end.

-spec process_notification(_, machine(_), _, _) -> result(_) | no_return().
process_notification(_Args, _Machine, _HandlerArgs, _Opts) ->
    #{}.
