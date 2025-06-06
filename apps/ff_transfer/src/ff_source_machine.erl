%%%
%%% Source machine
%%%

-module(ff_source_machine).

%% API

-type id() :: machinery:id().
-type ctx() :: ff_entity_context:context().
-type source() :: ff_source:source_state().
-type change() :: ff_source:event().
-type event() :: {integer(), ff_machine:timestamped_event(change())}.
-type events() :: [event()].
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type params() :: ff_source:params().
-type st() :: ff_machine:st(source()).

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([st/0]).
-export_type([event/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).
-export_type([params/0]).
-export_type([event_range/0]).

%% API

-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).

%% Accessors

-export([source/1]).
-export([ctx/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).
-export([process_notification/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
-define(NS, 'ff/source_v1').

-spec create(params(), ctx()) ->
    ok
    | {error, ff_source:create_error() | exists}.
create(#{id := ID} = Params, Ctx) ->
    do(fun() ->
        Events = unwrap(ff_source:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}
    | {error, notfound}.
get(ID) ->
    ff_machine:get(ff_source, ?NS, ID).

-spec get(id(), event_range()) ->
    {ok, st()}
    | {error, notfound}.
get(ID, {After, Limit}) ->
    ff_machine:get(ff_source, ?NS, ID, {After, Limit, forward}).

-spec events(id(), event_range()) ->
    {ok, events()}
    | {error, notfound}.
events(ID, {After, Limit}) ->
    do(fun() ->
        History = unwrap(ff_machine:history(ff_source, ?NS, ID, {After, Limit, forward})),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

%% Accessors

-spec source(st()) -> source().
source(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().
ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type machine() :: ff_machine:machine(change()).
-type result() :: ff_machine:result(change()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[change()], ctx()}, machine(), _, handler_opts()) -> result().
init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events => ff_machine:emit_events(Events),
        action => continue,
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), handler_args(), handler_opts()) -> result().
process_timeout(_Machine, _, _Opts) ->
    #{}.

%%

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) -> {ok, result()}.
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_source, Machine, Scenario).

-spec process_notification(_, machine(), handler_args(), handler_opts()) -> result() | no_return().
process_notification(_Args, _Machine, _HandlerArgs, _Opts) ->
    #{}.

%% Internals

backend() ->
    fistful:backend(?NS).
