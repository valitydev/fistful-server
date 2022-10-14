%%%
%%% Test machine
%%%

-module(ff_ct_machine).

-type id() :: machinery:id().
-type state() :: map().
-type ctx() :: ff_entity_context:context().
% -type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type st() :: ff_machine:st(state()).

-type params() :: map().

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([params/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).

%% Accessors

-export([state/1]).
-export([ctx/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).
-export([process_notification/4]).

%% Accessors

-spec state(st()) -> state().
state(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().
ctx(St) ->
    ff_machine:ctx(St).

%% machinery

-type event() :: tuple().

-type machine() :: ff_machine:machine(event()).
-type result() :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[event()], ctx()} | [event()], machine(), _, handler_opts()) -> result().
init(InitData, Machine, Args, #{handler := Handler}) ->
    machinery:dispatch_signal(
        {init, InitData},
        Machine,
        {Handler, Args},
        #{}
    ).

-spec process_timeout(machine(), _, handler_opts()) -> result().
process_timeout(Machine, Args, #{handler := Handler}) ->
    do_if_in_options(
        Machine,
        Handler,
        fun() -> #{} end,
        fun() ->
            machinery:dispatch_signal(
                timeout,
                Machine,
                {Handler, Args},
                #{}
            )
        end
    ).

-spec process_call(_CallArgs, machine(), _, handler_opts()) -> {ok, result()}.
process_call(mock_test, Machine, Args, #{handler := Handler}) ->
    Result = do_if_in_options(
        Machine,
        Handler,
        fun() ->
            machinery:dispatch_signal(
                timeout,
                Machine,
                {Handler, Args},
                #{}
            )
        end,
        fun() -> #{} end
    ),
    {ok, Result};
process_call(CallArgs, Machine, Args, #{handler := Handler}) ->
    machinery:dispatch_call(
        CallArgs,
        Machine,
        {Handler, Args},
        #{}
    ).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, Args, #{handler := Handler}) ->
    machinery:dispatch_repair(
        Scenario,
        Machine,
        {Handler, Args},
        #{}
    ).

-spec process_notification(_, machine(), handler_args(), handler_opts()) -> result().
process_notification(NotificationArgs, Machine, Args, #{handler := Handler}) ->
    machinery:dispatch_signal(
        {notification, NotificationArgs},
        Machine,
        {Handler, Args},
        #{}
    ).

do_if_in_options(Machine, Handler, YesFunc, NoFunc) ->
    case ff_ct_machine_options:get_options(Handler) of
        BreakPoints when is_list(BreakPoints), length(BreakPoints) > 0 ->
            case lists:member(apply(Handler, activity, [Machine]), BreakPoints) of
                true -> YesFunc();
                false -> NoFunc()
            end;
        _ ->
            NoFunc()
    end.
