%%%
%%% Test machine
%%%

-module(ff_ct_machine).

-export([load_per_suite/0]).
-export([unload_per_suite/0]).

-spec load_per_suite() -> ok.
load_per_suite() ->
    meck:new(machinery, [no_link, passthrough]),
    meck:expect(machinery, dispatch_signal, fun dispatch_signal/4),
    meck:expect(machinery, dispatch_call, fun dispatch_call/4).

-spec unload_per_suite() -> ok.
unload_per_suite() ->
    meck:unload(machinery).

dispatch_signal({init, Args}, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:init(Args, Machine, HandlerArgs, Opts);
dispatch_signal(timeout, Machine, {Handler, HandlerArgs}, Opts) when Handler =/= fistful ->
    do_if_in_options(
        Machine,
        Handler,
        fun() -> #{} end,
        fun() ->
            Handler:process_timeout(Machine, HandlerArgs, Opts)
        end
    );
dispatch_signal(timeout, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_timeout(Machine, HandlerArgs, Opts);
dispatch_signal({notification, Args}, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_notification(Args, Machine, HandlerArgs, Opts).

dispatch_call(mock_test, Machine, {Handler, HandlerArgs}, Opts) when Handler =/= fistful ->
    Result = do_if_in_options(
        Machine,
        Handler,
        fun() ->
            Handler:process_timeout(Machine, HandlerArgs, Opts)
        end,
        fun() -> #{} end
    ),
    {ok, Result};
dispatch_call(Args, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_call(Args, Machine, HandlerArgs, Opts).

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
