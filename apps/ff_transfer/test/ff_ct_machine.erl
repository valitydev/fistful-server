%%%
%%% Test machine
%%%

-module(ff_ct_machine).

-dialyzer({nowarn_function, dispatch_signal/4}).

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
    ok = case ff_ct_barrier:check(Handler, Machine) of
        {true, ID} -> ff_ct_barrier:enter(barrier, {ID, Handler});
        false -> ok
    end,
    Handler:process_timeout(Machine, HandlerArgs, Opts);
dispatch_signal(timeout, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_timeout(Machine, HandlerArgs, Opts);
dispatch_signal({notification, Args}, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_notification(Args, Machine, HandlerArgs, Opts).

dispatch_call(Args, Machine, {Handler, HandlerArgs}, Opts) ->
    Handler:process_call(Args, Machine, HandlerArgs, Opts).
