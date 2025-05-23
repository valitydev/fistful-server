-module(ct_sup).

-export([start/0]).
-export([stop/1]).

%%

-behaviour(supervisor).

-export([init/1]).

%%

-spec start() -> pid().
start() ->
    {ok, Pid} = supervisor:start_link(?MODULE, []),
    true = unlink(Pid),
    Pid.

-spec stop(pid()) -> ok.
stop(Pid) ->
    ok = proc_lib:stop(Pid).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.
