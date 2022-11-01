%%%
%%% Test machine options
%%%

-module(ff_ct_barrier).

%% For mock in test cases
-export([load/0]).
-export([unload/0]).
-export([init/1]).
-export([enter/3]).
-export([release/2]).

-type handler() :: machinery:modopts(_).
-type machine() :: machinery:machine(_, _).
-type id() :: binary().
-type activity() :: atom() | tuple().

-spec load() -> ok.
load() ->
    meck:new(ff_ct_barrier, [no_link, passthrough]).

-spec unload() -> ok.
unload() ->
    meck:unload(ff_ct_barrier).

-spec init(function()) -> ok.
init(EnterFunc) when is_function(EnterFunc) ->
    meck:expect(ff_ct_barrier, enter, EnterFunc);
init(_) ->
    meck:expect(ff_ct_barrier, enter, fun(_, _, _) -> false end).

-spec enter(activity(), handler(), machine()) -> boolean().
enter(_Activity, _Handler, _) ->
    false.

-spec release(id(), handler()) -> ok.
release(ID, Handler) ->
    Handler:call(ID, release).
