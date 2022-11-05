%%%
%%% Test machine options
%%%

-module(ff_ct_barrier).

%% For mock in test cases
-export([load/1]).
-export([unload/1]).
-export([init_barrier/1]).
-export([check/2]).
-export([enter/2]).
-export([release/2]).

%% Gen Server

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-type handler() :: machinery:modopts(_).
-type machine() :: machinery:machine(_, _).
-type id() :: binary().
-type config() :: ct_helper:config().

-type channel() :: {binary(), atom()}.
-type st() :: #{
    blocked := #{channel() => pid()}
}.

-spec load(config()) -> config().
load(C) ->
    #{suite_sup := Sup} = ct_helper:cfg('payment_system', C),
    {ok, ServerPid} = start_barrier(Sup),
    register(barrier, ServerPid),
    meck:new(ff_ct_barrier, [no_link, passthrough]),
    ct_helper:cfg('$barrier', ServerPid, C).

-spec unload(config()) -> ok.
unload(C) ->
    #{suite_sup := Sup} = ct_helper:cfg('payment_system', C),
    % ServerPid = ct_helper:cfg('$barrier', C),
    % error({test, ServerPid, supervisor:which_children(Sup)}),
    ok = supervisor:terminate_child(Sup, ff_ct_barrier),
    ok = supervisor:delete_child(Sup, ff_ct_barrier),
    meck:unload(ff_ct_barrier).

-spec init_barrier(function()) -> ok.
init_barrier(EnterFunc) when is_function(EnterFunc) ->
    meck:expect(ff_ct_barrier, check, EnterFunc);
init_barrier(_) ->
    meck:expect(ff_ct_barrier, check, fun(_, _) -> false end).

-spec check(handler(), machine()) -> {true, id()} | false.
check(_Handler, _) ->
    false.

%%

-spec enter(gen_server:server_ref(), channel()) -> ok.
enter(ServerRef, Ch) ->
    gen_server:call(ServerRef, {enter, Ch}).

-spec release(channel(), config()) -> channel().
release(Ch, C) ->
    gen_server:call(ct_helper:cfg('$barrier', C), {release, Ch}).

-spec start_barrier(gen_server:server_ref()) -> {ok, pid()}.
start_barrier(SupPid) ->
    supervisor:start_child(SupPid, child_spec()).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ff_ct_barrier,
        start => {ff_ct_barrier, start_link, []}
    }.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(_) -> {ok, st()}.
init(_Args) ->
    {ok, #{blocked => #{}}}.

-spec handle_call({enter | release, channel()}, {pid(), reference()}, st()) ->
    {noreply, st()}
    | {reply, channel(), st()}.
handle_call({enter, Ch}, From, St = #{blocked := Blocked}) ->
    false = maps:is_key(Ch, Blocked),
    {noreply, St#{blocked => Blocked#{Ch => From}}};
handle_call({release, Ch}, _From, St = #{blocked := Blocked0}) ->
    #{Ch := From} = Blocked0,
    Blocked1 = maps:remove(Ch, Blocked0),
    gen_server:reply(From, ok),
    {reply, Ch, St#{blocked => Blocked1}};
handle_call(Call, _From, _St) ->
    error({badcall, Call}).

-spec handle_cast(_Cast, st()) -> no_return().
handle_cast(Cast, _St) ->
    error({badcast, Cast}).
