%%%
%%% Persistent entity context
%%%

-module(ff_entity_context).

-type context() :: #{namespace() => md()}.

-type namespace() :: binary().
%% as stolen from `machinery_msgpack`
-type md() ::
    nil
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [md()]
    | #{md() => md()}.

-export_type([context/0]).
-export_type([md/0]).

-export([new/0]).
-export([get/2]).
-export([try_get_legacy_metadata/1]).

%%

-spec new() -> context().
new() ->
    #{}.

-spec get(namespace(), context()) ->
    {ok, md()}
    | {error, notfound}.
get(Ns, Ctx) ->
    ff_map:find(Ns, Ctx).

-spec try_get_legacy_metadata(context() | undefined) -> md() | undefined.
try_get_legacy_metadata(#{<<"com.valitydev.wapi">> := #{<<"metadata">> := Metadata}}) ->
    Metadata;
try_get_legacy_metadata(_) ->
    undefined.
