-module(ff_machine_tag).

-define(BENDER_NS, <<"machinegun-tag">>).

-export([get_binding/2]).
-export([create_binding/3]).
-export([create_binding/4]).

-type tag() :: mg_proto_base_thrift:'Tag'().
-type ns() :: machinery:namespace().
-type entity_id() :: dmsl_base_thrift:'ID'().
-type machine_id() :: machinery:id().

-spec get_binding(ns(), tag()) -> {ok, entity_id(), machine_id()} | {error, not_found}.
get_binding(NS, Tag) ->
    WoodyContext = ff_context:get_woody_context(ff_context:load()),
    case bender_client:get_internal_id(tag_to_external_id(NS, Tag), WoodyContext) of
        {ok, EntityID, #{<<"machine-id">> := MachineID}} ->
            {ok, EntityID, MachineID};
        {error, internal_id_not_found} ->
            {error, not_found}
    end.

-spec create_binding(ns(), tag(), entity_id()) -> ok | no_return().
create_binding(NS, Tag, EntityID) ->
    create_binding(NS, Tag, EntityID, EntityID).

-spec create_binding(ns(), tag(), entity_id(), machine_id()) -> ok | no_return().
create_binding(NS, Tag, EntityID, MachineID) ->
    WoodyContext = ff_context:get_woody_context(ff_context:load()),
    Context = #{<<"machine-id">> => MachineID},
    case bender_client:gen_constant(tag_to_external_id(NS, Tag), EntityID, WoodyContext, Context) of
        {ok, EntityID, Context} ->
            ok
    end.

%%

tag_to_external_id(NS, Tag) ->
    BinNS = atom_to_binary(NS, utf8),
    <<?BENDER_NS/binary, "-", BinNS/binary, "-", Tag/binary>>.
