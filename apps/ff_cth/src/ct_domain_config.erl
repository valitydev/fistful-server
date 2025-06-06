%%%
%%% Domain config helpers
%%%

-module(ct_domain_config).

-export([head/0]).
-export([get/1]).
-export([get/2]).

-export([commit/2]).
-export([insert/1]).
-export([update/1]).
-export([upsert/1]).
-export([remove/1]).
-export([reset/1]).
-export([cleanup/0]).
-export([bump_revision/0]).

%%

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-type revision() :: dmt_client:version().
-type object() :: dmsl_domain_thrift:'DomainObject'().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-spec head() -> revision().
head() ->
    dmt_client:get_last_version().

-spec get(ref()) -> data() | no_return().
get(Ref) ->
    get(latest, Ref).

-spec get(dmt_client:version(), ref()) -> data() | no_return().
get(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object(Revision, Ref))
    catch
        throw:#domain_conf_ObjectNotFound{} ->
            error({object_not_found, {Revision, Ref}})
    end.

extract_data({_Tag, {_Name, _Ref, Data}}) ->
    Data.

-spec all(revision()) -> dmsl_domain_thrift:'Domain'().
all(Revision) ->
    #'domain_conf_Snapshot'{domain = Domain} = dmt_client:checkout(Revision),
    Domain.

-spec commit(revision(), dmt_client:commit()) -> revision() | no_return().
commit(Revision, Commit) ->
    dmt_client:commit(Revision, Commit).

-spec insert(object() | [object()]) -> revision() | no_return().
insert(ObjectOrMany) ->
    dmt_client:insert(ObjectOrMany).

-spec update(object() | [object()]) -> revision() | no_return().
update(NewObjectOrMany) ->
    dmt_client:update(NewObjectOrMany).

-spec upsert(object() | [object()]) -> revision() | no_return().
upsert(NewObjectOrMany) ->
    upsert(latest, NewObjectOrMany).

-spec upsert(revision(), object() | [object()]) -> revision() | no_return().
upsert(Revision, NewObjectOrMany) ->
    dmt_client:upsert(Revision, NewObjectOrMany).

-spec remove(object() | [object()]) -> revision() | no_return().
remove(ObjectOrMany) ->
    dmt_client:remove(ObjectOrMany).

-spec reset(revision()) -> revision() | no_return().
reset(Revision) ->
    upsert(maps:values(all(Revision))).

-spec cleanup() -> revision() | no_return().
cleanup() ->
    #'domain_conf_Snapshot'{domain = Domain} = dmt_client:checkout(latest),
    remove(maps:values(Domain)).

-spec bump_revision() -> revision() | no_return().
bump_revision() ->
    dmt_client:commit(#'domain_conf_Commit'{ops = []}).
