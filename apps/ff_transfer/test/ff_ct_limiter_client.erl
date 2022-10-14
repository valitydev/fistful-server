-module(ff_ct_limiter_client).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").

-export([get/3]).
-export([hold/4]).
-export([commit/4]).
-export([rollback/4]).

-export([create_config/2]).
-export([get_config/2]).

-type client() :: woody_context:ctx().

-type limit_id() :: limproto_limiter_thrift:'LimitID'().
-type change_id() :: limproto_limiter_thrift:'LimitChangeID'().
-type limit_context() :: limproto_limiter_thrift:'LimitContext'().
-type clock() :: limproto_limiter_thrift:'Clock'().
-type limit_config_params() :: limproto_config_thrift:'LimitConfigParams'().

%%% API

-spec get(limit_id(), limit_context(), client()) -> woody:result() | no_return().
get(LimitID, Context, Client) ->
    call('Get', {LimitID, clock(), Context}, Client).

-spec hold(limit_id(), change_id(), limit_context(), client()) -> woody:result() | no_return().
hold(LimitID, ChangeID, Context, Client) ->
    Change = #limiter_LimitChange{
        id = LimitID,
        change_id = ChangeID
    },
    call('Hold', {Change, clock(), Context}, Client).

-spec commit(limit_id(), change_id(), limit_context(), client()) -> woody:result() | no_return().
commit(LimitID, ChangeID, Context, Client) ->
    Change = #limiter_LimitChange{
        id = LimitID,
        change_id = ChangeID
    },
    call('Commit', {Change, clock(), Context}, Client).

-spec rollback(limit_id(), change_id(), limit_context(), client()) -> woody:result() | no_return().
rollback(LimitID, ChangeID, Context, Client) ->
    Change = #limiter_LimitChange{
        id = LimitID,
        change_id = ChangeID
    },
    call('Rollback', {Change, clock(), Context}, Client).

-spec create_config(limit_config_params(), client()) -> woody:result() | no_return().
create_config(LimitCreateParams, Client) ->
    call_configurator('Create', {LimitCreateParams}, Client).

-spec get_config(limit_id(), client()) -> woody:result() | no_return().
get_config(LimitConfigID, Client) ->
    call_configurator('Get', {LimitConfigID}, Client).

%%% Internal functions

-spec call(atom(), tuple(), client()) -> woody:result() | no_return().
call(Function, Args, Client) ->
    Call = {{limproto_limiter_thrift, 'Limiter'}, Function, Args},
    Opts = #{
        url => <<"http://limiter:8022/v1/limiter">>,
        event_handler => scoper_woody_event_handler,
        transport_opts => #{
            max_connections => 10000
        }
    },
    woody_client:call(Call, Opts, Client).

-spec call_configurator(atom(), tuple(), client()) -> woody:result() | no_return().
call_configurator(Function, Args, Client) ->
    Call = {{limproto_configurator_thrift, 'Configurator'}, Function, Args},
    Opts = #{
        url => <<"http://limiter:8022/v1/configurator">>,
        event_handler => scoper_woody_event_handler,
        transport_opts => #{
            max_connections => 10000
        }
    },
    woody_client:call(Call, Opts, Client).

-spec clock() -> clock().
clock() ->
    {vector, #limiter_VectorClock{state = <<>>}}.
