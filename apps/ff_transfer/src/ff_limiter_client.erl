-module(ff_limiter_client).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").

-export([get/3]).
-export([hold/3]).
-export([commit/3]).
-export([rollback/3]).

-type limit() :: limproto_limiter_thrift:'Limit'().
-type limit_id() :: limproto_limiter_thrift:'LimitID'().
-type limit_change() :: limproto_limiter_thrift:'LimitChange'().
-type context() :: limproto_limiter_thrift:'LimitContext'().
-type clock() :: limproto_limiter_thrift:'Clock'().

-export_type([limit/0]).
-export_type([limit_id/0]).
-export_type([limit_change/0]).
-export_type([context/0]).
-export_type([clock/0]).

-spec get(limit_id(), clock(), context()) -> limit() | no_return().
get(LimitID, Clock, Context) ->
    Args = {LimitID, Clock, Context},
    case call('Get', Args) of
        {ok, Limit} ->
            Limit;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitID});
        {exception, #base_InvalidRequest{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec hold(limit_change(), clock(), context()) -> clock() | no_return().
hold(LimitChange, Clock, Context) ->
    LimitID = LimitChange#limiter_LimitChange.id,
    Args = {LimitChange, Clock, Context},
    case call('Hold', Args) of
        {ok, ClockUpdated} ->
            ClockUpdated;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitID});
        {exception, #base_InvalidRequest{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec commit(limit_change(), clock(), context()) -> clock() | no_return().
commit(LimitChange, Clock, Context) ->
    Args = {LimitChange, Clock, Context},
    case call('Commit', Args) of
        {ok, ClockUpdated} ->
            ClockUpdated;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitChange#limiter_LimitChange.id});
        {exception, #limiter_LimitChangeNotFound{}} ->
            error({not_found, {limit_change, LimitChange#limiter_LimitChange.change_id}});
        {exception, #base_InvalidRequest{errors = Errors}} ->
            error({invalid_request, Errors});
        {exception, #limiter_ForbiddenOperationAmount{} = Ex} ->
            Amount = Ex#limiter_ForbiddenOperationAmount.amount,
            CashRange = Ex#limiter_ForbiddenOperationAmount.allowed_range,
            error({forbidden_op_amount, {Amount, CashRange}})
    end.

-spec rollback(limit_change(), clock(), context()) -> clock() | no_return().
rollback(LimitChange, Clock, Context) ->
    Args = {LimitChange, Clock, Context},
    case call('Rollback', Args) of
        {ok, ClockUpdated} ->
            ClockUpdated;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitChange#limiter_LimitChange.id});
        {exception, #limiter_LimitChangeNotFound{}} ->
            error({not_found, {limit_change, LimitChange#limiter_LimitChange.change_id}});
        {exception, #base_InvalidRequest{errors = Errors}} ->
            error({invalid_request, Errors});
        {exception, #limiter_ForbiddenOperationAmount{} = Ex} ->
            Amount = Ex#limiter_ForbiddenOperationAmount.amount,
            CashRange = Ex#limiter_ForbiddenOperationAmount.allowed_range,
            error({forbidden_op_amount, {Amount, CashRange}})
    end.

call(Func, Args) ->
    Service = {limproto_limiter_thrift, 'Limiter'},
    Request = {Service, Func, Args},
    ff_woody_client:call(limiter, Request).
