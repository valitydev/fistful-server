-module(ff_w2w_transfer_status_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/fistful_w2w_status_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal(status, pending) ->
    {pending, #w2w_status_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #w2w_status_Succeeded{}};
marshal(status, {failed, Failure}) ->
    {failed, #w2w_status_Failed{failure = marshal(failure, Failure)}};
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal(status, {pending, #w2w_status_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #w2w_status_Succeeded{}}) ->
    succeeded;
unmarshal(status, {failed, #w2w_status_Failed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};
unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec pending_symmetry_test() -> _.

pending_symmetry_test() ->
    Status = pending,
    ?assertEqual(Status, unmarshal(status, marshal(status, Status))).

-spec succeeded_symmetry_test() -> _.
succeeded_symmetry_test() ->
    Status = succeeded,
    ?assertEqual(Status, unmarshal(status, marshal(status, Status))).

-spec failed_symmetry_test() -> _.
failed_symmetry_test() ->
    Status =
        {failed, #{
            code => <<"test">>,
            reason => <<"why not">>,
            sub => #{
                code => <<"sub">>
            }
        }},
    ?assertEqual(Status, unmarshal(status, marshal(status, Status))).

-endif.
