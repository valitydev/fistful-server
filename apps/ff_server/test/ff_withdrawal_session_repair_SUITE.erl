-module(ff_withdrawal_session_repair_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/fistful_wthd_session_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([repair_failed_session_with_success/1]).
-export([repair_failed_session_with_failure/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            repair_failed_session_with_success,
            repair_failed_session_with_failure
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_, _) ->
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%% Tests

-spec repair_failed_session_with_success(config()) -> test_return().
repair_failed_session_with_success(C) ->
    Ctx = ct_objects:build_default_ctx(),
    #{
        party_id := PartyID,
        destination_id := DestinationID
    } = ct_objects:prepare_standard_environment(Ctx),
    SessionID = create_failed_session(PartyID, DestinationID, C),
    ?assertEqual(active, get_session_status(SessionID)),
    timer:sleep(3000),
    ?assertEqual(active, get_session_status(SessionID)),
    {ok, ok} = call_repair({
        SessionID,
        {set_session_result, #wthd_session_SetResultRepair{
            result =
                {success, #wthd_session_SessionResultSuccess{
                    trx_info = #'fistful_base_TransactionInfo'{
                        id = SessionID,
                        extra = #{}
                    }
                }}
        }}
    }),
    ?assertMatch({finished, success}, get_session_status(SessionID)).

-spec repair_failed_session_with_failure(config()) -> test_return().
repair_failed_session_with_failure(C) ->
    Ctx = ct_objects:build_default_ctx(),
    #{
        party_id := PartyID,
        destination_id := DestinationID
    } = ct_objects:prepare_standard_environment(Ctx),
    SessionID = create_failed_session(PartyID, DestinationID, C),
    ?assertEqual(active, get_session_status(SessionID)),
    timer:sleep(3000),
    ?assertEqual(active, get_session_status(SessionID)),
    {ok, ok} = call_repair({
        SessionID,
        {set_session_result, #wthd_session_SetResultRepair{
            result =
                {failed, #wthd_session_SessionResultFailed{
                    failure = #'fistful_base_Failure'{
                        code = SessionID
                    }
                }}
        }}
    }),
    Expected =
        {failed, #{
            code => SessionID
        }},
    ?assertMatch({finished, Expected}, get_session_status(SessionID)).

%%  Internals

create_failed_session(PartyID, DestinationID, _C) ->
    ID = genlib:unique(),

    {ok, DestinationMachine} = ff_destination_machine:get(DestinationID),
    Destination = ff_destination_machine:destination(DestinationMachine),
    {ok, DestinationResource} = ff_resource:create_resource(ff_destination:resource(Destination)),

    TransferData = #{
        id => ID,
        % invalid currency
        cash => {1000, <<"unknown_currency">>},
        sender => PartyID,
        receiver => PartyID
    },

    SessionParams = #{
        withdrawal_id => ID,
        resource => DestinationResource,
        route => #{
            version => 1,
            provider_id => 1,
            terminal_id => 1
        }
    },
    ok = ff_withdrawal_session_machine:create(ID, TransferData, SessionParams),
    ID.

-spec get_session_status(machinery:id()) -> ff_withdrawal_session:status().
get_session_status(ID) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(ID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    ff_withdrawal_session:status(Session).

call_repair(Args) ->
    Service = {fistful_wthd_session_thrift, 'Repairer'},
    Request = {Service, 'Repair', Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/repair/withdrawal/session">>,
        event_handler => ff_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).
