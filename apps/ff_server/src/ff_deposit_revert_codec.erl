-module(ff_deposit_revert_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/fistful_deposit_revert_thrift.hrl").
-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
-include_lib("fistful_proto/include/fistful_deposit_revert_status_thrift.hrl").
-include_lib("fistful_proto/include/fistful_deposit_revert_adj_thrift.hrl").
-include_lib("fistful_proto/include/fistful_cashflow_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal(change, {created, Revert}) ->
    {created, #deposit_revert_CreatedChange{revert = marshal(revert, Revert)}};
marshal(change, {status_changed, Status}) ->
    EncodedStatus = ff_deposit_revert_status_codec:marshal(status, Status),
    {status_changed, #deposit_revert_StatusChange{status = EncodedStatus}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #deposit_revert_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(change, {limit_check, Details}) ->
    {limit_check, #deposit_revert_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #deposit_revert_AdjustmentChange{
        id = marshal(id, ID),
        payload = ff_deposit_revert_adjustment_codec:marshal(change, Payload)
    }};
marshal(revert, Revert) ->
    #deposit_revert_Revert{
        id = marshal(id, ff_deposit_revert:id(Revert)),
        wallet_id = marshal(id, ff_deposit_revert:wallet_id(Revert)),
        source_id = marshal(id, ff_deposit_revert:source_id(Revert)),
        status = marshal(status, ff_deposit_revert:status(Revert)),
        body = marshal(cash, ff_deposit_revert:body(Revert)),
        created_at = marshal(timestamp_ms, ff_deposit_revert:created_at(Revert)),
        domain_revision = marshal(domain_revision, ff_deposit_revert:domain_revision(Revert)),
        party_revision = marshal(party_revision, ff_deposit_revert:party_revision(Revert)),
        reason = maybe_marshal(string, ff_deposit_revert:reason(Revert)),
        external_id = maybe_marshal(id, ff_deposit_revert:external_id(Revert))
    };
marshal(revert_params, RevertParams) ->
    #deposit_revert_RevertParams{
        id = marshal(id, maps:get(id, RevertParams)),
        body = marshal(cash, maps:get(body, RevertParams)),
        reason = maybe_marshal(string, maps:get(reason, RevertParams, undefined)),
        external_id = maybe_marshal(id, maps:get(external_id, RevertParams, undefined))
    };
marshal(revert_state, Revert) ->
    CashFlow = ff_deposit_revert:effective_final_cash_flow(Revert),
    Adjustments = ff_deposit_revert:adjustments(Revert),
    #deposit_revert_RevertState{
        id = marshal(id, ff_deposit_revert:id(Revert)),
        wallet_id = marshal(id, ff_deposit_revert:wallet_id(Revert)),
        source_id = marshal(id, ff_deposit_revert:source_id(Revert)),
        status = marshal(status, ff_deposit_revert:status(Revert)),
        body = marshal(cash, ff_deposit_revert:negative_body(Revert)),
        created_at = marshal(timestamp_ms, ff_deposit_revert:created_at(Revert)),
        domain_revision = marshal(domain_revision, ff_deposit_revert:domain_revision(Revert)),
        party_revision = marshal(party_revision, ff_deposit_revert:party_revision(Revert)),
        reason = maybe_marshal(string, ff_deposit_revert:reason(Revert)),
        external_id = maybe_marshal(id, ff_deposit_revert:external_id(Revert)),
        effective_final_cash_flow = ff_cash_flow_codec:marshal(final_cash_flow, CashFlow),
        adjustments = [ff_deposit_revert_adjustment_codec:marshal(adjustment_state, A) || A <- Adjustments]
    };
marshal(status, Status) ->
    ff_deposit_revert_status_codec:marshal(status, Status);
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal(change, {created, #deposit_revert_CreatedChange{revert = Revert}}) ->
    {created, unmarshal(revert, Revert)};
unmarshal(change, {status_changed, #deposit_revert_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {transfer, #deposit_revert_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(change, {limit_check, #deposit_revert_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};
unmarshal(change, {adjustment, Change}) ->
    #deposit_revert_AdjustmentChange{
        id = ID,
        payload = Payload
    } = Change,
    {adjustment, #{
        id => unmarshal(id, ID),
        payload => ff_deposit_revert_adjustment_codec:unmarshal(change, Payload)
    }};
unmarshal(status, Status) ->
    ff_deposit_revert_status_codec:unmarshal(status, Status);
unmarshal(revert, Revert) ->
    genlib_map:compact(#{
        id => unmarshal(id, Revert#deposit_revert_Revert.id),
        wallet_id => unmarshal(id, Revert#deposit_revert_Revert.wallet_id),
        source_id => unmarshal(id, Revert#deposit_revert_Revert.source_id),
        status => unmarshal(status, Revert#deposit_revert_Revert.status),
        body => unmarshal(cash, Revert#deposit_revert_Revert.body),
        created_at => unmarshal(timestamp_ms, Revert#deposit_revert_Revert.created_at),
        domain_revision => unmarshal(domain_revision, Revert#deposit_revert_Revert.domain_revision),
        party_revision => unmarshal(party_revision, Revert#deposit_revert_Revert.party_revision),
        reason => maybe_unmarshal(string, Revert#deposit_revert_Revert.reason),
        external_id => maybe_unmarshal(id, Revert#deposit_revert_Revert.external_id)
    });
unmarshal(revert_params, Params) ->
    genlib_map:compact(#{
        id => unmarshal(id, Params#deposit_revert_RevertParams.id),
        body => unmarshal(cash, Params#deposit_revert_RevertParams.body),
        external_id => maybe_unmarshal(id, Params#deposit_revert_RevertParams.external_id),
        reason => maybe_unmarshal(string, Params#deposit_revert_RevertParams.reason)
    });
unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec revert_symmetry_test() -> _.

revert_symmetry_test() ->
    Encoded = #deposit_revert_Revert{
        body = #'fistful_base_Cash'{
            amount = 10101,
            currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"Banana Republic">>}
        },
        source_id = genlib:unique(),
        wallet_id = genlib:unique(),
        domain_revision = 1,
        party_revision = 2,
        created_at = <<"2000-01-01T00:00:00Z">>,
        external_id = undefined,
        reason = <<"why not">>,
        status = {pending, #deposit_revert_status_Pending{}},
        id = genlib:unique()
    },
    ?assertEqual(Encoded, marshal(revert, unmarshal(revert, Encoded))).

-spec revert_params_symmetry_test() -> _.
revert_params_symmetry_test() ->
    Encoded = #deposit_revert_RevertParams{
        body = #'fistful_base_Cash'{
            amount = 10101,
            currency = #'fistful_base_CurrencyRef'{symbolic_code = <<"Banana Republic">>}
        },
        external_id = undefined,
        reason = <<"why not">>,
        id = genlib:unique()
    },
    ?assertEqual(Encoded, marshal(revert_params, unmarshal(revert_params, Encoded))).

-spec change_adjustment_symmetry_test() -> _.
change_adjustment_symmetry_test() ->
    Encoded =
        {adjustment, #deposit_revert_AdjustmentChange{
            id = genlib:unique(),
            payload =
                {created, #deposit_revert_adj_CreatedChange{
                    adjustment = #deposit_revert_adj_Adjustment{
                        id = genlib:unique(),
                        status = {pending, #deposit_revert_adj_Pending{}},
                        changes_plan = #deposit_revert_adj_ChangesPlan{
                            new_cash_flow = #deposit_revert_adj_CashFlowChangePlan{
                                old_cash_flow_inverted = #cashflow_FinalCashFlow{postings = []},
                                new_cash_flow = #cashflow_FinalCashFlow{postings = []}
                            }
                        },
                        created_at = <<"2000-01-01T00:00:00Z">>,
                        domain_revision = 123,
                        party_revision = 321,
                        operation_timestamp = <<"2000-01-01T00:00:00Z">>,
                        external_id = genlib:unique()
                    }
                }}
        }},
    ?assertEqual(Encoded, marshal(change, unmarshal(change, Encoded))).

-endif.
