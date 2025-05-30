-module(ff_withdrawal_adjustment_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/fistful_wthd_adj_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal(change, {created, Adjustment}) ->
    {created, #wthd_adj_CreatedChange{adjustment = marshal(adjustment, Adjustment)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #wthd_adj_StatusChange{status = marshal(status, Status)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #wthd_adj_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(adjustment, Adjustment) ->
    #wthd_adj_Adjustment{
        id = marshal(id, ff_adjustment:id(Adjustment)),
        status = maybe_marshal(status, ff_adjustment:status(Adjustment)),
        changes_plan = marshal(changes_plan, ff_adjustment:changes_plan(Adjustment)),
        created_at = marshal(timestamp_ms, ff_adjustment:created_at(Adjustment)),
        domain_revision = marshal(domain_revision, ff_adjustment:domain_revision(Adjustment)),
        operation_timestamp = marshal(timestamp_ms, ff_adjustment:operation_timestamp(Adjustment)),
        external_id = maybe_marshal(id, ff_adjustment:external_id(Adjustment))
    };
marshal(adjustment_params, Params) ->
    #wthd_adj_AdjustmentParams{
        id = marshal(id, maps:get(id, Params)),
        change = marshal(change_request, maps:get(change, Params)),
        external_id = maybe_marshal(id, maps:get(external_id, Params, undefined))
    };
marshal(adjustment_state, Adjustment) ->
    #wthd_adj_AdjustmentState{
        id = marshal(id, ff_adjustment:id(Adjustment)),
        status = maybe_marshal(status, ff_adjustment:status(Adjustment)),
        changes_plan = marshal(changes_plan, ff_adjustment:changes_plan(Adjustment)),
        created_at = marshal(timestamp_ms, ff_adjustment:created_at(Adjustment)),
        domain_revision = marshal(domain_revision, ff_adjustment:domain_revision(Adjustment)),
        operation_timestamp = marshal(timestamp_ms, ff_adjustment:operation_timestamp(Adjustment)),
        external_id = maybe_marshal(id, ff_adjustment:external_id(Adjustment))
    };
marshal(status, pending) ->
    {pending, #wthd_adj_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #wthd_adj_Succeeded{}};
marshal(changes_plan, Plan) ->
    #wthd_adj_ChangesPlan{
        new_cash_flow = maybe_marshal(cash_flow_change_plan, maps:get(new_cash_flow, Plan, undefined)),
        new_status = maybe_marshal(status_change_plan, maps:get(new_status, Plan, undefined)),
        new_domain_revision = maybe_marshal(domain_revision_change_plan, maps:get(new_domain_revision, Plan, undefined))
    };
marshal(cash_flow_change_plan, Plan) ->
    OldCashFlow = ff_cash_flow_codec:marshal(final_cash_flow, maps:get(old_cash_flow_inverted, Plan)),
    NewCashFlow = ff_cash_flow_codec:marshal(final_cash_flow, maps:get(new_cash_flow, Plan)),
    #wthd_adj_CashFlowChangePlan{
        old_cash_flow_inverted = OldCashFlow,
        new_cash_flow = NewCashFlow
    };
marshal(status_change_plan, Plan) ->
    #wthd_adj_StatusChangePlan{
        new_status = ff_withdrawal_status_codec:marshal(status, maps:get(new_status, Plan))
    };
marshal(domain_revision_change_plan, Plan) ->
    #wthd_adj_DataRevisionChangePlan{
        new_domain_revision = ff_codec:marshal(domain_revision, maps:get(new_domain_revision, Plan))
    };
marshal(change_request, {change_status, Status}) ->
    {change_status, #wthd_adj_ChangeStatusRequest{
        new_status = ff_withdrawal_status_codec:marshal(status, Status)
    }};
marshal(change_request, {change_cash_flow, DomainRevision}) ->
    {change_status, #wthd_adj_ChangeCashFlowRequest{
        domain_revision = ff_codec:marshal(domain_revision, DomainRevision)
    }};
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal(change, {created, #wthd_adj_CreatedChange{adjustment = Adjustment}}) ->
    {created, unmarshal(adjustment, Adjustment)};
unmarshal(change, {status_changed, #wthd_adj_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {transfer, #wthd_adj_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(adjustment, Adjustment) ->
    #{
        id => unmarshal(id, Adjustment#wthd_adj_Adjustment.id),
        status => unmarshal(status, Adjustment#wthd_adj_Adjustment.status),
        changes_plan => unmarshal(changes_plan, Adjustment#wthd_adj_Adjustment.changes_plan),
        created_at => unmarshal(timestamp_ms, Adjustment#wthd_adj_Adjustment.created_at),
        domain_revision => unmarshal(domain_revision, Adjustment#wthd_adj_Adjustment.domain_revision),
        operation_timestamp => unmarshal(timestamp_ms, Adjustment#wthd_adj_Adjustment.operation_timestamp),
        external_id => maybe_unmarshal(id, Adjustment#wthd_adj_Adjustment.external_id)
    };
unmarshal(adjustment_params, Params) ->
    genlib_map:compact(#{
        id => unmarshal(id, Params#wthd_adj_AdjustmentParams.id),
        change => unmarshal(change_request, Params#wthd_adj_AdjustmentParams.change),
        external_id => maybe_unmarshal(id, Params#wthd_adj_AdjustmentParams.external_id)
    });
unmarshal(status, {pending, #wthd_adj_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #wthd_adj_Succeeded{}}) ->
    succeeded;
unmarshal(changes_plan, Plan) ->
    genlib_map:compact(#{
        new_cash_flow => maybe_unmarshal(cash_flow_change_plan, Plan#wthd_adj_ChangesPlan.new_cash_flow),
        new_status => maybe_unmarshal(status_change_plan, Plan#wthd_adj_ChangesPlan.new_status),
        new_domain_revision => maybe_unmarshal(
            domain_revision_change_plan, Plan#wthd_adj_ChangesPlan.new_domain_revision
        )
    });
unmarshal(cash_flow_change_plan, Plan) ->
    OldCashFlow = Plan#wthd_adj_CashFlowChangePlan.old_cash_flow_inverted,
    NewCashFlow = Plan#wthd_adj_CashFlowChangePlan.new_cash_flow,
    #{
        old_cash_flow_inverted => ff_cash_flow_codec:unmarshal(final_cash_flow, OldCashFlow),
        new_cash_flow => ff_cash_flow_codec:unmarshal(final_cash_flow, NewCashFlow)
    };
unmarshal(status_change_plan, Plan) ->
    Status = Plan#wthd_adj_StatusChangePlan.new_status,
    #{
        new_status => ff_withdrawal_status_codec:unmarshal(status, Status)
    };
unmarshal(domain_revision_change_plan, Plan) ->
    DomainRevision = Plan#wthd_adj_DataRevisionChangePlan.new_domain_revision,
    #{
        new_domain_revision => ff_codec:unmarshal(domain_revision, DomainRevision)
    };
unmarshal(change_request, {change_status, Request}) ->
    Status = Request#wthd_adj_ChangeStatusRequest.new_status,
    {change_status, ff_withdrawal_status_codec:unmarshal(status, Status)};
unmarshal(change_request, {change_cash_flow, Request}) ->
    DomainRevision = Request#wthd_adj_ChangeCashFlowRequest.domain_revision,
    {change_cash_flow, ff_codec:unmarshal(domain_revision, DomainRevision)};
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

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec adjustment_codec_test() -> _.

adjustment_codec_test() ->
    FinalCashFlow = #{
        postings => [
            #{
                sender => #{
                    account => #{
                        realm => test,
                        party_id => genlib:unique(),
                        currency => <<"RUB">>,
                        account_id => 123
                    },
                    type => sender_source
                },
                receiver => #{
                    account => #{
                        realm => test,
                        party_id => genlib:unique(),
                        currency => <<"USD">>,
                        account_id => 321
                    },
                    type => receiver_settlement
                },
                volume => {100, <<"RUB">>}
            }
        ]
    },

    CashFlowChange = #{
        old_cash_flow_inverted => FinalCashFlow,
        new_cash_flow => FinalCashFlow
    },

    Plan = #{
        new_cash_flow => CashFlowChange,
        new_status => #{
            new_status => succeeded
        },
        new_domain_revision => #{new_domain_revision => 123}
    },

    Adjustment = #{
        id => genlib:unique(),
        status => pending,
        changes_plan => Plan,
        created_at => ff_time:now(),
        domain_revision => 123,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique()
    },

    Transfer = #{
        id => genlib:unique(),
        final_cash_flow => FinalCashFlow
    },

    Changes = [
        {created, Adjustment},
        {p_transfer, {created, Transfer}},
        {status_changed, pending}
    ],
    ?assertEqual(Changes, [unmarshal(change, marshal(change, C)) || C <- Changes]).

-endif.
