-module(ff_claim_committer_handler).

-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").

-behaviour(ff_woody_wrapper).

-export([handle_function/3]).

-spec handle_function(woody:func(), woody:args(), woody:options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        claims,
        #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

-spec handle_function_(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function_('Accept', {PartyID, #claimmgmt_Claim{changeset = Changeset}}, _Opts) ->
    % Params = ff_deposit_codec:unmarshal(deposit_params, MarshaledParams),
    % Context = ff_deposit_codec:unmarshal(context, MarshaledContext),
    ok = scoper:add_meta(#{party_id => PartyID}),
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Modifications = ff_claim_committer:filter_ff_modifications(Changeset),
    ok = ff_claim_committer:assert_modifications_applicable(Modifications, Timestamp, Revision),
    {ok, ok};
handle_function_('Commit', {PartyID, Claim}, _Opts) ->
    #claimmgmt_Claim{
        id = ID,
        changeset = Changeset
        % revision = Revision,
        % created_at = CreatedAt,
        % updated_at = UpdatedAt
    } = Claim,
    ok = scoper:add_meta(#{party_id => PartyID, claim_id => ID}),
    _Modifications = pm_claim_committer:filter_party_modifications(Changeset),
    %% make identity or wallet using modification
    %% resp ok if ok
    {ok, ok}.
