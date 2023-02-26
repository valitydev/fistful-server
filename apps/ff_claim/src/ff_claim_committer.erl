-module(ff_claim_committer).

-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-include("ff_claim_management.hrl").

-export([filter_ff_modifications/1]).
-export([assert_modifications_applicable/4]).
-export([raise_invalid_changeset/2]).

-type changeset() :: dmsl_claimmgmt_thrift:'ClaimChangeset'().
-type timestamp() :: ff_time:timestamp().
-type revision() :: ff_domain_config:revision().
-type modification() :: dmsl_claimmgmt_thrift:'PartyModification'().
-type modifications() :: [modification()].
-type effects() :: [effect()].
-type effect() :: identity_effect() | wallet_effect().

-type identity_effect() :: {identity, #{
    identity_id := dmsl_claimmgmt_thrift:'IdentityID'(),
    params := dmsl_claimmgmt_thrift:'IdentityParams'()
}}.

-type wallet_effect() :: {wallet, #{
    wallet_id := dmsl_claimmgmt_thrift:'WalletID'(),
    params := dmsl_claimmgmt_thrift:'NewWalletParams'()
}}.

-export_type([modification/0]).
-export_type([modifications/0]).

-spec filter_ff_modifications(changeset()) -> modifications().
filter_ff_modifications(Changeset) ->
    lists:filtermap(
        fun
            (?cm_identity_modification(_, _, Change, _)) ->
                {true, {identity_creation, Change}};
            (?cm_wallet_modification(_, _, Change, _)) ->
                {true, {wallet_creation, Change}};
            (_) ->
                false
        end,
        Changeset
    ).

%%% Internal functions

-spec assert_modifications_applicable(modifications(), timestamp(), revision(), effects()) -> ok | no_return().
assert_modifications_applicable([FFChange | Others], Timestamp, Revision, Effects) ->
    Effect = case FFChange of
        ?cm_identity_creation(PartyID, IdentityID, Name, Provider, Metadata, Params) ->
            case ff_identity_machine:get(IdentityID) of
                {ok, Machine} ->
                    Identity = ff_identity_machine:identity(Machine),
                    assert_identity_creation_applicable(PartyID, IdentityID, Name, Provider, Metadata, Identity);
                {error, notfound} ->
                    assert_identity_creation_applicable(PartyID, IdentityID, Name, Provider, Metadata, undefined)
            end,
            {identity, #{identity_id => IdentityID, params => Params}};
        ?cm_wallet_creation(IdentityID, WalletID, Name, Currency, Metadata, Params) ->
            case ff_wallet_machine:get(WalletID) of
                {ok, Machine} ->
                    Wallet = ff_wallet_machine:wallet(Machine),
                    assert_wallet_creation_modification_applicable(IdentityID, WalletID, Name, Currency, Metadata, Wallet);
                {error, notfound} ->
                    assert_wallet_creation_modification_applicable(IdentityID, WalletID, Name, Currency, Metadata, undefined)
            end,
            {wallet, #{wallet_id => WalletID, params => Params}}
    end,
    case compare_effects(Effect, Effects) of
        duplicated ->
            raise_invalid_changeset(?cm_invalid_wallet_not_exists(ID), [FFChange]);
        conflicted ->
            raise_invalid_changeset(?cm_invalid_wallet_not_exists(ID), [FFChange]);
        ok ->
            ok
    end,
    assert_modifications_applicable(Others, Timestamp, Revision, [Effect | Effects]);
assert_modifications_applicable([], _, _, _) ->
    ok.

% assert_identity_creation_applicable(_, {creation, _}, undefined, _) ->
%     ok;
% assert_identity_creation_applicable(ID, {creation, _}, #domain_Contract{}, PartyChange) ->
%     raise_invalid_changeset(?cm_invalid_contract_already_exists(ID), [PartyChange]);
% assert_identity_creation_applicable(ID, _AnyModification, undefined, PartyChange) ->
%     raise_invalid_changeset(?cm_invalid_contract_not_exists(ID), [PartyChange]);
assert_identity_creation_applicable(_, _, _, _, _, _) ->
    ok.

% assert_wallet_creation_modification_applicable(_, {creation, _}, undefined, _) ->
%     ok;
% assert_wallet_creation_modification_applicable(ID, _AnyModification, undefined, PartyChange) ->
%     raise_invalid_changeset(?cm_invalid_wallet_not_exists(ID), [PartyChange]);
% assert_wallet_creation_modification_applicable(ID, {creation, _}, #domain_Wallet{}, PartyChange) ->
%     raise_invalid_changeset(?cm_invalid_wallet_already_exists(ID), [PartyChange]);
% assert_wallet_creation_modification_applicable(
%     _ID,
%     {account_creation, _},
%     #domain_Wallet{account = Account},
%     _PartyChange
% ) when Account /= undefined ->
%     throw(#base_InvalidRequest{errors = [<<"Can't change wallet's account">>]});
assert_wallet_creation_modification_applicable(_, _, _, _, _, _) ->
    ok.

compare_effects(_Effect, []) ->
    ok;
compare_effects(Effect, [Effect | _]) ->
    duplicated;
compare_effects({identity, #{identity_id := ID}}, [{identity, #{identity_id := ID}} | _]) ->
    conflicted;
compare_effects({wallet, #{wallet_id := ID}}, [{wallet, #{wallet_id := ID}} | _]) ->
    conflicted;
compare_effects(Effect, [_ | Others]) ->
    compare_effects(Effect, Others).

-spec raise_invalid_changeset(dmsl_claimmgmt_thrift:'InvalidChangesetReason'(), modifications()) -> no_return().
raise_invalid_changeset(Reason, Modifications) ->
    throw(build_invalid_party_changeset(Reason, Modifications)).

build_invalid_party_changeset(Reason, Modifications) ->
    ?cm_invalid_party_changeset(Reason, [{party_modification, C} || C <- Modifications]).
