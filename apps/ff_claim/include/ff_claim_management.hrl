-ifndef(__ff_claim_management_hrl__).
-define(__ff_claim_management_hrl__, included).

-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").

-define(cm_modification_unit(ModID, Timestamp, Mod, UserInfo), #claimmgmt_ModificationUnit{
    modification_id = ModID,
    created_at = Timestamp,
    modification = Mod,
    user_info = UserInfo
}).

-define(cm_wallet_modification(ModID, Timestamp, Mod, UserInfo),
    ?cm_modification_unit(ModID, Timestamp, {wallet_modification, Mod}, UserInfo)
).

-define(cm_identity_modification(ModID, Timestamp, Mod, UserInfo),
    ?cm_modification_unit(ModID, Timestamp, {identity_modification, Mod}, UserInfo)
).

%%% Identity

-define(cm_identity_creation(PartyID, IdentityID, Name, Provider, Metadata, Params), #claimmgmt_IdentityModificationUnit{
    id = IdentityID,
    modification = {creation, Params = #claimmgmt_IdentityParams{
        party_id = PartyID,
        name = Name,
        provider = Provider,
        metadata = Metadata
    }}
}).

%%% Wallet

-define(cm_wallet_creation(IdentityID, WalletID, Name, Currency, Metadata, Params), #claimmgmt_NewWalletModificationUnit{
    id = WalletID,
    modification = {creation, Params = #claimmgmt_NewWalletParams{
        identity_id = IdentityID,
        name = Name,
        currency = Currency,
        metadata = Metadata
    }}
}).

%%% Error

-define(cm_invalid_party_changeset(Reason, InvalidChangeset), #claimmgmt_InvalidChangeset{
    reason = {invalid_party_changeset, Reason},
    invalid_changeset = InvalidChangeset
}).

-define(cm_invalid_contract(ID, Reason),
    {invalid_contract, #claimmgmt_InvalidContract{id = ID, reason = Reason}}
).

-define(cm_invalid_wallet(ID, Reason),
    {invalid_wallet, #claimmgmt_InvalidWallet{id = ID, reason = Reason}}
).

-define(cm_invalid_wallet_not_exists(ID),
    ?cm_invalid_wallet(ID, {not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_already_exists(ID),
    ?cm_invalid_wallet(ID, {already_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_account_not_exists(ID),
    ?cm_invalid_wallet(ID, {account_not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_contract_terms_violated(ID, ContractID, Terms),
    ?cm_invalid_wallet(
        ID,
        {contract_terms_violated, #claimmgmt_ContractTermsViolated{
            contract_id = ContractID,
            terms = Terms
        }}
    )
).

-endif.
