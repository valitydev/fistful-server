%%%
%%% Domain object helpers
%%%

-module(ct_domain).

-export([create_party/1]).
-export([create_wallet/5]).

-export([currency/1]).
-export([category/3]).
-export([payment_method/1]).
-export([payment_system/2]).
-export([payment_service/2]).
-export([crypto_currency/2]).
-export([inspector/3]).
-export([inspector/4]).
-export([proxy/2]).
-export([proxy/3]).
-export([proxy/4]).
-export([system_account_set/3]).
-export([external_account_set/3]).
-export([term_set_hierarchy/2]).
-export([term_set_hierarchy/3]).
-export([globals/2]).
-export([withdrawal_provider/4]).
-export([withdrawal_provider/5]).
-export([withdrawal_terminal/2]).
-export([withdrawal_terminal/3]).

%%

-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

-define(DTP(Type), dmsl_domain_thrift:Type()).

-type object() :: dmsl_domain_thrift:'DomainObject'().

-type party_id() :: dmsl_base_thrift:'ID'().
-type wallet_id() :: dmsl_base_thrift:'ID'().
-type party() :: dmsl_domain_thrift:'PartyConfig'().
-type termset_ref() :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type payment_inst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type currency() :: dmsl_domain_thrift:'CurrencySymbolicCode'().

-spec create_party(party_id()) -> party().
create_party(PartyID) ->
    PartyConfig = #domain_PartyConfig{
        name = <<"Test Party">>,
        description = <<"Test description">>,
        contact_info = #domain_PartyContactInfo{
            registration_email = <<"test@test.ru">>
        },
        block =
            {unblocked, #domain_Unblocked{
                reason = <<"">>,
                since = ff_time:rfc3339()
            }},
        suspension =
            {active, #domain_Active{
                since = ff_time:rfc3339()
            }}
    },

    % Вставляем Party в домен
    _ = ct_domain_config:upsert(
        {party_config, #domain_PartyConfigObject{
            ref = #domain_PartyConfigRef{id = PartyID},
            data = PartyConfig
        }}
    ),

    PartyConfig.

-spec create_wallet(wallet_id(), party_id(), currency(), termset_ref(), payment_inst_ref()) -> wallet_id().
create_wallet(WalletID, PartyID, Currency, TermsRef, PaymentInstRef) ->
    % Создаем счета
    {ok, SettlementID} = ff_accounting:create_account(Currency, atom_to_binary(test)),

    % Создаем Wallet как объект конфигурации
    WalletConfig = #domain_WalletConfig{
        block =
            {unblocked, #domain_Unblocked{
                reason = <<"">>,
                since = ff_time:rfc3339()
            }},
        suspension =
            {active, #domain_Active{
                since = ff_time:rfc3339()
            }},
        name = <<"Test Wallet">>,
        description = <<"Test description">>,
        account = #domain_WalletAccount{
            currency = #domain_CurrencyRef{symbolic_code = Currency},
            settlement = SettlementID
        },
        payment_institution = PaymentInstRef,
        terms = TermsRef,
        party_ref = #domain_PartyConfigRef{id = PartyID}
    },

    % Вставляем Wallet в домен
    _ = ct_domain_config:upsert(
        {wallet_config, #domain_WalletConfigObject{
            ref = #domain_WalletConfigRef{id = WalletID},
            data = WalletConfig
        }}
    ),

    WalletID.

-spec withdrawal_provider(
    ?DTP('ProviderRef'),
    ?DTP('ProxyRef'),
    ?DTP('PaymentInstitutionRealm'),
    ?DTP('ProvisionTermSet') | undefined
) -> object().
withdrawal_provider(Ref, ProxyRef, Realm, TermSet) ->
    {ok, AccountID} = ct_helper:create_account(<<"RUB">>),
    withdrawal_provider(AccountID, Ref, ProxyRef, Realm, TermSet).

-spec withdrawal_provider(
    ff_account:account_id(),
    ?DTP('ProviderRef'),
    ?DTP('ProxyRef'),
    ?DTP('PaymentInstitutionRealm'),
    ?DTP('ProvisionTermSet') | undefined
) -> object().
withdrawal_provider(AccountID, ?prv(ID) = Ref, ProxyRef, Realm, TermSet) ->
    {provider, #domain_ProviderObject{
        ref = Ref,
        data = #domain_Provider{
            name = genlib:format("Withdrawal provider #~B", [ID]),
            description = <<>>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            realm = Realm,
            terms = TermSet,
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            }
        }
    }}.

-spec withdrawal_terminal(?DTP('TerminalRef'), ?DTP('ProviderRef')) -> object().
withdrawal_terminal(Ref, ProviderRef) ->
    withdrawal_terminal(Ref, ProviderRef, undefined).

-spec withdrawal_terminal(
    ?DTP('TerminalRef'),
    ?DTP('ProviderRef'),
    ?DTP('ProvisionTermSet') | undefined
) -> object().
withdrawal_terminal(?trm(ID) = Ref, ?prv(ProviderID) = ProviderRef, TermSet) ->
    {terminal, #domain_TerminalObject{
        ref = Ref,
        data = #domain_Terminal{
            name = genlib:format("Withdrawal Terminal #~B", [ID]),
            description = genlib:format("Withdrawal Terminal @ Provider #~B", [ProviderID]),
            provider_ref = ProviderRef,
            terms = TermSet
        }
    }}.

-spec currency(?DTP('CurrencyRef')) -> object().
currency(?cur(<<"EUR">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Europe"/utf8>>,
            numeric_code = 978,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"RUB">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Яussian Яuble"/utf8>>,
            numeric_code = 643,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"USD">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"U$ Dollar">>,
            numeric_code = 840,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"BTC">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Bitcoin">>,
            numeric_code = 999,
            symbolic_code = SymCode,
            exponent = 10
        }
    }}.

-spec category(?DTP('CategoryRef'), binary(), ?DTP('CategoryType')) -> object().
category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name = Name,
            description = <<>>,
            type = Type
        }
    }}.

-spec payment_method(?DTP('PaymentMethodRef')) -> object().

payment_method(?pmt(?PAYMENT_METHOD_BANK_CARD(ID)) = Ref) when is_binary(ID) ->
    payment_method(ID, Ref);
payment_method(?pmt(?PAYMENT_METHOD_DIGITAL_WALLET(ID)) = Ref) when is_binary(ID) ->
    payment_method(ID, Ref);
payment_method(?pmt(?PAYMENT_METHOD_CRYPTO_CURRENCY(ID)) = Ref) when is_binary(ID) ->
    payment_method(ID, Ref);
payment_method(?pmt(?PAYMENT_METHOD_GENERIC(ID)) = Ref) when is_binary(ID) ->
    payment_method(ID, Ref).

payment_method(Name, Ref) ->
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name = Name,
            description = <<>>
        }
    }}.

-spec payment_system(?DTP('PaymentSystemRef'), binary()) -> object().
payment_system(Ref, Name) ->
    {payment_system, #domain_PaymentSystemObject{
        ref = Ref,
        data = #domain_PaymentSystem{
            name = Name
        }
    }}.

-spec payment_service(?DTP('PaymentServiceRef'), binary()) -> object().
payment_service(Ref, Name) ->
    {payment_service, #domain_PaymentServiceObject{
        ref = Ref,
        data = #domain_PaymentService{
            name = Name
        }
    }}.

-spec crypto_currency(?DTP('CryptoCurrencyRef'), binary()) -> object().
crypto_currency(Ref, Name) ->
    {crypto_currency, #domain_CryptoCurrencyObject{
        ref = Ref,
        data = #domain_CryptoCurrency{
            name = Name
        }
    }}.

-spec inspector(?DTP('InspectorRef'), binary(), ?DTP('ProxyRef')) -> object().
inspector(Ref, Name, ProxyRef) ->
    inspector(Ref, Name, ProxyRef, #{}).

-spec inspector(?DTP('InspectorRef'), binary(), ?DTP('ProxyRef'), ?DTP('ProxyOptions')) -> object().
inspector(Ref, Name, ProxyRef, Additional) ->
    {inspector, #domain_InspectorObject{
        ref = Ref,
        data = #domain_Inspector{
            name = Name,
            description = <<>>,
            proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = Additional
            }
        }
    }}.

-spec proxy(?DTP('ProxyRef'), Name :: binary()) -> object().
proxy(Ref, Name) ->
    proxy(Ref, Name, <<>>).

-spec proxy(?DTP('ProxyRef'), Name :: binary(), URL :: binary()) -> object().
proxy(Ref, Name, URL) ->
    proxy(Ref, Name, URL, #{}).

-spec proxy(?DTP('ProxyRef'), Name :: binary(), URL :: binary(), ?DTP('ProxyOptions')) -> object().
proxy(Ref, Name, URL, Opts) ->
    {proxy, #domain_ProxyObject{
        ref = Ref,
        data = #domain_ProxyDefinition{
            name = Name,
            description = <<>>,
            url = URL,
            options = Opts
        }
    }}.

-spec system_account_set(?DTP('SystemAccountSetRef'), binary(), ?DTP('CurrencyRef')) -> object().
system_account_set(Ref, Name, ?cur(SymCode)) ->
    {ok, AccountID1} = ct_helper:create_account(SymCode),
    {ok, AccountID2} = ct_helper:create_account(SymCode),
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = <<>>,
            accounts = #{
                ?cur(SymCode) => #domain_SystemAccount{
                    settlement = AccountID1,
                    subagent = AccountID2
                }
            }
        }
    }}.

-spec external_account_set(?DTP('ExternalAccountSetRef'), binary(), ?DTP('CurrencyRef')) ->
    object().
external_account_set(Ref, Name, ?cur(SymCode)) ->
    {ok, AccountID1} = ct_helper:create_account(SymCode),
    {ok, AccountID2} = ct_helper:create_account(SymCode),
    {external_account_set, #domain_ExternalAccountSetObject{
        ref = Ref,
        data = #domain_ExternalAccountSet{
            name = Name,
            description = <<>>,
            accounts = #{
                ?cur(SymCode) => #domain_ExternalAccount{
                    income = AccountID1,
                    outcome = AccountID2
                }
            }
        }
    }}.

-spec term_set_hierarchy(?DTP('TermSetHierarchyRef'), ?DTP('TermSet')) -> object().
term_set_hierarchy(Ref, TermSet) ->
    term_set_hierarchy(Ref, undefined, TermSet).

-spec term_set_hierarchy(Ref, ff_maybe:'maybe'(Ref), ?DTP('TermSet')) -> object() when
    Ref :: ?DTP('TermSetHierarchyRef').
term_set_hierarchy(Ref, ParentRef, TermSet) ->
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = Ref,
        data = #domain_TermSetHierarchy{
            parent_terms = ParentRef,
            term_set = TermSet
        }
    }}.

-spec globals(?DTP('ExternalAccountSetRef'), [?DTP('PaymentInstitutionRef')]) -> object().
globals(EASRef, PIRefs) ->
    {globals, #domain_GlobalsObject{
        ref = ?glob(),
        data = #domain_Globals{
            external_account_set = {value, EASRef},
            payment_institutions = ?ordset(PIRefs)
        }
    }}.
