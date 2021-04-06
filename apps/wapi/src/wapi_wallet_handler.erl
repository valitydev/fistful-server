-module(wapi_wallet_handler).

-behaviour(swag_server_wallet_logic_handler).
-behaviour(wapi_handler).

%% swag_server_wallet_logic_handler callbacks
-export([map_error/2]).
-export([authorize_api_key/4]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result() :: wapi_handler:request_result().
-type operation_id() :: swag_server_wallet:operation_id().
-type api_key() :: swag_server_wallet:api_key().
-type request_context() :: swag_server_wallet:request_context().
-type handler_opts() :: swag_server_wallet:handler_opts(_).

%% API

-spec map_error(atom(), swag_server_wallet_validation:error()) -> swag_server_wallet:error_reason().
map_error(validation_error, Error) ->
    Type = map_error_type(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"errorType">> => Type,
        <<"name">> => Name,
        <<"description">> => Message
    }).

-spec map_error_type(swag_server_wallet_validation:error_type()) -> binary().
map_error_type(no_match) -> <<"NoMatch">>;
map_error_type(not_found) -> <<"NotFound">>;
map_error_type(not_in_range) -> <<"NotInRange">>;
map_error_type(wrong_length) -> <<"WrongLength">>;
map_error_type(wrong_size) -> <<"WrongSize">>;
map_error_type(schema_violated) -> <<"SchemaViolated">>;
map_error_type(wrong_type) -> <<"WrongType">>;
map_error_type(wrong_array) -> <<"WrongArray">>.

-spec authorize_api_key(operation_id(), api_key(), request_context(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, _SwagContext, _Opts) ->
    ok = scoper:add_scope('swag.server', #{api => wallet, operation_id => OperationID}),
    case uac:authorize_api_key(ApiKey, wapi_auth:get_verification_options()) of
        {ok, Context0} ->
            Context = wapi_auth:create_wapi_context(Context0),
            {true, Context};
        {error, Error} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec handle_request(swag_server_wallet:operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(wallet, OperationID, Req, SwagContext, Opts).

%% Providers
-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) -> request_result().
process_request('ListProviders', #{'residence' := Residence}, Context, _Opts) ->
    Providers = wapi_wallet_ff_backend:get_providers(ff_maybe:to_list(Residence), Context),
    wapi_handler_utils:reply_ok(200, Providers);
process_request('GetProvider', #{'providerID' := Id}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_provider(Id, Context) of
        {ok, Provider} -> wapi_handler_utils:reply_ok(200, Provider);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListProviderIdentityClasses', #{'providerID' := Id}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_classes(Id, Context) of
        {ok, Classes} -> wapi_handler_utils:reply_ok(200, Classes);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'GetProviderIdentityClass',
    #{
        'providerID' := ProviderId,
        'identityClassID' := ClassId
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_provider_identity_class(ProviderId, ClassId, Context) of
        {ok, Class} -> wapi_handler_utils:reply_ok(200, Class);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'ListProviderIdentityLevels',
    #{
        'providerID' := _ProviderId,
        'identityClassID' := _ClassId
    },
    _Context,
    _Opts
) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_levels(ProviderId, ClassId, Context) of
    %%     {ok, Levels}      -> wapi_handler_utils:reply_ok(200, Levels);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request(
    'GetProviderIdentityLevel',
    #{
        'providerID' := _ProviderId,
        'identityClassID' := _ClassId,
        'identityLevelID' := _LevelId
    },
    _Context,
    _Opts
) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_level(ProviderId, ClassId, LevelId, Context) of
    %%     {ok, Level}       -> wapi_handler_utils:reply_ok(200, Level);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
%% Identities
process_request('ListIdentities', Params, Context, _Opts) ->
    case wapi_stat_backend:list_identities(Params, Context) of
        {ok, Result} ->
            wapi_handler_utils:reply_ok(200, Result);
        {error, {invalid, Errors}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"NoMatch">>,
                <<"description">> => Errors
            });
        {error, {bad_token, Reason}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidToken">>,
                <<"description">> => Reason
            })
    end;
process_request('GetIdentity', #{'identityID' := IdentityId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity(IdentityId, Context) of
        {ok, Identity} -> wapi_handler_utils:reply_ok(200, Identity);
        {error, {identity, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateIdentity', #{'Identity' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_identity(Params, Context) of
        {ok, Identity = #{<<"id">> := IdentityId}} ->
            wapi_handler_utils:reply_ok(201, Identity, get_location('GetIdentity', [IdentityId], Opts));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Party inaccessible">>));
        {error, {party, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Party does not exist">>));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {identity_class, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>));
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, {email, notfound}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"email">>,
                <<"description">> => <<"No email in JWT">>
            })
    end;
process_request('ListIdentityChallenges', #{'identityID' := Id, 'status' := Status}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenges(Id, ff_maybe:to_list(Status), Context) of
        {ok, Challenges} -> wapi_handler_utils:reply_ok(200, Challenges);
        {error, {identity, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'StartIdentityChallenge',
    #{
        'identityID' := IdentityId,
        'IdentityChallenge' := Params
    },
    Context,
    Opts
) ->
    case wapi_wallet_ff_backend:create_identity_challenge(IdentityId, Params, Context) of
        {ok, Challenge = #{<<"id">> := ChallengeId}} ->
            wapi_handler_utils:reply_ok(202, Challenge, get_location('GetIdentityChallenge', [ChallengeId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {challenge, conflict}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {pending, _}}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {class, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such challenge type">>));
        {error, {challenge, {proof, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Proof not found">>));
        {error, {challenge, {proof, insufficient}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Insufficient proof">>));
        {error, {challenge, {level, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Illegal identification type for current identity level">>)
            )
        %% TODO any other possible errors here?
    end;
process_request(
    'GetIdentityChallenge',
    #{
        'identityID' := IdentityId,
        'challengeID' := ChallengeId
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_identity_challenge(IdentityId, ChallengeId, Context) of
        {ok, Challenge} -> wapi_handler_utils:reply_ok(200, Challenge);
        {error, {identity, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {challenge, notfound}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('PollIdentityChallengeEvents', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_events(Params, Context) of
        {ok, Events} -> wapi_handler_utils:reply_ok(200, Events);
        {error, {identity, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentityChallengeEvent', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_event(Params, Context) of
        {ok, Event} -> wapi_handler_utils:reply_ok(200, Event);
        {error, {identity, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}} -> wapi_handler_utils:reply_ok(404)
    end;
%% Wallets
process_request('ListWallets', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_wallets(Params, Context) of
        {ok, {200, _, List}} -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}} -> wapi_handler_utils:reply_error(Code, Error)
    end;
process_request('GetWallet', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, Wallet} -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, {wallet, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetWalletByExternalID', #{externalID := ExternalID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet_by_external_id(ExternalID, Context) of
        {ok, Wallet} -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, {wallet, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWallet', #{'Wallet' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_wallet(Params, Context) of
        {ok, Wallet = #{<<"id">> := WalletId}} ->
            wapi_handler_utils:reply_ok(201, Wallet, get_location('GetWallet', [WalletId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>));
        {error, {terms, {terms_violation, {not_allowed_currency, _Data}}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not allowed">>))
    end;
process_request('GetWalletAccount', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet_account(WalletId, Context) of
        {ok, WalletAccount} -> wapi_handler_utils:reply_ok(200, WalletAccount);
        {error, {wallet, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'IssueWalletGrant',
    #{
        'walletID' := WalletId,
        'WalletGrantRequest' := #{<<"validUntil">> := Expiration, <<"asset">> := Asset}
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, _} ->
            case wapi_backend_utils:issue_grant_token({wallets, WalletId, Asset}, Expiration, Context) of
                {ok, Token} ->
                    wapi_handler_utils:reply_ok(201, #{
                        <<"token">> => Token,
                        <<"validUntil">> => Expiration,
                        <<"asset">> => Asset
                    });
                {error, expired} ->
                    wapi_handler_utils:reply_ok(
                        422,
                        wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
                    )
            end;
        {error, {wallet, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
%% Withdrawals
process_request('ListDestinations', Params, Context, _Opts) ->
    case wapi_stat_backend:list_destinations(Params, Context) of
        {ok, Result} ->
            wapi_handler_utils:reply_ok(200, Result);
        {error, {invalid, Errors}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"NoMatch">>,
                <<"description">> => Errors
            });
        {error, {bad_token, Reason}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidToken">>,
                <<"description">> => Reason
            })
    end;
process_request('GetDestination', #{'destinationID' := DestinationId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, Destination} -> wapi_handler_utils:reply_ok(200, Destination);
        {error, {destination, notfound}} -> wapi_handler_utils:reply_ok(404);
        {error, {destination, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetDestinationByExternalID', #{'externalID' := ExternalID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_destination_by_external_id(ExternalID, Context) of
        {ok, Destination} ->
            wapi_handler_utils:reply_ok(200, Destination);
        {error, {external_id, {unknown_external_id, ExternalID}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateDestination', #{'Destination' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_destination(Params, Context) of
        {ok, Destination = #{<<"id">> := DestinationId}} ->
            wapi_handler_utils:reply_ok(201, Destination, get_location('GetDestination', [DestinationId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>));
        {error, {invalid_resource_token, Type}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidResourceToken">>,
                <<"name">> => Type,
                <<"description">> => <<"Specified resource token is invalid">>
            })
    end;
process_request(
    'IssueDestinationGrant',
    #{
        'destinationID' := DestinationId,
        'DestinationGrantRequest' := #{<<"validUntil">> := Expiration}
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, _} ->
            case wapi_backend_utils:issue_grant_token({destinations, DestinationId}, Expiration, Context) of
                {ok, Token} ->
                    wapi_handler_utils:reply_ok(201, #{
                        <<"token">> => Token,
                        <<"validUntil">> => Expiration
                    });
                {error, expired} ->
                    wapi_handler_utils:reply_ok(
                        422,
                        wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
                    )
            end;
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWithdrawal', #{'WithdrawalParameters' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_withdrawal(Params, Context) of
        {ok, Withdrawal = #{<<"id">> := WithdrawalId}} ->
            wapi_handler_utils:reply_ok(202, Withdrawal, get_location('GetWithdrawal', [WithdrawalId], Opts));
        {error, {source, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>));
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such destination">>));
        {error, {destination, {unauthorized, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Destination unauthorized">>)
            );
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Destination unauthorized">>));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, {wallet, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>));
        {error, {wallet, {unauthorized, _}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Wallet unauthorized">>));
        {error, {wallet, {inaccessible, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Inaccessible source or destination">>)
            );
        {error, {wallet, {currency, invalid}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid currency for source or destination">>)
            );
        {error, {wallet, {provider, invalid}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid provider for source or destination">>)
            );
        {error, {quote_invalid_party, _}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Withdrawal owner differs from quote`s one">>)
            );
        {error, {quote_invalid_wallet, _}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Withdrawal wallet differs from quote`s one">>)
            );
        {error, {quote, {invalid_destination, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Withdrawal destination differs from quote`s one">>)
            );
        {error, {quote, {invalid_body, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Withdrawal body differs from quote`s one">>)
            );
        {error, {quote, token_expired}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Quote token expired">>)
            );
        {error, {terms, {terms_violation, {cash_range, _}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid cash amount">>)
            );
        {error, {destination_resource, {bin_data, not_found}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Unknown card issuer">>)
            );
        {error, {destination_resource, {bin_data, {unknown_payment_system, _PaymentSystem}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Unknown card payment system">>)
            );
        {error, {destination_resource, {bin_data, {unknown_residence, _Residence}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Unknown card issuer residence">>)
            );
        {error, {identity_providers_mismatch, _}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"This wallet and destination cannot be used together">>)
            )
    end;
process_request('GetWithdrawal', #{'withdrawalID' := WithdrawalId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal(WithdrawalId, Context) of
        {ok, Withdrawal} ->
            wapi_handler_utils:reply_ok(200, Withdrawal);
        {error, {withdrawal, {unknown_withdrawal, WithdrawalId}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('GetWithdrawalByExternalID', #{'externalID' := ExternalID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_by_external_id(ExternalID, Context) of
        {ok, Withdrawal} ->
            wapi_handler_utils:reply_ok(200, Withdrawal);
        {error, {external_id, {unknown_external_id, ExternalID}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, {unknown_withdrawal, _WithdrawalId}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('PollWithdrawalEvents', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_events(Params, Context) of
        {ok, Events} ->
            wapi_handler_utils:reply_ok(200, Events);
        {error, {withdrawal, {unknown_withdrawal, _WithdrawalId}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'GetWithdrawalEvents',
    #{
        'withdrawalID' := WithdrawalId,
        'eventID' := EventId
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_withdrawal_event(WithdrawalId, EventId, Context) of
        {ok, Event} ->
            wapi_handler_utils:reply_ok(200, Event);
        {error, {withdrawal, {unknown_withdrawal, WithdrawalId}}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('ListWithdrawals', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_withdrawals(Params, Context) of
        {ok, {200, _, List}} -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}} -> wapi_handler_utils:reply_error(Code, Error)
    end;
process_request('CreateQuote', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_quote(Params, Context) of
        {ok, Promise} ->
            wapi_handler_utils:reply_ok(202, Promise);
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Destination not found">>)
            );
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Destination unauthorized">>)
            );
        {error, {route, route_not_found}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Provider not found">>)
            );
        {error, {wallet, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Wallet not found">>)
            );
        {error, {identity_providers_mismatch, _}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(
                    <<"This wallet and destination cannot be used together">>
                )
            )
    end;
%% Residences
process_request('GetResidence', #{'residence' := ResidenceId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_residence(ResidenceId, Context) of
        {ok, Residence} -> wapi_handler_utils:reply_ok(200, Residence);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
%% Currencies
process_request('GetCurrency', #{'currencyID' := CurrencyId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_currency(CurrencyId, Context) of
        {ok, Currency} -> wapi_handler_utils:reply_ok(200, Currency);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
%% Reports
process_request('CreateReport', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_report(Params, Context) of
        {ok, Report} ->
            wapi_handler_utils:reply_ok(201, Report);
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, invalid_request} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NoMatch">>,
                <<"name">> => <<"timestamps">>,
                <<"description">> => <<"invalid time range">>
            });
        {error, invalid_contract} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"contractID">>,
                <<"description">> => <<"contract not found">>
            })
    end;
process_request(
    'GetReport',
    #{
        identityID := IdentityID,
        reportID := ReportId
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:get_report(ReportId, IdentityID, Context) of
        {ok, Report} ->
            wapi_handler_utils:reply_ok(200, Report);
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('GetReports', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_reports(Params, Context) of
        {ok, ReportList} ->
            wapi_handler_utils:reply_ok(200, ReportList);
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NotFound">>,
                <<"name">> => <<"identity">>,
                <<"description">> => <<"identity not found">>
            });
        {error, invalid_request} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"NoMatch">>,
                <<"name">> => <<"timestamps">>,
                <<"description">> => <<"invalid time range">>
            });
        {error, {dataset_too_big, Limit}} ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"WrongLength">>,
                <<"name">> => <<"limitExceeded">>,
                <<"description">> => io_lib:format("Max limit: ~p", [Limit])
            })
    end;
process_request('DownloadFile', #{fileID := FileId}, Context, _Opts) ->
    ExpiresAt = get_default_url_lifetime(),
    case wapi_wallet_ff_backend:download_file(FileId, ExpiresAt, Context) of
        {ok, URL} ->
            wapi_handler_utils:reply_ok(201, #{<<"url">> => URL, <<"expiresAt">> => ExpiresAt});
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end;
%% Deposits
process_request('ListDeposits', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_deposits(Params, Context) of
        {ok, {200, _, List}} -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}} -> wapi_handler_utils:reply_error(Code, Error)
    end;
process_request('ListDepositReverts', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_deposit_reverts(Params, Context) of
        {ok, {200, _, List}} -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}} -> wapi_handler_utils:reply_error(Code, Error)
    end;
process_request('ListDepositAdjustments', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_deposit_adjustments(Params, Context) of
        {ok, {200, _, List}} -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}} -> wapi_handler_utils:reply_error(Code, Error)
    end;
%% Webhooks
process_request('CreateWebhook', Params, Context, _Opts) ->
    case wapi_webhook_backend:create_webhook(Params, Context) of
        {ok, Webhook} ->
            wapi_handler_utils:reply_ok(201, Webhook);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {wallet, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>));
        {error, {wallet, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>))
    end;
process_request('GetWebhooks', #{identityID := IdentityID}, Context, _Opts) ->
    case wapi_webhook_backend:get_webhooks(IdentityID, Context) of
        {ok, Webhooks} ->
            wapi_handler_utils:reply_ok(200, Webhooks);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>))
    end;
process_request('GetWebhookByID', #{identityID := IdentityID, webhookID := WebhookID}, Context, _Opts) ->
    case wapi_webhook_backend:get_webhook(WebhookID, IdentityID, Context) of
        {ok, Webhook} ->
            wapi_handler_utils:reply_ok(200, Webhook);
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>))
    end;
process_request('DeleteWebhookByID', #{identityID := IdentityID, webhookID := WebhookID}, Context, _Opts) ->
    case wapi_webhook_backend:delete_webhook(WebhookID, IdentityID, Context) of
        ok ->
            wapi_handler_utils:reply_ok(204);
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>))
    end;
%% P2P
process_request('QuoteP2PTransfer', #{'QuoteParameters' := Params}, Context, _Opts) ->
    case wapi_wallet_ff_backend:quote_p2p_transfer(Params, Context) of
        {ok, Quote} ->
            wapi_handler_utils:reply_ok(201, Quote);
        {error, {identity, not_found}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {party, _PartyContractError}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such party">>)
            );
        {error, {sender, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid sender resource">>)
            );
        {error, {receiver, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid receiver resource">>)
            );
        {error, {terms, {terms_violation, {not_allowed_currency, _Details}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Currency not allowed">>)
            );
        {error, {terms, {terms_violation, {cash_range, {_Cash, _CashRange}}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Transfer amount is out of allowed range">>)
            );
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"P2P transfer not allowed">>)
            );
        {error, {invalid_resource_token, Type}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidResourceToken">>,
                <<"name">> => Type,
                <<"description">> => <<"Specified resource token is invalid">>
            })
    end;
process_request('CreateP2PTransfer', #{'P2PTransferParameters' := Params}, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_p2p_transfer(Params, Context) of
        {ok, P2PTransfer} ->
            wapi_handler_utils:reply_ok(202, P2PTransfer);
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {sender, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid sender resource">>)
            );
        {error, {sender, different_resource}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid sender resource">>)
            );
        {error, {receiver, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid receiver resource">>)
            );
        {error, {receiver, different_resource}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid receiver resource">>)
            );
        {error, {terms, {terms_violation, {not_allowed_currency, _Details}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Currency not allowed">>)
            );
        {error, {terms, {terms_violation, {cash_range, {_Cash, _CashRange}}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Transfer amount is out of allowed range">>)
            );
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"P2P transfer not allowed">>)
            );
        {error, {token, {not_verified, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Token can't be verified">>)
            );
        {error, {quote, token_expired}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Quote token expired">>)
            );
        {error, {invalid_resource_token, Type}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidResourceToken">>,
                <<"name">> => Type,
                <<"description">> => <<"Specified resource token is invalid">>
            })
    end;
process_request('GetP2PTransfer', #{p2pTransferID := ID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_p2p_transfer(ID, Context) of
        {ok, P2PTransfer} ->
            wapi_handler_utils:reply_ok(200, P2PTransfer);
        {error, {p2p_transfer, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {p2p_transfer, {unknown_p2p_transfer, _ID}}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('GetP2PTransferEvents', #{p2pTransferID := ID, continuationToken := CT}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_p2p_transfer_events({ID, CT}, Context) of
        {ok, P2PTransferEvents} ->
            wapi_handler_utils:reply_ok(200, P2PTransferEvents);
        {error, {p2p_transfer, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {p2p_transfer, not_found}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {token, {not_verified, invalid_signature}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Continuation Token can't be verified">>)
            )
    end;
%% P2P Templates
process_request('CreateP2PTransferTemplate', #{'P2PTransferTemplateParameters' := Params}, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_p2p_template(Params, Context) of
        {ok, P2PTemplate} ->
            wapi_handler_utils:reply_ok(201, P2PTemplate);
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {terms, {terms_violation, p2p_template_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"P2P template not allowed">>)
            )
    end;
process_request('GetP2PTransferTemplateByID', #{p2pTransferTemplateID := ID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_p2p_template(ID, Context) of
        {ok, P2PTemplate} ->
            wapi_handler_utils:reply_ok(200, P2PTemplate);
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('BlockP2PTransferTemplate', #{p2pTransferTemplateID := ID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:block_p2p_template(ID, Context) of
        ok ->
            wapi_handler_utils:reply_ok(204);
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {p2p_template, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'IssueP2PTransferTemplateAccessToken',
    #{
        p2pTransferTemplateID := ID,
        'P2PTransferTemplateTokenRequest' := #{<<"validUntil">> := Expiration}
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:issue_p2p_template_access_token(ID, Expiration, Context) of
        {ok, Token} ->
            wapi_handler_utils:reply_ok(201, #{
                <<"token">> => Token,
                <<"validUntil">> => Expiration
            });
        {error, {p2p_template, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, expired} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
            );
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'IssueP2PTransferTicket',
    #{
        p2pTransferTemplateID := ID,
        'P2PTransferTemplateTicketRequest' := #{<<"validUntil">> := Expiration0}
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:issue_p2p_transfer_ticket(ID, Expiration0, Context) of
        {ok, {Token, Expiration1}} ->
            wapi_handler_utils:reply_ok(201, #{
                <<"token">> => Token,
                <<"validUntil">> => Expiration1
            });
        {error, expired} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
            );
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request(
    'CreateP2PTransferWithTemplate',
    #{
        p2pTransferTemplateID := TemplateID,
        'P2PTransferWithTemplateParameters' := Params
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:create_p2p_transfer_with_template(TemplateID, Params, Context) of
        {ok, P2PTransfer} ->
            wapi_handler_utils:reply_ok(202, P2PTransfer);
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {external_id_conflict, ID, ExternalID}} ->
            wapi_handler_utils:logic_error(external_id_conflict, {ID, ExternalID});
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {sender, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid sender resource">>)
            );
        {error, {receiver, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid receiver resource">>)
            );
        {error, {terms, {terms_violation, {not_allowed_currency, _Details}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Currency not allowed">>)
            );
        {error, {terms, {terms_violation, {cash_range, {_Cash, _CashRange}}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Transfer amount is out of allowed range">>)
            );
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"P2P transfer not allowed">>)
            );
        {error, {token, {not_verified, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Token can't be verified">>)
            );
        {error, {quote, token_expired}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Quote token expired">>)
            );
        {error, {invalid_resource_token, Type}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidResourceToken">>,
                <<"name">> => Type,
                <<"description">> => <<"Specified resource token is invalid">>
            })
    end;
process_request(
    'QuoteP2PTransferWithTemplate',
    #{
        p2pTransferTemplateID := TemplateID,
        'P2PTransferTemplateQuoteParameters' := Params
    },
    Context,
    _Opts
) ->
    case wapi_wallet_ff_backend:quote_p2p_transfer_with_template(TemplateID, Params, Context) of
        {ok, Quote} ->
            wapi_handler_utils:reply_ok(201, Quote);
        {error, {unknown_p2p_template, _ID}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, not_found}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such identity">>)
            );
        {error, {party, _PartyContractError}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such party">>)
            );
        {error, {sender, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid sender resource">>)
            );
        {error, {receiver, {bin_data, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Invalid receiver resource">>)
            );
        {error, {terms, {terms_violation, {not_allowed_currency, _Details}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Currency not allowed">>)
            );
        {error, {terms, {terms_violation, {cash_range, {_Cash, _CashRange}}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Transfer amount is out of allowed range">>)
            );
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"P2P transfer not allowed">>)
            );
        {error, {invalid_resource_token, Type}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">> => <<"InvalidResourceToken">>,
                <<"name">> => Type,
                <<"description">> => <<"Specified resource token is invalid">>
            })
    end;
%% W2W
process_request('CreateW2WTransfer', #{'W2WTransferParameters' := Params}, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_w2w_transfer(Params, Context) of
        {ok, W2WTransfer} ->
            wapi_handler_utils:reply_ok(202, W2WTransfer);
        {error, {wallet_from, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such wallet sender">>)
            );
        {error, {wallet_to, notfound}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"No such wallet receiver">>)
            );
        {error, {wallet_from, {inaccessible, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Sender wallet is unaccessible">>)
            );
        {error, {wallet_to, {inaccessible, _}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Receiver wallet is unaccessible">>)
            );
        {error, {terms, {terms_violation, {not_allowed_currency, _Details}}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Currency not allowed">>)
            );
        {error, {terms, {terms_violation, w2w_forbidden}}} ->
            wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"W2W transfer not allowed">>)
            )
    end;
process_request('GetW2WTransfer', #{w2wTransferID := ID}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_w2w_transfer(ID, Context) of
        {ok, W2WTransfer} ->
            wapi_handler_utils:reply_ok(200, W2WTransfer);
        {error, {w2w_transfer, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {w2w_transfer, {unknown_w2w_transfer, _ID}}} ->
            wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

% seconds
-define(DEFAULT_URL_LIFETIME, 60).

get_default_url_lifetime() ->
    Now = erlang:system_time(second),
    Lifetime = application:get_env(wapi, file_storage_url_lifetime, ?DEFAULT_URL_LIFETIME),
    genlib_rfc3339:format(Now + Lifetime, second).
