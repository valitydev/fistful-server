-module(ff_server_utils).

-export([contruct_backend_childspec/3]).
-export([get_namespace_schema/1]).
-export([get_service_client/1]).

%%

-spec contruct_backend_childspec(
    machinery:namespace(),
    machinery:modopts(_),
    party_client:client()
) ->
    {_BackendSpec, machinery_mg_backend:handler(_), _ModernizerSpec}.
contruct_backend_childspec(NS, Handler, PartyClient) ->
    Schema = get_namespace_schema(NS),
    {
        construct_machinery_backend_spec(NS, Schema),
        construct_machinery_handler_spec(NS, Handler, Schema, PartyClient),
        construct_machinery_modernizer_spec(NS, Schema)
    }.

construct_machinery_backend_spec(NS, Schema) ->
    {NS,
        {machinery_mg_backend, #{
            schema => Schema,
            client => get_service_client(automaton)
        }}}.

construct_machinery_handler_spec(NS, Handler, Schema, PartyClient) ->
    {{fistful, #{handler => Handler, party_client => PartyClient}}, #{
        path => ff_string:join(["/v1/stateproc/", NS]),
        backend_config => #{schema => Schema}
    }}.

construct_machinery_modernizer_spec(NS, Schema) ->
    #{
        path => ff_string:join(["/v1/modernizer/", NS]),
        backend_config => #{schema => Schema}
    }.

-spec get_service_client(atom()) ->
    ff_woody_client:client().
get_service_client(ServiceID) ->
    case genlib_app:env(fistful, services, #{}) of
        #{ServiceID := V} ->
            ff_woody_client:new(V);
        #{} ->
            error({unknown_service, ServiceID})
    end.

-spec get_namespace_schema(machinery:namespace()) ->
    module().
get_namespace_schema('ff/identity') ->
    ff_identity_machinery_schema;
get_namespace_schema('ff/wallet_v2') ->
    ff_wallet_machinery_schema;
get_namespace_schema('ff/source_v1') ->
    ff_source_machinery_schema;
get_namespace_schema('ff/destination_v2') ->
    ff_destination_machinery_schema;
get_namespace_schema('ff/deposit_v1') ->
    ff_deposit_machinery_schema;
get_namespace_schema('ff/withdrawal_v2') ->
    ff_withdrawal_machinery_schema;
get_namespace_schema('ff/withdrawal/session_v2') ->
    ff_withdrawal_session_machinery_schema;
get_namespace_schema('ff/w2w_transfer_v1') ->
    ff_w2w_transfer_machinery_schema.
