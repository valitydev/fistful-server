%%%
%%% Tranfer
%%%
%%% TODOs
%%%
%%%  - We must synchronise any transfers on wallet machine, as one may request
%%%    us to close wallet concurrently. Moreover, we should probably check any
%%%    limits there too.
%%%  - What if we get rid of some failures in `prepare`, specifically those
%%%    which related to wallet blocking / suspension? It would be great to get
%%%    rid of the `wallet closed` failure but I see no way to do so.
%%%

-module(ff_postings_transfer).

-type final_cash_flow() :: ff_cash_flow:final_cash_flow().

-type status() ::
    created
    | prepared
    | committed
    | cancelled.

-type transfer() :: #{
    id := id(),
    final_cash_flow := final_cash_flow(),
    status => status()
}.

-type event() ::
    {created, transfer()}
    | {status_changed, status()}.

-export_type([transfer/0]).
-export_type([final_cash_flow/0]).
-export_type([status/0]).
-export_type([event/0]).

-export([id/1]).
-export([final_cash_flow/1]).
-export([status/1]).

-export([create/2]).
-export([prepare/1]).
-export([commit/1]).
-export([cancel/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id() :: ff_accounting:id().

%%

-spec id(transfer()) -> id().
-spec final_cash_flow(transfer()) -> final_cash_flow().
-spec status(transfer()) -> status().

id(#{id := V}) ->
    V.

final_cash_flow(#{final_cash_flow := V}) ->
    V.

status(#{status := V}) ->
    V.

%%

-spec create(id(), final_cash_flow()) ->
    {ok, [event()]}
    | {error,
        empty
        | {account, ff_party:inaccessibility()}
        | {currency, ff_currency:id()}
        | {provider, id()}}.
create(_TrxID, #{postings := []}) ->
    {error, empty};
create(ID, CashFlow) ->
    do(fun() ->
        Accounts = ff_cash_flow:gather_used_accounts(CashFlow),
        valid = validate_currencies(Accounts),
        valid = validate_realms(Accounts),
        accessible = validate_accessible(Accounts),
        [
            {created, #{
                id => ID,
                final_cash_flow => CashFlow
            }},
            {status_changed, created}
        ]
    end).

validate_accessible(Accounts) ->
    _ = [accessible = unwrap(account, ff_account:is_accessible(A)) || A <- Accounts],
    accessible.

validate_currencies([A0 | Accounts]) ->
    Currency = ff_account:currency(A0),
    _ = [ok = unwrap(currency, valid(Currency, ff_account:currency(A))) || A <- Accounts],
    valid.

validate_realms([A0 | Accounts]) ->
    Realm0 = ff_account:realm(A0),
    _ = [
        ok = unwrap(provider, valid(Realm0, ff_account:realm(Account)))
     || Account <- Accounts
    ],
    valid.

%%

-spec prepare(transfer()) ->
    {ok, [event()]}
    | {error, {status, committed | cancelled}}.
prepare(#{status := created} = Transfer) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun() ->
        _PostingPlanLog = unwrap(ff_accounting:prepare_trx(ID, construct_trx_postings(CashFlow))),
        [{status_changed, prepared}]
    end);
prepare(#{status := prepared}) ->
    {ok, []};
prepare(#{status := Status}) ->
    {error, Status}.

%% TODO
% validate_balances(Affected) ->
%     {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, [event()]}
    | {error, {status, created | cancelled}}.
commit(#{status := prepared} = Transfer) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun() ->
        _PostingPlanLog = unwrap(ff_accounting:commit_trx(ID, construct_trx_postings(CashFlow))),
        [{status_changed, committed}]
    end);
commit(#{status := committed}) ->
    {ok, []};
commit(#{status := Status}) ->
    {error, Status}.

%%

-spec cancel(transfer()) ->
    {ok, [event()]}
    | {error, {status, created | committed}}.
cancel(#{status := prepared} = Transfer) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun() ->
        _PostingPlanLog = unwrap(ff_accounting:cancel_trx(ID, construct_trx_postings(CashFlow))),
        [{status_changed, cancelled}]
    end);
cancel(#{status := cancelled}) ->
    {ok, []};
cancel(#{status := Status}) ->
    {error, {status, Status}}.

%%

-spec apply_event(event(), ff_maybe:'maybe'(transfer())) -> transfer().
apply_event({created, Transfer}, undefined) ->
    Transfer;
apply_event({status_changed, S}, Transfer) ->
    Transfer#{status => S}.

%%

-spec construct_trx_postings(final_cash_flow()) -> [ff_accounting:posting()].
construct_trx_postings(#{postings := Postings}) ->
    lists:map(fun construct_trx_posting/1, Postings).

-spec construct_trx_posting(ff_cash_flow:final_posting()) -> ff_accounting:posting().
construct_trx_posting(Posting) ->
    #{
        sender := #{account := Sender},
        receiver := #{account := Receiver},
        volume := Volume
    } = Posting,
    SenderAccount = ff_account:account_id(Sender),
    ReceiverAccount = ff_account:account_id(Receiver),
    {SenderAccount, ReceiverAccount, Volume}.
