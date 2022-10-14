%%%
%%% Test machine options
%%%

-module(ff_ct_machine_options).

%% For mock in test cases
-export([get_options/1]).

-type handler() :: machinery:modopts(_).

-spec get_options(handler()) -> list(atom()).
get_options(test_handler) ->
    [test_activity];
get_options(_Handler) ->
    [].
