%% @doc TCP networking backend.
-module(raft_net_tcp).

%% API
-export([ init/1
        , send/3
        ]).

%%% API ========================================================================
init(_Config) ->
  undefined.

send(_To, _Msg, _State) ->
  undefined.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
