%% @doc Erlang distribution networking backend.
-module(raft_net_erl_dist).

%% API
-export([ init/1
        , send/3
        ]).

%%% API ========================================================================
init(Config) ->
  Nodes = proplists:get_value(nodes, Config),
  lists:foreach(fun net_kernel:connect_node/1, Nodes),
  undefined.

send(_To, _Msg, _State) ->
  undefined.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
