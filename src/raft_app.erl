%% @doc Main application.
-module(raft_app).
-behaviour(application).

%% API
-export([ start/2
        , stop/1
        ]).

%%% API ========================================================================
start(_StartType, _StartArgs) ->
  raft_sup:start_link().

stop(_State) ->
  ok.
