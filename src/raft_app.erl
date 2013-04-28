%% @doc Main application.
-module(raft_app).
-behaviour(application).

%% API
-export([ start/2
        , stop/1
        ]).

%%% API ========================================================================
start(normal, _StartArgs) ->
  Conf = application:get_all_env(raft),
  raft_sup:start_link(Conf).

stop(_State) ->
  ok.
