%% @doc Main application interface.
-module(raft).

-export([ start/0
        , stop/0
        ]).

%%% API ========================================================================
start() ->
  ehf_application:ensure_started(raft).

stop() ->
  application:stop(raft).
