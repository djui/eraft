-module(raft).

-export([ start/0
        , stop/0
        ]).

%%% API ========================================================================
start() ->
  application:start(raft).

stop() ->
  application:stop(raft).
