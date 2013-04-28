%% @doc Leveldb logging backend.
-module(raft_log_leveldb).

%% API
-export([ init/1 ]).

%%% API ========================================================================
init(_Config) ->
  undefined.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
