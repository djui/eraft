%% @doc Disk_log logging backend.
-module(raft_log_disk_log).

%% API
-export([ init/1 ]).

%%% API ========================================================================
init(_Config) ->
  undefined.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
