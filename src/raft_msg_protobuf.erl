%% @doc Protobuf messaging backend.
-module(raft_msg_protobuf).

%% API
-export([ init/1 ]).

%%% API ========================================================================
init(_Config) ->
  undefined.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
