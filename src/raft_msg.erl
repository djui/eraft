%% @doc Messaging behaviour.
-module(raft_msg).

%%% Callbacks ==================================================================
-callback(init(term()) -> term()).

%%% Exports ====================================================================
-export([ init/1 ]).

%%% Code =======================================================================
init(Config) ->
  HandlerMod = proplists:get_value(msg_handler, Config),
  {HandlerMod, HandlerMod:init(Config)}.
