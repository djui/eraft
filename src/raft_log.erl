%% @doc Logging behaviour.
-module(raft_log).

%%% Callbacks ==================================================================
-callback(init(term()) -> term()).

%%% Exports ====================================================================
-export([ init/1 ]).

%%% Code =======================================================================
init(Config) ->
  HandlerMod = proplists:get_value(log_handler, Config),
  {HandlerMod, HandlerMod:init(Config)}.
