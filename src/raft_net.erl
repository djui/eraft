%% @doc Networking behaviour.
-module(raft_net).

%%% Callbacks ==================================================================
-callback(init(term()) -> term()).
-callback(send(term(), term(), term()) -> term()).

%%% Exports ====================================================================
-export([ init/1
        , send/3
        ]).

%%% Code =======================================================================
init(Config) ->
  HandlerMod = proplists:get_value(net_handler, Config),
  {HandlerMod, HandlerMod:init(Config)}.

send(To, Msg, HandlerMod) ->
  HandlerMod:send(To, Msg).
