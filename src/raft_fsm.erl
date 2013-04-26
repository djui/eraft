%% @doc Finite-State machine. Keeps server state with the state `follower`,
%% `candidate`, `leader`.
-module(raft_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% Callbacks
-export([ init/1
        , state_name/2
        , state_name/3
        , handle_event/3
          , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

%%% Macros =====================================================================
-define(SERVER, ?MODULE).

%%% Records ====================================================================
-record(state, {}).

%%% API ========================================================================
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% Callbacks ==================================================================
init([]) ->
  {ok, state_name, #state{}}.

state_name(_Event, State) ->
  {next_state, state_name, State}.

state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
