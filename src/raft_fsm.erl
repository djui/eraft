%% @doc Finite-State machine. Keeps server state with the state `follower`,
%% `candidate`, `leader`.
-module(raft_fsm).
-behaviour(gen_fsm).

%% API
-export([ start_link/1
        ]).

%% Callbacks
-export([ init/1
        , initial/2
        , follower/2
        , candidate/2
        , leader/2
        , handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

%%% Macros =====================================================================
-define(FSM, ?MODULE).

%%% Records ====================================================================
-type(replica()      :: string()).
-type(current_term() :: pos_integer()).

%%% Records ====================================================================
-record(tate, { msg_state        :: {module(), term()}
              , net_state        :: {module(), term()}
              , log_state        :: {module(), term()}
              , current_term = 1 :: current_term()
              , voted_for        :: replica()
              }).

%%% API ========================================================================
start_link(Config) ->
  gen_fsm:start_link({local, ?FSM}, ?MODULE, Config, []).

%%% States =====================================================================
initial(timeout, Config) ->
  {next_state, follower, defer_init(Config)}.

%%% Follower -------------------------------------------------------------------
follower({election_timeout, _Election}, S=#tate{msg_state=NetState}) ->
  raft_net:send('TODO', 'TODO', NetState),
  {next_state, candidate, S}.

%%% Candidate ------------------------------------------------------------------
candidate({election_timeout, _Election}, S) ->
  {next_state, candidate, S};

candidate({higher_term, _Term}, S) ->
  {next_state, follower, S};

candidate({append_entries, _Entries}, S) ->
  {next_state, follower, S};

candidate({majority, _Votes}, S) ->
  {next_state, leader, S}.

%%% Leader ---------------------------------------------------------------------
leader({higher_term, _Term}, S) ->
  {next_state, follower, S}.

%%% Internals ==================================================================
defer_init(Config) ->
  init_handlers(Config).

init_handlers(Config) ->
  #tate{ msg_state = raft_msg:init(Config)
       , net_state = raft_net:init(Config)
       , log_state = raft_log:init(Config)
       }.

%%% Callbacks ==================================================================
init(Config) ->
  {ok, initial, Config, 0}.

handle_event(_Event, StateName, S) ->
  io:format("handle_event: ~p|~p~n", [_Event, StateName]),
  {next_state, StateName, S}.

handle_sync_event(_Event, _From, StateName, S) ->
  io:format("handle_sync_event: ~p|~p~n", [_Event, StateName]),
  Reply = ok,
  {reply, Reply, StateName, S}.

handle_info(_Info, StateName, S) ->
  io:format("handle_info: ~p|~p~n", [_Info, StateName]),
  {next_state, StateName, S}.

terminate(_Reason, _StateName, _S) ->
  ok.

code_change(_OldVsn, StateName, S, _Extra) ->
  {ok, StateName, S}.

%%% TESTS ======================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
