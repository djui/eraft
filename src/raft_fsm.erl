%% @doc Finite-State machine. Keeps server state with the state `follower`,
%% `candidate`, `leader`.
-module(raft_fsm).
-behaviour(gen_fsm).

%% API
-export([ start_link/1
        ]).

%% Callbacks
-export([ init/1
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
-record(state, { start_args   = [] :: [_]
               , current_term = 1  :: current_term()
               , voted_for         :: replica()
               }).

%%% API ========================================================================
start_link(Args) ->
  gen_fsm:start_link({local, ?FSM}, ?MODULE, Args, []).

%%% States =====================================================================
%%% Follower -------------------------------------------------------------------
follower(timeout, State) ->
  deferred_init(State#state.start_args),
  {next_state, follower, State};

follower({election_timeout, _Election}, State) ->
  {next_state, candidate, State}.

%%% Candidate ------------------------------------------------------------------
candidate({election_timeout, _Election}, State) ->
  {next_state, candidate, State};

candidate({higher_term, _Term}, State) ->
  {next_state, follower, State};

candidate({append_entries, _Entries}, State) ->
  {next_state, follower, State};

candidate({majority, _Votes}, State) ->
  {next_state, leader, State}.

%%% Leader  --------------------------------------------------------------------
leader({higher_term, _Term}, State) ->
  {next_state, follower, State}.

%%% Internals ==================================================================
deferred_init(Args) ->
  Args.

%%% Callbacks ==================================================================
init(Args) ->
  {ok, follower, #state{start_args=Args}, 0}.

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
