%% @doc Top level supervisor.
-module(raft_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0 ]).

%% Callbacks
-export([ init/1 ]).

%%% API ========================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Callbacks ==================================================================
init([]) ->
  ehf_supervisor:supervision_for_all([ ehf_supervisor:work(raft_fsm) ]).
