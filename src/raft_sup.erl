%% @doc Top level supervisor.
-module(raft_sup).
-behaviour(supervisor).

%% API
-export([ start_link/1 ]).

%% Callbacks
-export([ init/1 ]).

%%% API ========================================================================
start_link(Conf) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).

%%% Callbacks ==================================================================
init(Conf) ->
  ehf_supervisor:supervision_for_one([ ehf_supervisor:work(raft_fsm, [Conf]) ]).
