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
  MsgConf = [],
  NetConf = [],
  LogConf = [],
  FsmConf = [],
  ehf_supervisor:supervision_for_all([ ehf_supervisor:work(raft_msg, [MsgConf])
                                     , ehf_supervisor:work(raft_net, [NetConf])
                                     , ehf_supervisor:work(raft_log, [LogConf])
                                     , ehf_supervisor:work(raft_fsm, [FsmConf])
                                     ]).
